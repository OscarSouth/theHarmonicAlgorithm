-- |
-- Module      : Harmonic.Database
-- Description : Neo4j connection and query transport over the HTTP Query API
--
-- The single place the codebase talks to Neo4j. Queries go over the modern
-- HTTP endpoint (@POST \/db\/neo4j\/query\/v2@, Neo4j 5.23+\/2025.x), which
-- replaced both the Bolt binary protocol dependency and the deprecated
-- @tx\/commit@ HTTP API. HTTP keep-alive pooling in the shared
-- 'Network.HTTP.Client.Manager' plays the role a long-lived Bolt pipe used
-- to: one 'DbConn' serves a whole generation run, including the full
-- K-attempt loop under @attempt N K@.
--
-- Why not Bolt: the last maintained Haskell Bolt driver speaks protocol 3.0
-- only, which Neo4j 5 removed — it pinned this project to the EOL Neo4j 4.4.
-- The HTTP endpoint is perf-neutral here (measured ~7.5ms vs ~10ms per
-- generation step on the hottest node, including JSON parsing).
--
-- Rows come back as @Map Text Aeson.Value@ — the same field-keyed shape the
-- old driver produced — so query-site parsing stays a lookup plus a pattern
-- match.

module Harmonic.Database (
    -- * Connection
    DbConn,
    connectNeo4j,
    connectNeo4jAt,

    -- * Running actions
    DbActionT,
    runDb,

    -- * Queries
    runQuery,
    runQueryP,
) where

import           Control.Exception (throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
import           System.Environment (lookupEnv)

import           Harmonic.Config (neo4jUri, neo4jUser, neo4jPassword)

-- | An open connection to Neo4j: a keep-alive HTTP manager plus the
-- pre-built request template (endpoint URL and auth header).
data DbConn = DbConn
  { dcManager :: Manager
  , dcRequest :: Request  -- ^ POST template for the query endpoint
  }

-- | Database actions, threaded over a 'DbConn'. Run with 'runDb'.
-- @ReaderT@ keeps call sites shaped exactly like the old Bolt action monad:
-- query functions compose in the same do-blocks and @liftIO@ works as before.
type DbActionT = ReaderT DbConn IO

-- | Run a database action against a connection.
runDb :: DbConn -> DbActionT a -> IO a
runDb conn action = runReaderT action conn

-- | Connect to the local Neo4j from "Harmonic.Config" (override with the
-- @HA_NEO4J_URL@ environment variable, e.g. @http:\/\/localhost:7477@ to
-- point a REPL at a scratch container). Probes the server with @RETURN 1@
-- so an unreachable database surfaces the error here, at connect time —
-- matching what every online generation path expects.
connectNeo4j :: IO DbConn
connectNeo4j = do
  override <- lookupEnv "HA_NEO4J_URL"
  connectNeo4jAt (maybe (T.unpack neo4jUri) id override)

-- | 'connectNeo4j' against an explicit base URL (no trailing slash).
connectNeo4jAt :: String -> IO DbConn
connectNeo4jAt base = do
  manager <- newManager defaultManagerSettings
  template <- parseUrlThrow (base ++ "/db/neo4j/query/v2")
  let request = applyBasicAuth (TE.encodeUtf8 neo4jUser) (TE.encodeUtf8 neo4jPassword)
              $ template
                  { method = "POST"
                  , requestHeaders = ("Content-Type", "application/json")
                                   : requestHeaders template
                  }
      conn = DbConn manager request
  _ <- runDb conn (runQuery "RETURN 1")
  pure conn

-- | Run a Cypher query with no parameters.
runQuery :: Text -> DbActionT [Map Text A.Value]
runQuery cypher = runQueryP cypher Map.empty

-- | Run a Cypher query with parameters. Each result row is keyed by the
-- RETURN field names. Server-side errors (Cypher failures, auth) are thrown
-- as 'IOError's carrying the Neo4j error message.
runQueryP :: Text -> Map Text A.Value -> DbActionT [Map Text A.Value]
runQueryP cypher params = do
  conn <- ask
  liftIO $ do
    let payload = A.object
          [ "statement"  A..= cypher
          , "parameters" A..= A.object
              (Map.foldrWithKey (\k v acc -> (Key.fromText k A..= v) : acc) [] params)
          ]
        request = (dcRequest conn)
          { requestBody = RequestBodyLBS (A.encode payload)
          , checkResponse = \_ _ -> pure ()  -- surface 4xx bodies ourselves
          }
    response <- httpLbs request (dcManager conn)
    decodeRows (responseBody response)

-- | Parse a query\/v2 response: @{"data": {"fields": [...], "values": [[...]]}}@
-- on success, @{"errors": [{"code", "message"}]}@ on failure.
decodeRows :: BL.ByteString -> IO [Map Text A.Value]
decodeRows body =
  case A.decode body of
    Nothing -> failWith ("unparseable response: " <> preview)
    Just (A.Object o)
      | Just (A.Array errs) <- KM.lookup "errors" o
      , not (null errs) ->
          failWith (T.intercalate "; " (map errText (foldr (:) [] errs)))
      | Just (A.Object d) <- KM.lookup "data" o
      , Just (A.Array fieldsV) <- KM.lookup "fields" d
      , Just (A.Array valuesV) <- KM.lookup "values" d ->
          let fields = [ f | A.String f <- foldr (:) [] fieldsV ]
              row vs = Map.fromList (zip fields (foldr (:) [] vs))
          in pure [ row vs | A.Array vs <- foldr (:) [] valuesV ]
    _ -> failWith ("unexpected response shape: " <> preview)
  where
    preview = TE.decodeUtf8 (BS8.take 200 (BL.toStrict body))
    errText (A.Object e)
      | Just (A.String m) <- KM.lookup "message" e = m
    errText v = T.pack (show v)
    failWith msg = throwIO (userError ("Neo4j query failed: " <> T.unpack msg))
