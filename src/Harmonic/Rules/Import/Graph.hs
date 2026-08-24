-- |
-- Module      : Harmonic.Rules.Import.Graph
-- Description : Neo4j graph database connection and cadence storage
--
-- Write operations for storing cadence nodes and transition edges during
-- data ingestion, plus a re-export of 'connectNeo4j' (the connection itself
-- lives in "Harmonic.Database").
--
-- Writes are BATCHED and PARAMETERISED: edges travel as a @$rows@ JSON
-- parameter into one @UNWIND@ + @MERGE@ statement per batch, so a full
-- corpus write is a few hundred transactional round trips rather than one
-- auto-committing request per edge, and no value is ever spliced into
-- Cypher text.

module Harmonic.Rules.Import.Graph (
    -- * Connection
    connectNeo4j,

    -- * Schema
    initGraph, truncateCadenceGraph,

    -- * Writing cadence transitions
    ComposerWeights, writeCadenceEdges, edgeRow, batchCypher,
) where

import           Harmonic.Database (DbActionT, connectNeo4j, runQuery, runQueryP)
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Evaluation.Scoring.Dissonance as D

import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | How much each composer contributes to one @NEXT@ edge. Serialised to a
-- JSON object string on the edge's @weights@ property (sparse: absent
-- composer = weight 0), and summed into a single @confidence@ property.
type ComposerWeights = Map T.Text Double

-- |Initialise schema: the uniqueness constraint on the @show@ node key.
-- Idempotent (@IF NOT EXISTS@) and non-destructive — it touches nothing
-- else in the database. Node identity is the @show@ string (movement +
-- functionality); the functionality half of every key follows the naming
-- contract documented at the head of "Harmonic.Rules.Import.Transform".
initGraph :: DbActionT ()
initGraph = do
  _ <- runQuery "CREATE CONSTRAINT IF NOT EXISTS FOR (n:Cadence) REQUIRE n.show IS UNIQUE"
  pure ()

-- | Delete every @Cadence@ node and its @NEXT@ edges. Run before a
-- re-ingestion. The cadence graph is small by construction (at most
-- 55 zero-forms x 12 movements = 660 nodes), so a plain @DETACH DELETE@
-- suffices — no batching machinery required.
truncateCadenceGraph :: DbActionT ()
truncateCadenceGraph = do
  _ <- runQuery "MATCH (n:Cadence) DETACH DELETE n"
  pure ()

-- | Write cadence transitions in parameterised batches of 1000 rows.
-- Each batch is one transactional @UNWIND@ round trip; @MERGE@ keeps every
-- row idempotent, so a failed run can simply be re-run. Node properties
-- are set only @ON CREATE@ (they are pure functions of the node key, so
-- re-setting them per edge would be redundant writes). Transitions with
-- no composer weight are skipped rather than written with zero confidence.
writeCadenceEdges :: [(H.Cadence, H.Cadence, ComposerWeights)] -> DbActionT ()
writeCadenceEdges edges =
  mapM_ writeBatch (filter (not . null) (chunksOf 1000 (filter hasWeight edges)))
  where
    hasWeight (_, _, weights) = not (Map.null weights)
    writeBatch batch = do
      let rows = A.toJSON (map edgeRow batch)
      _ <- runQueryP batchCypher (Map.singleton "rows" rows)
      pure ()
    chunksOf n xs = case splitAt n xs of
      (chunk, [])   -> [chunk]
      (chunk, rest) -> chunk : chunksOf n rest

-- | The one write statement: unwind the batch, merge both endpoint nodes
-- and the @NEXT@ edge, stamp the edge's confidence and per-composer
-- weights.
batchCypher :: T.Text
batchCypher = T.unlines
  [ "UNWIND $rows AS row"
  , "MERGE (from:Cadence {show: row.fromShow})"
  , "  ON CREATE SET from.movement = row.fromMovement,"
  , "                from.chord = row.fromChord,"
  , "                from.dissonance = row.fromDissonance"
  , "MERGE (to:Cadence {show: row.toShow})"
  , "  ON CREATE SET to.movement = row.toMovement,"
  , "                to.chord = row.toChord,"
  , "                to.dissonance = row.toDissonance"
  , "MERGE (from)-[r:NEXT]->(to)"
  , "SET r.confidence = row.confidence, r.weights = row.weights"
  ]

-- | One edge as a @$rows@ element. Node fields are pure functions of the
-- cadence (the @show@ key, its movement and chord halves, and the
-- dissonance level computed at write time so queries can filter on it).
-- @weights@ is the JSON object string the read side
-- ('Harmonic.Evaluation.Database.Query.parseWeightsJson') parses back;
-- @confidence@ is the sum of all composer weights (what a @"*"@ query
-- ranks on).
edgeRow :: (H.Cadence, H.Cadence, ComposerWeights) -> A.Value
edgeRow (fromCadence, toCadence, weights) = A.object
  [ "fromShow"       .= show fromCadence
  , "fromMovement"   .= show (fst (H.deconstructCadence fromCadence))
  , "fromChord"      .= show (snd (H.deconstructCadence fromCadence))
  , "fromDissonance" .= dissonanceOf fromCadence
  , "toShow"         .= show toCadence
  , "toMovement"     .= show (fst (H.deconstructCadence toCadence))
  , "toChord"        .= show (snd (H.deconstructCadence toCadence))
  , "toDissonance"   .= dissonanceOf toCadence
  , "confidence"     .= sum (Map.elems weights)
  , "weights"        .= TE.decodeUtf8 (BL.toStrict (A.encode weights))
  ]
  where
    dissonanceOf cadence =
      let (_, chord) = H.deconstructCadence cadence
          (value, _) = D.dissonanceLevel (fmap P.unPitchClass chord)
       in value
