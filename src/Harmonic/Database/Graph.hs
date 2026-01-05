{-# LANGUAGE OverloadedStrings #-}
module Harmonic.Database.Graph where

import           Harmonic.Config
import qualified Harmonic.Core.Harmony as H
import qualified Harmonic.Core.Pitch as P
import qualified Harmonic.Core.Dissonance as D

import qualified Database.Bolt as Bolt
import           Data.Default (def)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T

type ComposerWeights = Map T.Text Double

initGraph :: Bolt.BoltActionT IO ()
initGraph = do
  _ <- Bolt.query "CALL apoc.schema.assert({}, {})"
  _ <- Bolt.query "CREATE CONSTRAINT IF NOT EXISTS FOR (n:Cadence) REQUIRE n.show IS UNIQUE"
  pure ()

truncateCadenceGraph :: Bolt.BoltActionT IO ()
truncateCadenceGraph = do
  _ <- Bolt.query deleteCadences
  pure ()
  where
    deleteCadences = T.unlines
      [ "CALL apoc.periodic.iterate("
      , "  \"MATCH (n:Cadence) RETURN n\"," -- batch MATCH avoids memory spikes
      , "  \"DETACH DELETE n\"," -- deletes cadences plus NEXT edges
      , "  {batchSize: 5000, parallel: true}"
      , ")"
      ]

writeCadenceEdges :: [(H.Cadence, H.Cadence, ComposerWeights)] -> Bolt.BoltActionT IO ()
writeCadenceEdges = mapM_ writeOne
  where
    writeOne (fromCadence, toCadence, weights)
      | Map.null weights = pure ()
      | otherwise = Bolt.query (buildQuery fromCadence toCadence weights) >> pure ()

buildQuery :: H.Cadence -> H.Cadence -> ComposerWeights -> T.Text
buildQuery fromCadence toCadence weights =
  T.concat
    [ "MERGE (from:Cadence {show: '", showText fromCadence, "'}) "
    , "SET from.movement = '", movementText fromCadence, "', from.chord = '", chordText fromCadence
    , "', from.dissonance = ", dissonanceText fromCadence, " "
    , "MERGE (to:Cadence {show: '", showText toCadence, "'}) "
    , "SET to.movement = '", movementText toCadence, "', to.chord = '", chordText toCadence
    , "', to.dissonance = ", dissonanceText toCadence, " "
    , "MERGE (from)-[r:NEXT]->(to) "
    , "SET r.confidence = ", confidenceText weights
    , ", r.weights = ", weightsLiteral weights
    ]

showText :: H.Cadence -> T.Text
showText = T.pack . show

movementText :: H.Cadence -> T.Text
movementText cadence =
  let (movement, _) = H.deconstructCadence cadence
   in T.pack (show movement)

chordText :: H.Cadence -> T.Text
chordText cadence =
  let (_, chord) = H.deconstructCadence cadence
   in T.pack (show chord)

dissonanceText :: H.Cadence -> T.Text
dissonanceText cadence =
  let (_, chord) = H.deconstructCadence cadence
      ints = fmap P.unPitchClass chord
      (value, _) = D.dissonanceLevel ints
   in T.pack (show value)

confidenceText :: ComposerWeights -> T.Text
confidenceText weights = T.pack . show $ sum (Map.elems weights)

weightsLiteral :: ComposerWeights -> T.Text
weightsLiteral weights =
  let entries = Map.toList weights
      pieces = map formatEntry entries
   in T.concat ["'", "{", T.intercalate "," pieces, "}", "'"]
  where
    formatEntry (name, value) = T.concat ["\"", name, "\":", T.pack (show value)]

queryNextCadences :: T.Text -> Bolt.BoltActionT IO [T.Text]
queryNextCadences _ = pure []

connectNeo4j :: IO Bolt.Pipe
connectNeo4j = Bolt.connect $ def
  { Bolt.user = neo4jUser
  , Bolt.password = neo4jPassword
  , Bolt.host = "localhost"
  , Bolt.port = 7687
  }
