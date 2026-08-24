{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Rules.Import.Graph
-- Description : Neo4j graph database connection and cadence storage
--
-- Write operations for storing cadence nodes and transition edges during
-- data ingestion, plus a re-export of 'connectNeo4j' (the connection itself
-- lives in "Harmonic.Database").

module Harmonic.Rules.Import.Graph (
    -- * Connection
    connectNeo4j,

    -- * Schema
    initGraph, truncateCadenceGraph,

    -- * Writing cadence transitions
    ComposerWeights, writeCadenceEdges, buildQuery,

    -- * Cypher field rendering
    showText, movementText, chordText, dissonanceText,
    confidenceText, weightsLiteral,
) where

import           Harmonic.Database (DbActionT, connectNeo4j, runQuery)
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Evaluation.Scoring.Dissonance as D

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T

-- | How much each composer contributes to one @NEXT@ edge. Written to the
-- edge as a JSON literal by 'weightsLiteral', and summed into a single
-- @confidence@ property by 'confidenceText'.
type ComposerWeights = Map T.Text Double

-- |Initialise schema. Node identity is the @show@ string (movement +
-- functionality) — the functionality half of every key follows the naming
-- contract documented at the head of "Harmonic.Rules.Import.Transform"
-- (the live DB carries legacy names; read the warning there BEFORE any
-- re-ingestion).
initGraph :: DbActionT ()
initGraph = do
  _ <- runQuery "CALL apoc.schema.assert({}, {})"
  _ <- runQuery "CREATE CONSTRAINT IF NOT EXISTS FOR (n:Cadence) REQUIRE n.show IS UNIQUE"
  pure ()

-- | Delete every @Cadence@ node and its @NEXT@ edges, batched through
-- @apoc.periodic.iterate@ to avoid memory spikes on a full corpus. Run before
-- a re-ingestion.
truncateCadenceGraph :: DbActionT ()
truncateCadenceGraph = do
  _ <- runQuery deleteCadences
  pure ()
  where
    deleteCadences = T.unlines
      [ "CALL apoc.periodic.iterate("
      , "  \"MATCH (n:Cadence) RETURN n\"," -- batch MATCH avoids memory spikes
      , "  \"DETACH DELETE n\"," -- deletes cadences plus NEXT edges
      , "  {batchSize: 5000, parallel: true}"
      , ")"
      ]

-- | Write a batch of cadence transitions. Each triple merges both endpoint
-- nodes and the @NEXT@ edge between them. Transitions with no composer weight
-- are skipped rather than written with zero confidence.
writeCadenceEdges :: [(H.Cadence, H.Cadence, ComposerWeights)] -> DbActionT ()
writeCadenceEdges = mapM_ writeOne
  where
    writeOne (fromCadence, toCadence, weights)
      | Map.null weights = pure ()
      | otherwise = runQuery (buildQuery fromCadence toCadence weights) >> pure ()

-- | Build the Cypher @MERGE@ for one transition. Node identity is the @show@
-- string; see 'initGraph' for the naming contract that governs it.
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

-- | Node identity: the cadence's @show@ string, used as the @MERGE@ key.
showText :: H.Cadence -> T.Text
showText = T.pack . show

-- | The movement half of a cadence, as a Cypher string value.
movementText :: H.Cadence -> T.Text
movementText cadence =
  let (movement, _) = H.deconstructCadence cadence
   in T.pack (show movement)

-- | The chord half of a cadence, as a Cypher string value.
chordText :: H.Cadence -> T.Text
chordText cadence =
  let (_, chord) = H.deconstructCadence cadence
   in T.pack (show chord)

-- | The cadence's dissonance level, as a Cypher numeric value. Computed at
-- write time so queries can filter on it without recomputing.
dissonanceText :: H.Cadence -> T.Text
dissonanceText cadence =
  let (_, chord) = H.deconstructCadence cadence
      ints = fmap P.unPitchClass chord
      (value, _) = D.dissonanceLevel ints
   in T.pack (show value)

-- | Total edge weight across all composers, stored as @r.confidence@. This is
-- what a @\"*\"@ (all-composers) query ranks on.
confidenceText :: ComposerWeights -> T.Text
confidenceText weights = T.pack . show $ sum (Map.elems weights)

-- | Per-composer weights as a JSON literal, stored as @r.weights@. Single
-- composer and blend queries read this rather than @r.confidence@.
weightsLiteral :: ComposerWeights -> T.Text
weightsLiteral weights =
  let entries = Map.toList weights
      pieces = map formatEntry entries
   in T.concat ["'", "{", T.intercalate "," pieces, "}", "'"]
  where
    formatEntry (name, value) = T.concat ["\"", name, "\":", T.pack (show value)]
