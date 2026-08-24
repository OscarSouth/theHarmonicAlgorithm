{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : YCACL corpus ingestion — CSV artefact to Neo4j cadence graph
--
-- Run-once executable (@stack run@). Reads the artefact produced by
-- @scripts\/export_ycacl.R@ and rebuilds the @:Cadence@ \/ @:NEXT@ graph.
-- See @app\/README.md@ for the pipeline stages and
-- "Harmonic.Rules.Import.Transform" for the counting semantics and the
-- node-key naming contract.
--
-- DESTRUCTIVE: the cadence subgraph at @HA_NEO4J_URL@ (default: the local
-- live database) is truncated before the first edge is written. Point it
-- at a scratch container and compare before promoting.
module Main where

import           Harmonic.Config
import           Harmonic.Rules.Import.Graph
import           Harmonic.Rules.Import.CSV
import           Harmonic.Rules.Import.Transform
import           Harmonic.Rules.Import.Merge
import           Harmonic.Evaluation.Analysis.Markov (probabilitiesFromCounts)

import           Harmonic.Database (runDb)
import           Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Text (Text)

-- | Curation knob: composer keys (post-'slug') to EXCLUDE from the graph.
-- Empty by default — every composer in the artefact is ingested, and every
-- exclusion is reported at run time.
--
-- The historical 574-entry machine-generated allow-list that lived here was
-- deleted (2026-08-24): it was generated under the R exporter's normaliser
-- (strip all non-alphanumerics), so 22 composers whose 'slug' keys never
-- matched it — including both Strausses, de Falla, Nunes Garcia and three
-- Bach sons, 96,133 slices in total — were dropped silently, while 105 of
-- its entries matched nothing in the artefact at all. Curation, when it is
-- wanted, is now an explicit and reported exclusion rather than an
-- unreconciled allow-list.
composerExclude :: [Text]
composerExclude = []

main :: IO ()
main = do
  putStrLn "theHarmonicAlgorithm: Populating Neo4j graph from YCACL artifact"
  dataset <- loadYCACLData ycaclArtifactPath
  let normalized = normalizeComposers dataset
      (activeComposers, droppedComposers) = filterComposers [] composerExclude normalized
  putStrLn $ "Active composers: " ++ show (Map.size activeComposers)
  forM_ droppedComposers $ \(name, pieceCount) ->
    putStrLn $ "  REFUSED (excluded composer key): "
             ++ T.unpack name ++ " (" ++ show pieceCount ++ " pieces)"
  putStrLn "Raw YCACL coverage by composer:"
  logRawComposerStats activeComposers

  putStrLn "Deriving cadences and Markov transitions per composer:"
  -- One composer at a time: build the cadence stream, log its size, fold
  -- it into transition probabilities, and release it before moving on.
  -- (The previous two-pass shape logged all cadence counts before any
  -- folding, which kept every composer's expanded stream simultaneously
  -- resident — a ~20-25 GB peak on the full artefact.)
  composerTransitions <- flip Map.traverseWithKey activeComposers $ \name pieces -> do
    let counts      = buildTransitionCountsPerPiece (Map.elems pieces)
        transitions = probabilitiesFromCounts counts
    putStrLn $ "    edges -> " ++ T.unpack name ++ ": " ++ show (Map.size transitions)
    -- force the per-edge probabilities now so no thunk retains this
    -- composer's counts/totals maps into the merge phase
    Map.foldl' (\acc v -> v `seq` acc) () transitions `seq` pure transitions

  let edges = mergeComposerTransitions composerTransitions

  pipe <- connectNeo4j
  -- Truncate the cadence subgraph each run so composer-specific MERGEs
  -- remain deterministic (660 nodes — a plain DETACH DELETE).
  putStrLn "Clearing existing cadences from Neo4j..."
  runDb pipe truncateCadenceGraph
  runDb pipe initGraph
  putStrLn $ "Writing " ++ show (length edges) ++ " transitions into Neo4j..."
  runDb pipe (writeCadenceEdges edges)
  putStrLn $ "Wrote " ++ show (length edges) ++ " transitions"

-------------------------------------------------------------------------------
-- Logging Utilities
-------------------------------------------------------------------------------

logRawComposerStats :: ComposerPieces -> IO ()
logRawComposerStats composers =
  forM_ (Map.toList composers) $ \(name, pieces) ->
    let pieceCount = Map.size pieces
        chordCount = sum (map length (Map.elems pieces))
     in putStrLn $ "  - " ++ T.unpack name ++ ": " ++ show pieceCount ++ " pieces / " ++ show chordCount ++ " chord events"
