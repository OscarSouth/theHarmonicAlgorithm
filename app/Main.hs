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
import qualified Harmonic.Rules.Import.Jazz as J
import qualified Data.Text.IO as TIO
import           System.Directory (listDirectory)
import           System.Environment (getArgs, lookupEnv)
import           System.FilePath ((</>), takeBaseName)
import           Data.Maybe (fromMaybe)
import           Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Text (Text)

-- | Curation knob: composer keys (post-'slug') to EXCLUDE from the graph.
-- Empty by default — every composer in the artefact is ingested, and every
-- exclusion is reported at run time.
--
-- Curation is an explicit exclusion rather than an allow-list because an
-- allow-list keyed on normalised names drops composers silently whenever
-- the list and the normaliser drift out of step; an exclude list fails
-- open and every drop it does cause is printed.
composerExclude :: [Text]
composerExclude = []

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["jazz"] -> jazzMain
    []       -> classicalMain
    _        -> putStrLn "usage: theHarmonicAlgorithm-exe [jazz]"

-- | Ingest the Bunks jazz corpus into the @Change@ subgraph. Reads the
-- corpus from @HA_JAZZ_CORPUS@ (default: ~\/musicdata\/Jazz-Chord-
-- Progressions-Corpus). Label-scoped: only @Change@ nodes are truncated
-- and written; resident @Cadence@ data is untouched. Refused songs are
-- reported and counted, never silently dropped.
jazzMain :: IO ()
jazzMain = do
  home <- fromMaybe "/Users/oscarsouth" <$> lookupEnv "HOME"
  corpus <- fromMaybe (home </> "musicdata/Jazz-Chord-Progressions-Corpus")
              <$> lookupEnv "HA_JAZZ_CORPUS"
  let songRoot = corpus </> "SongDB"
  dirs <- filter (\d -> take 5 d == "Songs") <$> listDirectory songRoot
  files <- concat <$> mapM (\d -> map ((songRoot </> d) </>)
                              <$> listDirectory (songRoot </> d)) dirs
  putStrLn $ "theHarmonicAlgorithm: jazz ingest from " ++ songRoot
           ++ " (" ++ show (length files) ++ " files)"
  songs <- mapM (\f -> J.parseSong (T.pack (takeBaseName f)) <$> TIO.readFile f) files
  let refused = [ r | Left r <- songs ]
      parsed  = [ s | Right s <- songs ]
  forM_ refused $ \r ->
    putStrLn $ "  REFUSED: " ++ T.unpack (J.refusalInput r)
             ++ " (" ++ T.unpack (J.refusalReason r) ++ ")"
  putStrLn $ "Parsed songs: " ++ show (length parsed)
           ++ "  refusals: " ++ show (length refused)
  let edges = J.buildChangeEdges parsed
  putStrLn $ "Change edges: " ++ show (length edges)
  pipe <- connectNeo4j
  putStrLn "Clearing existing Change subgraph (label-scoped)..."
  runDb pipe truncateChangeGraph
  runDb pipe initChangeGraph
  putStrLn "Writing Change edges..."
  runDb pipe (writeChangeEdges edges)
  putStrLn "Jazz ingest complete."

classicalMain :: IO ()
classicalMain = do
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
  -- One composer at a time, so only a single composer's counts are
  -- resident at once — logging across the whole map first would force
  -- every composer's data to stay live simultaneously.
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
