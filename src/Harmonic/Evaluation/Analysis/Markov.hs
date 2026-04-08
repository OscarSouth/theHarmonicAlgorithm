-- |
-- Module      : Harmonic.Evaluation.Analysis.Markov
-- Description : Markov transition probability computation (ingestion-only)
--
-- This module is used exclusively during data ingestion (@app\/Main.hs@) to
-- compute transition probabilities from the YCACL corpus. These probabilities
-- are stored as edge weights in the Neo4j graph database.
--
-- This module is NOT used in the runtime generation path. At runtime,
-- transition weights are read from Neo4j edges by
-- "Harmonic.Evaluation.Database.Query".
--
-- == Academic Lineage
--
-- /Data Science In The Creative Process/ (South, 2018), Section: Markov
-- module. The Markov chain approach resolves "Generative Uninspiration" —
-- the problem of manually traversing exhaustive overtone combination charts
-- — by training transition probabilities on the Yale Classical Archives
-- Corpus (Bach chorales and other composers).

module Harmonic.Evaluation.Analysis.Markov
  ( Edge
  , TransitionCounts
  , transitionCounts
  , transitionProbabilities
  ) where

import           Harmonic.Rules.Types.Harmony (Cadence)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.List (foldl')

-- |Representation of a transition between cadences.
type Edge = (Cadence, Cadence)

type TransitionCounts = Map Edge Double

type Totals = Map Cadence Double

transitionCounts :: [Cadence] -> TransitionCounts
transitionCounts cadences =
  foldl' insertEdge Map.empty (zip cadences (drop 1 cadences))
  where
    insertEdge acc edge = Map.insertWith (+) edge 1 acc

transitionProbabilities :: [Cadence] -> Map Edge Double
transitionProbabilities cadences =
  let counts = transitionCounts cadences
      totals = buildTotals counts
   in Map.mapWithKey (normalise totals) counts
  where
    buildTotals :: TransitionCounts -> Totals
    buildTotals = foldl' accumulate Map.empty . Map.toList
      where
        accumulate acc ((from,_), weight) = Map.insertWith (+) from weight acc

    normalise :: Totals -> Edge -> Double -> Double
    normalise totals (from, _) weight =
      case Map.lookup from totals of
        Just total | total > 0 -> weight / total
        _                     -> 0
