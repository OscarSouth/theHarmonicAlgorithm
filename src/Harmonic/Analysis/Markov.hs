module Harmonic.Analysis.Markov
  ( Edge
  , TransitionCounts
  , transitionCounts
  , transitionProbabilities
  ) where

import           Harmonic.Core.Harmony (Cadence)

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
