module Harmonic.Ingestion.Transform where

import           Harmonic.Ingestion.Types
import qualified Data.Vector as V
import qualified Data.List as L
import           Data.List (sortOn)

-- Phase B modules
import qualified Harmonic.Core.Harmony as H
import qualified Harmonic.Core.Dissonance as D
import qualified Harmonic.Core.Overtone as O

-- Extract fundamental bass notes (lowest pitch class in each chord)
fundamentals :: V.Vector [Int] -> [Int]
fundamentals v = V.toList $ V.map fundamental v
  where
    fundamental :: [Int] -> Int
    fundamental []  = 0
    fundamental xs  = minimum xs

-- |Convert a sequence of chord slices into a cadence list that reflects every
-- reasonable triad interpretation. Instead of picking a single "best" triad, we
-- duplicate the top three options (3/2/1 copies) and cross-multiply adjacent
-- slices so the Markov model can learn from alternate paths without fractional
-- weights.
buildCadences :: [ChordSlice] -> [H.Cadence]
buildCadences slices =
  let triadOptions = map sliceTriads slices
      transitions  = zip triadOptions (drop 1 triadOptions)
   in concatMap expand transitions
  where
    expand (fromChoices, toChoices) =
      [ H.toCadence (fromChord, toChord)
      | fromChord <- fromChoices
      , toChord   <- toChoices
      ]

    sliceTriads slice =
      -- Fall back to a single flat triad if the ranking step fails (e.g.,
      -- exporter filtered the slice down to fewer than three unique pitch
      -- classes). This keeps the cadence stream contiguous.
      let ranked = rankedTriads slice
       in if null ranked
            then [H.flatTriad (slicePitches slice)]
            else ranked

    rankedTriads slice =
      let fundamental = sliceFundamental slice
          uniquePcs   = L.nub (slicePitches slice)
          overtones   = filter (/= fundamental) uniquePcs
          candidates  = O.possibleTriads'' (fundamental, overtones)
          scored      = map score candidates
          top         = take 3 (sortOn fst scored)
          weights     = [3,2,1]
       in concat $ zipWith replicate weights (map snd top)

    score triad =
      -- Hindemith dissonance from Dissonance module gives us ordinal ranking;
      -- lower values are more consonant, so we sort ascending before duplicating.
      let pcs    = map (`mod` 12) triad
          chord  = H.flatTriad pcs
          (diss, _) = D.dissonanceLevel pcs
       in (diss, chord)

buildCadencesPerPiece :: [[ChordSlice]] -> [H.Cadence]
buildCadencesPerPiece pieces = concatMap buildCadences pieces
