-- |
-- Module      : Harmonic.Rules.Import.Transform
-- Description : YCACL data transformation into cadence structures
--
-- Transforms raw CSV corpus records into 'Harmonic.Rules.Types.Harmony.Cadence' structures suitable for
-- Neo4j graph storage: fundamental extraction, triad generation via
-- "Harmonic.Rules.Constraints.Overtone", and dissonance scoring.
--
-- == BEFORE RE-INGESTING THE CORPUS, READ THIS ==
--
-- The LIVE database's node keys (@show@ = movement + functionality) carry
-- functionality names produced under LEGACY naming rules — e.g. zero form
-- @[0,3,8]@ is stored as @maj_1stInv@, @[0,2,7]@ as @sus4_1stInv@,
-- @[0,5,10]@ as @sus4_2ndInv@. The CURRENT namers in this pipeline
-- ('H.toTriad' via 'H.toCadence' below) deliberately diverge from those
-- rules (@min#5@, @sus2@, @7sus4@ for the same forms). Every read-side
-- fetch key is therefore built through 'H.corpusFunctionality', whose
-- 55-form @corpusNameTable@ ("Harmonic.Rules.Types.Harmony") was
-- transcribed verbatim from the live database (2026-08-19).
--
-- Consequence: running this ingestion pipeline as-is would create a graph
-- whose keyspace DIVERGES from @corpusNameTable@ — every fetch would miss
-- and generation would silently drop to fallback-only. Any re-ingestion
-- must do one of:
--
--   (a) route write-side naming through 'H.corpusFunctionality' so the
--       new keyspace is identical to the table (preferred — keeps read
--       and write sides on one contract), or
--   (b) re-ingest with the current namers and then REGENERATE
--       @corpusNameTable@ from the fresh database:
--       @MATCH (c:Cadence) RETURN DISTINCT c.chord, c.show@.
--
-- Either way, verify afterwards with an online @gen'@ run: graph counts
-- (@[nG/...]@) must stay nonzero across steps that select inversion forms.

module Harmonic.Rules.Import.Transform (
    -- * Cadence construction
    buildCadences, buildCadencesPerPiece,

    -- * Helpers
    fundamentals,
) where

import           Harmonic.Rules.Import.Types
import qualified Data.Vector as V
import qualified Data.List as L
import           Data.List (sortOn)

-- Phase B modules
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Evaluation.Scoring.Dissonance as D
import qualified Harmonic.Rules.Constraints.Overtone as O

-- | Extract the fundamental bass note of each chord: the lowest pitch present.
-- Empty chords yield @0@.
fundamentals :: V.Vector [Int] -> [Int]
fundamentals v = V.toList $ V.map fundamental v
  where
    fundamental :: [Int] -> Int
    fundamental []  = 0
    fundamental xs  = minimum xs

-- |Convert a sequence of chord slices into a cadence list that reflects every
-- reasonable triad interpretation. Instead of picking a single "best" triad, we
-- duplicate the top three options (3\/2\/1 copies) and cross-multiply adjacent
-- slices so the Markov model can learn from alternate paths without fractional
-- weights.
buildCadences :: [ChordSlice] -> [H.Cadence]
buildCadences slices =
  let triadOptions = map sliceTriads slices
      transitions  = zip triadOptions (drop 1 triadOptions)
   in concatMap expand transitions
  where
    expand (fromChoices, toChoices) =
      -- NODE-KEY DECISION POINT: 'H.toCadence' stamps the functionality
      -- that becomes the graph node's @show@ key. See the module-header
      -- warning — the live DB's keys were stamped under legacy naming,
      -- which the current 'H.toTriad'-derived names diverge from. Do not
      -- re-ingest without resolving that (route through
      -- 'H.corpusFunctionality', or rebuild @corpusNameTable@ after).
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

-- | Build cadences piece by piece, then concatenate. Keeping pieces separate
-- matters: it stops a transition being invented across the boundary between
-- the last chord of one piece and the first of the next.
buildCadencesPerPiece :: [[ChordSlice]] -> [H.Cadence]
buildCadencesPerPiece pieces = concatMap buildCadences pieces
