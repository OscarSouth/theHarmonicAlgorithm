-- |
-- Module      : Harmonic.Rules.Import.Transform
-- Description : YCACL data transformation into cadence structures
--
-- Transforms raw CSV corpus records into 'Harmonic.Rules.Types.Harmony.Cadence' structures suitable for
-- Neo4j graph storage: fundamental extraction, triad generation via
-- "Harmonic.Rules.Constraints.Overtone", and dissonance scoring.
--
-- == Node-key naming contract ==
--
-- The LIVE database's node keys (@show@ = movement + functionality) carry
-- functionality names produced under LEGACY naming rules — e.g. zero form
-- @[0,3,8]@ is stored as @maj_1stInv@, @[0,2,7]@ as @sus4_1stInv@. The
-- modern namers ('H.toTriad' via 'H.toCadence') deliberately diverge from
-- those rules (@min#5@, @sus2@ for the same forms). BOTH sides of the
-- pipeline therefore route through 'H.corpusFunctionality' (the 55-form
-- @corpusNameTable@ transcribed from the live database, 2026-08-19):
-- the read side when building fetch keys, and this module when stamping
-- the functionality that becomes a node's @show@ key. Re-ingestion under
-- this contract reproduces the live keyspace exactly (verified: the node
-- set is the complete 55 zero-forms x 12 movements grid, and @show@ is a
-- pure function of the zero form).
--
-- After any re-ingestion, verify with an online @gen'@ run: graph counts
-- (@[nG\/...]@) must stay nonzero across steps that select inversion forms.

module Harmonic.Rules.Import.Transform (
    -- * Transition counting
    buildTransitionCounts, buildTransitionCountsPerPiece,

    -- * Interpretation expansion
    sliceInterpretations,
) where

import           Harmonic.Rules.Import.Types
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import           Data.List (sortOn)

import           Harmonic.Evaluation.Analysis.Markov (TransitionCounts)

-- Phase B modules
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Evaluation.Scoring.Dissonance as D
import qualified Harmonic.Rules.Constraints.Overtone as O

-- |Weighted triad interpretations of one slice, most consonant first.
-- The top three candidates (by Hindemith dissonance, "Harmonic.Evaluation.Scoring.Dissonance")
-- carry a @[3,2,1]@ preference profile, NORMALISED so every
-- slice's interpretation weights sum to 1 — an ambiguous 7-PC vertical
-- contributes exactly the same total probability mass as an unambiguous
-- triad, instead of shouting several times louder purely because it was
-- ambiguous. Ties keep 'Harmonic.Rules.Constraints.Overtone.possibleTriads'
-- enumeration order ('sortOn' is stable).
--
-- Falls back to a single flat triad at weight 1 if ranking yields no
-- candidates (e.g. the exporter filtered the slice below 3 unique PCs).
sliceInterpretations :: ChordSlice -> [(H.Chord, Double)]
sliceInterpretations slice =
  let ranked = rankedTriads slice
   in if null ranked
        then [(H.flatTriad (slicePitches slice), 1)]
        else ranked
  where
    rankedTriads sl =
      let fundamental = sliceFundamental sl
          uniquePcs   = L.nub (slicePitches sl)
          overtones   = filter (/= fundamental) uniquePcs
          candidates  = O.possibleTriads'' (fundamental, overtones)
          scored      = map score candidates
          top         = take 3 (sortOn fst scored)
          raw         = zip (map snd top) [3, 2, 1]
          total       = sum (map snd raw)
       in [ (chord, w / total) | (chord, w) <- raw ]

    score triad =
      -- Hindemith dissonance gives an ordinal ranking; lower is more
      -- consonant, so ascending sort puts the preferred reading first.
      let pcs    = map (`mod` 12) triad
          (diss, _) = D.dissonanceLevel pcs
       in (diss, H.flatTriad pcs)

-- |Fold one piece's slice sequence into Markov transition counts over
-- every CONSISTENT interpretation path.
--
-- A cadence is a movement onto a target chord, so an edge is built from a
-- slice TRIPLE @(s1, s2, s3)@: the cadence @a -> b@ followed by the cadence
-- @b -> c@, where the middle interpretation @b@ is shared by both sides.
-- Each such edge accumulates weight @w_a * w_b * w_c@ from the (per-slice
-- normalised) interpretation weights, so the total mass contributed by
-- each slice triple is exactly 1.
--
-- Sharing the middle reading is what keeps the expansion honest: every
-- reasonable reading of an ambiguous vertical informs the model
-- (recovering movement that triad reduction trims away), while two
-- alternative readings of the SAME moment can never be counted as a
-- transition between moments. Genuine corpus pedals (roughly a quarter
-- of YCACL slices repeat their predecessor) produce self-edges via the
-- same triple rule as every other transition.
--
-- Pieces with fewer than three slices contribute no edges (no complete
-- triple exists).
buildTransitionCounts :: [ChordSlice] -> TransitionCounts
buildTransitionCounts slices =
  let ts      = map sliceInterpretations slices
      triples = zip3 ts (drop 1 ts) (drop 2 ts)
   in Map.unionsWith (+)
        [ Map.fromListWith (+)
            [ ( ( corpusNamed (H.toCadence (a, b))
                , corpusNamed (H.toCadence (b, c)) )
              , wa * wb * wc )
            | (a, wa) <- t1, (b, wb) <- t2, (c, wc) <- t3
            ]
        | (t1, t2, t3) <- triples
        ]
  where
    -- NODE-KEY DECISION POINT: the functionality stamped here becomes
    -- the graph node's @show@ key. Write-side naming is routed through
    -- 'H.corpusFunctionality' so the emitted keyspace is IDENTICAL to
    -- the live database's legacy-named keys (@corpusNameTable@) — the
    -- read side ("Harmonic.Evaluation.Database.Query") builds fetch
    -- keys through the same table, keeping both sides on one contract.
    -- For 3-PC forms the table is total (55/55 transcribed); non-triad
    -- forms fall back to the modern namers inside 'H.corpusFunctionality'
    -- (they are never corpus keys).
    corpusNamed c =
      c { H.cadenceFunctionality = H.corpusFunctionality (H.cadenceIntervals c) }

-- |Sum 'buildTransitionCounts' over a composer's pieces. Each piece is
-- counted independently, so no transition can be invented across the
-- boundary between the last slices of one piece and the first of the
-- next (the flattened pipeline leaked exactly such edges).
buildTransitionCountsPerPiece :: [[ChordSlice]] -> TransitionCounts
buildTransitionCountsPerPiece = Map.unionsWith (+) . map buildTransitionCounts
