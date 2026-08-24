-- |
-- Module      : Harmonic.Rules.Import.TransformSpec
-- Description : Contracts for the un-flattened ingestion counting
--
-- Locks the consistent-path transition counting contracts: per-slice
-- normalised interpretation weights, middle-interpretation consistency,
-- unit mass per slice triple, piece isolation, and the write-side
-- naming contract.
module Harmonic.Rules.Import.TransformSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.List (sort)

import Harmonic.Rules.Import.Types
import Harmonic.Rules.Import.Transform
import qualified Harmonic.Rules.Types.Harmony as H
import Harmonic.Rules.Types.Pitch (unPitchClass)

-- | Shorthand: a slice whose fundamental is its first pitch.
slice :: [Int] -> ChordSlice
slice ps = ChordSlice ps (head ps)

cMaj :: ChordSlice
cMaj = slice [0,4,7]

-- | 5-PC ambiguous vertical over C (three ranked readings).
amb5 :: ChordSlice
amb5 = slice [0,2,4,7,9]

shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo x y = abs (x - y) `shouldSatisfy` (< 1e-12)

spec :: Spec
spec = do
  describe "sliceInterpretations" $ do
    it "an unambiguous triad carries all its mass on one reading" $ do
      let ws = sliceInterpretations cMaj
      map snd ws `shouldBe` [1]

    it "an ambiguous slice splits [3,2,1]-profiled mass summing to 1, most consonant first" $ do
      let ws = sliceInterpretations amb5
      length ws `shouldBe` 3
      sum (map snd ws) `shouldBeCloseTo` 1
      map snd ws `shouldBe` [3/6, 2/6, 1/6]

  describe "buildTransitionCounts" $ do
    it "fewer than three slices yields no edges (no complete cadence pair exists)" $ do
      buildTransitionCounts [cMaj] `shouldBe` Map.empty
      buildTransitionCounts [cMaj, cMaj] `shouldBe` Map.empty

    it "a genuine three-slice pedal yields exactly one self-edge at weight 1" $ do
      let counts = buildTransitionCounts [cMaj, cMaj, cMaj]
      Map.size counts `shouldBe` 1
      let [((from, to), w)] = Map.toList counts
      from `shouldBe` to
      show from `shouldBe` "( pedal -> maj )"
      w `shouldBeCloseTo` 1

    it "each slice triple contributes total mass 1 regardless of ambiguity" $ do
      -- Normalised per-slice weights make every moment speak at the
      -- same volume: ambiguity splits a moment's vote, never raises it.
      sum (Map.elems (buildTransitionCounts [cMaj, amb5, cMaj]))
        `shouldBeCloseTo` 1
      sum (Map.elems (buildTransitionCounts [amb5, amb5, amb5]))
        `shouldBeCloseTo` 1
      -- n slices = n - 2 triples = n - 2 total mass
      sum (Map.elems (buildTransitionCounts [cMaj, amb5, cMaj, amb5, cMaj]))
        `shouldBeCloseTo` 3

    it "middle interpretations are CONSISTENT: ambiguity fans out per reading, not per pair" $ do
      -- unambiguous -> 3-way ambiguous -> unambiguous: the two cadences
      -- of each edge share the middle reading, so exactly 3 edges appear
      -- (one per middle reading, at that reading's weight) — never the
      -- 9 an inconsistent block-pairing would manufacture.
      let counts = buildTransitionCounts [cMaj, amb5, cMaj]
      Map.size counts `shouldBe` 3
      sort (Map.elems counts) `shouldBe` sort [3/6, 2/6, 1/6]

    it "NAMING CONTRACT: every edge endpoint's functionality matches corpusFunctionality of its zero form" $ do
      -- The write-side tripwire that makes re-ingestion safe: node keys
      -- stamped here must be exactly the legacy-named keys the live
      -- graph carries and the read side fetches through. Exercised over
      -- every 3-PC set [a,b,c] with 0 <= a < b < c < 12 chained as one
      -- slice sequence.
      let allTriples = [ [a, b, c] | a <- [0..9], b <- [a+1..10], c <- [b+1..11] ]
          counts = buildTransitionCounts (map slice allTriples)
          endpoints = concat [ [f, t] | (f, t) <- Map.keys counts ]
      length allTriples `shouldBe` 220
      endpoints `shouldSatisfy`
        all (\c -> H.cadenceFunctionality c
                    == H.corpusFunctionality (H.cadenceIntervals c))
      -- a form where corpus and modern names disagree: [0,2,7] must be
      -- the corpus's sus4_1stInv, not the modern namers' sus2
      map H.cadenceFunctionality endpoints `shouldContain` ["sus4_1stInv"]

    it "counting is invariant under transposition (mod-12 P5 bonus is correct)" $ do
      -- The legacy P5 bonus (elem (7+f) xs, no mod) silently vanished
      -- for fundamentals >= 5; the current mod-12 form restores
      -- invariance. Locks the ruling that the CODE is right and the old
      -- "VERBATIM" comment was the bug.
      let base = [[0,4,7,9], [2,5,7,11], [0,2,4,7,9]]
          structure t =
            let ps = [ map (\p -> (p + t) `mod` 12) xs | xs <- base ]
                counts = buildTransitionCounts [ ChordSlice xs (head xs) | xs <- ps ]
            in sort [ ( map unPitchClass (H.cadenceIntervals f)
                      , map unPitchClass (H.cadenceIntervals to)
                      , w )
                    | ((f, to), w) <- Map.toList counts ]
      mapM_ (\t -> structure t `shouldBe` structure 0) [1..11]

  describe "buildTransitionCountsPerPiece" $ do
    it "never invents a transition across a piece boundary" $ do
      let p1 = [cMaj, slice [2,7,11], cMaj]
          p2 = [slice [5,9,0], slice [7,11,2], slice [5,9,0]]
          separate = buildTransitionCountsPerPiece [p1, p2]
          fused    = buildTransitionCounts (p1 ++ p2)
      -- per-piece counting equals the sum of the pieces' own counts...
      separate `shouldBe`
        Map.unionWith (+) (buildTransitionCounts p1) (buildTransitionCounts p2)
      -- ...and carries strictly less mass than the fused stream, whose
      -- extra triples span the boundary
      sum (Map.elems separate) `shouldBeCloseTo` 2
      sum (Map.elems fused) `shouldBeCloseTo` 4
