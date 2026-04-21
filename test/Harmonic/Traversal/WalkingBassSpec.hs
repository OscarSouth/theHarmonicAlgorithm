-- |
-- Module      : Harmonic.Traversal.WalkingBassSpec
-- Description : Tests for the three-pass walking-bass generator
--
-- Invariants covered:
--
--   Shape and determinism
--     * walkLine produces nBars rows of beatsPerBar columns
--     * Degenerate input: empty progression -> []
--     * Same (voiceFn, progression) -> identical output
--
--   Voice-function wiring (beat 1 is a hard invariant)
--     * Beat 0 PC equals the voice function's output PC for every bar
--     * Holds for both 'fund' (harmonic root) and 'root' (bass, slash-aware)
--
--   Beat-3 consonance ranking
--     * Beat 3 is always a chord tone
--     * Beat 3 favours the P5 of the fundamental across a run (≥ 50%)
--
--   Beat-4 connector idiom (iteration 4)
--     * Beat 4 does not copy the next bar's beat 1 MIDI (tritone-shuttle guard)
--     * Static bars (b1==b3) produce at least one chord-tone connector per bar
--
--   Derived entropy
--     * Diatonic progressions yield moderate entropy (<= 0.35)
--     * Tritone-heavy progressions yield high entropy (>= 0.50)
--     * Tritone entropy strictly exceeds diatonic entropy
--     * Deterministic: same progression -> same entropy
--
--   Repeat rate sensitivity to progression character
--     * Diatonic progressions show connector repeat rate in a loose band
--     * Angular progressions keep the connector repeat rate low (<= 0.25)
--
--   Musical continuity
--     * Range: every MIDI in [lowestMidi, highestMidi]
--     * Smoothness: adjacent beats differ by <= 7 semitones
--     * Loop closure: last beat -> first note <= 7 semitones
--
--   Utility helpers
--     * closestLowMidi picks the lowest in-range MIDI for a given PC
module Harmonic.Traversal.WalkingBassSpec (spec) where

import Test.Hspec

import Data.Foldable (toList)
import qualified Data.Set as Set

import Harmonic.Rules.Types.Pitch (pitchClass, unPitchClass)
import Harmonic.Rules.Types.Harmony
import Harmonic.Rules.Types.Progression
import Harmonic.Traversal.WalkingBass
import Harmonic.Interface.Tidal.Groove (fund)
import Harmonic.Interface.Tidal.Arranger (root)


-------------------------------------------------------------------------------
-- Test fixtures
-------------------------------------------------------------------------------

cMaj, gMaj, aMin, fMaj, dMin, eMin :: CadenceState
cMaj = initCadenceState 0 "C" [0,4,7]
gMaj = initCadenceState 0 "G" [0,4,7]
aMin = initCadenceState 0 "A" [0,3,7]
fMaj = initCadenceState 0 "F" [0,4,7]
dMin = initCadenceState 0 "D" [0,3,7]
eMin = initCadenceState 0 "E" [0,3,7]

-- 4-bar diatonic: I-V-vi-IV in C (all P4/P5 root motion, minor root-step)
prog4 :: Progression
prog4 = fromCadenceStates [cMaj, gMaj, aMin, fMaj]

-- 8-bar diatonic progression
prog8 :: Progression
prog8 = fromCadenceStates [cMaj, aMin, dMin, gMaj, eMin, aMin, fMaj, gMaj]

-- Tritone shuttle: all-tritone root motion, maximally angular
progTritone :: Progression
progTritone = fromCadenceStates
  [ initCadenceState 0 "C"  [0,4,7]
  , initCadenceState 0 "F#" [0,4,7]
  , initCadenceState 0 "C"  [0,4,7]
  , initCadenceState 0 "F#" [0,4,7]
  ]

-- Progression containing a diminished triad (Bdim = B D F, intervals 0,3,6).
-- Iteration-4 output on this shape landed on a B1 C2 B1 C2 static cell;
-- iteration 5 should break out of that pattern via symmetric-aware scoring.
progDim :: Progression
progDim = fromCadenceStates
  [ initCadenceState 0 "D"  [0,3,7]    -- Dm
  , initCadenceState 0 "Ab" [0,4,7]    -- Ab
  , initCadenceState 0 "C"  [0,4,7]    -- C
  , initCadenceState 0 "B"  [0,3,6]    -- Bdim (symmetric)
  ]

-- Rising chromatic roots: the chord-root itself is a leading tone to the
-- next bar's root. Iteration-5 should favour root-on-beat-4 where that
-- expresses the chromatic transition cleanly.
progRisingChromatic :: Progression
progRisingChromatic = fromCadenceStates
  [ initCadenceState 0 "C"  [0,4,7]   -- C  -> C#
  , initCadenceState 0 "C#" [0,3,7]   -- C#m -> D
  , initCadenceState 0 "D"  [0,4,7]   -- D  -> Eb
  , initCadenceState 0 "Eb" [0,4,7]   -- Eb -> C (loop)
  ]

-- Descending chromatic roots: current root is a semitone above next root,
-- so the root-on-beat-4 expresses the descent via the target-distance=1 branch.
progDescChromatic :: Progression
progDescChromatic = fromCadenceStates
  [ initCadenceState 0 "C"  [0,4,7]
  , initCadenceState 0 "B"  [0,4,7]
  , initCadenceState 0 "Bb" [0,4,7]
  , initCadenceState 0 "A"  [0,4,7]
  ]

-- Descending whole-step roots: current root is a tone above next root.
-- Tests the tone-distance branch of the iteration-6 approach bonus.
progDescTone :: Progression
progDescTone = fromCadenceStates
  [ initCadenceState 0 "C"  [0,4,7]
  , initCadenceState 0 "Bb" [0,4,7]
  , initCadenceState 0 "Ab" [0,4,7]
  , initCadenceState 0 "Gb" [0,4,7]
  ]

-- ii-V-I in C: Dm -> G7 -> Cmaj7. P5 of Dm is A (PC 9), next root G (PC 7),
-- so |P5 - nextRoot| = 2. Tests the half-strength P5 approach variant.
progDmG :: Progression
progDmG = fromCadenceStates
  [ initCadenceState 0 "D" [0,3,7]
  , initCadenceState 0 "G" [0,4,7,10]
  , initCadenceState 0 "C" [0,4,7,11]
  , initCadenceState 0 "C" [0,4,7,11]
  ]

chordPCsFor :: CadenceState -> Set.Set Int
chordPCsFor cs =
  let r   = unPitchClass (pitchClass (stateCadenceRoot cs))
      ivs = map unPitchClass (cadenceIntervals (stateCadence cs))
  in Set.fromList [ (r + iv) `mod` 12 | iv <- ivs ]

fundPCOf :: CadenceState -> Int
fundPCOf = unPitchClass . pitchClass . stateCadenceRoot

firstRootPC :: Progression -> Int
firstRootPC prog =
  case toList (unProgression prog) of
    (cs:_) -> fundPCOf cs
    []     -> error "firstRootPC: empty progression"

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "walkLine shape" $ do

    it "returns one row per bar" $ do
      length (walkLine fund prog4) `shouldBe` 4

    it "returns beatsPerBar beats per row" $ do
      map length (walkLine fund prog4) `shouldBe` replicate 4 beatsPerBar

    it "returns [] for the empty progression" $ do
      walkLine fund (fromCadenceStates []) `shouldBe` []

  describe "voice function wiring (beat 1 is a hard invariant)" $ do

    it "bar 0 beat 0 = closestLowMidi of first chord's root" $ do
      head (head (walkLine fund prog4))
        `shouldBe` closestLowMidi (firstRootPC prog4)

    it "beat 0 of every bar equals fund's output PC" $ do
      let line = walkLine fund prog4
          pcs  = map head (fund prog4)
          bs0  = map ((`mod` 12) . head) line
      bs0 `shouldBe` pcs

    it "beat 0 of every bar equals root's output PC" $ do
      let line = walkLine root prog4
          pcs  = map head (root prog4)
          bs0  = map ((`mod` 12) . head) line
      bs0 `shouldBe` pcs

  describe "range constraint" $ do

    it "every beat lies within [lowestMidi, highestMidi]" $ do
      let ns = concat (walkLine fund prog8)
      all (\n -> n >= lowestMidi && n <= highestMidi) ns `shouldBe` True

  describe "determinism" $ do

    it "same voiceFn + progression produces identical lines" $ do
      walkLine fund prog8 `shouldBe` walkLine fund prog8

  describe "beat 3 (chord-tone re-anchor)" $ do

    it "beat 3 of every bar is a chord tone of that bar" $ do
      let line = walkLine fund prog8
          bars = toList (unProgression prog8)
          ok   = and $ zipWith
            (\cs beats -> ((beats !! 2) `mod` 12) `Set.member` chordPCsFor cs)
            bars line
      ok `shouldBe` True

    it "beat 3 favours the P5 of fund in the majority of bars" $ do
      let line = walkLine fund prog8
          bars = toList (unProgression prog8)
          isP5 cs beats =
            let fPC = fundPCOf cs
                b3  = (beats !! 2) `mod` 12
            in (b3 - fPC) `mod` 12 == 7
          hits = length (filter id (zipWith isP5 bars line))
      hits * 2 `shouldSatisfy` (> length bars)

  describe "beat 4 connector idiom (iteration 4)" $ do

    it "beat 4 does not copy the next bar's beat 1 (tritone shuttle)" $ do
      let line = walkLine fund progTritone
          n    = length line
          copies =
            [ (last bs) == head (line !! ((i + 1) `mod` n))
            | (i, bs) <- zip [0..] line ]
      or copies `shouldBe` False

    it "static bars (b1==b3) produce at least one chord-tone connector" $ do
      let line = walkLine fund prog8
          bars = toList (unProgression prog8)
          ok cs beats =
            let chord = chordPCsFor cs
                b1 = beats !! 0
                b2 = beats !! 1
                b3 = beats !! 2
                b4 = beats !! 3
                isChord m = (m `mod` 12) `Set.member` chord
            in b1 /= b3                         -- not a static bar, vacuous
               || isChord b2 || isChord b4     -- at least one chord-tone connector
      all id (zipWith ok bars line) `shouldBe` True

  describe "progressionEntropy" $ do

    it "is deterministic for a given progression" $ do
      progressionEntropy prog8 `shouldBe` progressionEntropy prog8

    it "is moderate (<= 0.35) for a diatonic progression" $ do
      progressionEntropy prog4 `shouldSatisfy` (<= 0.35)

    it "is high (>= 0.50) for an all-tritone progression" $ do
      progressionEntropy progTritone `shouldSatisfy` (>= 0.50)

    it "is strictly greater for tritone than for diatonic" $ do
      progressionEntropy progTritone `shouldSatisfy`
        (> progressionEntropy prog4)

    it "is in [0, 1]" $ do
      let e8 = progressionEntropy prog8
          eT = progressionEntropy progTritone
      (e8 >= 0 && e8 <= 1 && eT >= 0 && eT <= 1) `shouldBe` True

  describe "connector repeat rate varies with progression character" $ do

    it "diatonic progression's repeat rate sits in a loose band" $ do
      let line = walkLine fund prog8
          reps = concatMap
            (\beats -> [ beats !! 0 == beats !! 1
                       , beats !! 2 == beats !! 3
                       ]) line
          rate = fromIntegral (length (filter id reps))
               / (fromIntegral (length reps) :: Double)
      rate `shouldSatisfy` (\r -> r >= 0.0 && r <= 0.40)

    it "angular progression's repeat rate stays low (<= 0.25)" $ do
      let line = walkLine fund progTritone
          reps = concatMap
            (\beats -> [ beats !! 0 == beats !! 1
                       , beats !! 2 == beats !! 3
                       ]) line
          rate = fromIntegral (length (filter id reps))
               / (fromIntegral (length reps) :: Double)
      rate `shouldSatisfy` (<= 0.25)

  describe "smoothness" $ do

    it "adjacent beats differ by <= 7 semitones" $ do
      let line  = concat (walkLine fund prog4)
          steps = zipWith (\a b -> abs (b - a)) line (tail line)
      all (<= 7) steps `shouldBe` True

    it "loop closure (last beat -> first note) <= 7 semitones" $ do
      let line      = walkLine fund prog4
          lastBeat  = last (last line)
          firstBeat = head (head line)
      abs (firstBeat - lastBeat) `shouldSatisfy` (<= 7)

  describe "closestLowMidi" $ do

    it "returns the lowest MIDI in [lowestMidi, highestMidi] matching the PC" $ do
      closestLowMidi 0 `shouldBe` 36   -- C2 (C1=24 is below 28)
      closestLowMidi 4 `shouldBe` 28   -- E1
      closestLowMidi 7 `shouldBe` 31   -- G1
      closestLowMidi 5 `shouldBe` 29   -- F1

  describe "isSymmetricChord" $ do

    it "detects diminished triad ([0,3,6]) as symmetric" $ do
      isSymmetricChord (Set.fromList [0,3,6]) `shouldBe` True

    it "detects augmented triad ([0,4,8]) as symmetric" $ do
      isSymmetricChord (Set.fromList [0,4,8]) `shouldBe` True

    it "detects diminished seventh ([0,3,6,9]) as symmetric" $ do
      isSymmetricChord (Set.fromList [0,3,6,9]) `shouldBe` True

    it "detects whole-tone hexachord as symmetric" $ do
      isSymmetricChord (Set.fromList [0,2,4,6,8,10]) `shouldBe` True

    it "rejects major triad ([0,4,7]) as non-symmetric" $ do
      isSymmetricChord (Set.fromList [0,4,7]) `shouldBe` False

    it "rejects minor triad ([0,3,7]) as non-symmetric" $ do
      isSymmetricChord (Set.fromList [0,3,7]) `shouldBe` False

    it "rejects maj7 ([0,4,7,11]) as non-symmetric" $ do
      isSymmetricChord (Set.fromList [0,4,7,11]) `shouldBe` False

    it "rejects sets with fewer than 3 PCs" $ do
      isSymmetricChord Set.empty                   `shouldBe` False
      isSymmetricChord (Set.fromList [0])          `shouldBe` False
      isSymmetricChord (Set.fromList [0,6])        `shouldBe` False

  describe "symmetric-chord handling (iteration 5)" $ do

    it "symmetric bar in progDim is not a static cell (b1 /= b3)" $ do
      let line = walkLine fund progDim
          bars = toList (unProgression progDim)
          symBars =
            [ (cs, beats)
            | (cs, beats) <- zip bars line
            , isSymmetricChord (chordPCsFor cs)
            ]
          ok (_, beats) = (beats !! 0) /= (beats !! 2)
      all ok symBars `shouldBe` True

    it "progDim retains no-copy-next-b1 guard" $ do
      let line = walkLine fund progDim
          n    = length line
          copies =
            [ last bs == head (line !! ((i + 1) `mod` n))
            | (i, bs) <- zip [0..] line ]
      or copies `shouldBe` False

  describe "chord-tone chromatic approach on beat 4 (iteration 5)" $ do

    it "progRisingChromatic: majority of non-final bars close with a chord-tone \
       \within 1 semitone of the next bar's beat 1" $ do
      let line = walkLine fund progRisingChromatic
          bars = toList (unProgression progRisingChromatic)
          n    = length line
          hit i =
            let cs     = bars !! i
                beats  = line !! i
                b4     = last beats
                nextB1 = head (line !! ((i + 1) `mod` n))
                pcB4   = b4 `mod` 12
                chord  = chordPCsFor cs
            in pcB4 `Set.member` chord && abs (b4 - nextB1) == 1
          hits = length (filter hit [0 .. n - 2])
      hits `shouldSatisfy` (>= 2)

  describe "root/P5 approach bonus on beat 4 (iteration 6)" $ do

    it "progDescChromatic: at least 2 of 3 non-final bars close on current \
       \root at semitone distance from next b1" $ do
      let line = walkLine fund progDescChromatic
          bars = toList (unProgression progDescChromatic)
          n    = length line
          hit i =
            let cs     = bars !! i
                beats  = line !! i
                b4     = last beats
                nextB1 = head (line !! ((i + 1) `mod` n))
            in (b4 `mod` 12) == fundPCOf cs && abs (b4 - nextB1) == 1
          hits = length (filter hit [0 .. n - 2])
      hits `shouldSatisfy` (>= 2)

    it "progDescTone: at least 2 of 3 non-final bars close on current root \
       \at tone distance OR via the b3-aware chromatic in-between override" $ do
      let line = walkLine fund progDescTone
          bars = toList (unProgression progDescTone)
          n    = length line
          rootAtTone i =
            let cs     = bars !! i
                beats  = line !! i
                b4     = last beats
                nextB1 = head (line !! ((i + 1) `mod` n))
            in (b4 `mod` 12) == fundPCOf cs && abs (b4 - nextB1) == 2
          inBetween i =
            let cs     = bars !! i
                beats  = line !! i
                b3     = beats !! 2
                b4     = last beats
                nextB1 = head (line !! ((i + 1) `mod` n))
                rootPC = fundPCOf cs
            in (b3 `mod` 12) == rootPC
               && abs (b3 - nextB1) == 2
               && abs (b4 - nextB1) == 1
               && (b4 - b3) * (nextB1 - b4) > 0
          hits = length (filter (\i -> rootAtTone i || inBetween i) [0 .. n - 2])
      hits `shouldSatisfy` (>= 2)

    it "progDescTone: at least one non-final bar exercises the b3-aware \
       \in-between override (b3 on root, b4 strictly between b3 and next b1)" $ do
      let line = walkLine fund progDescTone
          bars = toList (unProgression progDescTone)
          n    = length line
          inBetween i =
            let cs     = bars !! i
                beats  = line !! i
                b3     = beats !! 2
                b4     = last beats
                nextB1 = head (line !! ((i + 1) `mod` n))
                rootPC = fundPCOf cs
            in (b3 `mod` 12) == rootPC
               && (b4 `mod` 12) /= rootPC
               && abs (b4 - nextB1) == 1
               && (b4 - b3) * (nextB1 - b4) > 0
          hits = length (filter inBetween [0 .. n - 2])
      hits `shouldSatisfy` (>= 1)

    it "progDmG bar 0: P5 variant fires when b3 lands on the P5 and \
       \|P5 - next b1| == 2 (Dm: A is tone above G)" $ do
      let line   = walkLine fund progDmG
          bars   = toList (unProgression progDmG)
          beats0 = head line
          b30    = beats0 !! 2
          b40    = last beats0
          nextB1 = head (line !! 1)
          p5PC0  = (fundPCOf (head bars) + 7) `mod` 12
      -- Expectation: b3 on P5, b4 strictly between b3 and next b1, |b4-next|==1.
      (b30 `mod` 12) `shouldBe` p5PC0
      abs (b40 - nextB1) `shouldBe` 1
      ((b40 - b30) * (nextB1 - b40) > 0) `shouldBe` True
