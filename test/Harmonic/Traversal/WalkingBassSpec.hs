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
--     * closestLowMidi picks the lowest in-range MIDI for a given PC;
--       closestMidMidi the instance nearest the register centre (bar-0 anchor)
module Harmonic.Traversal.WalkingBassSpec (spec) where

import Test.Hspec

import Data.Foldable (toList)
import qualified Data.Set as Set

import Harmonic.Rules.Types.Pitch (pitchClass, unPitchClass, mkPitchClass, NoteName(..))
import Harmonic.Rules.Types.Harmony
import Harmonic.Rules.Types.Progression
import Harmonic.Traversal.WalkingBass
import Harmonic.Interface.Tidal.Groove (fund)
import Harmonic.Interface.Tidal.Arranger (root)
import qualified Harmonic.Rules.Import.Jazz as J


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

-- Cardinality-preserving fixture constructor: 'initCadenceState' silently
-- truncates >3-PC chords to triads, so tetrad fixtures build through
-- 'mkCadenceStatePCs'.
mkCS :: NoteName -> [Int] -> CadenceState
mkCS nn = mkCadenceStatePCs nn (toMovement (mkPitchClass 0) (mkPitchClass 0))

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

    it "bar 0 beat 0 = closestMidMidi of first chord's root" $ do
      head (head (walkLine fund prog4))
        `shouldBe` closestMidMidi (firstRootPC prog4)

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

  describe "closestMidMidi" $ do

    it "picks the register instance nearest the centre (lower on tie)" $ do
      closestMidMidi 0 `shouldBe` 36   -- C2 (|36-38| < |48-38|)
      closestMidMidi 2 `shouldBe` 38   -- D2 = the centre itself
      closestMidMidi 4 `shouldBe` 40   -- E2 beats E1 (2 vs 10)
      closestMidMidi 7 `shouldBe` 43   -- G2 beats G1 (5 vs 7)

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

  describe "progConsonance" $ do

    it "is 1.0 for pure major/minor triad progressions" $ do
      progConsonance prog4 `shouldBe` 1.0

    it "normalises per cardinality: an m7 vamp reads as consonant" $ do
      let vamp = fromCadenceStates (replicate 4 (mkCS C [0,3,7,10]))
      progConsonance vamp `shouldBe` 1.0

    it "ranks a dominant-7 progression below a triad progression" $ do
      let doms = fromCadenceStates [ mkCS r [0,4,7,10] | r <- [F, Bb, F, C] ]
      progConsonance doms `shouldSatisfy` (< progConsonance prog4)

    it "stays in [0, 1]" $ do
      mapM_ (\p -> progConsonance p `shouldSatisfy` (\x -> x >= 0 && x <= 1))
            [prog4, prog8, progTritone, progDim, progDmG]

  describe "duplicate-run beat 1 (root-fifth alternation)" $ do

    it "a 4-bar one-chord vamp walks beat 1 as root-P5-root-P5" $ do
      let vamp = fromCadenceStates (replicate 4 (mkCS C [0,3,7,10]))
          b1pcs = map ((`mod` 12) . head) (walkLine fund vamp)
      b1pcs `shouldBe` [0, 7, 0, 7]

    it "distinct-chord progressions never substitute the P5 on beat 1" $ do
      let b1pcs = map ((`mod` 12) . head) (walkLine fund prog8)
          roots = map fundPCOf (toList (unProgression prog8))
      b1pcs `shouldBe` roots

  describe "register loop closure" $ do

    it "the last bar's beat 1 stays within a fifth of bar 0's beat 1" $ do
      mapM_ (\p -> do
              let line = walkLine fund p
              abs (head (last line) - head (head line))
                `shouldSatisfy` (<= 7))
            [prog4, prog8, progDim, progDescTone, progDmG]

  describe "inferKeyCentre (regional, walk-internal)" $ do

    -- The five harvested fixtures from notes/walking_bass_theory.md:477-486.
    let keyPCs prog = map unPitchClass (inferKeyCentre prog)

    it "BbM7 Eb7 AbM7 infers Ab" $ do
      let p = fromCadenceStates [mkCS Bb [0,4,7,11], mkCS Eb [0,4,7,10], mkCS Ab [0,4,7,11]]
      keyPCs p `shouldBe` replicate 3 8

    it "Am7 Em7 Bm7 infers G" $ do
      let p = fromCadenceStates [mkCS A [0,3,7,10], mkCS E [0,3,7,10], mkCS B [0,3,7,10]]
      keyPCs p `shouldBe` replicate 3 7

    it "FM7 Am7 BbM7 infers F" $ do
      let p = fromCadenceStates [mkCS F [0,4,7,11], mkCS A [0,3,7,10], mkCS Bb [0,4,7,11]]
      keyPCs p `shouldBe` replicate 3 5

    it "FM7 Em7 Dm7 infers C" $ do
      let p = fromCadenceStates [mkCS F [0,4,7,11], mkCS E [0,3,7,10], mkCS D [0,3,7,10]]
      keyPCs p `shouldBe` replicate 3 0

    it "DM7 F#m7 Bm7 GM7 infers D" $ do
      let p = fromCadenceStates [ mkCS D [0,4,7,11], mkCS F' [0,3,7,10]
                                , mkCS B [0,3,7,10], mkCS G [0,4,7,11] ]
      keyPCs p `shouldBe` replicate 4 2

    it "returns one centre per bar and is deterministic" $ do
      let ks = inferKeyCentre prog8
      length ks `shouldBe` 8
      inferKeyCentre prog8 `shouldBe` ks

  describe "dynamics-coupled register arc (walkLineDyn)" $ do

    it "Nothing is byte-identical to walkLine" $ do
      walkLineDyn Nothing fund prog8 `shouldBe` walkLine fund prog8

    it "a constant dynamic vector is neutral (mean-centred bias is zero)" $ do
      walkLineDyn (Just (replicate 8 0.9)) fund prog8
        `shouldBe` walkLine fund prog8

    it "a 0.25+ drop resets that bar's beat 1 to the lowest register instance" $ do
      let line = walkLineDyn (Just [0.9, 0.9, 0.5, 0.5]) fund prog4
          bars = toList (unProgression prog4)
          pc2  = fundPCOf (bars !! 2)
      head (line !! 2) `shouldBe` closestLowMidi pc2

    it "the arc bias steers exactly-tied register choices with the dynamics" $ do
      -- Eb-A alternation: Eb has a single register instance (39); from it
      -- the A instances (33, 45) are equidistant, so the neutral walk takes
      -- the lower on the tie and a quieter-than-mean bar flips it upward.
      let progEbA = fromCadenceStates
            [ initCadenceState 0 "Eb" [0,4,7], aMin
            , initCadenceState 0 "Eb" [0,4,7], aMin ]
          neutral = map head (walkLineDyn Nothing fund progEbA)
          shaped  = map head
            (walkLineDyn (Just [0.6, 0.4, 0.6, 0.4]) fund progEbA)
      neutral `shouldBe` [39, 33, 39, 33]
      shaped  `shouldBe` [39, 45, 39, 45]

  describe "degenerate bars" $ do

    it "a bar with no chord tones does not crash; its beat 3 falls back to \
       \a regional-key tone" $ do
      let progEmpty = fromCadenceStates
            [ cMaj, initCadenceState 0 "C" [], gMaj, fMaj ]
          line = walkLine fund progEmpty
          keyPC = unPitchClass (inferKeyCentre progEmpty !! 1)
          keySet = Set.fromList [ (keyPC + st) `mod` 12
                                | st <- [0, 2, 4, 5, 7, 9, 11] ]
      length line `shouldBe` 4
      ((line !! 1 !! 2) `mod` 12) `shouldSatisfy` (`Set.member` keySet)

  describe "walkLineP (octatripentatonic path)" $ do

    it "produces nBars x 4 beats, all in register" $ do
      let line = walkLineP fund progGenP genPChromas
      length line `shouldBe` 8
      mapM_ (\bar -> do
              length bar `shouldBe` beatsPerBar
              mapM_ (\m -> m `shouldSatisfy`
                      (\x -> x >= lowestMidi && x <= highestMidi)) bar)
            line

    it "is deterministic" $
      walkLineP fund progGenP genPChromas
        `shouldBe` walkLineP fund progGenP genPChromas

    it "falls back to walkLine when the chromas list length mismatches" $
      walkLineP fund progGenP [] `shouldBe` walkLine fund progGenP

    it "keeps EVERY beat inside the bar's strata/overlap/mode chroma" $
      chromaClean progGenP genPChromas `shouldBe` True

    -- The generator refuses to let even the bass leave the stratum
    -- (pcStrictContainment). A duplicate-chord run offers the bar's fifth
    -- as a beat-1 alternative, so the fifth a genP bar walks must itself
    -- be chroma-resident: C# diminished in stratum V has no natural fifth
    -- anywhere in its chroma — A natural is not even in the
    -- octatripentatonic universe.
    it "a duplicate genP bar never alternates onto a foreign fifth" $
      chromaClean progGenPDup genPChromasDup `shouldBe` True

    it "never emits a pitch class outside the octatripentatonic universe" $ do
      let universe = Set.fromList [1, 2, 4, 6, 7, 9, 10, 11]
          pcsOf pr chs = [ m `mod` 12 | bar <- walkLineP fund pr chs, m <- bar ]
      all (`Set.member` universe) (pcsOf progGenP genPChromas) `shouldBe` True
      all (`Set.member` universe) (pcsOf progGenPDup genPChromasDup) `shouldBe` True

    it "leading-tone parity: at least half of the bars approach the next \
       \beat 1 within two semitones on beat 4" $ do
      let line = walkLineP fund progGenP genPChromas
          n    = length line
          hit i = abs ((line !! i !! 3)
                       - head (line !! ((i + 1) `mod` n))) `elem` [1, 2]
          hits = length (filter hit [0 .. n - 1])
      hits * 2 `shouldSatisfy` (>= n)


  describe "walkLineJ (jazz bass-vocabulary path)" $ do

    let jazzVocab prog =
          [ J.bassVocabFor
              [ unPitchClass iv | iv <- cadenceIntervals (stateCadence cs) ]
          | cs <- toList (unProgression prog) ]
        walkJ prog = walkLineJ root prog (jazzVocab prog)
        prog13 = fromCadenceStates          -- C13 C13 F13 G13sus4
          [ mkCS C [0,2,4,9,10], mkCS C [0,2,4,9,10]
          , mkCS F [0,2,4,9,10], mkCS G [0,2,5,9,10] ]
        progAlt = fromCadenceStates         -- A7alt A7alt Dm9 Dm9
          [ mkCS A [0,1,4,8,10], mkCS A [0,1,4,8,10]
          , mkCS D [0,2,3,7,10], mkCS D [0,2,3,7,10] ]

    it "a duplicate-run 13th chord alternates onto the RESTORED fifth" $ do
      let line  = walkJ prog13
          b1pc1 = head (line !! 1) `mod` 12
      -- bar 2 of C13 x2: root C or the restored natural fifth G — never a
      -- tone outside the bass vocabulary.
      b1pc1 `shouldSatisfy` (`elem` [0, 7])

    it "an altered dominant anchors its #5; the natural five never lands \
       \on a strong beat" $ do
      let line = walkJ progAlt
          strongPCs = [ (line !! i !! k) `mod` 12 | i <- [0, 1], k <- [0, 2] ]
      -- natural five (E over A7alt) on no strong beat of the alt bars;
      -- weak-beat chromatic use (E approaching the #5) remains legal.
      strongPCs `shouldSatisfy` notElem 4
      -- the duplicate-run fifth alternation lands the #5, not the natural 5
      (head (line !! 1) `mod` 12) `shouldSatisfy` (`elem` [9, 5])

    it "notated avoid tones never land on strong beats" $ do
      let checkProg prog =
            let line  = walkJ prog
                vs    = jazzVocab prog
                bars' = toList (unProgression prog)
            in and [ ((line !! i !! k) - fundPCOf (bars' !! i)) `mod` 12
                       `notElem` J.bvAvoid (vs !! i)
                   | i <- [0 .. length line - 1], k <- [0, 2] ]
      checkProg prog13 `shouldBe` True
      checkProg progAlt `shouldBe` True

    it "the restored fifth is reachable as a beat-3 anchor on 13th chords" $ do
      let line  = walkJ prog13
          bars' = toList (unProgression prog13)
          hits  = [ i | i <- [0 .. 3]
                      , ((line !! i !! 2) - fundPCOf (bars' !! i)) `mod` 12 == 7 ]
      hits `shouldSatisfy` (not . null)

    it "beat 2 prefers an anchor tone over a passing extension" $ do
      -- Dm9 | G13 | Cmaj9 | C6: bar 1 beat 2 takes the minor 3rd (F),
      -- not the 11th (G) — both are bass vocabulary, but the quality's
      -- defining tone outranks its passing extension in that slot.
      let progIIVI = fromCadenceStates
            [ mkCS D [0,2,3,7,10], mkCS G [0,2,4,9,10]
            , mkCS C [0,2,4,7,11], mkCS C [0,4,7,9] ]
          line = walkJ progIIVI
      ((line !! 0 !! 1) `mod` 12) `shouldBe` 5     -- F, the m3 of Dm

    it "falls back to walkLine on a vocab length mismatch" $ do
      walkLineJ root prog13 [] `shouldBe` walkLine root prog13

    it "the #9 never lands on a strong beat (avoid tones leave the pool)" $ do
      -- the #9 is the cheapest avoid tone by interval quality, so it is
      -- the one a soft surcharge would let through on geometry alone
      let progSharp9 = fromCadenceStates
            [ mkCS C [0,3,4,7,10], mkCS F [0,3,4,9,10]   -- C7#9, F13#9
            , mkCS C [0,3,4,7,10], mkCS G [0,4,7,10] ]
          line  = walkJ progSharp9
          bars' = toList (unProgression progSharp9)
          strongIvs = [ ((line !! i !! k) - fundPCOf (bars' !! i)) `mod` 12
                      | i <- [0 .. 3], k <- [0, 2] ]
      strongIvs `shouldSatisfy` all (/= 3)

    it "a quality's own fifth is priced as a fifth, whatever semitone it sits on" $ do
      -- b5 and #5 are the declared targets of these qualities; before
      -- role pricing they were the dearest strong-beat options in the bar
      let progHalfDim = fromCadenceStates
            [ mkCS B [0,3,6,10], mkCS B [0,3,6,10]       -- Bm7b5 x2
            , mkCS E [0,1,4,8,10], mkCS A [0,3,7,10] ]
          line  = walkJ progHalfDim
          bars' = toList (unProgression progHalfDim)
          ivAt i k = ((line !! i !! k) - fundPCOf (bars' !! i)) `mod` 12
      -- the b5 is reachable as a strong-beat anchor on a half-diminished bar
      [ ivAt i k | i <- [0, 1], k <- [0, 2] ] `shouldSatisfy` elem 6
      -- and the natural 5 the chord does not contain never appears there
      [ ivAt i k | i <- [0, 1], k <- [0, 2] ] `shouldSatisfy` notElem 7


  describe "golden lines (one fixture per generation family)" $ do

    -- Literal note lists, so any claim that a change "cannot move legacy
    -- output" is checked by the suite rather than asserted. A diff here is
    -- not necessarily a regression — but it must be a deliberate, explained
    -- one, re-pinned in the same commit that causes it.

    it "gen (triads)" $
      walkLine fund progGoldGen `shouldBe`
        [[36,33,31,32],[33,36,40,39],[38,41,45,44],[43,40,38,37]]

    it "extended (four-note, hand-built)" $
      walkLine fund progGoldExtended `shouldBe`
        [[36,33,31,32],[33,36,40,39],[38,41,45,44],[43,41,38,37]]

    it "genP (strata chroma)" $
      walkLineP fund progGenP genPChromas `shouldBe`
        [[33,37,40,37],[38,40,40,35],[34,31,31,34],[37,38,40,37]
        ,[38,40,38,35],[37,38,40,37],[38,40,38,35],[33,31,28,31]]

    it "genJ (bass vocabulary)" $
      walkLineJ root progGoldJazz (goldJazzVocab) `shouldBe`
        [[36,34,31,30],[31,29,28,31],[29,31,33,29],[31,36,38,37]]

-- Golden-line fixtures: fixed material per family, deliberately plain so
-- the pinned lines stay readable.
progGoldGen, progGoldExtended, progGoldJazz :: Progression
progGoldGen = fromCadenceStates
  [ cMaj, aMin, dMin, gMaj ]
progGoldExtended = fromCadenceStates
  [ mkCS C [0,4,7,11], mkCS A [0,3,7,10], mkCS D [0,3,7,10], mkCS G [0,4,7,10] ]
progGoldJazz = fromCadenceStates
  [ mkCS C [0,2,4,9,10], mkCS C [0,2,4,9,10]
  , mkCS F [0,2,4,9,10], mkCS G [0,2,5,9,10] ]

goldJazzVocab :: [J.BassVocab]
goldJazzVocab =
  [ J.bassVocabFor [ unPitchClass iv | iv <- cadenceIntervals (stateCadence cs) ]
  | cs <- toList (unProgression progGoldJazz) ]

-------------------------------------------------------------------------------
-- genP fixtures (frozen from a live strata-V generation; ChromaSources
-- reconstructed the same way LineHarmony.chromaSourcesFor does)
-------------------------------------------------------------------------------

progGenP :: Progression
progGenP = fromCadenceStates
  [ initCadenceState 0 "A"  [0,4,7]
  , initCadenceState 0 "D"  [0,2,4]
  , initCadenceState 0 "Bb" [0,8,9]
  , initCadenceState 0 "C#" [0,3,6]
  , initCadenceState 0 "D"  [0,2,5]
  , initCadenceState 0 "C#" [0,3,6]
  , initCadenceState 0 "D"  [0,2,5]
  , initCadenceState 0 "C#" [0,3,8]
  ]

-- Every emitted pitch class of a genP walk must belong to its bar's
-- chroma: the bar's own stratum, its mode, or the neighbouring-bar triad
-- overlap the connector pool admits by design.
chromaClean :: Progression -> [ChromaSources] -> Bool
chromaClean prog chromas =
  all ok [0 .. n - 1]
  where
    line  = walkLineP fund prog chromas
    bars  = toList (unProgression prog)
    n     = length bars
    chSet i = chordPCsFor (bars !! i)
    overlapAt i = Set.unions [ chSet ((i + d) `mod` n) | d <- [-1, 0, 1] ]
    poolAt i =
      let ChromaSources st md = chromas !! i
      in st `Set.union` overlapAt i `Set.union` md
    ok i = all (\k -> ((line !! i !! k) `mod` 12) `Set.member` poolAt i)
               [0 .. beatsPerBar - 1]

-- The stratum-V C# diminished bar, duplicated: `dupOdd` fires on the
-- second occurrence and offers that bar's fifth on beat 1. Reproduces
-- exactly what `rep s 2` and repeating warps manufacture in performance.
progGenPDup :: Progression
progGenPDup = fromCadenceStates
  [ initCadenceState 0 "C#" [0,3,6]
  , initCadenceState 0 "C#" [0,3,6]
  , initCadenceState 0 "D"  [0,2,5]
  , initCadenceState 0 "A"  [0,4,7]
  ]

genPChromasDup :: [ChromaSources]
genPChromasDup =
  [ ChromaSources (absSetT r sIvs) (absSetT r mIvs)
  | (r, sIvs, mIvs) <-
      [ (1,  [0,1,3,6,8],  [0,1,3,5,6,8,9])
      , (1,  [0,1,3,6,8],  [0,1,3,5,6,8,9])
      , (2,  [0,2,4,5,9],  [0,2,4,5,7,9,11])
      , (9,  [0,4,5,7,10], [0,2,4,5,7,9,10])
      ] ]

absSetT :: Int -> [Int] -> Set.Set Int
absSetT r ivs = Set.fromList [ (r + i) `mod` 12 | i <- ivs ]

genPChromas :: [ChromaSources]
genPChromas =
  [ ChromaSources (absSet r sIvs) (absSet r mIvs)
  | (r, sIvs, mIvs) <- rows ]
  where
    absSet r ivs = Set.fromList [ (r + i) `mod` 12 | i <- ivs ]
    rows =
      [ (9,  [0,4,5,7,10], [0,2,4,5,7,9,10])
      , (2,  [0,2,4,5,9],  [0,2,4,5,7,9,11])
      , (10, [0,3,4,8,9],  [0,1,3,4,6,8,9])
      , (1,  [0,1,3,6,8],  [0,1,3,5,6,8,9])
      , (2,  [0,2,4,5,9],  [0,2,4,5,7,9,11])
      , (1,  [0,1,3,6,8],  [0,1,3,5,6,8,10])
      , (2,  [0,2,4,5,9],  [0,2,4,5,7,9,11])
      , (9,  [0,4,5,7,10], [0,2,4,5,7,9,10])
      ]
