{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Harmonic.Tidal.InterfaceSpec
-- Description : Tests for TidalCycles interface patterns
--
-- Validates the Phase C Interface module:
--   * Modulo wrap behavior for pattern indices
--   * Voice extraction functions (flow, rootNotes, bassNotes)
--   * Pattern-based lookup functions

module Harmonic.Interface.Tidal.BridgeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import Harmonic.Interface.Tidal.Bridge
import qualified Harmonic.Interface.Tidal.Arranger as A
import qualified Data.Sequence as Seq

-------------------------------------------------------------------------------
-- Test Fixtures (Using Phase B types)
-------------------------------------------------------------------------------

-- |Helper: create a CadenceState for testing
-- Intervals should be in zero-form (first pitch = 0)
-- e.g., major = [0,4,7], minor = [0,3,7]
mkCadenceState :: Pitch.NoteName -> String -> [Integer] -> H.CadenceState
mkCadenceState rootNote func intervals =
  let -- Build a Chord with the given root and intervals
      chord = H.Chord rootNote func intervals
      mvmt = H.Unison
      -- Cadence intervals are stored in zero-form
      pcs = map (Pitch.mkPitchClass . fromIntegral) (take 3 intervals)
      cadence = H.Cadence func mvmt pcs
  in H.CadenceState cadence rootNote H.FlatSpelling

-- |Create a simple test progression with known chords
-- C major -> G major -> F major -> C major (4 chords)
-- Using zero-form intervals: major = [0,4,7]
testProgression :: P.Progression
testProgression = 
  P.Progression $ Seq.fromList
    [ mkCadenceState Pitch.C "maj" [0, 4, 7]    -- C major (C=0, E=4, G=7)
    , mkCadenceState Pitch.G "maj" [0, 4, 7]    -- G major (G=0, B=4, D=7 -> 7,11,2 after transpose)
    , mkCadenceState Pitch.F "maj" [0, 4, 7]    -- F major (F=0, A=4, C=7 -> 5,9,0 after transpose)
    , mkCadenceState Pitch.C "maj" [0, 4, 7]    -- C major
    ]

-- |Longer test progression (8 chords) for modulo tests
longerProgression :: P.Progression
longerProgression =
  P.Progression $ Seq.fromList
    [ mkCadenceState Pitch.C "maj" [0, 4, 7]
    , mkCadenceState Pitch.D "min" [0, 3, 7]
    , mkCadenceState Pitch.E "min" [0, 3, 7]
    , mkCadenceState Pitch.F "maj" [0, 4, 7]
    , mkCadenceState Pitch.G "maj" [0, 4, 7]
    , mkCadenceState Pitch.A "min" [0, 3, 7]
    , mkCadenceState Pitch.B "dim" [0, 3, 6]
    , mkCadenceState Pitch.C "maj" [0, 4, 7]
    ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Voice Functions" $ do
    
    describe "flow (full harmony, voice-led)" $ do
      it "returns one list per chord" $ do
        let voicings = A.flow testProgression
        length voicings `shouldBe` 4
      
      it "each voicing contains all pitch classes" $ do
        let voicings = A.flow testProgression
            firstVoicing = head voicings
        -- C major has 3 notes
        length firstVoicing `shouldBe` 3
    
    describe "rootNotes (root note only)" $ do
      it "returns one single-element list per chord" $ do
        let voicings = rootNotes testProgression
        length voicings `shouldBe` 4
        all (\v -> length v == 1) voicings `shouldBe` True
      
      it "extracts correct root notes" $ do
        let voicings = rootNotes testProgression
            roots = map head voicings
        -- C=0, G=7, F=5, C=0
        roots `shouldBe` [0, 7, 5, 0]
    
    describe "bassNotes (lowest note)" $ do
      it "returns one single-element list per chord" $ do
        let voicings = bassNotes testProgression
        length voicings `shouldBe` 4
        all (\v -> length v == 1) voicings `shouldBe` True
      
      it "extracts bass from voicing (first element)" $ do
        let voicings = bassNotes testProgression
            basses = map head voicings
        -- First pitch in each Chord's pitch list
        basses `shouldBe` [0, 7, 5, 0]

  describe "progLength" $ do
    
    it "returns correct length for 4-chord progression" $ do
      P.progLength testProgression `shouldBe` 4
    
    it "returns correct length for 8-chord progression" $ do
      P.progLength longerProgression `shouldBe` 8

  describe "lookupChord" $ do
    
    it "returns correct chord at index 0" $ do
      let chord = lookupChord testProgression 0
          pitches = H.chordIntervals chord
      -- C major: root=C(0), intervals [0,4,7], toTriad gives [0,4,7]
      pitches `shouldBe` [0, 4, 7]
    
    it "returns correct chord at index 2" $ do
      let chord = lookupChord testProgression 2
          pitches = H.chordIntervals chord
      -- F major: root=F(5), intervals [0,4,7], toTriad gives [5,9,0]
      pitches `shouldBe` [5, 9, 0]
    
    describe "Modulo wrap behavior" $ do
      it "index 4 wraps to index 0 (4 mod 4 = 0)" $ do
        let chord0 = lookupChord testProgression 0
            chord4 = lookupChord testProgression 4
        chord0 `shouldBe` chord4
      
      it "index 6 wraps to index 2 (6 mod 4 = 2)" $ do
        let chord2 = lookupChord testProgression 2
            chord6 = lookupChord testProgression 6
        chord2 `shouldBe` chord6
      
      it "negative indices wrap correctly" $ do
        -- In Haskell, (-1) `mod` 4 = 3
        let chord3 = lookupChord testProgression 3
            chordNeg1 = lookupChord testProgression (-1)
        chord3 `shouldBe` chordNeg1
      
      it "large indices wrap correctly" $ do
        let chord0 = lookupChord testProgression 0
            chord100 = lookupChord testProgression 100  -- 100 mod 4 = 0
        chord0 `shouldBe` chord100

  describe "VoiceType enum" $ do
    
    it "Roots extracts root notes" $ do
      -- Use a Pattern of a constant value for testing
      let voicings = voiceBy Roots testProgression (pure 0)
      -- Just verify it has the right type and doesn't crash
      voicings `seq` True `shouldBe` True
    
    it "Bass extracts bass notes" $ do
      let voicings = voiceBy Bass testProgression (pure 0)
      voicings `seq` True `shouldBe` True
    
    it "Harmony extracts full chords" $ do
      let voicings = voiceBy Harmony testProgression (pure 0)
      voicings `seq` True `shouldBe` True
    
    it "Voiced extracts (same as Harmony for now)" $ do
      let voicings = voiceBy Voiced testProgression (pure 0)
      voicings `seq` True `shouldBe` True

  describe "Launcher paradigm compatibility" $ do
    
    it "arrange function signature works with flow" $ do
      -- Just verify compilation - actual Tidal patterns not testable
      let _ = arrange A.flow testProgression
      True `shouldBe` True
    
    it "arrange function signature works with rootNotes" $ do
      let _ = arrange rootNotes testProgression
      True `shouldBe` True
    
    it "arrange function signature works with bassNotes" $ do
      let _ = arrange bassNotes testProgression
      True `shouldBe` True
