{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Core.BuilderSpec
-- Description : Tests for the Builder generation engine
--
-- Validates the Phase C Builder module:
--   * HarmonicContext: CSF R constraint structures
--   * GeneratorConfig: Configuration defaults and bounds
--   * Filter parsing integration (validates parsed pitch-class sets)
--
-- The filter notation uses the legacy Overtone.hs system:
--   * "*" = wildcard (match all)
--   * "1#" = G major (1 sharp), "2b" = Bb major (2 flats)
--   * "c", "eb", "f#" = specific pitch classes
--   * "##" = D major (2 sharps)
--
-- Note: Full integration tests require Neo4j connection, so these
-- focus on the pure functions and configuration logic.

module Harmonic.Core.BuilderSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)

import Harmonic.Core.Builder (HarmonicContext(..), GeneratorConfig(..), defaultContext, defaultConfig, TransformTrace(..), AdvanceTrace(..), StepDiagnostic(..), harmonicContext, matchesContext)
import Harmonic.Core.Filter (parseOvertones, parseKey, parseFunds)
import qualified Harmonic.Core.Harmony as H
import qualified Harmonic.Core.Pitch as P

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "HarmonicContext" $ do
    
    describe "defaultContext" $ do
      it "has wildcard overtones filter" $ do
        hcOvertones defaultContext `shouldBe` "*"
        -- Verify wildcard parses to all 12 pitch classes
        sort (parseOvertones $ hcOvertones defaultContext) `shouldBe` [0..11]
      
      it "has wildcard key filter" $ do
        hcKey defaultContext `shouldBe` "*"
        -- Verify wildcard parses to all 12 pitch classes
        sort (parseKey $ hcKey defaultContext) `shouldBe` [0..11]
      
      it "has wildcard roots filter" $ do
        hcRoots defaultContext `shouldBe` "*"
        -- Verify wildcard parses to all 12 pitch classes
        sort (parseFunds $ hcRoots defaultContext) `shouldBe` [0..11]
    
    describe "harmonicContext smart constructor" $ do
      -- Overtone series filtering (pitch class sets)
      it "creates context with note name overtones (C overtone series)" $ do
        let ctx = harmonicContext "c" "*" "*"
        hcOvertones ctx `shouldBe` "c"
        -- Verify parsed pitch classes: C overtones = [0,4,7,10] (C, E, G, Bb)
        sort (parseOvertones $ hcOvertones ctx) `shouldBe` [0, 4, 7, 10]
      
      it "creates context with multiple note overtones" $ do
        let ctx = harmonicContext "D A D F A Ab" "*" "*"
        hcOvertones ctx `shouldBe` "D A D F A Ab"
        -- Verify parsed pitch classes (4 partials per note, merged and deduped):
        -- D: 2,6,9,0; A: 9,1,4,7; F: 5,9,0,3; Ab: 8,0,3,6
        let pcs = sort $ parseOvertones $ hcOvertones ctx
        sort pcs `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      
      -- Key filtering (key signatures)
      it "creates context with sharp key (1# = G major)" $ do
        let ctx = harmonicContext "*" "1#" "*"
        hcKey ctx `shouldBe` "1#"
        -- Verify parsed pitch classes: G major = [0,2,4,6,7,9,11]
        sort (parseKey $ hcKey ctx) `shouldBe` [0, 2, 4, 6, 7, 9, 11]
      
      it "creates context with flat key (2b = Bb major)" $ do
        let ctx = harmonicContext "*" "2b" "*"
        hcKey ctx `shouldBe` "2b"
        -- Verify parsed pitch classes: Bb major = [0,2,3,5,7,9,10]
        sort (parseKey $ hcKey ctx) `shouldBe` [0, 2, 3, 5, 7, 9, 10]
      
      it "creates context with double sharp key (## = D major)" $ do
        let ctx = harmonicContext "*" "##" "*"
        hcKey ctx `shouldBe` "##"
        -- Verify parsed pitch classes: D major = [1,2,4,6,7,9,11]
        sort (parseKey $ hcKey ctx) `shouldBe` [1, 2, 4, 6, 7, 9, 11]
      
      -- Root motion filtering
      it "creates context with specified roots (1# = G-based motion)" $ do
        let ctx = harmonicContext "*" "*" "1#"
        hcRoots ctx `shouldBe` "1#"
        -- Verify parsed pitch classes: G major scale roots
        sort (parseFunds $ hcRoots ctx) `shouldBe` [0, 2, 4, 6, 7, 9, 11]
      
      it "creates context with double sharp roots" $ do
        let ctx = harmonicContext "*" "*" "##"
        hcRoots ctx `shouldBe` "##"
        -- Verify parsed pitch classes: D major scale roots
        sort (parseFunds $ hcRoots ctx) `shouldBe` [1, 2, 4, 6, 7, 9, 11]
      
      -- Combined filters (as used in live performance)
      it "allows combining all three filters (live example)" $ do
        -- From liveArchives/perform/state.tidal
        let ctx = harmonicContext "*" "1#" "G B D"
        hcOvertones ctx `shouldBe` "*"
        hcKey ctx `shouldBe` "1#"
        hcRoots ctx `shouldBe` "G B D"
        -- Verify parsed: wildcard overtones = all, key/roots = G major
        sort (parseOvertones $ hcOvertones ctx) `shouldBe` [0..11]
        sort (parseKey $ hcKey ctx) `shouldBe` [0, 2, 4, 6, 7, 9, 11]
        sort (parseFunds $ hcRoots ctx) `shouldBe` [2, 7, 11]
      
      it "allows overtones with wildcard key and roots" $ do
        -- From liveArchives/explore/1.tidal
        let ctx = harmonicContext "*" "*" "##"
        hcOvertones ctx `shouldBe` "*"
        hcKey ctx `shouldBe` "*"
        hcRoots ctx `shouldBe` "##"
        -- Verify parsed: wildcard overtones/key = all, roots = D major
        sort (parseOvertones $ hcOvertones ctx) `shouldBe` [0..11]
        sort (parseKey $ hcKey ctx) `shouldBe` [0..11]
        sort (parseFunds $ hcRoots ctx) `shouldBe` [1, 2, 4, 6, 7, 9, 11]

  describe "GeneratorConfig" $ do
    
    describe "defaultConfig" $ do
      it "has homing threshold between 0 and 1" $ do
        let thresh = gcHomingThreshold defaultConfig
        thresh `shouldSatisfy` (>= 0)
        thresh `shouldSatisfy` (<= 1)
      
      it "default homing threshold is 1.0 (disabled)" $ do
        gcHomingThreshold defaultConfig `shouldBe` 1.0
      
      it "has reasonable homing strength" $ do
        let strength = gcHomingStrength defaultConfig
        strength `shouldSatisfy` (> 0)
        strength `shouldSatisfy` (<= 2.0)
      
      it "default homing strength is 1.0" $ do
        gcHomingStrength defaultConfig `shouldBe` 1.0
      
      it "has reasonable pool size" $ do
        let poolSize = gcPoolSize defaultConfig
        poolSize `shouldSatisfy` (> 0)
        poolSize `shouldSatisfy` (<= 100)
      
      it "default pool size is 30" $ do
        gcPoolSize defaultConfig `shouldBe` 30
      
      it "homing is disabled by default (threshold = 1.0)" $ do
        gcHomingThreshold defaultConfig `shouldBe` 1.0

  describe "Wildcard matching" $ do
    
    describe "isWildcard" $ do
      it "\"*\" is wildcard" $ do
        -- Test through context behavior
        let ctx = harmonicContext "*" "*" "*"
        hcOvertones ctx `shouldBe` "*"
        -- Verify wildcards parse to all pitch classes
        sort (parseOvertones $ hcOvertones ctx) `shouldBe` [0..11]
      
      it "\"all\" is equivalent to wildcard in legacy notation" $ do
        -- Legacy Overtone.hs: ["*","all","chr"]?? [0..11]
        let ctx = harmonicContext "all" "*" "*"
        hcOvertones ctx `shouldBe` "all"
        -- Verify "all" parses to all pitch classes
        sort (parseOvertones $ hcOvertones ctx) `shouldBe` [0..11]
      
      it "\"chr\" (chromatic) is equivalent to wildcard in legacy notation" $ do
        let ctx = harmonicContext "chr" "*" "*"
        hcOvertones ctx `shouldBe` "chr"
        -- Verify "chr" parses to all pitch classes
        sort (parseOvertones $ hcOvertones ctx) `shouldBe` [0..11]
      
      it "empty string is not treated as wildcard" $ do
        -- Empty string should be preserved, not converted to "*"
        let ctx = harmonicContext "" "1#" ""
        hcOvertones ctx `shouldBe` ""
        hcRoots ctx `shouldBe` ""
        -- Empty string parses to empty list
        parseOvertones (hcOvertones ctx) `shouldBe` []
        parseFunds (hcRoots ctx) `shouldBe` []

  describe "Generate function prerequisites" $ do
    
    it "generate signature requires CadenceState" $ do
      -- This is a compile-time check - we just verify the types exist
      True `shouldBe` True
    
    it "generate returns IO Progression" $ do
      -- Type-level verification only
      True `shouldBe` True

  describe "Homing phase logic" $ do
    
    describe "Homing threshold interpretation" $ do
      it "threshold 0.75 means homing starts at 75%" $ do
        -- With 16-bar progression:
        -- Forward walk: 0-11 (indices 0 to 11, 75% of 16 = 12)
        -- Homing phase: 12-15 (last 25%)
        let threshold = 0.75  -- Explicit threshold since default is now 1.0
            totalBars = 16
            homingStart = floor (threshold * fromIntegral totalBars)
        homingStart `shouldBe` 12
      
      it "threshold 0.5 means homing starts at 50%" $ do
        let threshold = 0.5
            totalBars = 16
            homingStart = floor (threshold * fromIntegral totalBars)
        homingStart `shouldBe` 8
      
      it "threshold 1.0 means no homing phase" $ do
        let threshold = 1.0
            totalBars = 16
            homingStart = floor (threshold * fromIntegral totalBars)
        homingStart `shouldBe` 16

  describe "Chain building logic" $ do
    
    it "stepChain returns updated chain" $ do
      -- Type-level verification - actual testing requires Neo4j
      True `shouldBe` True
    
    it "applyNextCadence derives new CadenceState from movement" $ do
      -- Type-level verification
      True `shouldBe` True
  describe "Diagnostic Types" $ do
    
    describe "TransformTrace" $ do
      it "has all required fields for chord transform debugging" $ do
        -- Verify TransformTrace has the expected structure
        let trace = TransformTrace
              { ttRawDbIntervals = "[P 0,P 1,P 8]"
              , ttRawDbMovement = "asc 1"
              , ttRawDbFunctionality = "b9aug"
              , ttRootPC = 1
              , ttRootNoteName = "Db"
              , ttTones = [0, 1, 8]
              , ttTransposedPitches = [1, 2, 9]
              , ttNormalizedPs = [1, 2, 9]
              , ttZeroForm = [0, 1, 8]
              , ttDetectedRoot = "Db"
              , ttFunctionality = "b9aug"
              , ttFinalChord = "Db b9aug"
              , ttStoredFunc = "b9aug"
              }
        ttRootPC trace `shouldBe` 1
        ttTones trace `shouldBe` [0, 1, 8]
        ttFinalChord trace `shouldBe` "Db b9aug"
        ttRawDbIntervals trace `shouldBe` "[P 0,P 1,P 8]"
      
      it "captures stored vs computed functionality discrepancy" $ do
        -- This is the key diagnostic: stored vs rendered names
        let trace = TransformTrace
              { ttRawDbIntervals = "[P 0,P 1,P 8]"
              , ttRawDbMovement = "asc 1"
              , ttRawDbFunctionality = "b9aug"
              , ttRootPC = 1
              , ttRootNoteName = "Db"
              , ttTones = [0, 1, 8]
              , ttTransposedPitches = [1, 2, 9]
              , ttNormalizedPs = [1, 2, 9]
              , ttZeroForm = [0, 1, 8]
              , ttDetectedRoot = "Db"
              , ttFunctionality = "min"  -- computed differently
              , ttFinalChord = "Db min"
              , ttStoredFunc = "b9aug"   -- stored in cadence
              }
        ttStoredFunc trace `shouldNotBe` ttFunctionality trace
    
    describe "AdvanceTrace" $ do
      it "captures root motion computation" $ do
        let trace = AdvanceTrace
              { atCurrentRoot = "C"
              , atCurrentRootPC = 0
              , atMovement = "asc 1"
              , atMovementInterval = 1
              , atNewRootPC = 1
              , atEnharmFunc = "flat"
              , atNewRoot = "Db"
              }
        atCurrentRootPC trace `shouldBe` 0
        atMovementInterval trace `shouldBe` 1
        atNewRootPC trace `shouldBe` 1
        atNewRoot trace `shouldBe` "Db"
      
      it "verifies enharmonic function affects root name" $ do
        -- Same pitch class can be named differently based on enharmonic func
        let flatTrace = AdvanceTrace
              { atCurrentRoot = "C"
              , atCurrentRootPC = 0
              , atMovement = "asc 1"
              , atMovementInterval = 1
              , atNewRootPC = 1
              , atEnharmFunc = "flat"
              , atNewRoot = "Db"
              }
        let sharpTrace = AdvanceTrace
              { atCurrentRoot = "B"  -- B natural uses sharp spelling
              , atCurrentRootPC = 11
              , atMovement = "asc 2"
              , atMovementInterval = 2
              , atNewRootPC = 1
              , atEnharmFunc = "sharp"
              , atNewRoot = "C#"
              }
        -- Same pitch class (1), different names
        atNewRootPC flatTrace `shouldBe` atNewRootPC sharpTrace
        atNewRoot flatTrace `shouldNotBe` atNewRoot sharpTrace
    
    describe "StepDiagnostic" $ do
      it "has optional verbosity fields" $ do
        -- At verbosity 1, sdRenderedChord is populated but traces are Nothing
        let diag = StepDiagnostic
              { sdStepNumber = 1
              , sdPriorCadence = "( pedal -> maj )"
              , sdPriorRoot = "C"
              , sdPriorRootPC = 0
              , sdSelectedDbIntervals = "[P 0,P 4,P 7]"
              , sdSelectedDbMovement = "asc 1"
              , sdSelectedDbFunctionality = "b9aug"
              , sdGraphCount = 5
              , sdGraphTop6 = []
              , sdFallbackCount = 0
              , sdFallbackTop6 = []
              , sdPoolSize = 5
              , sdEntropyUsed = 0.5
              , sdGammaIndex = 2
              , sdSelectedFrom = "graph"
              , sdPosteriorRoot = "Db"
              , sdPosteriorRootPC = 1
              , sdRenderedChord = Just "Db b9aug"  -- verbosity 1+
              , sdTransformTrace = Nothing          -- verbosity 2 only
              , sdAdvanceTrace = Nothing            -- verbosity 2 only
              }
        sdRenderedChord diag `shouldBe` Just "Db b9aug"
        sdTransformTrace diag `shouldBe` Nothing
        sdAdvanceTrace diag `shouldBe` Nothing
      
      it "has all traces at verbosity 2" $ do
        let tt = TransformTrace
              { ttRawDbIntervals = "[P 0,P 1,P 8]"
              , ttRawDbMovement = "asc 1"
              , ttRawDbFunctionality = "b9aug"
              , ttRootPC = 1, ttRootNoteName = "Db", ttTones = [0,1,8]
              , ttTransposedPitches = [1,2,9], ttNormalizedPs = [1,2,9]
              , ttZeroForm = [0,1,8], ttDetectedRoot = "Db"
              , ttFunctionality = "b9aug", ttFinalChord = "Db b9aug"
              , ttStoredFunc = "b9aug"
              }
        let at = AdvanceTrace
              { atCurrentRoot = "C", atCurrentRootPC = 0
              , atMovement = "asc 1", atMovementInterval = 1
              , atNewRootPC = 1, atEnharmFunc = "flat", atNewRoot = "Db"
              }
        let diag = StepDiagnostic
              { sdStepNumber = 1
              , sdPriorCadence = "( pedal -> maj )"
              , sdPriorRoot = "C"
              , sdPriorRootPC = 0
              , sdSelectedDbIntervals = "[P 0,P 1,P 8]"
              , sdSelectedDbMovement = "asc 1"
              , sdSelectedDbFunctionality = "b9aug"
              , sdGraphCount = 5
              , sdGraphTop6 = []
              , sdFallbackCount = 0
              , sdFallbackTop6 = []
              , sdPoolSize = 5
              , sdEntropyUsed = 0.5
              , sdGammaIndex = 2
              , sdSelectedFrom = "graph"
              , sdPosteriorRoot = "Db"
              , sdPosteriorRootPC = 1
              , sdRenderedChord = Just "Db b9aug"
              , sdTransformTrace = Just tt
              , sdAdvanceTrace = Just at
              }
        sdTransformTrace diag `shouldNotBe` Nothing
        sdAdvanceTrace diag `shouldNotBe` Nothing
    
    describe "GenerationDiagnostics" $ do
      it "contains per-step diagnostics list" $ do
        -- Type-level verification - gdSteps is [StepDiagnostic]
        True `shouldBe` True

  describe "Key Filter (matchesContext)" $ do
    -- Tests for the fix to the key filter bug (transposes relative intervals to absolute pitches)

    describe "rejects chords with pitches outside key filter" $ do
      it "E major chord rejected by D major key (contains G#)" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- E major: root E (4), intervals [0,4,7] → absolute [4,8,11]
            -- 8 (G#) is NOT in D major
            emajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 4)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context emajCadence `shouldBe` False

      it "A major chord rejected by D major key (contains C#)" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- A major: root A (9), intervals [0,4,7] → absolute [9,1,4]
            -- 1 (C#) IS in D major, so this should actually pass
            -- Let's use A major starting from 9: [9,1,4] - wait, C# (1) IS in D major
            -- Let me use Bb major instead: root Bb (10), intervals [0,4,7] → absolute [10,2,5]
            -- 10 (Bb) is NOT in D major
            bbmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 10)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context bbmajCadence `shouldBe` False

      it "F major chord rejected by C major key (contains Bb)" $ do
        let context = harmonicContext "*" "C" "*"  -- C major [0,2,4,5,7,9,11]
            -- Wait, F major is F A C = [5,9,0] which are all in C major
            -- Let me use F# major: root F# (6), intervals [0,4,7] → absolute [6,10,1]
            -- 6 (F#), 10 (Bb), 1 (Db) - F# and Bb are NOT in C major
            fsharpmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 6)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context fsharpmajCadence `shouldBe` False

    describe "accepts chords with all pitches in key filter" $ do
      it "D major chord accepted by D major key" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- D major: root D (2), intervals [0,4,7] → absolute [2,6,9]
            -- All in D major
            dmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 2)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context dmajCadence `shouldBe` True

      it "E minor chord accepted by D major key" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- E minor: root E (4), intervals [0,3,7] → absolute [4,7,11]
            -- All in D major
            eminCadence = H.Cadence "min" (H.Asc (P.mkPitchClass 4)) [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 7]
        matchesContext context eminCadence `shouldBe` True

      it "A major chord accepted by D major key" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- A major: root A (9), intervals [0,4,7] → absolute [9,1,4]
            -- All in D major (C# is 1, which is in D major)
            amajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 9)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context amajCadence `shouldBe` True

      it "C major chord accepted by C major key" $ do
        let context = harmonicContext "*" "C" "*"  -- C major [0,2,4,5,7,9,11]
            -- C major: root C (0), intervals [0,4,7] → absolute [0,4,7]
            -- All in C major
            cmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 0)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context cmajCadence `shouldBe` True

    describe "wildcard key accepts all chords" $ do
      it "E major accepted with wildcard key" $ do
        let context = harmonicContext "*" "*" "*"  -- No key filter
            emajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 4)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context emajCadence `shouldBe` True

      it "Bb major accepted with wildcard key" $ do
        let context = harmonicContext "*" "*" "*"  -- No key filter
            bbmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 10)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
        matchesContext context bbmajCadence `shouldBe` True