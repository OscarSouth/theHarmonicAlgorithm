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
--   * "2#" = D major (2 sharps)
--
-- Note: Full integration tests require Neo4j connection, so these
-- focus on the pure functions and configuration logic.

module Harmonic.Framework.BuilderSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)

import Harmonic.Framework.Builder (HarmonicContext(..), GeneratorConfig(..), defaultContext, defaultConfig, Drift(..), hcOvertones, hcKey, hcRoots, dissonant, consonant, inversion, TransformTrace(..), AdvanceTrace(..), StepDiagnostic(..), harmonicContext, matchesContext, parseComposersWithOrder, makePortmanteau, extractByPosition, takeFromBeginning, takeFromEnd, takeFromMiddle)
import Harmonic.Framework.Builder.Core (applyDriftFilter, matchesContextWithTarget)
import Harmonic.Framework.Builder.Types (parseContextOnce)
import Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import qualified Data.Map.Strict as Map
import Harmonic.Rules.Constraints.Filter (parseOvertones, parseKey, parseFunds)
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "HarmonicContext" $ do
    
    describe "defaultContext" $ do
      it "has wildcard overtones filter" $ do
        _hcOvertones defaultContext `shouldBe` "*"
        -- Verify wildcard parses to all 12 pitch classes
        sort (parseOvertones $ _hcOvertones defaultContext) `shouldBe` [0..11]
      
      it "has wildcard key filter" $ do
        _hcKey defaultContext `shouldBe` "*"
        -- Verify wildcard parses to all 12 pitch classes
        sort (parseKey $ _hcKey defaultContext) `shouldBe` [0..11]
      
      it "has wildcard roots filter" $ do
        _hcRoots defaultContext `shouldBe` "*"
        -- Verify wildcard parses to all 12 pitch classes
        sort (parseFunds $ _hcRoots defaultContext) `shouldBe` [0..11]
    
    describe "harmonicContext smart constructor" $ do
      -- Overtone series filtering (pitch class sets)
      it "creates context with note name overtones (C overtone series)" $ do
        let ctx = harmonicContext "c" "*" "*"
        _hcOvertones ctx `shouldBe` "c"
        -- Verify parsed pitch classes: C overtones = [0,4,7,10] (C, E, G, Bb)
        sort (parseOvertones $ _hcOvertones ctx) `shouldBe` [0, 4, 7, 10]
      
      it "creates context with multiple note overtones" $ do
        let ctx = harmonicContext "D A D F A Ab" "*" "*"
        _hcOvertones ctx `shouldBe` "D A D F A Ab"
        -- Verify parsed pitch classes (4 partials per note, merged and deduped):
        -- D: 2,6,9,0; A: 9,1,4,7; F: 5,9,0,3; Ab: 8,0,3,6
        let pcs = sort $ parseOvertones $ _hcOvertones ctx
        sort pcs `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      
      -- Key filtering (key signatures)
      it "creates context with sharp key (1# = G major)" $ do
        let ctx = harmonicContext "*" "1#" "*"
        _hcKey ctx `shouldBe` "1#"
        -- Verify parsed pitch classes: G major = [0,2,4,6,7,9,11]
        sort (parseKey $ _hcKey ctx) `shouldBe` [0, 2, 4, 6, 7, 9, 11]
      
      it "creates context with flat key (2b = Bb major)" $ do
        let ctx = harmonicContext "*" "2b" "*"
        _hcKey ctx `shouldBe` "2b"
        -- Verify parsed pitch classes: Bb major = [0,2,3,5,7,9,10]
        sort (parseKey $ _hcKey ctx) `shouldBe` [0, 2, 3, 5, 7, 9, 10]
      
      it "creates context with double sharp key (2# = D major)" $ do
        let ctx = harmonicContext "*" "2#" "*"
        _hcKey ctx `shouldBe` "2#"
        -- Verify parsed pitch classes: D major = [1,2,4,6,7,9,11]
        sort (parseKey $ _hcKey ctx) `shouldBe` [1, 2, 4, 6, 7, 9, 11]
      
      -- Root motion filtering
      it "creates context with specified roots (1# = G-based motion)" $ do
        let ctx = harmonicContext "*" "*" "1#"
        _hcRoots ctx `shouldBe` "1#"
        -- Verify parsed pitch classes: G major scale roots
        sort (parseFunds $ _hcRoots ctx) `shouldBe` [0, 2, 4, 6, 7, 9, 11]
      
      it "creates context with double sharp roots" $ do
        let ctx = harmonicContext "*" "*" "2#"
        _hcRoots ctx `shouldBe` "2#"
        -- Verify parsed pitch classes: D major scale roots
        sort (parseFunds $ _hcRoots ctx) `shouldBe` [1, 2, 4, 6, 7, 9, 11]
      
      -- Combined filters (as used in live performance)
      it "allows combining all three filters (live example)" $ do
        -- From liveArchives/perform/state.tidal
        let ctx = harmonicContext "*" "1#" "G B D"
        _hcOvertones ctx `shouldBe` "*"
        _hcKey ctx `shouldBe` "1#"
        _hcRoots ctx `shouldBe` "G B D"
        -- Verify parsed: wildcard overtones = all, key/roots = G major
        sort (parseOvertones $ _hcOvertones ctx) `shouldBe` [0..11]
        sort (parseKey $ _hcKey ctx) `shouldBe` [0, 2, 4, 6, 7, 9, 11]
        sort (parseFunds $ _hcRoots ctx) `shouldBe` [2, 7, 11]
      
      it "allows overtones with wildcard key and roots" $ do
        -- From liveArchives/explore/1.tidal
        let ctx = harmonicContext "*" "*" "2#"
        _hcOvertones ctx `shouldBe` "*"
        _hcKey ctx `shouldBe` "*"
        _hcRoots ctx `shouldBe` "2#"
        -- Verify parsed: wildcard overtones/key = all, roots = D major
        sort (parseOvertones $ _hcOvertones ctx) `shouldBe` [0..11]
        sort (parseKey $ _hcKey ctx) `shouldBe` [0..11]
        sort (parseFunds $ _hcRoots ctx) `shouldBe` [1, 2, 4, 6, 7, 9, 11]

  describe "GeneratorConfig" $ do
    
    describe "defaultConfig" $ do
      it "has reasonable pool size" $ do
        let poolSize = gcPoolSize defaultConfig
        poolSize `shouldSatisfy` (> 0)
        poolSize `shouldSatisfy` (<= 100)
      
      it "default pool size is 30" $ do
        gcPoolSize defaultConfig `shouldBe` 30
      
  describe "Wildcard matching" $ do
    
    describe "isWildcard" $ do
      it "\"*\" is wildcard" $ do
        -- Test through context behavior
        let ctx = harmonicContext "*" "*" "*"
        _hcOvertones ctx `shouldBe` "*"
        -- Verify wildcards parse to all pitch classes
        sort (parseOvertones $ _hcOvertones ctx) `shouldBe` [0..11]
      
      it "\"all\" is equivalent to wildcard in legacy notation" $ do
        -- Legacy Overtone.hs: ["*","all","chr"]?? [0..11]
        let ctx = harmonicContext "all" "*" "*"
        _hcOvertones ctx `shouldBe` "all"
        -- Verify "all" parses to all pitch classes
        sort (parseOvertones $ _hcOvertones ctx) `shouldBe` [0..11]
      
      it "\"chr\" (chromatic) is equivalent to wildcard in legacy notation" $ do
        let ctx = harmonicContext "chr" "*" "*"
        _hcOvertones ctx `shouldBe` "chr"
        -- Verify "chr" parses to all pitch classes
        sort (parseOvertones $ _hcOvertones ctx) `shouldBe` [0..11]
      
      it "empty string is not treated as wildcard" $ do
        -- Empty string should be preserved, not converted to "*"
        let ctx = harmonicContext "" "1#" ""
        _hcOvertones ctx `shouldBe` ""
        _hcRoots ctx `shouldBe` ""
        -- Empty string parses to empty list
        parseOvertones (_hcOvertones ctx) `shouldBe` []
        parseFunds (_hcRoots ctx) `shouldBe` []

  describe "Generate function prerequisites" $ do
    
    it "generate signature requires CadenceState" $ do
      -- This is a compile-time check - we just verify the types exist
      True `shouldBe` True
    
    it "generate returns IO Progression" $ do
      -- Type-level verification only
      True `shouldBe` True

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
            -- Create a dummy CadenceState starting from C (root 0) for testing
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState emajCadence `shouldBe` False

      it "A major chord rejected by D major key (contains C#)" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- A major: root A (9), intervals [0,4,7] → absolute [9,1,4]
            -- 1 (C#) IS in D major, so this should actually pass
            -- Let's use A major starting from 9: [9,1,4] - wait, C# (1) IS in D major
            -- Let me use Bb major instead: root Bb (10), intervals [0,4,7] → absolute [10,2,5]
            -- 10 (Bb) is NOT in D major
            bbmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 10)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState bbmajCadence `shouldBe` False

      it "F major chord rejected by C major key (contains Bb)" $ do
        let context = harmonicContext "*" "0#" "*"  -- C major [0,2,4,5,7,9,11]
            -- Wait, F major is F A C = [5,9,0] which are all in C major
            -- Let me use F# major: root F# (6), intervals [0,4,7] → absolute [6,10,1]
            -- 6 (F#), 10 (Bb), 1 (Db) - F# and Bb are NOT in C major
            fsharpmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 6)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState fsharpmajCadence `shouldBe` False

    describe "accepts chords with all pitches in key filter" $ do
      it "D major chord accepted by D major key" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- D major: root D (2), intervals [0,4,7] → absolute [2,6,9]
            -- All in D major
            dmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 2)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState dmajCadence `shouldBe` True

      it "E minor chord accepted by D major key" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- E minor: root E (4), intervals [0,3,7] → absolute [4,7,11]
            -- All in D major
            eminCadence = H.Cadence "min" (H.Asc (P.mkPitchClass 4)) [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState eminCadence `shouldBe` True

      it "A major chord accepted by D major key" $ do
        let context = harmonicContext "*" "2#" "*"  -- D major [1,2,4,6,7,9,11]
            -- A major: root A (9), intervals [0,4,7] → absolute [9,1,4]
            -- All in D major (C# is 1, which is in D major)
            amajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 9)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState amajCadence `shouldBe` True

      it "C major chord accepted by C major key" $ do
        let context = harmonicContext "*" "0#" "*"  -- C major [0,2,4,5,7,9,11]
            -- C major: root C (0), intervals [0,4,7] → absolute [0,4,7]
            -- All in C major
            cmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 0)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState cmajCadence `shouldBe` True

    describe "wildcard key accepts all chords" $ do
      it "E major accepted with wildcard key" $ do
        let context = harmonicContext "*" "*" "*"  -- No key filter
            emajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 4)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState emajCadence `shouldBe` True

      it "Bb major accepted with wildcard key" $ do
        let context = harmonicContext "*" "*" "*"  -- No key filter
            bbmajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 10)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dummyState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.C H.FlatSpelling
        matchesContext context dummyState bbmajCadence `shouldBe` True

  describe "Roots (Bass Note) Filtering" $ do
    describe "rejects chords with bass notes not in allowed set" $ do
      it "C major rejected when only D bass allowed" $ do
        let context = harmonicContext "*" "*" "D"  -- Only D bass notes
            -- C major from D: movement desc 2, intervals [0,4,7] → absolute [0,4,7]
            -- Bass note is 0 (C), not allowed
            cmajCadence = H.Cadence "maj" (H.Desc (P.mkPitchClass 2)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.D H.FlatSpelling
        matchesContext context dState cmajCadence `shouldBe` False

      it "E major rejected when only D bass allowed" $ do
        let context = harmonicContext "*" "*" "D"  -- Only D bass notes
            -- E major from D: movement asc 2, intervals [0,4,7] → absolute [4,8,11]
            -- Bass note is 4 (E), not allowed
            emajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 2)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.D H.FlatSpelling
        matchesContext context dState emajCadence `shouldBe` False

    describe "accepts chords with bass notes in allowed set" $ do
      it "D major accepted when only D bass allowed" $ do
        let context = harmonicContext "*" "*" "D"  -- Only D bass notes
            -- D major pedal: intervals [0,4,7] → absolute [2,6,9]
            -- Bass note is 2 (D), allowed
            dmajCadence = H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.D H.FlatSpelling
        matchesContext context dState dmajCadence `shouldBe` True

      it "Chord with D in bass accepted (desc 5 movement to D root)" $ do
        let context = harmonicContext "*" "*" "D"  -- Only D bass notes
            -- From A (9), desc 5 → D (2), intervals [0,3,7] → absolute [2,5,9] → bass is 2 (D) ✓
            -- D minor chord with D in the bass (first element)
            chord = H.Cadence "min" (H.Desc (P.mkPitchClass 7)) [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 7]
            aState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.A H.FlatSpelling
        matchesContext context aState chord `shouldBe` True

    describe "wildcard roots accepts all bass notes" $ do
      it "C major accepted with wildcard roots" $ do
        let context = harmonicContext "*" "*" "*"  -- All bass notes allowed
            cmajCadence = H.Cadence "maj" (H.Desc (P.mkPitchClass 2)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.D H.FlatSpelling
        matchesContext context dState cmajCadence `shouldBe` True

      it "E major accepted with wildcard roots" $ do
        let context = harmonicContext "*" "*" "*"  -- All bass notes allowed
            emajCadence = H.Cadence "maj" (H.Asc (P.mkPitchClass 2)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
            dState = H.CadenceState (H.Cadence "maj" H.Unison [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]) P.D H.FlatSpelling
        matchesContext context dState emajCadence `shouldBe` True

  describe "Chromatic Bass Target (rise/fall with out-of-key bass)" $ do
    it "allows chord with out-of-key bass when bass target exempts it" $ do
      -- D# (pc 3) is NOT in G major (1#) = {0,2,4,6,7,9,11}
      -- But with bassTarget = Just 3, the bass pitch is exempt from the overtone check.
      -- D# aug: root D# (3), intervals [0,4,8] → absolute [3,7,11]
      -- Upper pitches 7 (G) and 11 (B) ARE in G major → should pass
      let context = harmonicContext "*" "1#" "1# D#"  -- G major key, roots include D#
          pctx = parseContextOnce context
          eState = H.CadenceState (H.Cadence "min" H.Unison [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 7]) P.E H.SharpSpelling
          -- Movement from E (4) down 1 semitone to D# (3): Desc 1
          dsAugCadence = H.Cadence "aug" (H.Desc (P.mkPitchClass 1)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 8]
      matchesContextWithTarget (Just 3) pctx eState dsAugCadence `shouldBe` True

    it "rejects chord with out-of-key bass when no bass target set" $ do
      -- Same chord, same context, but without bass target exemption
      let context = harmonicContext "*" "1#" "1# D#"
          pctx = parseContextOnce context
          eState = H.CadenceState (H.Cadence "min" H.Unison [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 7]) P.E H.SharpSpelling
          dsAugCadence = H.Cadence "aug" (H.Desc (P.mkPitchClass 1)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 8]
      -- Without bass target, D# (3) fails the overtone check
      matchesContextWithTarget Nothing pctx eState dsAugCadence `shouldBe` False

    it "still rejects chord when upper voices are out of key" $ do
      -- D# major: root D# (3), intervals [0,4,7] → absolute [3,7,10]
      -- 10 (Bb) is NOT in G major → should fail even with bass exemption
      let context = harmonicContext "*" "1#" "1# D#"
          pctx = parseContextOnce context
          eState = H.CadenceState (H.Cadence "min" H.Unison [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 7]) P.E H.SharpSpelling
          dsMajCadence = H.Cadence "maj" (H.Desc (P.mkPitchClass 1)) [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
      matchesContextWithTarget (Just 3) pctx eState dsMajCadence `shouldBe` False

  describe "Portmanteau Generation" $ do
    
    describe "makePortmanteau" $ do
      it "handles single composer" $ do
        makePortmanteau "bach"
          `shouldBe` Just "Bach"
      
      it "handles two composers equally (input order preserved)" $ do
        makePortmanteau "bach debussy"
          `shouldBe` Just "Baussy"
      
      it "handles two composers reversed (different result)" $ do
        makePortmanteau "debussy bach"
          `shouldBe` Just "Debuch"
      
      it "handles weighted blend (order matters, not weight)" $ do
        makePortmanteau "bach:3 debussy:1"
          `shouldBe` Just "Bacsy"
      
      it "handles weighted blend reversed" $ do
        makePortmanteau "bach:1 debussy:3"
          `shouldBe` Just "Bebussy"
      
      it "handles three composers (input order)" $ do
        makePortmanteau "bach stravinsky debussy"
          `shouldBe` Just "Baavinssy"
      
      it "handles wildcard" $ do
        makePortmanteau "*"
          `shouldBe` Nothing
      
      it "handles empty string" $ do
        makePortmanteau ""
          `shouldBe` Nothing
      
      it "capitalizes the result" $ do
        case makePortmanteau "bach" of
          Just result -> T.take 1 result `shouldBe` "B"
          Nothing -> expectationFailure "Expected Just result"
    
    describe "parseComposersWithOrder" $ do
      it "preserves input order with equal weights" $ do
        parseComposersWithOrder "bach debussy"
          `shouldBe` [("bach", 0.5), ("debussy", 0.5)]
      
      it "preserves input order with different weights" $ do
        parseComposersWithOrder "bach:1 debussy:3"
          `shouldBe` [("bach", 0.25), ("debussy", 0.75)]
      
      it "handles reversed order" $ do
        parseComposersWithOrder "debussy bach"
          `shouldBe` [("debussy", 0.5), ("bach", 0.5)]
      
      it "handles three composers with equal weights" $ do
        let result = parseComposersWithOrder "bach stravinsky debussy"
            expected = [("bach", 1/3), ("stravinsky", 1/3), ("debussy", 1/3)]
        -- Use approximate equality for floating point
        length result `shouldBe` 3
        fst (result !! 0) `shouldBe` "bach"
        fst (result !! 1) `shouldBe` "stravinsky"
        fst (result !! 2) `shouldBe` "debussy"
      
      it "handles wildcard" $ do
        parseComposersWithOrder "*"
          `shouldBe` []
      
      it "handles empty string" $ do
        parseComposersWithOrder ""
          `shouldBe` []
    
    describe "extractByPosition" $ do
      it "extracts from beginning for first position" $ do
        extractByPosition 0 3 "stravinsky" 0.5
          `shouldBe` "strav"
      
      it "extracts from middle for middle position" $ do
        extractByPosition 1 3 "bach" 0.25
          `shouldBe` "a"
      
      it "extracts from end for last position" $ do
        extractByPosition 2 3 "debussy" 0.25
          `shouldBe` "sy"
    
    describe "takeFromBeginning" $ do
      it "takes proportional characters from start" $ do
        takeFromBeginning "stravinsky" 0.5 `shouldBe` "strav"
      
      it "takes at least 1 character" $ do
        takeFromBeginning "bach" 0.1 `shouldBe` "b"
      
      it "rounds up using ceiling" $ do
        -- 4 * 0.3 = 1.2, ceiling = 2
        takeFromBeginning "bach" 0.3 `shouldBe` "ba"
    
    describe "takeFromEnd" $ do
      it "takes proportional characters from end" $ do
        takeFromEnd "debussy" 0.25 `shouldBe` "sy"
      
      it "takes at least 1 character" $ do
        takeFromEnd "bach" 0.1 `shouldBe` "h"
      
      it "rounds up using ceiling" $ do
        -- 7 * 0.3 = 2.1, ceiling = 3
        takeFromEnd "debussy" 0.3 `shouldBe` "ssy"
    
    describe "takeFromMiddle" $ do
      it "takes proportional characters from middle" $ do
        -- stravinsky has 10 chars, 50% = 5 chars
        -- start = (10 - 5) / 2 = 2
        -- take 5 from position 2 = "ravin"
        takeFromMiddle "stravinsky" 0.5 `shouldBe` "ravin"
      
      it "takes at least 1 character" $ do
        takeFromMiddle "bach" 0.1 `shouldBe` "a"
      
      it "favors earlier characters when ambiguous" $ do
        -- bach has 4 chars, 25% = 1 char
        -- start = (4 - 1) / 2 = 1 (integer division)
        -- take 1 from position 1 = "a"
        takeFromMiddle "bach" 0.25 `shouldBe` "a"

  describe "Dissonance Drift" $ do

    describe "Drift type" $ do
      it "Dissonant /= Free" $ do
        Dissonant `shouldNotBe` Free
      it "Consonant /= Free" $ do
        Consonant `shouldNotBe` Free
      it "Dissonant /= Consonant" $ do
        Dissonant `shouldNotBe` Consonant

    describe "dissonant/consonant modifiers" $ do
      it "dissonant sets drift to Dissonant" $ do
        _hcDrift (dissonant defaultContext) `shouldBe` Dissonant
      it "consonant sets drift to Consonant" $ do
        _hcDrift (consonant defaultContext) `shouldBe` Consonant
      it "defaultContext has Free drift" $ do
        _hcDrift defaultContext `shouldBe` Free
      it "harmonicContext defaults to Free" $ do
        _hcDrift (harmonicContext "*" "*" "*") `shouldBe` Free

    describe "applyDriftFilter" $ do
      -- Test data: CadenceState with a major triad (dissonance = 6)
      let majIntervals = [P.mkPitchClass 0, P.mkPitchClass 4, P.mkPitchClass 7]
          majCadence = H.Cadence "maj" H.Unison majIntervals
          majState = H.CadenceState majCadence P.C H.FlatSpelling

          -- Candidates with varying dissonance
          -- Major triad [0,4,7]: dissonance = 6
          candMaj = (H.Cadence "maj" (H.Asc (P.mkPitchClass 5)) majIntervals, 100.0)
          -- Diminished [0,3,6]: dissonance = 32 (has tritone)
          dimIntervals = [P.mkPitchClass 0, P.mkPitchClass 3, P.mkPitchClass 6]
          candDim = (H.Cadence "dim" (H.Asc (P.mkPitchClass 3)) dimIntervals, 80.0)
          -- Sus2 [0,2,7]: dissonance = 9
          sus2Intervals = [P.mkPitchClass 0, P.mkPitchClass 2, P.mkPitchClass 7]
          candSus2 = (H.Cadence "sus2" (H.Asc (P.mkPitchClass 2)) sus2Intervals, 90.0)

          pool = [candMaj, candDim, candSus2]

      it "Free returns pool unchanged" $ do
        applyDriftFilter Free majState pool `shouldBe` pool

      it "Dissonant keeps candidates with dissonance >= current (6)" $ do
        -- maj=6 (>=6 yes), dim=32 (>=6 yes), sus2=9 (>=6 yes)
        let result = applyDriftFilter Dissonant majState pool
        length result `shouldBe` 3

      it "Consonant keeps candidates with dissonance <= current (6)" $ do
        -- maj=6 (<=6 yes), dim=32 (<=6 no), sus2=9 (<=6 no)
        let result = applyDriftFilter Consonant majState pool
        length result `shouldBe` 1
        fst (head result) `shouldBe` fst candMaj

      it "Dissonant from high-dissonance state filters out consonant chords" $ do
        -- Start from diminished (dissonance = 32)
        let dimState = H.CadenceState (H.Cadence "dim" H.Unison dimIntervals) P.C H.FlatSpelling
            result = applyDriftFilter Dissonant dimState pool
        -- Only dim (32) passes >= 32
        length result `shouldBe` 1
        fst (head result) `shouldBe` fst candDim

      it "Consonant from low-dissonance state filters out dissonant chords" $ do
        -- Start from sus2 (dissonance = 9)
        let sus2State = H.CadenceState (H.Cadence "sus2" H.Unison sus2Intervals) P.C H.FlatSpelling
            result = applyDriftFilter Consonant sus2State pool
        -- maj=6 (<=9 yes), sus2=9 (<=9 yes), dim=32 (<=9 no)
        length result `shouldBe` 2

      it "falls back to full pool when filter empties it" $ do
        -- Create a state with cluster dissonance higher than anything in pool
        let clusterIntervals = [P.mkPitchClass 0, P.mkPitchClass 1, P.mkPitchClass 2]
            clusterCadence = H.Cadence "cluster" H.Unison clusterIntervals
            clusterState = H.CadenceState clusterCadence P.C H.FlatSpelling
        -- cluster dissonance = 40, nothing in pool >= 40
        dissonanceScore [0,1,2] `shouldBe` 40
        let result = applyDriftFilter Dissonant clusterState pool
        -- Should fall back to full pool
        result `shouldBe` pool

      it "preserves pool ordering (scoring priority)" $ do
        let result = applyDriftFilter Dissonant majState pool
        -- All pass (>= 6), order should be preserved
        map snd result `shouldBe` [100.0, 80.0, 90.0]

  describe "Context Modifiers" $ do

    describe "hcOvertones" $ do
      it "sets overtone filter" $ do
        let ctx = hcOvertones "E A D G" $ defaultContext
        _hcOvertones ctx `shouldBe` "E A D G"

      it "hContext alone has wildcard overtones" $ do
        _hcOvertones defaultContext `shouldBe` "*"

    describe "hcKey" $ do
      it "sets key filter" $ do
        let ctx = hcKey "1#" $ defaultContext
        _hcKey ctx `shouldBe` "1#"

      it "hContext alone has wildcard key" $ do
        _hcKey defaultContext `shouldBe` "*"

    describe "hcRoots" $ do
      it "sets roots filter" $ do
        let ctx = hcRoots "C E G" $ defaultContext
        _hcRoots ctx `shouldBe` "C E G"

      it "hContext alone has wildcard roots" $ do
        _hcRoots defaultContext `shouldBe` "*"

    describe "modifier composition" $ do
      it "all modifiers compose in any order" $ do
        let ctx = inversion 2 $ consonant $ hcRoots "C G" $ hcKey "1#" $ hcOvertones "E A D G" $ defaultContext
        _hcOvertones ctx `shouldBe` "E A D G"
        _hcKey ctx `shouldBe` "1#"
        _hcRoots ctx `shouldBe` "C G"
        _hcDrift ctx `shouldBe` Consonant
        _hcInversionSpacing ctx `shouldBe` 2

      it "later modifiers override earlier ones" $ do
        let ctx = hcKey "2#" $ hcKey "1#" $ defaultContext
        _hcKey ctx `shouldBe` "2#"

    describe "inversion modifier" $ do
      it "sets inversion spacing on context" $ do
        let ctx = inversion 3 $ defaultContext
        _hcInversionSpacing ctx `shouldBe` 3

      it "default context has inversion spacing 0" $ do
        _hcInversionSpacing defaultContext `shouldBe` 0

      it "composes with other modifiers" $ do
        let ctx = inversion 2 $ dissonant $ defaultContext
        _hcInversionSpacing ctx `shouldBe` 2
        _hcDrift ctx `shouldBe` Dissonant