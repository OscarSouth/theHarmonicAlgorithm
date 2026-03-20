-- |
-- Module      : Spec
-- Description : Phase B + Phase C validation test suite entry point
--
-- This test suite validates:
--
-- Phase B (Core Music Types):
--   * PitchSpec: ℤ₁₂ algebraic properties (QuickCheck)
--   * HarmonySpec: Legacy chord naming fidelity (Golden tests)
--   * OvertoneSpec: Constructive generation invariants
--   * VoiceLeadingSpec: Cost function behavior
--   * ProgressionSpec: Monoid laws and manipulation
--   * FilterSpec: Legacy filter notation parsing
--
-- Phase C (Interactive Behaviour):
--   * QuerySpec: Composer weight parsing and resolution
--   * ProbabilisticSpec: Gamma distribution sampling
--   * BuilderSpec: Generation engine configuration
--   * InterfaceSpec: TidalCycles pattern interface

module Main where

import Test.Hspec

-- Phase B: Core Music Types
import qualified Harmonic.Rules.Types.PitchSpec as PitchSpec
import qualified Harmonic.Rules.Types.HarmonySpec as HarmonySpec
import qualified Harmonic.Rules.Constraints.OvertoneSpec as OvertoneSpec
import qualified Harmonic.Evaluation.Scoring.VoiceLeadingSpec as VoiceLeadingSpec
import qualified Harmonic.Rules.Types.ProgressionSpec as ProgressionSpec
import qualified Harmonic.Rules.Constraints.FilterSpec as FilterSpec
import qualified Harmonic.Evaluation.Scoring.DissonanceSpec as DissonanceSpec

-- Phase C: Interactive Behaviour
import qualified Harmonic.Traversal.ProbabilisticSpec as ProbabilisticSpec
import qualified Harmonic.Framework.BuilderSpec as BuilderSpec
import qualified Harmonic.Evaluation.Database.QuerySpec as QuerySpec
import qualified Harmonic.Interface.Tidal.BridgeSpec as InterfaceSpec
import qualified Harmonic.Interface.Tidal.GrooveSpec as GrooveSpec

main :: IO ()
main = hspec $ do
  -- Phase B
  describe "Harmonic.Core.Pitch" PitchSpec.spec
  describe "Harmonic.Core.Harmony" HarmonySpec.spec
  describe "Harmonic.Core.Overtone" OvertoneSpec.spec
  describe "Harmonic.Core.VoiceLeading" VoiceLeadingSpec.spec
  describe "Harmonic.Core.Progression" ProgressionSpec.spec
  describe "Harmonic.Core.Filter" FilterSpec.spec
  describe "Harmonic.Core.Dissonance" DissonanceSpec.spec
  -- Phase C
  describe "Harmonic.Database.Query" QuerySpec.spec
  describe "Harmonic.Core.Probabilistic" ProbabilisticSpec.spec
  describe "Harmonic.Core.Builder" BuilderSpec.spec
  describe "Harmonic.Tidal.Interface" InterfaceSpec.spec
  describe "Harmonic.Tidal.Groove" GrooveSpec.spec
