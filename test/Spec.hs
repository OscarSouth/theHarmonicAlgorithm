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
import qualified Harmonic.Core.PitchSpec as PitchSpec
import qualified Harmonic.Core.HarmonySpec as HarmonySpec
import qualified Harmonic.Core.OvertoneSpec as OvertoneSpec
import qualified Harmonic.Core.VoiceLeadingSpec as VoiceLeadingSpec
import qualified Harmonic.Core.ProgressionSpec as ProgressionSpec
import qualified Harmonic.Core.FilterSpec as FilterSpec

-- Phase C: Interactive Behaviour
import qualified Harmonic.Core.ProbabilisticSpec as ProbabilisticSpec
import qualified Harmonic.Core.BuilderSpec as BuilderSpec
import qualified Harmonic.Database.QuerySpec as QuerySpec
import qualified Harmonic.Tidal.InterfaceSpec as InterfaceSpec

main :: IO ()
main = hspec $ do
  -- Phase B
  describe "Harmonic.Core.Pitch" PitchSpec.spec
  describe "Harmonic.Core.Harmony" HarmonySpec.spec
  describe "Harmonic.Core.Overtone" OvertoneSpec.spec
  describe "Harmonic.Core.VoiceLeading" VoiceLeadingSpec.spec
  describe "Harmonic.Core.Progression" ProgressionSpec.spec
  describe "Harmonic.Core.Filter" FilterSpec.spec
  -- Phase C
  describe "Harmonic.Database.Query" QuerySpec.spec
  describe "Harmonic.Core.Probabilistic" ProbabilisticSpec.spec
  describe "Harmonic.Core.Builder" BuilderSpec.spec
  describe "Harmonic.Tidal.Interface" InterfaceSpec.spec
