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
import qualified Harmonic.Evaluation.Scoring.ProgressionSpec as ScoringProgressionSpec
import qualified Harmonic.Rules.Types.ProgressionSpec as ProgressionSpec
import qualified Harmonic.Rules.Types.ProgressionContextSpec as ProgressionContextSpec
import qualified Harmonic.Rules.Types.ScaleSpec as ScaleSpec
import qualified Harmonic.Rules.Constraints.FilterSpec as FilterSpec
import qualified Harmonic.Rules.Import.TransformSpec as TransformSpec
import qualified Harmonic.Rules.Import.MergeSpec as MergeSpec
import qualified Harmonic.Rules.Import.GraphSpec as GraphSpec
import qualified Harmonic.Rules.Import.JazzSpec as JazzSpec
import qualified Harmonic.Evaluation.Scoring.DissonanceSpec as DissonanceSpec

-- Phase C: Interactive Behaviour
import qualified Harmonic.Traversal.ProbabilisticSpec as ProbabilisticSpec
import qualified Harmonic.Traversal.WalkingBassSpec as WalkingBassSpec
import qualified Harmonic.Framework.BuilderSpec as BuilderSpec
import qualified Harmonic.Framework.BuilderPSpec as BuilderPSpec
import qualified Harmonic.Framework.PolyGenSpec as PolyGenSpec
import qualified Harmonic.Evaluation.Database.QuerySpec as QuerySpec
import qualified Harmonic.Interface.Tidal.BridgeSpec as InterfaceSpec
import qualified Harmonic.Interface.Tidal.LineHarmonySpec as LineHarmonySpec
import qualified Harmonic.Interface.Tidal.GrooveSpec as GrooveSpec
import qualified Harmonic.Interface.Tidal.FormSpec as FormSpec
import qualified Harmonic.Interface.Tidal.OrchestraSpec as OrchestraSpec
import qualified Harmonic.Interface.Tidal.ArrangerSpec as ArrangerSpec

main :: IO ()
main = hspec $ do
  -- Phase B
  describe "Harmonic.Rules.Types.Pitch" PitchSpec.spec
  describe "Harmonic.Rules.Types.Harmony" HarmonySpec.spec
  describe "Harmonic.Rules.Constraints.Overtone" OvertoneSpec.spec
  describe "Harmonic.Evaluation.Scoring.VoiceLeading" VoiceLeadingSpec.spec
  describe "Harmonic.Evaluation.Scoring.Progression" ScoringProgressionSpec.spec
  describe "Harmonic.Rules.Types.Progression" ProgressionSpec.spec
  describe "Harmonic.Rules.Types.ProgressionContext" ProgressionContextSpec.spec
  describe "Harmonic.Rules.Types.Scale" ScaleSpec.spec
  describe "Harmonic.Rules.Constraints.Filter" FilterSpec.spec
  describe "Harmonic.Rules.Import.Transform" TransformSpec.spec
  describe "Harmonic.Rules.Import.Merge" MergeSpec.spec
  describe "Harmonic.Rules.Import.Graph" GraphSpec.spec
  describe "Harmonic.Rules.Import.Jazz" JazzSpec.spec
  describe "Harmonic.Evaluation.Scoring.Dissonance" DissonanceSpec.spec
  -- Phase C
  describe "Harmonic.Evaluation.Database.Query" QuerySpec.spec
  describe "Harmonic.Traversal.Probabilistic" ProbabilisticSpec.spec
  describe "Harmonic.Traversal.WalkingBass" WalkingBassSpec.spec
  describe "Harmonic.Framework.Builder" BuilderSpec.spec
  describe "Harmonic.Framework.Builder (genP)" BuilderPSpec.spec
  describe "Harmonic.Framework.Builder (genE polytonal)" PolyGenSpec.spec
  describe "Harmonic.Interface.Tidal.Bridge" InterfaceSpec.spec
  describe "Harmonic.Interface.Tidal.Groove" GrooveSpec.spec
  describe "Harmonic.Interface.Tidal.Form" FormSpec.spec
  describe "Harmonic.Interface.Tidal.Orchestra" OrchestraSpec.spec
  describe "Harmonic.Interface.Tidal.Arranger" ArrangerSpec.spec
  describe "Harmonic.Interface.Tidal.LineHarmony" LineHarmonySpec.spec
