-- |
-- Module      : Harmonic.Interface.Tidal.ChordscaleT
-- Description : REPL report for the chordscale key-area analysis
--
-- Pretty-printer over "Harmonic.Evaluation.Analysis.KeyArea": one row per
-- bar with the chord, the detected key area, the realised scale form, the
-- mode on the bar's root, the assigned pentatonic, and the flags
-- (@\<@ key boundary, @!@ override mode, @?@ gap bar, @*@ out-of-key
-- pentatonic). Mirrors 'Harmonic.Interface.Tidal.OctatripentatonicT.genPReport'
-- \/ 'Harmonic.Interface.Tidal.PolytonalT.genEReport' for the gen \/ genJ
-- families.
module Harmonic.Interface.Tidal.ChordscaleT
  ( renderChordscaleReport
  , chordscaleReport
  ) where

import           Data.Foldable (toList)
import           Data.Maybe (fromMaybe)

import           Harmonic.Evaluation.Analysis.KeyArea
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pt
import qualified Harmonic.Rules.Types.Progression as Pr
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc

-- |Render the per-bar key-area analysis of a gen \/ genJ (or hand-built)
-- context. Returns 'Nothing' for 'PC.FStrata' \/ 'PC.FPoly' contexts,
-- whose layers carry their own semantics and their own reports.
renderChordscaleReport :: PC.ProgressionContext -> Maybe String
renderChordscaleReport pc
  | PC.pcFamily pc `elem` [PC.FStrata, PC.FPoly] = Nothing
  | otherwise =
      let bars = toList (Pr.unProgression (PC.triadLayer pc))
          anns = analyzeProgression (PC.triadLayer pc)
          hdr  = "bar  chord           key    form  mode           penta   flags"
          sep  = replicate (length hdr) '-'
          rows = zipWith3 renderBar [1 :: Int ..] bars anns
      in if null bars then Nothing else Just (unlines (hdr : sep : rows))
  where
    renderBar i cs a =
      let chord = show (H.stateCadenceRoot cs) ++ " "
                    ++ H.cadenceFunctionality (H.stateCadence cs)
          form  = case baForm a of
                    MajForm  -> "maj"
                    NatForm  -> "nat"
                    HarmForm -> "harm"
                    MelForm  -> "mel"
          mode  = fromMaybe "(unclassified)"
                    (fmap (Sc.showModeQuality . Sc.modeQuality) (baMode a))
          penta = show (Pt.flat (Pt.mkPitchClass (baPentaRoot a))) ++ " pent"
          flags = concat
            [ if baBoundary a then "<" else ""
            , case baTier a of
                TierForm   -> ""
                TierSeed   -> "!"
                TierSearch -> "!"
                TierGap    -> "?"
            , if baPentaInKey a then "" else "*"
            ]
      in pad 5 (show i) ++ pad 16 chord ++ pad 7 (showKeyArea (baKey a))
           ++ pad 6 form ++ pad 15 mode ++ pad 8 penta ++ flags
    pad n s = s ++ replicate (max 0 (n - length s)) ' '

-- |Live-coding helper: print the chordscale report for a gen \/ genJ
-- result alongside the standard 'Show' output.
chordscaleReport :: PC.ProgressionContext -> IO ()
chordscaleReport pc = do
  putStrLn ""
  case renderChordscaleReport pc of
    Just report -> putStr report
    Nothing     -> putStrLn "[chordscaleReport applies to gen / genJ / hand-built contexts — genP has genPReport, genE has genEReport]"
  putStrLn ""
  print pc
