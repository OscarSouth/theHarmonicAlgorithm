-- |
-- Module      : Harmonic.Interface.Tidal.PolytonalT
-- Description : Live-coding helpers for the polytonal genE family
--
-- Pretty-printers for inspecting a polytonal
-- 'Harmonic.Rules.Types.ProgressionContext.ProgressionContext' at the
-- REPL: every layer view a pattern can select — the three triad layers,
-- each pair, the full pentad, and the pivot tones — printed as the
-- standard progression grids. No semantic behaviour — purely diagnostic
-- output for humans.
--
-- @
-- s \<- seek \"*\" $ len 8 $ entropy 0.3 $ genE
-- genEReport s
-- @

module Harmonic.Interface.Tidal.PolytonalT
  ( polyLayerViews
  , genEReport
  ) where

import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC

-- |Every layer view of a polytonal context, labelled: single layers,
-- pair unions, the pentad, the pivot tones. Returns 'Nothing' for other
-- families (their layers duplicate one progression, so the views carry
-- no information).
polyLayerViews :: PC.ProgressionContext -> Maybe [(String, Prog.Progression)]
polyLayerViews pc
  | PC.pcFamily pc /= PC.FPoly = Nothing
  | otherwise = Just
      [ (label, PC.layer sel pc)
      | (label, sel) <-
          [ ("T (foundation)",   PC.T)
          , ("S (partner)",      PC.S)
          , ("M (partner)",      PC.M)
          , ("TS",               PC.TS)
          , ("TM",               PC.TM)
          , ("SM",               PC.SM)
          , ("TSM (pentad)",     PC.TSM)
          , ("PT (pivot tones)", PC.PT)
          ] ]

-- |Live-coding helper: print every layer view of a polytonal context as
-- progression grids. Use at the REPL between takes to see what each
-- pattern-level layer selection will sound.
genEReport :: PC.ProgressionContext -> IO ()
genEReport pc = case polyLayerViews pc of
  Nothing -> putStrLn "[not a polytonal context — genEReport applies to genE results]"
  Just views -> mapM_ printView views
  where
    printView (label, prog) = do
      putStrLn ""
      putStrLn ("   " ++ label ++ ":")
      print prog
