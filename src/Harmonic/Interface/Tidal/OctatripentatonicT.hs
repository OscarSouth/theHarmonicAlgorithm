-- |
-- Module      : Harmonic.Interface.Tidal.OctatripentatonicT
-- Description : Live-coding helpers for the octatripentatonic framework
--
-- Pretty-printers for inspecting the strata / tristrata state of a 'genP'-
-- generated 'ProgressionContext' at the REPL during a live session. No
-- semantic behaviour — purely diagnostic output for humans.

module Harmonic.Interface.Tidal.OctatripentatonicT
  ( renderTristrataReport
  , genPReport
  ) where

import Data.Foldable (toList)
import Data.List (intercalate)

import qualified Harmonic.Rules.Types.Scale as Sc
import qualified Harmonic.Rules.Types.ProgressionContext as PC

-- |Render a multi-line report of the per-bar provenance of a
-- 'ProgressionContext': for each bar, the tristrata index, its three strata,
-- and the selected strata for that bar. Returns 'Nothing' when the context
-- has no provenance (e.g., a legacy 'gen' result).
renderTristrataReport :: PC.ProgressionContext -> Maybe String
renderTristrataReport pc = do
  prov <- PC.pcProvenance pc
  let rows = zipWith renderBar [1 :: Int ..] (toList prov)
      hdr  = "bar  tristrata              strata"
      sep  = replicate (length hdr) '-'
  pure (unlines (hdr : sep : rows))
  where
    renderBar i (t, s) =
      let idx = case lookup t indexedTristratas of
                  Just n  -> show n
                  Nothing -> "?"
          shown = show (Sc.ts1 t) ++ "-" ++ show (Sc.ts2 t) ++ "-" ++ show (Sc.ts3 t)
      in pad 4 (show i) ++ " #" ++ pad 3 idx ++ pad 20 shown ++ show s

    indexedTristratas = zip Sc.validTristrata [1 :: Int ..]
    pad n s = s ++ replicate (max 0 (n - length s)) ' '

-- |Live-coding helper: execute a 'genP'-style 'IO ProgressionContext' and
-- print a pretty tristrata report alongside the standard 'Show' output.
-- Useful at the REPL for sanity-checking a strata walk.
genPReport :: IO PC.ProgressionContext -> IO ()
genPReport action = do
  pc <- action
  putStrLn ""
  case renderTristrataReport pc of
    Just report -> putStr report
    Nothing     -> putStrLn "[no provenance — legacy gen result]"
  putStrLn ""
  print pc
