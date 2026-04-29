-- |
-- Module      : Harmonic.Framework.Builder.Strata
-- Description : Strata-first traversal helpers for the 'genP' paradigm
--
-- Helpers that decide which (strata, tristrata) pair bar @i@ is drawn from,
-- given the allowed tristrata list, the previous bar's assignment, and
-- optional per-bar constraints ('relStrata' / 'absStrata'). Pure functions;
-- no IO, no randomness.

module Harmonic.Framework.Builder.Strata
  ( adjacentInTristrata
  , allowedNext
  , initialPlacement
  , modeForTriad
  , pickPartner
  , positionalPartner
  , selectNext
  , selectNextSeeded
  ) where

import Data.List (find, nub, sortBy)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe)

import qualified Harmonic.Rules.Types.Scale as Sc
import Harmonic.Rules.Types.Scale
  ( StrataLabel, Tristrata(..), Mode(..), ModeQuality(..), ModeResult(..)
  , tristrataStrataAt, tristrataOf, tristrataDissonance
  , strataDissonance, strataChroma, classifyModeAt
  )
import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass)

-- |Position of a strata inside a tristrata (1, 2, or 3); 'Nothing' if the
-- strata is not a member of that tristrata.
positionOf :: Tristrata -> StrataLabel -> Maybe Int
positionOf t s
  | ts1 t == s = Just 1
  | ts2 t == s = Just 2
  | ts3 t == s = Just 3
  | otherwise  = Nothing

-- |In-tristrata circular neighbourhood of a strata: positions @{p-1, p, p+1}@
-- mod 3 in the tristrata @t@. The current strata is included so a bar may
-- repeat the previous strata within the same tristrata.
adjacentInTristrata :: Tristrata -> StrataLabel -> [StrataLabel]
adjacentInTristrata t s = case positionOf t s of
  Nothing -> []
  Just p  ->
    let positions = [ ((p - 2) `mod` 3) + 1
                    , p
                    , (p `mod` 3) + 1
                    ]
    in map (tristrataStrataAt t) positions

-- |Complete viable @(strata', tristrata')@ candidate set for a given
-- @(strata, tristrata)@ step, respecting the allowed-tristrata allow-list.
--
-- For every @(t', p')@ in @'tristrataOf' s@ where @t'@ is allowed, emit every
-- @(s', t')@ with @s'@ drawn from @'adjacentInTristrata' t' s@.
allowedNext :: [Tristrata] -> (StrataLabel, Tristrata) -> [(StrataLabel, Tristrata)]
allowedNext allowed (s, _t) =
  [ (s', t')
  | (t', _p') <- tristrataOf s
  , t' `elem` allowed
  , s' <- adjacentInTristrata t' s
  ]

-- |Initial @(strata, tristrata)@ placement: use the supplied starting strata,
-- and pick the lowest-dissonance allowed tristrata that contains it. If no
-- allowed tristrata contains the strata, fall back to the first allowed one
-- (edge case — caller should usually avoid this via a sensible default).
initialPlacement :: [Tristrata] -> StrataLabel -> (StrataLabel, Tristrata)
initialPlacement allowed s =
  case sortBy (comparing tristrataDissonance)
       [ t | (t, _) <- tristrataOf s, t `elem` allowed ] of
    (t:_) -> (s, t)
    []    -> case allowed of
               (t:_) -> (s, t)
               []    -> (s, Sc.tristrataIndex 1)  -- extreme edge case

-- |Bar-0 / no-prior starter rule: pick a fixed partner strata inside the
-- given tristrata based on the current strata's position.
--
--   * pos 1 → ts2
--   * pos 2 → ts1
--   * pos 3 → ts1
--
-- Yields a 7-PC pair-union (which is what the framework guarantees inside a
-- single tristrata).
positionalPartner :: Tristrata -> StrataLabel -> StrataLabel
positionalPartner t s = case positionOf t s of
  Just 1 -> ts2 t
  Just 2 -> ts1 t
  Just 3 -> ts1 t
  _      -> ts2 t

-- |Choose a partner strata for the union construction at bar @i@.
--
-- Preference order:
--
--   1. The /most recent/ prior bar whose strata @≠ s_curr@ — "last
--      different" (preserves the user's intended behaviour for
--      mid-run bars where the mode reflects the incoming transition).
--   2. If no different prior bar exists (bar 0, or the entire history
--      so far shares @s_curr@), look /forward/ for the first future
--      bar whose strata @≠ s_curr@ — the "first 'next' different" rule.
--      This lets bar 0's mode reflect the /outgoing/ transition to the
--      next distinct strata, which is musically more informative than
--      the positional fallback.
--   3. Otherwise (single-strata run), fall back to 'positionalPartner'
--      inside the current bar's tristrata.
pickPartner :: [(StrataLabel, Tristrata)]   -- ^ full bar sequence (0-indexed)
            -> Int                          -- ^ current bar index
            -> StrataLabel                  -- ^ current strata
            -> Tristrata                    -- ^ current tristrata (used for fallback)
            -> StrataLabel
pickPartner barSeq i sCurr tCurr =
  case lastDifferent of
    Just sLast -> sLast
    Nothing    -> case firstForwardDifferent of
      Just sNext -> sNext
      Nothing    -> positionalPartner tCurr sCurr
  where
    lastDifferent =
      listToMaybe
        [ s
        | j <- [i - 1, i - 2 .. 0]
        , j >= 0
        , let (s, _) = barSeq !! j
        , s /= sCurr
        ]
    firstForwardDifferent =
      listToMaybe
        [ s
        | j <- [i + 1 .. length barSeq - 1]
        , let (s, _) = barSeq !! j
        , s /= sCurr
        ]

-- |Triad-anchored, history-aware mode selector for bar @i@.
--
-- Walks @barSeq@ backward from @i-1@ to find the most recent strata
-- @≠ barSeq[i].strata@; if none, uses 'positionalPartner' on
-- @barSeq[i].tristrata@. Unions @strataChroma sCurr@ with the partner's
-- chroma, then classifies via 'classifyModeAt' pinned to @triadRootPC@.
--
-- Returns 'ModeOk' for a normal classification; 'ModeInvalid' when the
-- union doesn't have exactly 7 unique pitch classes (only reachable via
-- 'absStrata' overrides that violate tristrata adjacency). When the
-- 7-PC union doesn't classify under any of the 28 mode patterns pinned
-- to @triadRootPC@, returns @ModeOk (Mode Aeolian (P triadRootPC))@ as
-- a benign fallback.
modeForTriad :: [(StrataLabel, Tristrata)]
             -> Int
             -> Int                          -- ^ triad's harmonic root PC (0..11)
             -> ModeResult
modeForTriad barSeq i triadRootPC
  | i < 0 || i >= length barSeq = ModeInvalid []
  | otherwise =
      let (sCurr, tCurr) = barSeq !! i
          partnerStrata  = pickPartner barSeq i sCurr tCurr
          unionPCs       = nub (strataChroma sCurr ++ strataChroma partnerStrata)
      in if length unionPCs /= 7
           then ModeInvalid unionPCs
           else case classifyModeAt triadRootPC unionPCs of
                  Just m  -> ModeOk m
                  Nothing -> ModeOk (Mode Aeolian (mkPitchClass triadRootPC))

-- |Select the next @(s', t')@ from a candidate pool using the tie-break rule:
--
--   1. Prefer @s' == s_{prev}@ (strata continuity); within that, prefer
--      @t' == t_{prev}@ if still valid, otherwise lowest-dissonance tristrata.
--   2. Prefer @t' == t_{prev}@ (tristrata continuity) — lowest-dissonance
--      option inside the previous tristrata wins over a more-consonant strata
--      in another tristrata.
--   3. Otherwise pick by @('strataDissonance' s', 'tristrataDissonance' t')@.
--
-- Returns 'Nothing' on empty pool.
selectNext :: (StrataLabel, Tristrata)
           -> [(StrataLabel, Tristrata)]
           -> Maybe (StrataLabel, Tristrata)
selectNext _ []   = Nothing
selectNext (sPrev, tPrev) pool =
  let sameStrata     = filter (\(s, _) -> s == sPrev) pool
      sameTristrata  = filter (\(_, t) -> t == tPrev) pool
      byDissonance   = sortBy (comparing keyDiss) pool
      keyDiss (s, t) = (strataDissonance s, tristrataDissonance t)
  in case sameStrata of
       (_:_) ->
         -- Within same-strata, prefer t' == tPrev, else lowest tristrata dissonance.
         case find (\(_, t) -> t == tPrev) sameStrata of
           Just hit -> Just hit
           Nothing  -> listToMaybe (sortBy (comparing (tristrataDissonance . snd)) sameStrata)
       [] -> case sameTristrata of
               (_:_) -> listToMaybe (sortBy (comparing keyDiss) sameTristrata)
               []    -> listToMaybe byDissonance

-- |Seeded stochastic variant of 'selectNext'. Permits the walk to
-- actually traverse — the categorical same-strata-first rule of
-- 'selectNext' locks the walk to its starting strata indefinitely,
-- which is musically sterile.
--
-- Given an @Int@ sample drawn from an RNG by the caller, this picks a
-- candidate from @pool@ with the following preference order:
--
--   1. Prefer candidates whose strata @≠ sPrev@ (forces exploration).
--      Among those, prefer ones sharing @tPrev@ (tristrata continuity)
--      over those in other tristrata.
--   2. Fall back to the self-loop only when the pool contains nothing
--      else (rare — happens under extreme @hcTristrata@ / @relStrata@
--      narrowing).
--
-- Within the preferred group, the sample index (mod group size) picks
-- the concrete candidate. This gives run-to-run variety while
-- respecting 'genP's continuity intent (staying inside the current
-- tristrata's harmonic region when possible).
selectNextSeeded :: Int                               -- ^ rng sample (any Int)
                 -> (StrataLabel, Tristrata)          -- ^ prior bar
                 -> [(StrataLabel, Tristrata)]        -- ^ candidate pool
                 -> Maybe (StrataLabel, Tristrata)
selectNextSeeded _    _             []   = Nothing
selectNextSeeded seed (sPrev, tPrev) pool =
  let nonSelf     = filter (\(s, _) -> s /= sPrev) pool
      sameTri     = filter (\(_, t) -> t == tPrev) nonSelf
      group       = if not (null sameTri)
                      then sameTri
                      else if not (null nonSelf)
                             then nonSelf
                             else pool        -- self-loop fallback
      idx         = (abs seed) `mod` length group
  in Just (group !! idx)
