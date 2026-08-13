-- |
-- Module      : Harmonic.Interface.Tidal.Form
-- Description : Kinetics framework for form-driven range gating
--
-- Encodes macro-level compositional arc as programmable structure.
-- Form is defined in wall-clock seconds at a single global tempo,
-- realized as TidalCycles patterns, and loops endlessly.

module Harmonic.Interface.Tidal.Form
  ( -- * Types
    FormNode(..)
  , FormTime(..)
  , Transition(..)
  , Kinetics(..)
  , IK

    -- * Construction
  , at
  , at'
  , rh
  , rh'
  , iK
  , lK

    -- * Realization
  , formK

    -- * Primitives
  , ki
  , slate
  , kinPick
  , withForm

  ) where

import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A node's position in time — wall-clock 'Secs' or musical 'Bars' (4/4).
-- Resolved to Tidal cycles at realization (see 'formK'). Mix freely in one form.
data FormTime = Secs Double | Bars Double
  deriving (Show, Eq)

-- |How a section moves to the next node: 'Smooth' (ramped) or 'Snap'
-- (hold this node's value, then jump on the next node's exact time).
data Transition = Smooth | Snap
  deriving (Show, Eq)

-- |A node in a form definition: a point in time with kinetics level,
-- dynamic level, active progression, and the transition style of the
-- section starting here.
data FormNode = FormNode
  { fnTime     :: FormTime                -- ^ Position (seconds or bars)
  , fnKinetics :: Double                  -- ^ 0.0-1.0 kinetics level
  , fnDynamic  :: Double                  -- ^ 0.0-1.0 dynamic level
  , fnProg     :: PC.ProgressionContext   -- ^ Active 3-layer progression at this node
  , fnTrans    :: Transition              -- ^ Transition of the segment starting at this node
  } deriving (Show, Eq)

-- |Realized form: continuous and discrete signals for live performance.
data Kinetics = Kinetics
  { kSignal   :: Pattern Double                -- ^ Kinetics level 0-1 (continuous interpolated)
  , kDynamic  :: Pattern Double                -- ^ Dynamic envelope 0-1 (continuous interpolated)
  , kProg     :: Pattern PC.ProgressionContext -- ^ Active 3-layer progression (step function)
  , kLoopSecs :: Double                        -- ^ Form total duration in seconds; 0 = atemporal
                                               --   (single-node iK or lK). Consumers like the
                                               --   4-char-display helper read this to drive a
                                               --   wall-clock counter that wraps every kLoopSecs.
  , kCps      :: Double                        -- ^ Cycles per second at form construction
                                               --   (= bpm/60). Used by the display broadcaster to
                                               --   convert cycle time → seconds. Stays coherent
                                               --   with Tidal's actual cps because both are
                                               --   derived from the same @bpm@ on every launcher
                                               --   re-evaluation.
  }

-- |Performance context: Kinetics bundled with chord selection pattern.
-- Reduces parameter threading — @r@ and @k@ are always passed together.
type IK = (Kinetics, Pattern Int)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- |Form node builders. Time unit and transition are orthogonal:
-- @at@\/@at'@ take wall-clock seconds, @rh@\/@rh'@ take bars (rehearsal marks, 4/4);
-- unprimed = smooth transition, primed = snap. @at@ is unchanged from before.
--
-- @at  0 0 0 s@   seconds, smooth   @rh  8 0.5 0.5 s@   bars, smooth
-- @at' 60 1 1 s@  seconds, snap     @rh' 16 0.9 0.9 s@  bars, snap
at, at', rh, rh' :: Double -> Double -> Double -> PC.ProgressionContext -> FormNode
at  t k d pc = FormNode (Secs t) k d pc Smooth
at' t k d pc = FormNode (Secs t) k d pc Snap
rh  b k d pc = FormNode (Bars b) k d pc Smooth
rh' b k d pc = FormNode (Bars b) k d pc Snap

-- |Construct performance context from BPM, form nodes, and chord selection.
--
-- @k = iK tempo [at 0 0 0 s, at 30 1 1 s] (warp \"[1 2 3 4]\/8\")@
iK :: Double -> [FormNode] -> Pattern Int -> IK
iK bpm nodes chordPat = (formK bpm nodes, chordPat)

-- |Live kinetics: build IK from reactive kinetics/dynamics signals.
-- Bypasses form interpolation — use when the envelope is driven by live
-- input (e.g. MIDI CC) rather than a static keyframed form.
--
-- @k = lK exP exP s r@  -- pedal drives both kinetics and dynamics
lK :: Pattern Double          -- ^ Kinetics signal (0-1, live)
   -> Pattern Double          -- ^ Dynamics signal (0-1, live)
   -> PC.ProgressionContext   -- ^ Active 3-layer progression
   -> Pattern Int             -- ^ Chord-selection pattern
   -> IK
lK sig dyn pc chordPat = (Kinetics sig dyn (pure pc) 0 0, chordPat)

-------------------------------------------------------------------------------
-- Realization
-------------------------------------------------------------------------------

-- |Beats per bar for 'Bars' resolution. 4/4, matching BootTidal's @bar@ helper
-- (1 bar = 4 cycles, since @cps = bpm/60@ makes a cycle one beat).
beatsPerBar :: Double
beatsPerBar = 4

-- |Resolve a node's position to Tidal cycles from form start.
nodeCycles :: Double -> FormNode -> Double
nodeCycles cps n = case fnTime n of
  Secs s -> s * cps
  Bars b -> b * beatsPerBar

-- |Node position in wall-clock seconds (for 'kLoopSecs' / the display).
nodeSecs :: Double -> FormNode -> Double
nodeSecs cps n = case fnTime n of
  Secs s -> s
  Bars b -> b * beatsPerBar / cps

-- |Realize a form definition into Kinetics signals at a given BPM.
-- Single-node forms produce constant signals (global state).
-- Multi-node forms produce per-segment signals — smooth (ramp) or snap (step)
-- per each node's 'fnTrans' — and a step-function progression, looping at the
-- form's total duration. Time is resolved from each node's 'FormTime'.
formK :: Double -> [FormNode] -> Kinetics
formK bpm nodes = Kinetics
  { kSignal   = formSignal cps nodes fnKinetics
  , kDynamic  = formSignal cps nodes fnDynamic
  , kProg     = formStep   cps nodes fnProg
  , kLoopSecs = case nodes of
                  (_:_:_) -> nodeSecs cps (last nodes)   -- multi-node: form duration (seconds)
                  _       -> 0                           -- single-node or empty: atemporal
  , kCps      = cps
  }
  where cps = bpm / 60

-- |Kinetics/dynamic signal: piecewise per segment, ramped when the segment's
-- start node is 'Smooth', held (stepped) when 'Snap'. Single node: constant.
formSignal :: Double -> [FormNode] -> (FormNode -> Double) -> Pattern Double
formSignal _   [node] accessor = pure (realToFrac $ accessor node)
formSignal cps nodes  accessor =
  let totalCycles = realToFrac (nodeCycles cps (last nodes)) :: Time
      pairs       = zip nodes (tail nodes)
      segments    = [ ( realToFrac (nodeCycles cps n2 - nodeCycles cps n1)
                      , case fnTrans n1 of
                          Snap   -> pure (realToFrac $ accessor n1)
                          Smooth -> segment 16 $ range (realToFrac $ accessor n1)
                                                       (realToFrac $ accessor n2) saw
                      )
                    | (n1, n2) <- pairs
                    ]
  in slow (pure totalCycles) $ timecat segments

-- |Step signal: hold each node's value until the next (progression can't ramp).
-- Independent of 'fnTrans'. Single node: constant value.
formStep :: Double -> [FormNode] -> (FormNode -> a) -> Pattern a
formStep _   [node] accessor = pure (accessor node)
formStep cps nodes  accessor =
  let totalCycles = realToFrac (nodeCycles cps (last nodes)) :: Time
      pairs       = zip nodes (tail nodes)
      segments    = [ ( realToFrac (nodeCycles cps n2 - nodeCycles cps n1)
                      , pure (accessor n1)
                      )
                    | (n1, n2) <- pairs
                    ]
  in slow (pure totalCycles) $ timecat segments

-------------------------------------------------------------------------------
-- Primitives
-------------------------------------------------------------------------------

-- |Range gate: mask a pattern by kinetics signal level.
-- Events pass only when kSignal is within the (lo, hi) range.
ki :: (Double, Double) -> IK -> Pattern a -> Pattern a
ki (lo, hi) (kin, _) = mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin))

-- |Gated stack: stack patterns and gate by kinetics range.
slate :: (Double, Double) -> IK -> [Pattern a] -> Pattern a
slate range k pats = ki range k $ stack pats

-- |Kinetics-windowed dispatch: partition [0,1] into N equal windows of
-- width 1/N, where N = length pats, and play only the pattern whose
-- window contains the current kSignal. Windows are derived at call time;
-- any N >= 1 works (N=1 collapses to "always play this pattern").
--
-- Boundaries belong to the lower window: window i covers
-- @(i/N, (i+1)/N]@, with window 0 also including 0. So at the boundary
-- between window i and window i+1, the lower window plays.
--
--   N=2 → [0, 1/2], (1/2, 1]
--   N=3 → [0, 1/3], (1/3, 2/3], (2/3, 1]
--
-- Empty list → silence. Outside [0,1] → silence (no window matches).
kinPick :: IK -> [Pattern a] -> Pattern a
kinPick _ [] = silence
kinPick (kin, _) pats =
  let n      = length pats
      step   = 1 / fromIntegral n
      window i p =
        let lo = fromIntegral i       * step
            hi = fromIntegral (i + 1) * step
            inWin x | i == 0    = x >= lo && x <= hi
                    | otherwise = x >  lo && x <= hi
        in mask (fmap inWin (kSignal kin)) p
  in stack (zipWith window [0..] pats)

-- |Bridge helper: apply a function taking 'ProgressionContext' to a Kinetics context.
-- Uses innerJoin to reactively switch when the form changes progressions.
withForm :: IK -> (PC.ProgressionContext -> Pattern ValueMap) -> Pattern ValueMap
withForm (kin, _) f = innerJoin $ fmap f (kProg kin)

