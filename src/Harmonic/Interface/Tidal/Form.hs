-- |
-- Module      : Harmonic.Interface.Tidal.Form
-- Description : Kinetics framework for form-driven range gating
--
-- Encodes macro-level compositional arc as programmable structure, realised
-- as TidalCycles patterns, looping endlessly.
--
-- A form is a list of nodes. Each node fixes a point in time and the two
-- signals that drive everything downstream: /kinetics/ (how active the music
-- is) and /dynamics/ (how loud). Instruments gate themselves on kinetics, so
-- the arc is written once and every line follows it.
--
-- Nodes come in two time bases — 'at' takes wall-clock seconds, 'rh' takes
-- bars (rehearsal marks). Primed variants ('at'', 'rh'') snap rather than
-- interpolate, for a hard cut into a new section:
--
-- @
-- form =           -- time    k     d
--   [ rh    0      0.0   0.0   s
--   , rh   16      0.2   0.3   s    -- elements layer in
--   , rh'  28      0.35  0.55  s    -- riser: sparse but loud, then snap
--   , rh   32      1.0   1.0   s    -- drop
--   ]
-- @
--
-- The form is compiled into an 'IK' — the performance context every
-- instrument reads — by pairing it with a tempo and a chord-selection
-- pattern:
--
-- @k = iK tempo form (warp \"[1 2 3 4]\/4\")@
--
-- The simplest useful form is a single constant node, which holds one
-- progression at full kinetics forever:
--
-- @form = [ at 0 1.0 1.0 s ]@

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
  , beatsPerBar
  , ki
  , slate
  , kinPick
  , withForm

  ) where

import Data.List (nub, sortOn)
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A node's position in time — wall-clock 'Secs' or musical 'Bars' (4\/4).
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
  , kProgs    :: [PC.ProgressionContext]       -- ^ The distinct progressions 'kProg' can emit —
                                               --   finite and known at construction (form nodes,
                                               --   or the single 'lK' progression). Voicing
                                               --   caches derive from THIS list, exactly and
                                               --   horizon-free, instead of sampling 'kProg'
                                               --   over an arbitrary time window.
  , kLoopSecs :: Double                        -- ^ Form total duration in seconds; 0 = atemporal
                                               --   (single-node iK or lK). Consumers like the
                                               --   4-char-display helper read this to drive a
                                               --   wall-clock counter that wraps every kLoopSecs.
  , kCps      :: Double                        -- ^ Cycles per second at form construction
                                               --   (= bpm\/60). Used by the display broadcaster to
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
-- @at@\/@at'@ take wall-clock seconds, @rh@\/@rh'@ take bars (rehearsal marks, 4\/4);
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

-- |Live kinetics: build IK from reactive kinetics\/dynamics signals.
-- Bypasses form interpolation — use when the envelope is driven by live
-- input (e.g. MIDI CC) rather than a static keyframed form.
--
-- @k = lK exP exP s r@  -- pedal drives both kinetics and dynamics
lK :: Pattern Double          -- ^ Kinetics signal (0-1, live)
   -> Pattern Double          -- ^ Dynamics signal (0-1, live)
   -> PC.ProgressionContext   -- ^ Active 3-layer progression
   -> Pattern Int             -- ^ Chord-selection pattern
   -> IK
lK sigP dyn pc chordPat = (Kinetics sigP dyn (pure pc) [pc] 0 0, chordPat)

-------------------------------------------------------------------------------
-- Realization
-------------------------------------------------------------------------------

-- |Beats per bar for 'Bars' resolution. 4\/4, matching BootTidal's @bar@ helper
-- (1 bar = 4 cycles, since @cps = bpm\/60@ makes a cycle one beat).
beatsPerBar :: Double
beatsPerBar = 4

-- |Resolve a node's position to Tidal cycles from form start.
nodeCycles :: Double -> FormNode -> Double
nodeCycles cpsV nd = case fnTime nd of
  Secs secs -> secs * cpsV
  Bars b    -> b * beatsPerBar

-- |Node position in wall-clock seconds (for 'kLoopSecs' \/ the display).
nodeSecs :: Double -> FormNode -> Double
nodeSecs cpsV nd = case fnTime nd of
  Secs secs -> secs
  Bars b    -> b * beatsPerBar / cpsV

-- |Realize a form definition into Kinetics signals at a given BPM.
-- Single-node forms produce constant signals (global state).
-- Multi-node forms produce per-segment signals — smooth (ramp) or snap (step)
-- per each node's 'fnTrans' — and a step-function progression, looping at the
-- form's total duration. Time is resolved from each node's 'FormTime'.
--
-- Contract details:
--
-- * The LAST node is the loop terminator, not a playable section: segments
--   run between consecutive node pairs, and the final node's time sets
--   'kLoopSecs'. Its kinetics\/dynamic values only shape the ramp INTO it.
-- * Nodes are sorted by resolved time before use, so a moved rehearsal
--   mark cannot produce negative-width segments. Nodes sharing a time
--   yield a zero-width segment (the earlier one is inaudible).
-- * @formK bpm []@ yields silence on every signal — safe to evaluate
--   mid-edit; nothing sounds until a node exists.
-- * Smooth ramps render at 16 steps per SEGMENT regardless of length
--   (see @formSignal@) — a long segment crosses kinetics thresholds on a
--   correspondingly coarse grid.
formK :: Double -> [FormNode] -> Kinetics
formK bpm nodes0 = Kinetics
  { kSignal   = formSignal cpsV nodes fnKinetics
  , kDynamic  = formSignal cpsV nodes fnDynamic
  , kProg     = formStep   cpsV nodes fnProg
  , kProgs    = nub (map fnProg nodes)
  , kLoopSecs = case nodes of
                  (_:_:_) -> nodeSecs cpsV (last nodes)   -- multi-node: form duration (seconds)
                  _       -> 0                           -- single-node or empty: atemporal
  , kCps      = cpsV
  }
  where
    cpsV = bpm / 60
    -- Nodes sorted by resolved time: an out-of-order form (a rehearsal
    -- mark moved without reordering the list) previously produced
    -- negative-width timecat segments with no diagnostic.
    nodes = sortOn (nodeCycles cpsV) nodes0

-- |Kinetics\/dynamic signal: piecewise per segment, ramped when the segment's
-- start node is 'Smooth', held (stepped) when 'Snap'. Single node: constant.
-- Ramp resolution is a fixed 16 steps per segment (a 148-second segment
-- moves in ~9-second stairs) — deliberate coarseness so instrument
-- activation bands switch on a musically legible grid rather than
-- per-event.
formSignal :: Double -> [FormNode] -> (FormNode -> Double) -> Pattern Double
formSignal _   []     _        = silence
formSignal _   [node] accessor = pure (realToFrac $ accessor node)
formSignal cpsV nodes  accessor =
  let totalCycles = realToFrac (nodeCycles cpsV (last nodes)) :: Time
      pairs       = zip nodes (drop 1 nodes)
      segments    = [ ( realToFrac (nodeCycles cpsV n2 - nodeCycles cpsV n1)
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
formStep _   []     _        = silence
formStep _   [node] accessor = pure (accessor node)
formStep cpsV nodes  accessor =
  let totalCycles = realToFrac (nodeCycles cpsV (last nodes)) :: Time
      pairs       = zip nodes (drop 1 nodes)
      segments    = [ ( realToFrac (nodeCycles cpsV n2 - nodeCycles cpsV n1)
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
slate band k pats = ki band k $ stack pats

-- |Kinetics-windowed dispatch: partition [0,1] into N equal windows of
-- width 1\/N, where N = length pats, and play only the pattern whose
-- window contains the current kSignal. Windows are derived at call time;
-- any N >= 1 works (N=1 collapses to "always play this pattern").
--
-- Boundaries belong to the lower window: window i covers
-- @(i\/N, (i+1)\/N]@, with window 0 also including 0. So at the boundary
-- between window i and window i+1, the lower window plays.
--
--   N=2 → [0, 1\/2], (1\/2, 1]
--   N=3 → [0, 1\/3], (1\/3, 2\/3], (2\/3, 1]
--
-- Empty list → silence. Outside [0,1] → silence (no window matches).
kinPick :: IK -> [Pattern a] -> Pattern a
kinPick _ [] = silence
kinPick (kin, _) pats =
  let nPats  = length pats
      step   = 1 / fromIntegral nPats
      window i p =
        let lo = fromIntegral i       * step
            hi = fromIntegral (i + 1) * step
            inWin x | i == 0    = x >= lo && x <= hi
                    | otherwise = x >  lo && x <= hi
        in mask (fmap inWin (kSignal kin)) p
  in stack (zipWith window [0 :: Int ..] pats)

-- |Bridge helper: apply a function taking 'Harmonic.Rules.Types.ProgressionContext.ProgressionContext' to a Kinetics context.
-- Uses innerJoin to reactively switch when the form changes progressions.
withForm :: IK -> (PC.ProgressionContext -> Pattern ValueMap) -> Pattern ValueMap
withForm (kin, _) f = innerJoin $ fmap f (kProg kin)

