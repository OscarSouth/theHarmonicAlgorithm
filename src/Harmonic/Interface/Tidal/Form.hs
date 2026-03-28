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
  , Kinetics(..)

    -- * Construction
  , at

    -- * Realization
  , formK

    -- * Primitives
  , ki
  , slate
  , withForm

  ) where

import qualified Harmonic.Rules.Types.Progression as P
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A node in a form definition: a point in time with kinetics level,
-- dynamic level, and active progression.
data FormNode = FormNode
  { fnTime     :: Double         -- ^ Wall-clock seconds from start
  , fnKinetics :: Double         -- ^ 0.0-1.0 kinetics level
  , fnDynamic  :: Double         -- ^ 0.0-1.0 dynamic level
  , fnProg     :: P.Progression  -- ^ Active progression at this node
  } deriving (Show, Eq)

-- |Realized form: continuous and discrete signals for live performance.
data Kinetics = Kinetics
  { kSignal  :: Pattern Double         -- ^ Kinetics level 0-1 (continuous interpolated)
  , kDynamic :: Pattern Double         -- ^ Dynamic envelope 0-1 (continuous interpolated)
  , kProg    :: Pattern P.Progression  -- ^ Active progression (step function)
  }

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- |Construct a form node at a given time with kinetics, dynamic, and progression.
at :: Double -> Double -> Double -> P.Progression -> FormNode
at t k d prog = FormNode t k d prog

-------------------------------------------------------------------------------
-- Realization
-------------------------------------------------------------------------------

-- |Realize a form definition into Kinetics signals at a given BPM.
-- Single-node forms produce constant signals (global state).
-- Multi-node forms produce interpolated continuous signals and step-function
-- discrete signals that loop at the form's total duration.
formK :: Double -> [FormNode] -> Kinetics
formK bpm nodes = Kinetics
  { kSignal  = formContinuous cps nodes fnKinetics
  , kDynamic = formContinuous cps nodes fnDynamic
  , kProg    = formDiscrete cps nodes fnProg
  }
  where cps = bpm / 60

-- |Continuous signal: piecewise linear interpolation via timecat.
-- Single node: constant signal.
formContinuous :: Double -> [FormNode] -> (FormNode -> Double) -> Pattern Double
formContinuous _   [node] accessor = pure (realToFrac $ accessor node)
formContinuous cps nodes  accessor =
  let totalSecs   = fnTime (last nodes)
      totalCycles = realToFrac (totalSecs * cps) :: Time
      pairs       = zip nodes (tail nodes)
      segments    = [ ( realToFrac ((fnTime n2 - fnTime n1) * cps)
                      , segment 64 $ range (realToFrac $ accessor n1)
                                           (realToFrac $ accessor n2) saw
                      )
                    | (n1, n2) <- pairs
                    ]
  in slow (pure totalCycles) $ timecat segments

-- |Discrete signal: step function via timecat + pure.
-- Single node: constant value.
formDiscrete :: Double -> [FormNode] -> (FormNode -> a) -> Pattern a
formDiscrete _   [node] accessor = pure (accessor node)
formDiscrete cps nodes  accessor =
  let totalSecs   = fnTime (last nodes)
      totalCycles = realToFrac (totalSecs * cps) :: Time
      pairs       = zip nodes (tail nodes)
      segments    = [ ( realToFrac ((fnTime n2 - fnTime n1) * cps)
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
ki :: (Double, Double) -> Kinetics -> Pattern a -> Pattern a
ki (lo, hi) k = mask (fmap (\x -> x >= lo && x <= hi) (kSignal k))

-- |Gated stack: stack patterns and gate by kinetics range.
slate :: (Double, Double) -> Kinetics -> [Pattern a] -> Pattern a
slate range k pats = ki range k $ stack pats

-- |Bridge helper: apply a function taking Progression to a Kinetics context.
-- Uses innerJoin to reactively switch when the form changes progressions.
withForm :: Kinetics -> (P.Progression -> Pattern ValueMap) -> Pattern ValueMap
withForm k f = innerJoin $ fmap f (kProg k)

