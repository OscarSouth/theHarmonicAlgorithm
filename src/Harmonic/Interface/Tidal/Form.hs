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

    -- * Pre-built Forms
  , form444
  , form720
  , form1164
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

-------------------------------------------------------------------------------
-- Pre-built Forms (Fichtean Motive Arc)
-------------------------------------------------------------------------------

-- Total cycles at common BPMs (1 cycle = 1 beat):
--         | 7m24s (444s) | 12m (720s) | 19m24s (1164s)
-- BPM  80 |   592        |   960      |   1552
-- BPM  90 |   666        |  1080      |   1746
-- BPM 100 |   740        |  1200      |   1940
-- BPM 120 |   888        |  1440      |   2328

-- |7 minute 24 second form (444s) with Fichtean motive arc.
-- Takes two progressions: @a@ (home) and @b@ (development).
form444 :: P.Progression -> P.Progression -> [FormNode]
form444 a b =
  --       time    kin   dyn   prog  | narrative
  [ at     0.0     0.0   0.0   a     -- start (silence)
  , at    55.5     0.2   0.35  a     -- 1/8: exposition building
  , at   111.0     0.35  0.5   a     -- 1/4: inciting event
  , at   166.5     0.5   0.6   b     -- 3/8: development tension
  , at   222.0     0.4   0.5   b     -- 1/2: THE SWERVE
  , at   275.0     0.8   0.85  b     -- PRIMARY: cumulation begins
  , at   333.0     1.0   1.0   b     -- 3/4: peak tension
  , at   380.0     0.7   0.8   a     -- secondary: hope/return
  , at   420.0     0.2   0.3   a     -- tertiary: convergence
  , at   444.0     0.0   0.0   a     -- fine (loops to start)
  ]

-- |12 minute form (720s) with Fichtean motive arc.
-- Same proportional arc as form444, scaled to 720s.
form720 :: P.Progression -> P.Progression -> [FormNode]
form720 a b =
  [ at     0.0     0.0   0.0   a
  , at    90.0     0.2   0.35  a
  , at   180.0     0.35  0.5   a
  , at   270.0     0.5   0.6   b
  , at   360.0     0.4   0.5   b
  , at   446.0     0.8   0.85  b
  , at   540.0     1.0   1.0   b
  , at   616.0     0.7   0.8   a
  , at   680.0     0.2   0.3   a
  , at   720.0     0.0   0.0   a
  ]

-- |19 minute 24 second form (1164s) with Fichtean motive arc.
-- Same proportional arc as form444, scaled to 1164s.
form1164 :: P.Progression -> P.Progression -> [FormNode]
form1164 a b =
  [ at     0.0     0.0   0.0   a
  , at   145.5     0.2   0.35  a
  , at   291.0     0.35  0.5   a
  , at   436.5     0.5   0.6   b
  , at   582.0     0.4   0.5   b
  , at   720.8     0.8   0.85  b
  , at   873.0     1.0   1.0   b
  , at   996.0     0.7   0.8   a
  , at  1100.0     0.2   0.3   a
  , at  1164.0     0.0   0.0   a
  ]
