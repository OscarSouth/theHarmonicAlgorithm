-- |
-- Module      : Harmonic.Interface.Tidal.Motif
-- Description : Motivic development operators for TidalCycles patterns
--
-- A motif is a plain pattern: a @Pattern Bool@ rhythm and a @Pattern Int@
-- contour of voicing-index degrees, realised against the active harmony by
-- @arrange@. The classic developments are already Tidal —
--
-- @
-- retrograde  = rev        augmentation = slow n      transposition = |+ \/ |-
-- diminution  = fast n     rotation     = \<~ \/ ~>     combination   = struct
-- @
--
-- — so this module supplies only what Tidal lacks: a combining operator that
-- reads as one gesture, a melodic inversion, and a retrograde that spans a
-- phrase rather than a cycle.
module Harmonic.Interface.Tidal.Motif
  ( -- * Combination
    (>:<)

    -- * Inversion
  , mirror

    -- * Retrograde
  , retro
  , retroN
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Form (beatsPerBar)

infixl 4 >:<

-- | Combine a rhythm with a contour: the rhythm's onsets sample the fragment.
-- 'struct' under a name that reads as motivic development at the call site.
--
-- @motif = rhythm >:< contour@
(>:<) :: Pattern Bool -> Pattern Int -> Pattern Int
(>:<) = struct

-- | Melodic inversion of a contour about a degree axis. Degree @d@ reflects to
-- @2 * axis - d@, so @mirror 0@ turns an ascending line into its descent
-- around the root.
mirror :: Int -> Pattern Int -> Pattern Int
mirror axis = fmap (\d -> 2 * axis - d)

-- | Retrograde over @cyc@ cycles rather than one.
--
-- Tidal's 'rev' reverses within each cycle. One cycle here is one beat, so a
-- motif written the house way (@\"[0 3 1 2 …]\/4\"@, a bar) spans four cycles,
-- and 'rev' on it reverses each beat in place and leaves the phrase order
-- untouched — which reads as \"rev did nothing\" and is easy to ship by
-- mistake.
--
-- @retroN 8 motif   -- retrograde a two-bar phrase@
retroN :: Pattern Time -> Pattern a -> Pattern a
retroN cyc = slow cyc . rev . fast cyc

-- | Retrograde a whole bar. One bar is 'beatsPerBar' cycles.
--
-- @retro contour   -- true phrase retrograde@
retro :: Pattern a -> Pattern a
retro = retroN (realToFrac beatsPerBar)
