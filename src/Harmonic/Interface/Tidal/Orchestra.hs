-- |
-- Module      : Harmonic.Interface.Tidal.Orchestra
-- Description : Orchestral instrument functions for TidalCycles
--
-- Voice line system (SATB + octave variants) and instrument functions
-- with built-in MIDI range clipping and octave handling.
-- Each instrument is a thin 'arrange' wrapper — clip and octave shift
-- are internal; the composer sees only voice assignment and kinetics range.

module Harmonic.Interface.Tidal.Orchestra (
    -- * Voice lines
    Voice(..), VoiceLines(..), voiceLines, vlGet, voiceOct,

    -- * Pitched instruments
    -- | Every pitched instrument shares the 'Instrument' shape and carries its
    -- own MIDI range and channel. Ranges are given in Tidal note space
    -- (MIDI &#8722; 60); the physical range follows in parentheses.
    --
    -- +------------+---------+-----------------+
    -- | Instrument | Channel | Range           |
    -- +============+=========+=================+
    -- | flute      | 1       | C3-D6           |
    -- | oboe       | 2       | Bb3-A6          |
    -- | clarinet   | 3       | D2-Bb6          |
    -- | bassoon    | 4       | Bb1-Eb5         |
    -- | horn       | 5       | B1-F5           |
    -- | trombone   | 6       | Bb1-F5          |
    -- | basstrom   | 6       | A0-G3           |
    -- | harp       | 7       | B1-F#7          |
    -- | timpani    | 8       | D2-C4           |
    -- | violin1\/2 | 16      | G3-A7           |
    -- | viola      | 16      | C3-E6           |
    -- | cello      | 16      | C2-C6           |
    -- | contrabass | 16      | C1-C4           |
    -- +------------+---------+-----------------+
    flute, oboe, clarinet, bassoon,
    horn, trombone, basstrom,
    harp, timpani,
    violin1, violin2, viola, cello, contrabass,

    -- * Unpitched percussion
    bassdrum, tamtam,

    -- * String articulations
    pizz, spicc, marc, legg, arco,

    -- * Divisi
    Instrument, divisi, divisi2, divisi3,

    -- * Internal (for testing only)
    clip, voiceBase, voiceTier,
) where

import Sound.Tidal.Context hiding (voice, clip)
import qualified Data.Map.Strict as Map
import Harmonic.Interface.Tidal.Bridge (VoiceFunction, arrange, overlapF)
import Harmonic.Interface.Tidal.Form (IK)
import Harmonic.Interface.Tidal.Instruments (ch, vel)
import Harmonic.Interface.Tidal.Utils (oct)
import Harmonic.Rules.Types.ProgressionContext (Layer(..))

-------------------------------------------------------------------------------
-- Voice type (SATB + octave variants)
-------------------------------------------------------------------------------

-- | An SATB voice, at one of five octave transpositions and one of three
-- divisi tiers.
--
-- Divisi tiers: the base 20 voices (tier 0), then the same 20 primed (tier 1,
-- read the @'@ fields), then double-primed (tier 2, the @''@ fields). Declared
-- in three identical-order blocks of 20 so 'voiceBase' \/ 'voiceTier' derive by
-- enum arithmetic (@mod@ \/ @div@ 20). Octave rides the constructor, so e.g.
-- @Bass8vb'@ reads the @bass'@ field at octave &#8722;1.
data Voice
  -- tier 0 (base)
  = Soprano | Alto | Tenor | Bass                                      -- loco (normal register)
  | Soprano8va  | Alto8va  | Tenor8va  | Bass8va                      -- octave up
  | Soprano15va | Alto15va | Tenor15va | Bass15va                     -- two octaves up
  | Soprano8vb  | Alto8vb  | Tenor8vb  | Bass8vb                     -- octave down
  | Soprano15vb | Alto15vb | Tenor15vb | Bass15vb                    -- two octaves down
  -- tier 1 (')
  | Soprano' | Alto' | Tenor' | Bass'
  | Soprano8va'  | Alto8va'  | Tenor8va'  | Bass8va'
  | Soprano15va' | Alto15va' | Tenor15va' | Bass15va'
  | Soprano8vb'  | Alto8vb'  | Tenor8vb'  | Bass8vb'
  | Soprano15vb' | Alto15vb' | Tenor15vb' | Bass15vb'
  -- tier 2 ('')
  | Soprano'' | Alto'' | Tenor'' | Bass''
  | Soprano8va''  | Alto8va''  | Tenor8va''  | Bass8va''
  | Soprano15va'' | Alto15va'' | Tenor15va'' | Bass15va''
  | Soprano8vb''  | Alto8vb''  | Tenor8vb''  | Bass8vb''
  | Soprano15vb'' | Alto15vb'' | Tenor15vb'' | Bass15vb''
  deriving (Show, Eq, Ord, Enum, Bounded)

-------------------------------------------------------------------------------
-- VoiceLines
-------------------------------------------------------------------------------

-- | The scale-degree pattern assigned to each voice.
--
-- Each SATB voice has three divisi tiers: base, @'@, @''@. All are ordinary
-- scale-degree patterns (same syntax and semantics); the primed fields only
-- exist to hold extra divisi desks. Defaults stack one\/two degrees above the
-- base so an undeclared @divisi 3@ voices a chord; declared primes are whatever
-- the composer writes.
data VoiceLines = VoiceLines
  { _vl      :: Pattern Int     -- ^ structural placeholder (always silence)
  , soprano  :: Pattern Int     -- ^ Soprano
  , alto     :: Pattern Int     -- ^ Alto
  , tenor    :: Pattern Int     -- ^ Tenor
  , bass     :: Pattern Int     -- ^ Bass
  , soprano' :: Pattern Int     -- ^ Soprano, divisi desk 2
  , alto'    :: Pattern Int     -- ^ Alto, divisi desk 2
  , tenor'   :: Pattern Int     -- ^ Tenor, divisi desk 2
  , bass'    :: Pattern Int     -- ^ Bass, divisi desk 2
  , soprano'':: Pattern Int     -- ^ Soprano, divisi desk 3
  , alto''   :: Pattern Int     -- ^ Alto, divisi desk 3
  , tenor''  :: Pattern Int     -- ^ Tenor, divisi desk 3
  , bass''   :: Pattern Int     -- ^ Bass, divisi desk 3
  }

-- | Default voice lines: root in the bass, root 8va in the soprano, inner
-- voices on the 2nd and 3rd degrees. Override the fields you want:
--
-- @vl = voiceLines { soprano = \"3 4 3 2\", bass = \"0\" }@
voiceLines :: VoiceLines
voiceLines = VoiceLines
  { _vl      = "~"
  , soprano  = "3"      -- root 8va
  , alto     = "1"      -- 2nd degree
  , tenor    = "2"      -- 3rd degree
  , bass     = "0"      -- root
  , soprano' = "4"      -- one degree above base defaults …
  , alto'    = "2"
  , tenor'   = "3"
  , bass'    = "1"
  , soprano''= "5"      -- … two degrees above
  , alto''   = "3"
  , tenor''  = "4"
  , bass''   = "2"
  }

-------------------------------------------------------------------------------
-- vlGet \/ voiceOct
-------------------------------------------------------------------------------

-- | Strip the divisi prime, giving the tier-0 (base) voice. Relies on the
-- three identical-order 'Voice' blocks of 20.
voiceBase :: Voice -> Voice
voiceBase = toEnum . (`mod` 20) . fromEnum

-- | Divisi tier of a voice: 0 (base), 1 (@'@), 2 (@''@).
voiceTier :: Voice -> Int
voiceTier = (`div` 20) . fromEnum

-- | SATB letter of a voice, ignoring octave and divisi tier.
data VLetter = LtrS | LtrA | LtrT | LtrB

voiceLetter :: Voice -> VLetter
voiceLetter v = case voiceBase v of
  Soprano -> LtrS; Soprano8va -> LtrS; Soprano15va -> LtrS; Soprano8vb -> LtrS; Soprano15vb -> LtrS
  Alto -> LtrA; Alto8va -> LtrA; Alto15va -> LtrA; Alto8vb -> LtrA; Alto15vb -> LtrA
  Tenor -> LtrT; Tenor8va -> LtrT; Tenor15va -> LtrT; Tenor8vb -> LtrT; Tenor15vb -> LtrT
  _ -> LtrB   -- Bass* (voiceBase always yields a tier-0 constructor)

-- | Read the scale-degree pattern a given 'Voice' should play, resolving both
-- its SATB letter and its divisi tier.
vlGet :: Voice -> VoiceLines -> Pattern Int
vlGet v = case (voiceLetter v, voiceTier v) of
  (LtrS, 0) -> soprano;  (LtrS, 1) -> soprano';  (LtrS, _) -> soprano''
  (LtrA, 0) -> alto;     (LtrA, 1) -> alto';     (LtrA, _) -> alto''
  (LtrT, 0) -> tenor;    (LtrT, 1) -> tenor';    (LtrT, _) -> tenor''
  (LtrB, 0) -> bass;     (LtrB, 1) -> bass';     (LtrB, _) -> bass''

-- | Octave transposition carried by a 'Voice' constructor: @0@ loco, @1@ for
-- @8va@, @2@ for @15va@, @-1@ for @8vb@, @-2@ for @15vb@.
voiceOct :: Voice -> Int
voiceOct v = case voiceBase v of
  Soprano -> 0; Alto -> 0; Tenor -> 0; Bass -> 0
  Soprano8va  -> 1;  Alto8va  -> 1;  Tenor8va  -> 1;  Bass8va  -> 1
  Soprano15va -> 2;  Alto15va -> 2;  Tenor15va -> 2;  Bass15va -> 2
  Soprano8vb  -> (-1); Alto8vb  -> (-1); Tenor8vb  -> (-1); Bass8vb  -> (-1)
  _ -> (-2)   -- *15vb (voiceBase always yields a tier-0 constructor)

-------------------------------------------------------------------------------
-- clip (MIDI range enforcement — internal)
-------------------------------------------------------------------------------

-- | Drop any event whose note falls outside a MIDI range. Applied outermost by
-- 'Instrument', so it filters /after/ the octave shift.
clip :: (Int, Int) -> ControlPattern -> ControlPattern
clip (lo, hi) = filterValues (\vm ->
    case Map.lookup "note" vm of
        Just (VF v) -> v >= fromIntegral lo && v <= fromIntegral hi
        Just (VN nt) -> let v = unNote nt in v >= fromIntegral lo && v <= fromIntegral hi
        _ -> True)

-------------------------------------------------------------------------------
-- instrument (internal helper)
-------------------------------------------------------------------------------

-- Pipeline: arrange -> # ch -> |+ oct -> clip (outermost, filters AFTER octave shift)
instrument :: (Int, Int) -> Int -> Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
instrument bounds chan lyr ki k vl vf v =
    clip bounds $ arrange ki k (-9,9) lyr vf (overlapF 0) [vlGet v vl] # ch chan |+ oct (fromIntegral (voiceOct v))

-------------------------------------------------------------------------------
-- Pitched instruments (partial application of instrument)
-------------------------------------------------------------------------------

-- | Winds, channels 1&#8211;4.
--
-- Every pitched instrument takes a prepended 'Layer' argument — 'T' to voice
-- the triad layer (default harmonic behaviour), 'S' for the strata layer,
-- 'M' for the diatonic-mode layer:
--
-- @d1 $ flute T (0,1) k voiceLines flow Soprano@
-- RANGE-REVIEW NOTE (2026-08-25): the numeric bounds are tuned to the
-- range limits of the actual JV1010 sampler patches and are authoritative;
-- the note-name comments drift from them in four places (bassoon, horn,
-- trombone, harp — names sit 2-4 semitones above the coded MIDI floor).
-- Do not "correct" the numbers to the names: a practical per-instrument
-- review against the JV1010 will set both precisely. Details in
-- documents/ALGORITHMIC_ORCHESTRATION.md ("Range review").
flute, oboe, clarinet, bassoon :: Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
flute      = instrument (-12, 26) 1   -- C3–D6  (MIDI 48–86)
oboe       = instrument ( -2, 33) 2   -- Bb3–A6 (MIDI 58–93)
clarinet   = instrument (-22, 34) 3   -- D2–Bb6 (MIDI 38–94)
bassoon    = instrument (-28, 15) 4   -- Bb1–Eb5 (MIDI 32–75) [see range-review note]

-- | Brass, channels 5&#8211;6. @basstrom@ shares channel 6 with @trombone@ at a
-- lower range.
horn, trombone, basstrom :: Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
horn       = instrument (-29, 17) 5   -- B1–F5  (MIDI 31–77) [see range-review note]
trombone   = instrument (-28, 17) 6   -- Bb1–F5 (MIDI 32–77) [see range-review note]
basstrom   = instrument (-39, -5) 6   -- A0–G3  (MIDI 21–55)

-- | Harp, channel 7. The widest range in the orchestra.
harp :: Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
harp       = instrument (-29, 42) 7   -- B1–F#7 (MIDI 31–102) [see range-review note]

-- | Timpani, channel 8. Pitched percussion; the narrowest range.
timpani :: Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
timpani    = instrument (-22,  0) 8   -- D2–C4  (MIDI 38–60)

-- | Strings. All default to channel 16 (@arco@); prefix an articulation
-- ('pizz', 'spicc', 'marc', 'legg') to route elsewhere.
violin1, violin2, viola, cello, contrabass :: Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
violin1    = instrument ( -5, 45) 16  -- G3–A7  (MIDI 55–105)
violin2    = instrument ( -5, 45) 16  -- G3–A7  (MIDI 55–105)
viola      = instrument (-12, 28) 16  -- C3–E6  (MIDI 48–88)
cello      = instrument (-24, 24) 16  -- C2–C6  (MIDI 36–84)
contrabass = instrument (-36,  0) 16  -- C1–C4  (MIDI 24–60)

-------------------------------------------------------------------------------
-- Unpitched percussion (struct-based)
-------------------------------------------------------------------------------

-- | Bass drum, channel 9. Struct-based: takes a boolean pattern rather than a
-- voice, since it is unpitched.
--
-- @d1 $ bassdrum \"t ~ ~ t\"@
bassdrum :: Pattern Bool -> ControlPattern
bassdrum pat = struct pat $ midinote 36 # ch 9 # sustain 0.05

-- | Tam-tam, channel 11. Struct-based, with a long sustain.
tamtam :: Pattern Bool -> ControlPattern
tamtam pat = struct pat $ midinote 31 # ch 11 # sustain 0.5

-------------------------------------------------------------------------------
-- String articulations (channel aliases)
-------------------------------------------------------------------------------

-- | String articulations, applied as a postfix channel override:
--
-- @, violin1 T (0,1) k vl flow Soprano # pizz@
--
-- @pizz@ 12, @spicc@ 13, @marc@ 14, @legg@ 15, @arco@ 16 (the string default).
pizz, spicc, marc, legg, arco :: ControlPattern
pizz  = ch 12    -- pizzicato
spicc = ch 13    -- spiccato
marc  = ch 14    -- marcato
legg  = ch 15    -- legato
arco  = ch 16    -- arco (same as default)

-------------------------------------------------------------------------------
-- Divisi
-------------------------------------------------------------------------------

-- | The shared shape of every orchestral instrument function.
type Instrument =
  Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern

-- | Optional prefix wrapper: divide an instrument into @n@ desks, each reading
-- the next divisi tier of its voice (base, ' , ''), scaled by equal-power
-- @1\/sqrt n@ so the combined loudness matches the undivided line.
--
-- Used in the space form; drop @divisi n@ and the line is a normal single voice:
--
-- @, divisi 3 violin1 T (0,1) k vl grid Soprano@   — 3 desks: Soprano\/Soprano'/Soprano''
-- @, violin1 T (0,1) k vl grid Soprano@            — plain
--
-- Octave rides the 'Voice' argument, so @divisi 2 contrabass T (0.9,1) k vl grid Bass8vb@
-- voices Bass8vb and Bass8vb' (tier + octave together).
divisi :: Int -> Instrument -> Instrument
divisi nDesks instr lyr rng k vl vf v
  | nDesks <= 1 = instr lyr rng k vl vf v
  | otherwise =
      stack [ instr lyr rng k vl vf (primeN i v) | i <- [0 .. nDesks - 1] ]
        |* vel (1 / sqrt (fromIntegral nDesks))
  where
    -- 'Voice' has exactly 3 tiers of 20 (base \/ ' \/ ''); a bare
    -- @toEnum (+ i * 20)@ crashed the scheduler for @divisi 4@ or an
    -- already-primed voice. Clamp the TIER, keeping the base voice —
    -- desks beyond the top tier double it.
    primeN i v' =
      let idx  = fromEnum v'
          base = idx `mod` 20
          tier = min 2 (idx `div` 20 + i)
      in toEnum (base + 20 * tier)

-- | Equal-power volume scalers for hand-built desks that differ in articulation
-- or entry (not uniform 'divisi'). Postfix like the articulation tags — set
-- @amp@; the block's outer @|* vel d@ then multiplies, giving @d\/sqrt n@.
--
-- @, violin1 T (0,1)   k vl flow Soprano  # divisi2@
-- @, violin1 T (0.9,1) k vl grid Soprano' # divisi2@
divisi2, divisi3 :: ControlPattern
divisi2 = vel (1 / sqrt 2)
divisi3 = vel (1 / sqrt 3)
