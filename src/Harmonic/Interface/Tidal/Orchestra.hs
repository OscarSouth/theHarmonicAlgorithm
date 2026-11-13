-- |
-- Module      : Harmonic.Interface.Tidal.Orchestra
-- Description : Orchestral instrument functions for TidalCycles
--
-- Voice line system (SATB + octave variants) and instrument functions
-- with built-in MIDI range clipping and octave handling.
-- Each instrument is a thin 'arrange' wrapper — clip and octave shift
-- are internal; the composer sees only voice assignment and kinetics range.
--
-- Range belongs to the instrument and lives here; channel belongs to the
-- module the orchestra plays on, so every instrument takes a 'Rig' first. The
-- boot file declares one rig and binds the plain names to it
-- (@flute = O.flute rig@ …), which is why a performance file never names a
-- channel: @violin1 T (0,1) k vl flow Soprano # pizz@ sounds on whatever module
-- the session declared. The Roland JV-1010 map lives in
-- "Harmonic.Interface.Tidal.Devices.JV1010" as @jv1010@.

module Harmonic.Interface.Tidal.Orchestra (
    -- * Voice lines
    Voice(..), VoiceLines(..), voiceLines, vlGet, voiceOct,

    -- * Rig
    Rig(..), noArt,

    -- * Pitched instruments
    -- | Every pitched instrument shares the 'Instrument' shape and carries its
    -- own MIDI range, reading only its channel from the 'Rig'. Ranges are
    -- given in Tidal note space (MIDI &#8722; 60); the physical range follows
    -- in parentheses.
    --
    -- +------------+-----------------+
    -- | Instrument | Range           |
    -- +============+=================+
    -- | flute      | C3-D6           |
    -- | oboe       | Bb3-A6          |
    -- | clarinet   | D2-Bb6          |
    -- | bassoon    | Bb1-Eb5         |
    -- | horn       | B1-F5           |
    -- | trombone   | Bb1-F5          |
    -- | basstrom   | A0-G3           |
    -- | harp       | B1-F#7          |
    -- | timpani    | D2-C4           |
    -- | violin1\/2 | G3-A7           |
    -- | viola      | C3-E6           |
    -- | cello      | C2-C6           |
    -- | contrabass | C1-C4           |
    -- +------------+-----------------+
    flute, oboe, clarinet, bassoon,
    horn, trombone, basstrom,
    harp, timpani,
    violin1, violin2, viola, cello, contrabass,

    -- * Score mode
    -- | The same instruments reading written degrees against a layer of the
    -- progression in the 'IK': @flute' M 1 "p" k "[0 2 4]/4"@. See
    -- "Harmonic.Interface.Tidal.Score".
    flute', oboe', clarinet', bassoon',
    horn', trombone', basstrom',
    harp', timpani',
    violin1', violin2', viola', cello', contrabass',

    -- * Unpitched percussion
    bassdrum, tamtam, tambourine, triangle, woodblock,

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
import Harmonic.Interface.Tidal.Score (dyn, degrees)
import Harmonic.Interface.Tidal.Form (Kinetics(..))
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
-- Rig
-------------------------------------------------------------------------------

-- | One module's orchestral map: the MIDI channel each instrument answers on.
-- The instrument functions read their channel from here, so a single
-- declaration in the boot file selects the whole orchestra and the composer's
-- surface stays channel-free. Ranges are not the rig's: an instrument's range
-- is its own, whatever module plays it.
data Rig = Rig
  { rigName     :: String
  , rigFlute, rigOboe, rigClarinet, rigBassoon :: Int
  , rigHorn, rigTrombone, rigBasstrom          :: Int
  , rigHarp, rigTimpani                        :: Int
  , rigViolin1, rigViolin2, rigViola, rigCello, rigContrabass :: Int
  , rigBassdrum, rigTamtam, rigTambourine, rigTriangle, rigWoodblock :: Maybe (Int, Int)
    -- ^ @(MIDI note, channel)@; 'Nothing' when the module has no such voice,
    --   so the line loads and stays silent
  , rigPizz, rigSpicc, rigMarc, rigLegg, rigArco :: ControlPattern
    -- ^ postfix articulation tags; 'noArt' where the module keeps one channel
    --   per section and the technique has no separate destination
  , rigContinuo :: Int
    -- ^ the channel the continuo voice is retuned on
  , rigSetup    :: [ControlPattern]
    -- ^ one-shots that configure the module from cold, in receive order;
    --   empty when the patches live on the device
  }

-- | Identity under '#': contributes no fields, so the pattern keeps its own
-- channel. The articulation tag for a technique the rig does not route.
noArt :: ControlPattern
noArt = pure Map.empty

-------------------------------------------------------------------------------
-- instrument (internal helper)
-------------------------------------------------------------------------------

-- Pipeline: arrange -> # ch -> |+ oct -> clip (outermost, filters AFTER octave shift)
instrument :: (Int, Int) -> Int -> Instrument
instrument bounds chan lyr ki k vl vf v =
    clip bounds $ arrange ki k (-9,9) lyr vf (overlapF 0) [vlGet v vl] # ch chan |+ oct (fromIntegral (voiceOct v))

-- | The literal entry to the same instrument: a layer, an octave, a dynamic
-- mark and a degree pattern read against the progression in the 'IK' (see
-- 'Harmonic.Interface.Tidal.Score.degrees'). Rhythm comes from the degrees;
-- the dynamic is sampled at each onset, so one mark holds for the line and a
-- pattern of marks shapes a phrase; the form's dynamic arc scales the result
-- as it does in 'arrange'. The octave lands before the clip, as the voice's
-- octave does above.
instrument' :: (Int, Int) -> Int -> Layer -> Pattern Int -> Pattern String -> IK -> Pattern Int -> ControlPattern
instrument' bounds chan lyr octv dy k degs =
    clip bounds $ note (degrees lyr octv k degs) # ch chan # vel (dyn dy) |* vel (kDynamic (fst k))

-------------------------------------------------------------------------------
-- Pitched instruments (partial application of instrument)
-------------------------------------------------------------------------------

-- | Every pitched instrument takes the 'Rig' it sounds on, then a 'Layer'
-- argument — 'T' to voice the triad layer (default harmonic behaviour), 'S'
-- for the strata layer, 'M' for the diatonic-mode layer, or a combination
-- selector ('TS'\/'TM'\/'SM' pairs, 'TSM' the full union, 'PT' the shared
-- pivot tones — on a polytonal genE context these are the 4-note structures,
-- the pentad and the pivot-tone line; on a gen\/genJ context 'S' is the bar's
-- chordscale pentatonic, 'M' its mode, and 'PT' the chord tones the
-- pentatonic keeps). The boot file binds the rig, so in a session:
--
-- @d1 $ flute T (0,1) k voiceLines flow Soprano@
-- @d1 $ flute TS (0,1) k voiceLines flow Soprano   -- genE pair@
--
-- The numeric bounds are the range limits of the orchestral sampler patches
-- and are authoritative; the note-name comments sit 2-4 semitones above the
-- coded floor for bassoon, horn, trombone and harp. A practical review at the
-- instrument sets both — never the numbers to the names.
flute, oboe, clarinet, bassoon, horn, trombone, basstrom, harp, timpani,
  violin1, violin2, viola, cello, contrabass :: Rig -> Instrument
flute      = instrument  fluteRange . rigFlute
oboe       = instrument  oboeRange . rigOboe
clarinet   = instrument  clarinetRange . rigClarinet
bassoon    = instrument  bassoonRange . rigBassoon
horn       = instrument  hornRange . rigHorn
trombone   = instrument  tromboneRange . rigTrombone
basstrom   = instrument  basstromRange . rigBasstrom
harp       = instrument  harpRange . rigHarp
timpani    = instrument  timpaniRange . rigTimpani
violin1    = instrument  violin1Range . rigViolin1
violin2    = instrument  violin2Range . rigViolin2
viola      = instrument  violaRange . rigViola
cello      = instrument  celloRange . rigCello
contrabass = instrument  contrabassRange . rigContrabass

-- | The score-mode form of each instrument: a layer, an octave (a pattern:
-- @1@ the melodic register, @"[1,2]"@ two octaves), a dynamic mark, the 'IK'
-- and a degree pattern, sharing the plain form's range and channel; the
-- articulation tags apply as usual:
--
-- @, cello' M 0 "p" k "[-4 . 2@3 ~ . -6 . 0@3 ~]/4" # pizz@
flute', oboe', clarinet', bassoon', horn', trombone', basstrom', harp', timpani',
  violin1', violin2', viola', cello', contrabass'
  :: Rig -> Layer -> Pattern Int -> Pattern String -> IK -> Pattern Int -> ControlPattern
flute'     = instrument' fluteRange . rigFlute
oboe'      = instrument' oboeRange . rigOboe
clarinet'  = instrument' clarinetRange . rigClarinet
bassoon'   = instrument' bassoonRange . rigBassoon
horn'      = instrument' hornRange . rigHorn
trombone'  = instrument' tromboneRange . rigTrombone
basstrom'  = instrument' basstromRange . rigBasstrom
harp'      = instrument' harpRange . rigHarp
timpani'   = instrument' timpaniRange . rigTimpani
violin1'   = instrument' violin1Range . rigViolin1
violin2'   = instrument' violin2Range . rigViolin2
viola'     = instrument' violaRange . rigViola
cello'     = instrument' celloRange . rigCello
contrabass' = instrument' contrabassRange . rigContrabass

-- Range limits of the orchestral sampler patches, shared by both forms.
fluteRange, oboeRange, clarinetRange, bassoonRange, hornRange, tromboneRange, basstromRange, harpRange, timpaniRange, violin1Range, violin2Range, violaRange, celloRange, contrabassRange :: (Int, Int)
fluteRange       = (-12,  26)   -- C3–D6  (MIDI 48–86)
oboeRange        = ( -2,  33)   -- Bb3–A6 (MIDI 58–93)
clarinetRange    = (-22,  34)   -- D2–Bb6 (MIDI 38–94)
bassoonRange     = (-28,  15)   -- Bb1–Eb5 (MIDI 32–75)
hornRange        = (-29,  17)   -- B1–F5  (MIDI 31–77)
tromboneRange    = (-28,  17)   -- Bb1–F5 (MIDI 32–77)
basstromRange    = (-39,  -5)   -- A0–G3  (MIDI 21–55)
harpRange        = (-29,  42)   -- B1–F#7 (MIDI 31–102)
timpaniRange     = (-22,   0)   -- D2–C4  (MIDI 38–60)
violin1Range     = ( -5,  45)   -- G3–A7  (MIDI 55–105)
violin2Range     = ( -5,  45)   -- G3–A7  (MIDI 55–105)
violaRange       = (-12,  28)   -- C3–E6  (MIDI 48–88)
celloRange       = (-24,  24)   -- C2–C6  (MIDI 36–84)
contrabassRange  = (-36,   0)   -- C1–C4  (MIDI 24–60)

-------------------------------------------------------------------------------
-- Unpitched percussion (struct-based)
-------------------------------------------------------------------------------

-- | A struck voice: one MIDI note on one channel, a boolean rhythm, and a
-- fixed sustain. A rig without the voice yields silence.
hit :: Double -> Maybe (Int, Int) -> Pattern Bool -> ControlPattern
hit _ Nothing _ = silence
hit secs (Just (nt, c)) pat = struct pat $ midinote (fromIntegral nt) # ch c # sustain (pure secs)

-- | The unpitched section: a boolean pattern rather than a voice. The tam-tam
-- and the triangle ring, the rest are struck short. Where a rig's percussion
-- is a General MIDI kit on a channel the module shares with other gear, the
-- voices chosen here (54, 76, 81) sit above the band that gear uses, so a
-- tambourine never lands on a sub-bass pad.
--
-- @d1 $ bassdrum \"t ~ ~ t\"@   @, tambourine \"[~ t]\"@   @, woodblock \"t*2\"@
bassdrum, tamtam, tambourine, triangle, woodblock :: Rig -> Pattern Bool -> ControlPattern
bassdrum   r = hit 0.05 (rigBassdrum r)
tamtam     r = hit 0.5  (rigTamtam r)
tambourine r = hit 0.05 (rigTambourine r)
triangle   r = hit 0.3  (rigTriangle r)
woodblock  r = hit 0.05 (rigWoodblock r)

-------------------------------------------------------------------------------
-- String articulations (postfix tags)
-------------------------------------------------------------------------------

-- | String articulations, applied as a postfix tag:
--
-- @, violin1 T (0,1) k vl flow Soprano # pizz@
--
-- On a module that keeps one channel per technique the tag re-routes the
-- line; where a technique has no destination the tag is 'noArt' and the line
-- sounds as written.
pizz, spicc, marc, legg, arco :: Rig -> ControlPattern
pizz  = rigPizz
spicc = rigSpicc
marc  = rigMarc
legg  = rigLegg
arco  = rigArco

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
