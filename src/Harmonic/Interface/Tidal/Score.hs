-- |
-- Module      : Harmonic.Interface.Tidal.Score
-- Description : Score mode — bar blocks, degrees against the progression's layers, dynamic marks
--
-- The literal, linear way of writing for the orchestra: a piece is a list of
-- blocks addressed by bar number, each a stack of lines that read scale
-- degrees against a layer of the progression the form carries (T the chord,
-- S its pentatonic, M its mode), played by a primed instrument that takes its
-- dynamic as a mark. It is the counterpart of the kinetics workflow and shares
-- its slot: the progression in @k@ is a one-bar key from 'inKey', a generated
-- or recalled context, or anything else that feeds 'Harmonic.Interface.Tidal.Form.iK'.
--
-- @
-- key  = inKey D "major"                      -- one bar: D F# A | D E F# A B | D major
-- form = [rh 1 1 0.4 key, rh 9 1 0.8 key]     -- the dynamic arc, in score bars
--
-- tune k = stack [silence
--   ,flute'   M "[1,2]" "p"  k "[5 [2 4 . 5] [4 3 2 4 . 5] [4 5] ~!3]/28"
--   ,violin1' M 1       "mp" k "[12 [9 11 . 12] …]/28"
--   ]
--
-- andante f k d = p "score" $ f $ score pre [bars 3 9 $ tune k, …] |* vel d
--
-- do let k = preroll pre $ iK tempo form (rep key 1)
--    mapM_ id [hush, setbpm tempo, resetCycles, display k, andante id k $ (*1) 0.8]
-- @
--
-- Time is the session's: one cycle is one beat, four cycles a bar, bar 1 the
-- first bar of the piece. A block runs on its own clock ('seqP' starts each
-- pattern from its cycle 0 at the block's first beat), so an ostinato written
-- @\"[a b c]\/12\"@ starts on @a@ wherever the block is placed and a
-- @\"<~ x x>\"@ alternation counts its passes from the entry.
module Harmonic.Interface.Tidal.Score (
    -- * Blocks
    bars, score, staff,
    -- * Pitch
    inKey, degrees,
    -- * Dynamics
    dyn, dynTable, ramp,
) where

import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (sort)
import Text.Read (readMaybe)
import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Bridge (degreeLookup, closedPosition)
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK)
import qualified Harmonic.Rules.Types.Harmony as H
import Harmonic.Rules.Types.Pitch (NoteName, PitchClass(..), pitchClass, mkPitchClass, sharp)
import qualified Harmonic.Rules.Types.Progression as Pr
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Rules.Types.ProgressionContext (Layer)
import qualified Harmonic.Rules.Types.Scale as Sc

-- | A block of a score: bars @b1@ to @b2@ inclusive, 1-indexed, four cycles
-- per bar, as a 'seqP' triple. 'seqP' starts each block's pattern from its
-- own cycle 0, so the same mininotation reads the same at bar 1 and at bar 17.
bars :: Time -> Time -> Pattern a -> (Time, Time, Pattern a)
bars b1 b2 p = ((b1 - 1) * 4, b2 * 4, p)

-- | Sequence a list of blocks with @pre@ bars of pre-roll, so the count-in,
-- the clock start and any controller resets land before bar 1 begins at
-- cycle @pre * 4@. Pair it with 'Harmonic.Interface.Tidal.Form.preroll' on
-- the form, so the two clocks agree.
score :: Int -> [(Time, Time, Pattern a)] -> Pattern a
score pre = rotR (fromIntegral pre * 4) . seqP

-- | A phrase written one bar per string, as a single sequence of @beats@ beats:
-- the strings are joined and played as one pattern, so a weighted step may run
-- across a bar line (a note held over the bar is one step, not two). Written
-- by hand or by @scripts/midi2score.py@ from a MIDI render of an edition.
--
-- @harp' M 1 "p" k (staff 8 ["4@3 ~ 7 9@2 11@2", "12@8"])@
staff :: Int -> [String] -> Pattern Int
staff beats strs = parseBP_E ("[" ++ unwords strs ++ "]/" ++ show beats)

-- | A one-bar progression context for a key: the tonic triad on T (the
-- mode's degrees 1 3 5, so a minor key gives a minor triad and Lydian a
-- major one), the key's pentatonic on S (its parent major key's, rooted on
-- the tonic where the tonic belongs to it — A minor gives A C D E G — and
-- the mode's degrees 1 2 3 5 6 for the melodic and harmonic families) and
-- the mode itself on M. It feeds 'Harmonic.Interface.Tidal.Form.iK' like any
-- generated or recalled context, and round-trips through
-- 'Harmonic.Interface.Tidal.Recall.save'.
--
-- Names, case-insensitive: @major@ \/ @ionian@, @minor@ \/ @aeolian@,
-- @dorian@, @phrygian@, @lydian@, @mixolydian@, @locrian@, @melodic minor@,
-- @harmonic minor@, @harmonic major@, any 'Sc.ModeQuality' constructor
-- (@LydDom@, @AltDom@ …), and @chromatic@ (all twelve on every layer, so a
-- degree is a semitone). An unknown name is an error at declaration: a typo
-- must not become a silent scale.
inKey :: NoteName -> String -> PC.ProgressionContext
inKey tonic nm = PC.ProgressionContext (one tonic triad) (one sRoot penta) (one tonic mode) Nothing PC.FTriad
  where
    one root ivs = Pr.singleton (H.mkCadenceStatePCs root H.Unison ivs)
    r = unPitchClass (pitchClass tonic)
    (triad, penta, mode, sRoot) = case modeByName nm of
      Nothing | map toLower nm == "chromatic" -> ([0 .. 11], [0 .. 11], [0 .. 11], tonic)
              | otherwise -> error ("inKey: unknown mode " ++ show nm)
      Just q ->
        let ivs = sort [ (unPitchClass pc - r) `mod` 12 | pc <- Sc.modeChroma (Sc.Mode q (P r)) ]
            (parentPC, fam) = Sc.parentKey (Sc.Mode q (P r))
            parentPenta = map unPitchClass (Sc.pentaChroma (Sc.Pentatonic Sc.MajorPenta parentPC))
            (pentaRootPC, pentaIvs)
              | fam == Sc.Major && r `elem` parentPenta = (r, sort [ (p - r) `mod` 12 | p <- parentPenta ])
              | fam == Sc.Major = (unPitchClass parentPC, sort [ (p - unPitchClass parentPC) `mod` 12 | p <- parentPenta ])
              | otherwise = (r, map (ivs !!) [0, 1, 2, 4, 5])
            sName | pentaRootPC == r = tonic
                  | otherwise = sharp (mkPitchClass pentaRootPC)
        in (map (ivs !!) [0, 2, 4], pentaIvs, ivs, sName)

modeByName :: String -> Maybe Sc.ModeQuality
modeByName nm = case map toLower nm of
  "major"          -> Just Sc.Ionian
  "ionian"         -> Just Sc.Ionian
  "minor"          -> Just Sc.Aeolian
  "aeolian"        -> Just Sc.Aeolian
  "dorian"         -> Just Sc.Dorian
  "phrygian"       -> Just Sc.Phrygian
  "lydian"         -> Just Sc.Lydian
  "mixolydian"     -> Just Sc.Mixolydian
  "locrian"        -> Just Sc.Locrian
  "melodic minor"  -> Just Sc.MelMin
  "harmonic minor" -> Just Sc.HarmMin
  "harmonic major" -> Just Sc.HarmMaj
  _                -> readMaybe nm

-- | Scale degrees against a layer of the progression in @k@, at an octave.
-- Each degree reads the chord selected at its onset (the same rule as
-- 'Harmonic.Interface.Tidal.Bridge.arrange'); degree 0 is that bar's root at
-- the lattice's bass register (the root in [-12,-1]) and @n@ indexes the bar's
-- closed position above it, wrapping by octave — on a one-bar D major context
-- the M layer gives 0 → D3, 7 → D4, −7 → D2. The octave pattern shifts the
-- result by twelves and may carry a chord (@\"[1,2]\"@ sounds both octaves,
-- @\"[[1] [~]]\/16\"@ drops the notes under its rest).
degrees :: Layer -> Pattern Int -> IK -> Pattern Int -> Pattern Note
degrees lyr octv (kin, chordPat) degs =
  let scalesOf ctx =
        let sc = map (map fromIntegral . closedPosition) (toList (Pr.unProgression (PC.layer lyr ctx)))
        in (sc, length sc)
      cache = [ (ctx, scalesOf ctx) | ctx <- kProgs kin ]
      lookupCache ctx = maybe (scalesOf ctx) id (lookup ctx cache)
      pitches = innerJoin $ fmap (\ctx -> degreeLookup (lookupCache ctx) chordPat degs) (kProg kin)
  in pitches |+ fmap (fromIntegral . (* 12)) octv

-- | Dynamic marks as amplitude, 'dynTable' looked up per event. A mark the
-- table does not know yields no event, so under @#@ the note it would have
-- carried is dropped: a misspelt mark is heard as a missing note.
dyn :: Pattern String -> Pattern Double
dyn = filterJust . fmap (`lookup` dynTable)

-- | The amplitude of each dynamic mark, @ffff@ down to @pppp@.
dynTable :: [(String, Double)]
dynTable =
  [ ("ffff", 1.00), ("fff", 0.90), ("ff", 0.75), ("f", 0.65), ("mf", 0.55)
  , ("mp", 0.45), ("p", 0.40), ("pp", 0.35), ("ppp", 0.30), ("pppp", 0.25) ]

-- | A straight line from @lo@ to @hi@ across one cycle in 64 steps; @slow n@
-- stretches it over @n@ cycles. Multiply a line by it (@|* vel (slow 8 $
-- ramp 0.9 1.3)@) for a crescendo over two bars; the form's dynamic nodes do
-- the same for the whole arc.
ramp :: Pattern Double -> Pattern Double -> Pattern Double
ramp lo hi = segment 64 (range lo hi saw)
