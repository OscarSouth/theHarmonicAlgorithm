-- |
-- Module      : Harmonic.Rules.Import.Jazz
-- Description : Chord-symbol parser for the Bunks jazz corpus
--
-- Parses leadsheet chord symbols from the Jazz-Chord-Progressions-Corpus
-- (Bunks, Weyde, Dixon, Di Giorgi, ISMIR 2023) into absolute pitch-class
-- sets. Every symbol has the shape @root quality [\/bass]@ (or the bare
-- token @NC@ for silence); the parser honours the notation exactly:
--
-- * The written root and slash bass are never rewritten or dropped.
--   A slash bass is unioned into the pitch-class set and becomes the
--   chord's /anchor/ (the reference for zero-form and movement); without
--   a slash the root is the anchor.
-- * Qualities map to curated canonical tone sets ('qualityIntervals'):
--   the tones a jazz player reads from the symbol, not the full
--   theoretical extension stack. The conventions are documented on the
--   table itself.
-- * Symbols that fail to parse are returned as 'JazzRefusal' values —
--   recorded, never silently skipped.
--
-- Shared surface: the ingest run parses the corpus through it, genJ
-- names walked chords with 'jazzFunctionality', and @leadJ@ cues parse
-- through 'parseToken'.

module Harmonic.Rules.Import.Jazz (
    JazzToken(..),
    JazzChord(..),
    JazzRefusal(..),
    parseToken,
    JazzSong(..),
    parseSong,
    beatSlots,
    beatStream,
    jazzZeroForm,
    JazzCadence(..),
    jazzShow,
    songCadences,
    normalizeComposer,
    buildChangeEdges,
    qualityIntervals,
    qualityNames,
    qualityFrequency,
    canonicalQuality,
    jazzFunctionality,
    jazzFunctionalityR,
    parseSlashName,
    BassVocab(..),
    bassVocabFor,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.List (maximumBy, nub, sort)
import Data.Ord (comparing)
import Harmonic.Rules.Types.Pitch (PitchClass, mkPitchClass, unPitchClass)
import Harmonic.Rules.Types.Harmony (Movement, toMovement)

-- | One corpus chord token: sounding harmony or notated silence (@NC@).
data JazzToken
  = NoChord                -- ^ The @NC@ token: no harmony sounding.
  | Sounding JazzChord     -- ^ A parsed chord symbol.
  deriving (Show, Eq)

-- | A parsed chord symbol as pitch-class content.
data JazzChord = JazzChord
  { jcRoot    :: PitchClass        -- ^ Notated chord root.
  , jcBass    :: Maybe PitchClass  -- ^ Notated slash bass, when present.
  , jcQuality :: T.Text            -- ^ Quality string exactly as notated.
  , jcTones   :: [PitchClass]      -- ^ Absolute pitch-class set: root,
                                   --   quality tones and slash bass,
                                   --   sorted and deduplicated.
  , jcAnchor  :: PitchClass        -- ^ Slash bass when notated, else the
                                   --   root: the sounding fundamental,
                                   --   reference for zero-form/movement.
  } deriving (Show, Eq)

-- | A symbol the parser refused, with the reason. Refusals are
-- first-class data: aggregated and reported by the ingest run, never
-- silently dropped.
data JazzRefusal = JazzRefusal
  { refusalInput  :: T.Text  -- ^ The symbol as it appeared in the corpus.
  , refusalReason :: T.Text  -- ^ Why it failed to parse.
  } deriving (Show, Eq)

-- | Parse one whitespace-delimited corpus token.
--
-- >>> parseToken "Dm7/G"
-- Right (Sounding (JazzChord {jcRoot = P 2, jcBass = Just (P 7), ...}))
parseToken :: T.Text -> Either JazzRefusal JazzToken
parseToken t
  | t == "NC" = Right NoChord
  | otherwise = case parseNote t of
      Nothing -> Left (JazzRefusal t "no root note")
      Just (root, rest) -> do
        let (qual, slash) = T.breakOn "/" rest
        bass <- case T.stripPrefix "/" slash of
          Nothing -> Right Nothing
          Just b  -> case parseNote b of
            Just (bpc, leftover) | T.null leftover -> Right (Just bpc)
            _ -> Left (JazzRefusal t "malformed slash bass")
        ivs <- case Map.lookup qual qualityIntervals of
          Just ivs -> Right ivs
          Nothing  -> Left (JazzRefusal t ("unknown quality: " <> qual))
        let quals = map (\i -> root + mkPitchClass i) ivs
            tones = sort . nub $ maybe quals (: quals) bass
        Right . Sounding $ JazzChord
          { jcRoot    = root
          , jcBass    = bass
          , jcQuality = qual
          , jcTones   = tones
          , jcAnchor  = maybe root id bass
          }

-- Note-name parse: letter plus optional accidental, consumed greedily
-- (no corpus quality begins with 'b' or '#', so greed is always right).
-- The mod-12 arithmetic IS the enharmonic fold: Cb -> 11, B# -> 0, etc.
parseNote :: T.Text -> Maybe (PitchClass, T.Text)
parseNote t = do
  (c, rest) <- T.uncons t
  base <- lookup c [('C',0),('D',2),('E',4),('F',5),('G',7),('A',9),('B',11)]
  pure $ case T.uncons rest of
    Just ('#', r) -> (mkPitchClass (base + 1), r)
    Just ('b', r) -> (mkPitchClass (base - 1), r)
    _             -> (mkPitchClass base, rest)

-- | Every quality string the table covers (the corpus census: 122
-- qualities; @NC@ is handled at token level).
qualityNames :: [T.Text]
qualityNames = Map.keys qualityIntervals

-- | Curated canonical tone set per quality, as semitone intervals from
-- the root. Covers all 122 quality strings in the corpus census.
--
-- Conventions (the tones a player reads from the symbol, kept
-- deterministic so equal sonorities land on equal graph nodes):
--
-- 1. Written alterations and additions are always present, exactly as
--    notated, and displace their natural counterpart (@#5@\/@b5@ replace
--    the 5th, @b9@\/@#9@ replace the natural 9th where one is implied).
-- 2. The natural 5th is included except where altered — and except in
--    13th chords, which take the working-voicing form 1-3-b7-9-13 (no
--    5th, no 11th).
-- 3. Dominant 11th chords omit the 3rd (1-5-b7-9-11): at pitch-class
--    level @11@ and @9sus4@ are deliberately the same sonority.
-- 4. @b13@ stacks over an unaltered 5th (that presence is what separates
--    it from @#5@ at set level); @b6@ likewise.
-- 5. @alt@ is rendered as the classic altered voicing 1-3-#5-b7-b9
--    (identical set to @7#5b9@).
-- 6. Spelling variants are separate keys mapping to identical sets:
--    @M7@\/@maj7@, @o@\/@dim@, @h7@\/@m7b5@, @7+@\/@7#5@\/@+7@,
--    @sus@\/@sus4@\/@4@, @2@\/@sus2@, @mi@\/@m@, and so on.
-- 7. Literal oddities stay literal: @5@ is the bare power-chord dyad,
--    @67@ is 6th plus b7, @M@ is an explicit major triad, @susb9@ is a
--    sus4 triad with b9 and no 7th.
qualityIntervals :: Map.Map T.Text [Int]
qualityIntervals = Map.fromList
  [ -- Triads, dyads and added-tone colours
    ("",         [0,4,7])         -- bare symbol: major triad
  , ("M",        [0,4,7])         -- explicit major triad
  , ("m",        [0,3,7])
  , ("mi",       [0,3,7])
  , ("+",        [0,4,8])
  , ("m+",       [0,3,8])         -- minor with raised 5th
  , ("m#5",      [0,3,8])
  , ("o",        [0,3,6])
  , ("dim",      [0,3,6])
  , ("mb5",      [0,3,6])
  , ("Mb5",      [0,4,6])
  , ("5",        [0,7])           -- power chord: literal dyad
  , ("sus4",     [0,5,7])
  , ("sus",      [0,5,7])
  , ("4",        [0,5,7])
  , ("sus2",     [0,2,7])
  , ("2",        [0,2,7])
  , ("sus24",    [0,2,5,7])       -- both suspensions, no 3rd
  , ("susb9",    [0,1,5,7])       -- sus4 triad with b9, no 7th
  , ("mb6",      [0,3,7,8])
  , ("madd4",    [0,3,5,7])
  , ("add9",     [0,2,4,7])
  , ("madd9",    [0,2,3,7])
  , ("addb9",    [0,1,4,7])
  , ("add9no3",  [0,2,7])
  , ("+add9",    [0,2,4,8])
  , ("+add#9",   [0,3,4,8])
  , ("M#5add9",  [0,2,4,8])
    -- Sixths
  , ("6",        [0,4,7,9])
  , ("M6",       [0,4,7,9])
  , ("m6",       [0,3,7,9])
  , ("69",       [0,2,4,7,9])
  , ("M69",      [0,2,4,7,9])
  , ("m69",      [0,2,3,7,9])
  , ("6#11",     [0,4,6,7,9])
  , ("M69#11",   [0,2,4,6,7,9])
  , ("6b5",      [0,4,6,9])
  , ("67",       [0,4,7,9,10])    -- literal oddity: 6th plus b7
    -- Sevenths
  , ("7",        [0,4,7,10])
  , ("M7",       [0,4,7,11])
  , ("maj7",     [0,4,7,11])
  , ("m7",       [0,3,7,10])
  , ("m7b5",     [0,3,6,10])
  , ("h7",       [0,3,6,10])
  , ("o7",       [0,3,6,9])
  , ("dim7",     [0,3,6,9])
  , ("mM7",      [0,3,7,11])
  , ("mMaj7",    [0,3,7,11])
  , ("mM7b6",    [0,3,7,8,11])
  , ("oM7",      [0,3,6,11])      -- diminished triad, major 7th
  , ("o7M7",     [0,3,6,9,11])    -- full dim7 plus major 7th
  , ("7+",       [0,4,8,10])
  , ("7#5",      [0,4,8,10])
  , ("+7",       [0,4,8,10])
  , ("M7#5",     [0,4,8,11])
  , ("maj7#5",   [0,4,8,11])
  , ("M7+",      [0,4,8,11])
  , ("m7#5",     [0,3,8,10])
  , ("7b5",      [0,4,6,10])
  , ("M7b5",     [0,4,6,11])
  , ("7b6",      [0,4,7,8,10])    -- b6 over unaltered 5th (convention 4)
  , ("7#11",     [0,4,6,7,10])
  , ("M7#11",    [0,4,6,7,11])
  , ("7add6",    [0,4,7,9,10])
  , ("7add13",   [0,4,7,9,10])
  , ("M7add13",  [0,4,7,9,11])
  , ("7sus4",    [0,5,7,10])
  , ("7sus",     [0,5,7,10])
  , ("7sus4b9",  [0,1,5,7,10])
  , ("7b9sus4",  [0,1,5,7,10])
  , ("7susb9",   [0,1,5,7,10])
  , ("7sus4b9b13", [0,1,5,7,8,10])
  , ("7alt",     [0,1,4,8,10])    -- 1 3 #5 b7 b9 (convention 5)
  , ("7b9",      [0,1,4,7,10])
  , ("7#9",      [0,3,4,7,10])
  , ("m7b9",     [0,1,3,7,10])
  , ("7#5b9",    [0,1,4,8,10])
  , ("7#5#9",    [0,3,4,8,10])
  , ("7b5b9",    [0,1,4,6,10])
  , ("7b9b5",    [0,1,4,6,10])
  , ("7b5#9",    [0,3,4,6,10])
  , ("7b9#11",   [0,1,4,6,7,10])
  , ("7#9#11",   [0,3,4,6,7,10])
  , ("7#5b9#11", [0,1,4,6,8,10])
  , ("7b13",     [0,4,7,8,10])
  , ("7b9b13",   [0,1,4,7,8,10])
  , ("7#9b13",   [0,3,4,7,8,10])
  , ("M7#9b5",   [0,3,4,6,11])
  , ("M7#9#11",  [0,3,4,6,7,11])
  , ("m7add11",  [0,3,5,7,10])
  , ("m7add4",   [0,3,5,7,10])
    -- Ninths
  , ("9",        [0,2,4,7,10])
  , ("M9",       [0,2,4,7,11])
  , ("maj9",     [0,2,4,7,11])
  , ("m9",       [0,2,3,7,10])
  , ("mM9",      [0,2,3,7,11])
  , ("9#5",      [0,2,4,8,10])
  , ("9+",       [0,2,4,8,10])
  , ("M9#5",     [0,2,4,8,11])
  , ("9b5",      [0,2,4,6,10])
  , ("m9b5",     [0,2,3,6,10])
  , ("9#11",     [0,2,4,6,7,10])
  , ("M9#11",    [0,2,4,6,7,11])
  , ("maj9#11",  [0,2,4,6,7,11])
  , ("9b13",     [0,2,4,7,8,10])
  , ("9sus4",    [0,2,5,7,10])
  , ("9sus",     [0,2,5,7,10])
    -- Elevenths (dominant form omits the 3rd, convention 3)
  , ("11",       [0,2,5,7,10])
  , ("m11",      [0,2,3,5,7,10])
  , ("m11b5",    [0,2,3,5,6,10])
    -- Thirteenths (1-3-b7-9-13, no 5th or 11th, convention 2)
  , ("13",       [0,2,4,9,10])
  , ("13b9",     [0,1,4,9,10])
  , ("13#9",     [0,3,4,9,10])
  , ("13#11",    [0,2,4,6,9,10])
  , ("13b5",     [0,2,4,6,9,10])  -- b5 and #11 coincide at set level
  , ("13b9#11",  [0,1,4,6,9,10])
  , ("13sus4",   [0,2,5,9,10])    -- 4th replaces 3rd, 13th-form otherwise
  , ("13sus",    [0,2,5,9,10])
  , ("m13",      [0,2,3,9,10])
  , ("M13",      [0,2,4,9,11])
  , ("maj13",    [0,2,4,9,11])
  , ("M13#11",   [0,2,4,6,9,11])
  ]

-- | Corpus occurrence count per quality string (census of the 2,614-tune
-- Bunks corpus, 2026-08-25: 134,355 tokens). Drives the canonical-spelling
-- choice in 'canonicalQuality': where spelling variants share a tone set,
-- the spelling the corpus itself uses most is the one the graph shows.
qualityFrequency :: Map.Map T.Text Int
qualityFrequency = Map.fromList
  [ ("7", 34679)
  , ("m7", 28905)
  , ("", 17308)
  , ("M7", 13468)
  , ("m", 6266)
  , ("6", 4572)
  , ("m7b5", 3701)
  , ("7b9", 3060)
  , ("o7", 2527)
  , ("9", 2033)
  , ("m6", 1452)
  , ("7#9", 1197)
  , ("7alt", 1095)
  , ("7+", 1048)
  , ("13", 1010)
  , ("m9", 985)
  , ("7#11", 906)
  , ("7sus4", 822)
  , ("7#5", 758)
  , ("M7#11", 692)
  , ("69", 548)
  , ("7b5", 538)
  , ("+", 536)
  , ("m11", 417)
  , ("M9", 371)
  , ("mM7", 353)
  , ("9sus4", 351)
  , ("o", 335)
  , ("7#5#9", 278)
  , ("maj7", 239)
  , ("7#5b9", 233)
  , ("7sus", 228)
  , ("9#11", 219)
  , ("sus4", 158)
  , ("M7b5", 151)
  , ("13b9", 149)
  , ("dim", 141)
  , ("m69", 131)
  , ("add9", 120)
  , ("m+", 118)
  , ("M7#5", 117)
  , ("11", 117)
  , ("13#11", 111)
  , ("7b9#11", 99)
  , ("9#5", 85)
  , ("13sus4", 71)
  , ("M6", 54)
  , ("9b5", 50)
  , ("9+", 47)
  , ("7b9sus4", 47)
  , ("maj9", 46)
  , ("5", 43)
  , ("mMaj7", 42)
  , ("2", 41)
  , ("7sus4b9", 40)
  , ("9sus", 38)
  , ("7b5b9", 31)
  , ("madd9", 28)
  , ("13#9", 28)
  , ("sus", 24)
  , ("m13", 24)
  , ("7susb9", 24)
  , ("m#5", 23)
  , ("7b9b13", 23)
  , ("13b9#11", 22)
  , ("7#9#11", 20)
  , ("13sus", 20)
  , ("m9b5", 19)
  , ("M", 19)
  , ("sus24", 17)
  , ("7b5#9", 17)
  , ("m7b9", 15)
  , ("M69", 15)
  , ("h7", 15)
  , ("13b5", 15)
  , ("7b13", 14)
  , ("M9#5", 12)
  , ("maj13", 11)
  , ("m7add11", 11)
  , ("dim7", 11)
  , ("m7add4", 9)
  , ("sus2", 8)
  , ("oM7", 8)
  , ("m7#5", 8)
  , ("M13#11", 8)
  , ("7#9b13", 8)
  , ("mM9", 7)
  , ("mb6", 7)
  , ("M#5add9", 6)
  , ("addb9", 6)
  , ("7add6", 6)
  , ("+7", 6)
  , ("maj7#5", 5)
  , ("M9#11", 5)
  , ("M7add13", 5)
  , ("7b6", 5)
  , ("Mb5", 4)
  , ("mb5", 4)
  , ("maj9#11", 4)
  , ("M69#11", 4)
  , ("7#5b9#11", 4)
  , ("67", 4)
  , ("6#11", 4)
  , ("mM7b6", 3)
  , ("M7+", 3)
  , ("add9no3", 3)
  , ("4", 3)
  , ("+add9", 3)
  , ("madd4", 2)
  , ("M7#9b5", 2)
  , ("7add13", 2)
  , ("6b5", 2)
  , ("susb9", 1)
  , ("o7M7", 1)
  , ("mi", 1)
  , ("M7#9#11", 1)
  , ("M13", 1)
  , ("m11b5", 1)
  , ("9b13", 1)
  , ("7sus4b9b13", 1)
  , ("7b9b5", 1)
  , ("+add#9", 1)
  ]

-- | Canonical quality name per tone set: the reverse of
-- 'qualityIntervals', collapsing spelling variants (and pitch-class
-- coincidences such as @13b5@\/@13#11@) onto the corpus-preferred
-- spelling. 122 qualities reduce to 85 distinct sets.
canonicalQuality :: Map.Map [Int] T.Text
canonicalQuality =
  Map.fromListWith prefer [ (ivs, q) | (q, ivs) <- Map.toList qualityIntervals ]
  where
    freq q = Map.findWithDefault 0 q qualityFrequency
    prefer a b
      | freq a > freq b = a
      | freq b > freq a = b
      | otherwise       = min a b

-- | Name a zero-form pitch-class set (sorted, deduplicated, containing 0)
-- for the jazz graph: the pure set-to-name function that keeps node
-- identity and node label in lockstep.
--
-- A set matching a curated quality names directly (with the bare major
-- triad displayed as @maj@). Anything else is read as a slash shape: some
-- rotation of the set is a curated quality — guaranteed for every
-- corpus-derived set, whose rotation to the notated root is its quality
-- by construction — and the name is @quality\/bass-degree@
-- (@[0,5,9]@ is a major triad over its 5th: @maj\/5@). Candidate
-- rotations are ranked by corpus frequency of the quality, preferring the
-- reading whose bass is a chord member, then the smallest rotation.
-- 'Nothing' only for sets no rotation of which is a curated quality —
-- impossible for corpus data, reported (never invented) if it ever
-- happens downstream.
jazzFunctionality :: [Int] -> Maybe T.Text
jazzFunctionality = fmap fst . jazzFunctionalityR

-- | 'jazzFunctionality' with the rotation exposed: the second component
-- is the offset of the TRUE quality root above the anchor (0 when the
-- set names directly, the chosen rotation for slash shapes). The
-- structural authority for anchor-independent readings — e.g.
-- 'Harmonic.Interface.Tidal.Groove.fund' recovering the harmonic
-- fundamental of a slash bar.
jazzFunctionalityR :: [Int] -> Maybe (T.Text, Int)
jazzFunctionalityR raw
  | case set of { (s0 : _) -> s0 /= 0; [] -> True } = Nothing
  | otherwise = case Map.lookup set canonicalQuality of
      Just q  -> Just (display q, 0)
      Nothing -> case candidates of
        [] -> Nothing
        cs -> let (_, _, d, q) = maximumBy (comparing rank) cs
              in Just (display q <> "/" <> degreeLabel ((-d) `mod` 12), d)
  where
    set = sort (nub (map (`mod` 12) raw))
    freq q = Map.findWithDefault 0 q qualityFrequency
    rotate d xs = sort (nub (map (\x -> (x - d) `mod` 12) xs))
    candidates =
      [ (freq q, member, d, q)
      | d <- set, d /= 0
      , (member, rotated) <- [ (True,  rotate d set)
                             , (False, rotate d (filter (/= 0) set)) ]
      , Just q <- [Map.lookup rotated canonicalQuality]
      ]
    rank (f, member, d, _) = (f, member, negate d)
    display "" = "maj"
    display q  = q

-- | Exact inverse of the slash names 'jazzFunctionalityR' emits, over
-- the CLOSED corpus vocabulary: @quality\/degreeLabel@ where the quality
-- is a curated name and the label one of the eleven bass degrees.
-- Returns the quality and the true root's offset ABOVE the anchor (the
-- rotation the namer applied). Fails on anything else — classical names
-- that merely contain a slash (@sus2\/4no5@) fail the degree-label
-- membership, so a successful parse is proof the name came from the
-- jazz namer. The DISPLAY authority for chart-convention rendering:
-- parsing the stored name back can never disagree with the name the
-- walk stamped, where re-deriving from intervals could pick a
-- different rotation.
parseSlashName :: T.Text -> Maybe (T.Text, Int)
parseSlashName nm = case T.breakOn "/" nm of
  (q, rest) | Just lbl <- T.stripPrefix "/" rest
            , qualityOk q
            , Just off <- lookup lbl labelOffsets ->
      Just (q, (12 - off) `mod` 12)
  _ -> Nothing
  where
    qualityOk q = q == "maj" || Map.member q qualityIntervals
    labelOffsets = [ (degreeLabel n, n) | n <- [1 .. 11] ]

-- Bass-degree label for slash names: semitones above the upper root.
degreeLabel :: Int -> T.Text
degreeLabel n = case n of
  1 -> "b2"; 2 -> "2";  3 -> "b3"; 4 -> "3";  5 -> "4";  6 -> "b5"
  7 -> "5";  8 -> "b6"; 9 -> "6";  10 -> "b7"; 11 -> "7"; _ -> "1"

-- | Walking-bass understanding of a jazz tone set: what the SYMBOL means
-- to a bass player rather than what the working voicing contains. Corpus
-- sets omit degrees a bassist still needs (13th chords carry no 5th and
-- no 11th) and notate colours a bassist must not land on strong beats
-- (b9, #9, #11, b13). Root-relative intervals throughout.
data BassVocab = BassVocab
  { bvTarget  :: [Int] -- ^ The triadic core — root, third (or sus 4) and
                       --   THE fifth: the tones a line aims AT. Primary
                       --   strong-beat targets; the rest of the chord is
                       --   colour that guides passing motion and scale
                       --   choice rather than serving as a destination.
  , bvStrong  :: [Int] -- ^ Strong-beat anchors: the target triad plus the
                       --   seventh (or true 6th) — legal to land on, with
                       --   the target tones preferred.
  , bvPassing :: [Int] -- ^ Favourable passing tones — weak-beat preferred,
                       --   modest strong-beat access: the 9 where natural,
                       --   and the 11 over the minor family only (over a
                       --   major third it is the classic avoid note).
  , bvAvoid   :: [Int] -- ^ Notated tones never to land on strong beats
                       --   (colour alterations); still reachable as
                       --   weak-beat tension through the connector pools.
  , bvFifth   :: Int   -- ^ THE fifth of the quality: 7 natural (restored
                       --   for the 13th family), 6 where b5 defines it,
                       --   8 where #5 replaces it.
  } deriving (Show, Eq, Ord)

-- | Derive the walking-bass vocabulary from a zero-form tone set. The
-- rules codify the hand-inferred per-quality palettes of
-- notes\/walking_bass_theory.md (maj\/dom take the 9 and avoid the 11;
-- the minor family takes 9 and 11; defining tones reserved for strong
-- beats) plus the corpus table's own conventions (alterations displace
-- naturals; 13th chords omit 5th and 11th). Total over any zero-form
-- set, so hand-built and spliced material degrades gracefully.
--
-- Known limitation, deliberate: the same source supersedes per-QUALITY
-- palettes with per-FUNCTION ones (a iii chord takes the 11 but not the
-- 9, which is a b9 from the key; a IV takes the \#11). This signature
-- takes one bar's intervals and cannot see key or function, so it
-- implements the quality-level rule only. The walk does infer a key
-- centre, so the refinement is reachable later without changing callers.
bassVocabFor :: [Int] -> BassVocab
bassVocabFor raw = BassVocab
  { bvTarget  = target
  , bvStrong  = strong
  , bvPassing = passing
  , bvAvoid   = avoid
  , bvFifth   = fifth
  }
  where
    ivs = sort (nub (map (`mod` 12) raw))
    has = (`elem` ivs)
    -- The 13th family omits the 5th by convention, so a bare 6 there is
    -- the #11, not a b5. `13#11` and `13b5` share a tone set, so set-only
    -- inference must take the majority reading: #11-named 13ths outnumber
    -- `13b5` 141 to 15 in the corpus, and canonicalQuality already names
    -- that set `13#11`. The `has 4` term keeps o7M7's diminished fifth.
    thirteenish = has 9 && (has 10 || has 11) && has 4
    fifth
      | has 7                    = 7
      | has 6 && has 8           = if has 4 then 8 else 6
      | has 6 && not thirteenish = 6
      | has 8                    = 8
      | otherwise                = 7   -- restored: the symbol implies a natural 5th
    third
      | has 4     = [4]
      | has 3     = [3]
      | otherwise = []
    sus4    = has 5 && not (has 3) && not (has 4)
    seventh
      | has 10    = [10]
      | has 11    = [11]
      | otherwise = []
    -- a 9-semitone tone is the chord's 6th when no 7th is present
    -- (6-chords, dim7), and a 13 colour when one is.
    sixthStrong = has 9 && null seventh
    target = nub $ [0, fifth] ++ third ++ [5 | sus4]
    strong = nub $ target ++ seventh ++ [9 | sixthStrong]
    minorThird  = has 3 && not (has 4)
    -- the natural 9 is only vocabulary when the symbol carries no b9/#9.
    naturalNine = not (has 1) && not (has 3 && has 4)
    -- The 11 belongs to the MINOR family only: over a major third it is a
    -- tritone above it — the classic avoid note (walking_bass_theory.md
    -- ch. 10). The 13th family's table entries omit it for that reason,
    -- not for voicing economy, so it is never restored the way the 5th is.
    passing = filter (`notElem` strong) . filter (`notElem` avoid) . nub $
                 [2 | naturalNine]
              ++ [5 | minorThird]
    avoid = nub $ [1 | has 1]
               ++ [3 | has 3 && has 4]
               ++ [6 | has 6 && fifth /= 6]
               ++ [8 | has 8 && fifth /= 8]

-- | One corpus tune: identifying header fields and the bar-structured
-- token stream. @DBKeySig@ is deliberately not read — the graph is
-- zero-form, so key signatures carry no information it needs.
data JazzSong = JazzSong
  { jsTitle    :: T.Text          -- ^ Title header (filename stem when the header is corrupt).
  , jsComposer :: T.Text          -- ^ ComposedBy header, verbatim.
  , jsTimeSig  :: (Int, Int)      -- ^ TimeSig header as (numerator, denominator).
  , jsBars     :: [[JazzToken]]   -- ^ Parsed chord tokens, one list per bar.
  } deriving (Show, Eq)

-- | Parse one corpus song file. The first argument names the song in
-- refusals (and stands in for a corrupt @Title@ header); the second is
-- the file content. Header fields are matched by prefix so the two known
-- corrupt headers in the corpus degrade gracefully; the @Bars@ header is
-- ignored in favour of counting actual bar delimiters (it is wrong in
-- three corpus files). Any token refusal refuses the whole song, with
-- the song name attached.
parseSong :: T.Text -> T.Text -> Either JazzRefusal JazzSong
parseSong name content =
  let ls = T.lines content
      (headerLs, bodyLs) = splitAt 5 ls
      field key = [ T.strip v | l <- headerLs
                  , Just v <- [T.stripPrefix (key <> " =") (T.strip l)] ]
      title = case field "Title" of { (t:_) -> t; [] -> name }
      composer = case field "ComposedBy" of { (c:_) -> c; [] -> "" }
      timesig = case field "TimeSig" of
        (t:_) | [n, d] <- T.words t
              , Just n' <- readInt n, Just d' <- readInt d -> Just (n', d')
        _ -> Nothing
      bars = [ T.words b
             | b <- concatMap (T.splitOn "|") bodyLs
             , not (T.null (T.strip b)) ]
      parseBar b = traverse (\t -> either (refuseIn name) Right (parseToken t)) b
  in case timesig of
       Nothing -> Left (JazzRefusal name "missing or malformed TimeSig header")
       Just ts -> do
         parsed <- traverse parseBar bars
         Right (JazzSong title composer ts parsed)
  where
    readInt t = case TR.decimal t of { Right (n, r) | T.null r -> Just n; _ -> Nothing }
    refuseIn n (JazzRefusal i r) = Left (JazzRefusal (n <> ": " <> i) r)

-- | Beat slots per chord for one bar: how many one-beat cadence steps
-- each chord occupies. A bar's beats (the time-signature numerator;
-- numerator \`div\` 3 for compound x\/8 meters) are shared out with any
-- remainder going to the earliest chords — so 3 chords in 4\/4 hold
-- [2,1,1] beats. More chords than beats means sub-beat harmony: every
-- chord gets one slot. A chord holding n slots contributes n-1
-- self-cadences, mirroring the classical graph's sustained-harmony
-- self-edges.
beatSlots :: (Int, Int) -> Int -> [Int]
beatSlots (num, den) nChords
  | nChords <= 0      = []
  | nChords >= beats  = replicate nChords 1
  | otherwise         = zipWith (+) (replicate nChords q)
                                    (replicate r 1 ++ repeat 0)
  where
    beats = if den == 8 then max 1 (num `div` 3) else num
    (q, r) = beats `divMod` nChords

-- | Expand a song to its beat-level chord stream. Each chord repeats
-- once per beat slot; @NC@ beats vanish entirely, so the chords either
-- side of a silence become adjacent — the notated harmonic motion
-- bridges the gap, and silence never becomes a graph node.
beatStream :: JazzSong -> [JazzChord]
beatStream song =
  [ c
  | bar <- jsBars song
  , (tok, slots) <- zip bar (beatSlots (jsTimeSig song) (length bar))
  , Sounding c <- [tok]
  , _ <- [1 .. slots]
  ]

-- | Anchor-relative zero form of a parsed chord: intervals above the
-- sounding fundamental, sorted, starting at 0. This is the jazz graph's
-- node identity (paired with the arrival movement).
jazzZeroForm :: JazzChord -> [Int]
jazzZeroForm c = sort (nub (map (\p -> unPitchClass (p - jcAnchor c)) (jcTones c)))

-- | One jazz cadence: how the anchor moved and what sounds above it.
-- The jazz-graph analogue of the classical 'Harmonic.Rules.Types.Harmony.Cadence'.
data JazzCadence = JazzCadence
  { jzMovement :: Movement  -- ^ Anchor motion arriving at this chord.
  , jzSet      :: [Int]     -- ^ Anchor-relative zero-form tone set.
  , jzName     :: T.Text    -- ^ Canonical functionality ('jazzFunctionality').
  } deriving (Show, Eq, Ord)

-- | Graph node key for a jazz cadence, matching the classical key shape:
-- @( \<movement\> -> \<functionality\> )@.
jazzShow :: JazzCadence -> T.Text
jazzShow (JazzCadence mv _ nm) =
  "( " <> T.pack (show mv) <> " -> " <> nm <> " )"

-- | The cadence chain of one song: one cadence per beat-to-beat step
-- (n beats yield n-1 cadences; the opening beat has no arrival). A beat
-- sustaining the same harmony over the same anchor yields a pedal
-- self-cadence. Chains never cross song boundaries — the caller builds
-- edges from consecutive pairs within one song's chain only.
songCadences :: JazzSong -> [JazzCadence]
songCadences song =
  [ JazzCadence (toMovement (jcAnchor prev) (jcAnchor cur)) zf name
  | (prev, cur) <- zip stream (drop 1 stream)
  , let zf = jazzZeroForm cur
  , Just name <- [jazzFunctionality zf]
  ]
  where stream = beatStream song

-- | Composer name to weight key: lower-cased, alphanumerics only, with
-- the empty result mapped to @unknown@. The ONE normalisation applied at
-- every write and lookup, so a composer can never split across variant
-- spellings of the same name.
normalizeComposer :: T.Text -> T.Text
normalizeComposer t =
  let slug = T.filter (\c -> c `elem` (['a'..'z'] ++ ['0'..'9'])) (T.toLower t)
  in if T.null slug then "unknown" else slug

-- | Aggregate the whole corpus into weighted transition edges: every
-- consecutive cadence pair within one song contributes 1.0 to that
-- song's composer on the @from -> to@ edge. Chains never cross songs.
-- The result feeds 'Harmonic.Rules.Import.Graph.writeChangeEdges'.
buildChangeEdges :: [JazzSong]
                 -> [((JazzCadence, JazzCadence), Map.Map T.Text Double)]
buildChangeEdges songs = Map.toList (Map.fromListWith (Map.unionWith (+))
  [ ((a, b), Map.singleton (normalizeComposer (jsComposer song)) 1.0)
  | song <- songs
  , let chain = songCadences song
  , (a, b) <- zip chain (drop 1 chain)
  ])
