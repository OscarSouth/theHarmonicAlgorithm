{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Rules.Constraints.Filter
-- Description : Pitch class set filtering using legacy notation
--
-- This module implements the parsing and filtering system from the legacy
-- Overtone.hs, providing compatibility with the original command line app
-- and TidalCycles integration.
--
-- == Academic Lineage
--
-- /The Harmonic Algorithm/ (South, 2016), Section One: the three tuning
-- systems EAeGB (Electric Contrabass Cittern), EAeGC (with B\/C re-tuner),
-- and EADG (standard bass). The overtone series mapping
-- @[0, 7, 4, 10, 2]@ (root, P5, M3, m7, M2) is the equal-temperament
-- approximation of the first five unique partials.
--
-- == Filter Notation
--
-- === Overtones/Pitch Set Filter
-- Limits harmonic choices to pitches within a specified set.
--
-- * Fundamental pitches (derives overtones): @"E A D G"@ (bass tuning)
-- * Individual pitches with prime: @"E'"@ @"A'"@ @"A#'"@
-- * Combined: @"G E' A' A#'"@ (G overtones + E, A, A# pitches)
-- * Wildcard: @"*"@ (all pitches)
--
-- === Key Filter
-- Removes pitches not in the specified key.
-- Note names yield a /single pitch class/; numbered key sigs yield a /major scale/.
--
-- * Note name: @"C"@ = only pitch C, @"Bb"@ = only pitch Bb (PC 10)
-- * Numbered key sig: @"0#"@ = C major, @"1#"@ = G major, @"2b"@ = Bb major, @"4b"@
-- * Wildcard: @"*"@ (no key filtering)
--
-- === Root Notes Filter
-- Limits bass notes to specified pitch classes or key.
-- Uses the same unified rules as the key filter.
--
-- * Note name: @"E"@ @"F#"@ @"Bb"@ → individual pitches @[4, 6, 10]@
-- * Numbered key sig: @"1b"@ = F major roots, @"2#"@ = D major roots
-- * Wildcard: @"*"@ (all roots)
--
-- == Pitch Removal with '-' Operator
--
-- All parsing functions support pitch removal using the @-@ prefix:
--
-- * @"C E -E'"@ → (C overtones ∪ E overtones) \\ {E pitch}
-- * @"1b 2# -G"@ → (F major ∪ D major) \\ {G}
-- * @"* -C' -F#'"@ → All pitches except C and F#
--
-- === Context-Specific Behavior
--
-- * __Overtones__: note name generates overtone series; prime notation gives single pitch
-- * __Key__: note name = single pitch; numbered sig = major key scale
-- * __Roots__: same as Key (unified rules)
--
-- === Order of Operations
--
-- 1. Union all positive tokens
-- 2. Union all negative tokens
-- 3. Subtract: positive \\ negative
--
-- === Edge Cases
--
-- * @"*"@ → all pitches (wildcard shortcut)
-- * @"* -C'"@ → all except C
-- * @"-*"@ → empty set (no includes)
-- * @"-C -D"@ → empty set (no includes)
-- * @"C -C"@ → empty set (self-cancellation)

module Harmonic.Rules.Constraints.Filter
  ( -- * Parsing Functions (Text versions)
    parseOvertones
  , parseKey
  , parseFunds
  , parseTuning
  , resolveRoots

    -- * Parsing Functions (String versions for Tidal)
  , overtones
  , key
  , funds
  , tuning
  , wildcard

    -- * High-level API
  , filterPitchSet
  , filterByKey
  , filterRoots

    -- * Filtering Predicates
  , isWildcard
  , matchesPitchSet
  , matchesKey
  , matchesRoots

    -- * Bass Direction
  , BassDirection(..)
  , BassDirectionSpec(..)
  , BDKind(..)
  , BDSelector(..)
  , parseBassDirectionSpec
  , stripDirectionToken
  , closestAbove
  , closestBelow
  , nthAbove
  , nthBelow

    -- * Internal (for testing)
  , parseOvertones'
  , parseKey'
  , parseFunds'
  , parseTuning'
  , partitionTokens
  , keyToPitchClasses
  , noteNameToPitchClass

    -- * Overtone Annotation Support
  , parseTuningNamed
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (nub, sort, sortBy, partition, (\\))
import           Data.Maybe (mapMaybe)
import qualified Data.IntSet as IntSet

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type PitchClass = Int  -- 0-11

-------------------------------------------------------------------------------
-- Wildcard Handling
-------------------------------------------------------------------------------

-- |Check if a filter string is a wildcard (matches everything)
isWildcard :: Text -> Bool
isWildcard t = t' `elem` ["*", "all", "chr"]
  where t' = T.toLower $ T.strip t

-- |The chromatic set (all 12 pitch classes)
chromaticSet :: [PitchClass]
chromaticSet = [0..11]

-------------------------------------------------------------------------------
-- Overtone Generation
-------------------------------------------------------------------------------

-- |Generate first n overtones from a fundamental pitch class.
-- Uses the overtone series: root, P5, M3, m7, M2 (first 5 partials mapped to octave)
-- 
-- For example, C (0) generates: [0, 7, 4, 10, 2] = C, G, E, Bb, D
-- Default n=4 gives root, P5, M3, m7 (dominant 7th chord tones)
overtoneSeriesFrom :: Int -> PitchClass -> [PitchClass]
overtoneSeriesFrom n root = take n $ map ((`mod` 12) . (+ root)) [0, 7, 4, 10, 2]

-- |Generate pitches from a major key (Ionian mode)
-- Given a position on the circle of fifths (0 = C, 1 = G, -1 = F, etc.)
keyPitches :: Int -> [PitchClass]
keyPitches fifths = sort $ nub $ map ((`mod` 12) . (+ fifths * 7)) [0, 2, 4, 5, 7, 9, 11]

-------------------------------------------------------------------------------
-- Parsing: Tuning/Overtones
-------------------------------------------------------------------------------

-- |Parse a tuning string into pitch class set (overtones of fundamentals).
-- 
-- Examples:
--   * @"E A D G"@ → overtones of E, A, D, G (bass tuning)
--   * @"C"@ → overtones of C
--   * @"*"@ → all pitch classes
parseTuning' :: Int -> Text -> [PitchClass]
parseTuning' n input
  | isWildcard input = chromaticSet
  | otherwise = unique $ concatMap (parseToken n False) tokens
  where
    tokens = T.words $ T.toLower input
    
-- |Parse a tuning string preserving string names for overtone annotation.
-- Case is preserved for string identification (uppercase = lower octave,
-- lowercase = higher octave per thesis convention).
--
-- Examples:
--   @"E A D G"@ → @[("E",4), ("A",9), ("D",2), ("G",7)]@
--   @"E A e G B"@ → @[("E",4), ("A",9), ("e",4), ("G",7), ("B",11)]@
--   @"*"@ → @[]@ (wildcard has no named strings)
parseTuningNamed :: Text -> [(String, Int)]
parseTuningNamed input
  | isWildcard input = []
  | otherwise = mapMaybe parseNamedToken (T.words input)
  where
    parseNamedToken tok =
      case noteNameToPitchClass (T.toLower tok) of
        Just pc -> Just (T.unpack tok, pc)
        Nothing -> Nothing

-- |Parse a single token in tuning context
parseToken :: Int -> Bool -> Text -> [PitchClass]
parseToken n isPrime token
  | T.null token = []
  | "'" `T.isSuffixOf` token = 
      -- Prime notation: single pitch class only (legacy behavior)
      case noteNameToPitchClass (T.init token) of
        Just pc -> [pc]
        Nothing -> []
  | otherwise = case noteNameToPitchClass token of
      Just pc -> if isPrime then [pc] else overtoneSeriesFrom n pc
      Nothing -> []  -- Invalid token ignored

-- |Map note names to pitch classes
-- Handles: c, c#, db, d, d#, eb, e, f, f#, gb, g, g#, ab, a, a#, bb, b
-- Also: b# = c, cb = b, etc.
noteNameToPitchClass :: Text -> Maybe PitchClass
noteNameToPitchClass t = case T.toLower t of
  "c"   -> Just 0;  "b#"  -> Just 0;  "dbb" -> Just 0
  "c#"  -> Just 1;  "db"  -> Just 1;  "b##" -> Just 1
  "d"   -> Just 2;  "c##" -> Just 2;  "ebb" -> Just 2
  "d#"  -> Just 3;  "eb"  -> Just 3;  "fbb" -> Just 3
  "e"   -> Just 4;  "d##" -> Just 4;  "fb"  -> Just 4
  "f"   -> Just 5;  "e#"  -> Just 5;  "gbb" -> Just 5
  "f#"  -> Just 6;  "gb"  -> Just 6;  "e##" -> Just 6
  "g"   -> Just 7;  "f##" -> Just 7;  "abb" -> Just 7
  "g#"  -> Just 8;  "ab"  -> Just 8
  "a"   -> Just 9;  "g##" -> Just 9;  "bbb" -> Just 9
  "a#"  -> Just 10; "bb"  -> Just 10; "cbb" -> Just 10
  "b"   -> Just 11; "a##" -> Just 11; "cb"  -> Just 11
  _     -> Nothing

-------------------------------------------------------------------------------
-- Parsing: Key Signatures
-------------------------------------------------------------------------------

-- |Parse a key filter string.
--
-- Formats:
--   * Note name: @"C"@, @"Bb"@, @"F#"@ → single pitch class
--   * Numbered key sig: @"0#"@ = C major, @"1#"@ = G major, @"2b"@ = Bb major
--   * Wildcard: @"*"@
--   * Removal: @"1b -G"@ (F major minus G), @"* -C'"@ (all minus C)
parseKey' :: Int -> Text -> [PitchClass]
parseKey' _ input
  | isWildcard input = chromaticSet
  | otherwise =
      let (includes, excludes) = partitionTokens input
          includePcs = unique $ concatMap parseKeyToken includes
          excludePcs = unique $ concatMap parseKeyToken excludes
      in includePcs \\ excludePcs

-- |Convert a key specification to pitch classes
keyToPitchClasses :: Text -> [PitchClass]
keyToPitchClasses input =
  let t = T.toLower $ T.strip input
  in case parseKeySignature t of
       Just fifths -> keyPitches fifths
       Nothing -> case parseNamedKey t of
         Just fifths -> keyPitches fifths
         Nothing -> chromaticSet  -- Fallback to chromatic if unparseable

-- |Unified token parser for both key and roots contexts.
-- Note names always yield a single pitch class; numbered key signatures yield scales.
-- Prime notation is not supported in this context (returns empty).
parseUnifiedToken :: Text -> [PitchClass]
parseUnifiedToken token
  | T.null token = []
  | token == "*" || token == "all" || token == "chr" = chromaticSet
  | Just pc <- noteNameToPitchClass token = [pc]
  | Just fifths <- parseKeySignature token = keyPitches fifths
  | otherwise = []

-- |Parse a single token in key context.
-- Note name: single pitch class. Numbered key sig: major key scale.
parseKeyToken :: Text -> [PitchClass]
parseKeyToken = parseUnifiedToken

-- |Parse key signature notation: numbered form only: "1#", "2b", "4b", "0#", etc.
parseKeySignature :: Text -> Maybe Int
parseKeySignature = parseNumberedKeySig

-- |Parse numbered key signature: "1#", "2b", "0#", "11b", etc.
parseNumberedKeySig :: Text -> Maybe Int
parseNumberedKeySig t
  | "#" `T.isSuffixOf` t = 
      let numPart = T.init t
      in case reads (T.unpack numPart) :: [(Int, String)] of
           [(n, "")] -> Just n
           _ -> Nothing
  | "b" `T.isSuffixOf` t =
      let numPart = T.init t  
      in case reads (T.unpack numPart) :: [(Int, String)] of
           [(n, "")] -> Just (negate n)
           _ -> Nothing
  | otherwise = Nothing

-- |Parse named key: "C", "G", "F#", "Bb", "Am", "F#m", etc.
-- Returns position on circle of fifths (C=0, G=1, D=2, F=-1, Bb=-2)
parseNamedKey :: Text -> Maybe Int
parseNamedKey t =
  let t' = T.toLower t
      isMinor = "m" `T.isSuffixOf` t'
      rootPart = if isMinor then T.init t' else t'
  in case rootPart of
       "c"  -> Just 0
       "g"  -> Just 1
       "d"  -> Just 2
       "a"  -> Just 3
       "e"  -> Just 4
       "b"  -> Just 5
       "f#" -> Just 6
       "gb" -> Just (-6)
       "db" -> Just (-5)
       "ab" -> Just (-4)
       "eb" -> Just (-3)
       "bb" -> Just (-2)
       "f"  -> Just (-1)
       -- Also handle c#, etc.
       "c#" -> Just 7
       _    -> Nothing

-------------------------------------------------------------------------------
-- Parsing: Fundamentals/Roots
-------------------------------------------------------------------------------

-- |Parse fundamentals (root notes) filter.
--
-- Formats:
--   * Note names: @"E F# G"@ → individual pitches [4,6,7]
--   * Numbered key sig: @"1b"@ = F major roots, @"2#"@ = D major roots
--   * Wildcard: @"*"@
--   * Removal: @"C G -G"@ (C and G minus G), @"* -E -A"@ (all except E,A)
parseFunds' :: Int -> Text -> [PitchClass]
parseFunds' _ input
  | isWildcard input = chromaticSet
  | otherwise =
      let (includes, excludes) = partitionTokens input
          includePcs = unique $ concatMap parseFundsToken includes
          excludePcs = unique $ concatMap parseFundsToken excludes
      in includePcs \\ excludePcs

-- |Parse a single token in fundamentals/roots context.
-- Same unified rules as key context: note name = single pitch, numbered sig = scale.
parseFundsToken :: Text -> [PitchClass]
parseFundsToken = parseUnifiedToken

-- |Resolve roots with special options "key" and "tones".
-- 
-- This function allows root selection to be derived from other filter values:
--   * @"key"@: Use the same pitch classes as the key filter produces
--   * @"tones"@: Use the key-filtered overtones (effective upper structure)
--   * Other values: Parsed via parseFunds as normal
--
-- Arguments:
--   * overtoneFilter: The overtones/tuning filter string
--   * keyFilter: The key filter string  
--   * rootsFilter: The roots filter string (may be "key" or "tones")
--
-- Examples:
--   resolveRoots "E A D G" "#" "key"   -> G major scale degrees [0,2,4,6,7,9,11]
--   resolveRoots "E A D G" "#" "tones" -> key-filtered overtones from E A D G
--   resolveRoots "E A D G" "#" "C G"   -> [0, 7]
resolveRoots :: Text -> Text -> Text -> [PitchClass]
resolveRoots overtoneFilter keyFilter rootsFilter
  | T.toLower (T.strip rootsFilter) == "key" = parseKey keyFilter
  | T.toLower (T.strip rootsFilter) == "tones" = 
      let overtones = parseOvertones' 4 overtoneFilter
          keyPcs = parseKey keyFilter
      in if isWildcard keyFilter
         then overtones
         else Prelude.filter (`elem` keyPcs) overtones
  | otherwise = parseFunds' 4 rootsFilter

-------------------------------------------------------------------------------
-- Generalized Parsing (combines all notations)
-------------------------------------------------------------------------------

-- |Parse an overtones filter with full notation support.
-- This is the most general parser, combining tuning notation with individual pitches.
-- Supports removal with '-' prefix: "C E -E'" removes E pitch from (C ∪ E) overtones
parseOvertones' :: Int -> Text -> [PitchClass]
parseOvertones' n input
  | isWildcard input = chromaticSet
  | otherwise =
      let (includes, excludes) = partitionTokens input
          includePcs = unique $ concatMap (parseGeneralToken n) includes
          excludePcs = unique $ concatMap (parseGeneralToken n) excludes
      in includePcs \\ excludePcs

-- |Parse a general token (could be note name, prime notation, or key signature)
parseGeneralToken :: Int -> Text -> [PitchClass]
parseGeneralToken n token
  | T.null token = []
  | token == "*" || token == "all" || token == "chr" = chromaticSet
  | "'" `T.isSuffixOf` token =
      -- Prime notation: single pitch class only (legacy behavior)
      case noteNameToPitchClass (T.init token) of
        Just pc -> [pc]
        Nothing -> []
  | otherwise =
      -- Try as note name (generates overtones)
      case noteNameToPitchClass token of
        Just pc -> overtoneSeriesFrom n pc
        Nothing -> 
          -- Try as key signature
          case parseKeySignature token of
            Just fifths -> keyPitches fifths
            Nothing -> []

-------------------------------------------------------------------------------
-- Shortcut Functions (n=3 default, matching legacy)
-------------------------------------------------------------------------------

-- |Parse tuning with 4 overtones (default: root, P5, M3, m7)
parseTuning :: Text -> [PitchClass]
parseTuning = parseTuning' 4

-- |Parse key (overtone count not used for keys)
parseKey :: Text -> [PitchClass]
parseKey = parseKey' 4

-- |Parse fundamentals with 4 overtones (default)
parseFunds :: Text -> [PitchClass]
parseFunds = parseFunds' 4

-- |Parse overtones with 4 overtones (default: root, P5, M3, m7)
parseOvertones :: Text -> [PitchClass]
parseOvertones = parseOvertones' 4

-------------------------------------------------------------------------------
-- String-Friendly Versions (for Tidal live coding)
-- These accept String instead of Text for seamless REPL use
-------------------------------------------------------------------------------

-- |Parse overtones from a String (Tidal-friendly)
-- Example: overtones "E A D G" -> bass tuning overtones
overtones :: String -> [PitchClass]
overtones = parseOvertones . T.pack

-- |Parse key from a String (Tidal-friendly)
-- Example: key "#" -> G major, key "bb" -> Bb major
key :: String -> [PitchClass]
key = parseKey . T.pack

-- |Parse fundamentals from a String (Tidal-friendly)
-- Example: funds "E F# G" -> [4, 6, 7]
funds :: String -> [PitchClass]
funds = parseFunds . T.pack

-- |Parse tuning from a String (Tidal-friendly)
tuning :: String -> [PitchClass]
tuning = parseTuning . T.pack

-- |Check if a string is a wildcard (Tidal-friendly)
wildcard :: String -> Bool
wildcard = isWildcard . T.pack

-------------------------------------------------------------------------------
-- High-Level Filtering API
-------------------------------------------------------------------------------

-- |Filter a pitch class set by overtones/tuning filter
filterPitchSet :: Text -> [PitchClass] -> [PitchClass]
filterPitchSet filterStr pcs
  | isWildcard filterStr = pcs
  | otherwise = 
      let allowed = parseOvertones filterStr
      in Prelude.filter (`elem` allowed) pcs

-- |Filter a pitch class set by key
filterByKey :: Text -> [PitchClass] -> [PitchClass]
filterByKey keyFilter pcs
  | isWildcard keyFilter = pcs
  | otherwise =
      let keyPcs = parseKey keyFilter
      in Prelude.filter (`elem` keyPcs) pcs

-- |Filter root candidates by roots filter
filterRoots :: Text -> [PitchClass] -> [PitchClass]
filterRoots rootsFilter pcs
  | isWildcard rootsFilter = pcs
  | otherwise =
      let allowed = parseFunds rootsFilter
      in Prelude.filter (`elem` allowed) pcs

-------------------------------------------------------------------------------
-- Matching Predicates (for Builder.hs integration)
-------------------------------------------------------------------------------

-- |Check if a pitch class set matches the overtones filter
matchesPitchSet :: Text -> [PitchClass] -> Bool
matchesPitchSet filterStr pcs
  | isWildcard filterStr = True
  | otherwise =
      let allowed = parseOvertones filterStr
      in all (`elem` allowed) pcs

-- |Check if a pitch class set matches the key filter
matchesKey :: Text -> [PitchClass] -> Bool
matchesKey keyFilter pcs
  | isWildcard keyFilter = True
  | otherwise =
      let keyPcs = parseKey keyFilter
      in all (`elem` keyPcs) pcs

-- |Check if a root pitch class matches the roots filter
matchesRoots :: Text -> PitchClass -> Bool
matchesRoots rootsFilter rootPc
  | isWildcard rootsFilter = True
  | otherwise =
      let allowed = parseFunds rootsFilter
      in rootPc `elem` allowed

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- |Remove duplicates and sort
unique :: [PitchClass] -> [PitchClass]
unique = sort . nub

-- |Partition input tokens into positive and negative (those prefixed with '-')
-- Returns (positive tokens, negative tokens with '-' prefix stripped)
partitionTokens :: Text -> ([Text], [Text])
partitionTokens input =
  let tokens = T.words $ T.toLower input
      (negTokens, posTokens) = partition (T.isPrefixOf "-") tokens
      negTokens' = map (T.drop 1) negTokens
  in (posTokens, negTokens')

-------------------------------------------------------------------------------
-- Bass Direction
-------------------------------------------------------------------------------

-- |Concrete direction action resolved for a single generation step.
-- When active, the bass/root at the next step is forced to the Nth note
-- above (Rise) or below (Fall) in the allowed set, with mod-12 wrapping.
-- Step size 1 = closest note, 2 = skip one, etc.
data BassDirection = Rise !Int | Fall !Int
  deriving (Show, Eq)

-- |Whether a parsed direction rises or falls.
data BDKind = RiseK | FallK
  deriving (Show, Eq)

-- |How to pick a step size from 'bdsChoices' at each generation step.
data BDSelector
  = BDFixed       -- ^ single value (bare @rise@, @rise2@, or @rise\<n\>@)
  | BDRotate      -- ^ cycle choices by step index (space-delimited @\<…\>@)
  | BDRandomPick  -- ^ uniform random per step (comma-delimited @\<…\>@)
  deriving (Show, Eq)

-- |Parsed specification for a rise/fall direction token. Resolved per step
-- at generation time to a concrete 'BassDirection' (or 'Nothing' when the
-- optional @?@ flag causes the direction to be skipped for that step).
data BassDirectionSpec = BassDirectionSpec
  { bdsKind     :: !BDKind
  , bdsChoices  :: ![Int]       -- ^ non-empty, each in @1..6@
  , bdsSelector :: !BDSelector
  , bdsOptional :: !Bool        -- ^ @True@ when the token ended in @?@
  } deriving (Show, Eq)

-- |Split input on whitespace, but keep substrings inside matching
-- angle brackets as a single token. Used by the bass-direction parser
-- so that @rise\<1 2\>@ survives tokenization.
splitBracketed :: Text -> [Text]
splitBracketed = go (0 :: Int) "" [] . T.unpack
  where
    flushTok acc cur = if null cur then acc else T.pack (reverse cur) : acc
    go _ cur acc [] = reverse (flushTok acc cur)
    go 0 cur acc (c:cs)
      | Char.isSpace c = go 0 "" (flushTok acc cur) cs
      | c == '<'       = go 1 (c : cur) acc cs
      | otherwise      = go 0 (c : cur) acc cs
    go d cur acc (c:cs)
      | c == '<' = go (d + 1) (c : cur) acc cs
      | c == '>' = go (d - 1) (c : cur) acc cs
      | otherwise = go d (c : cur) acc cs

-- |Try to parse a single token (already bracket-aware) as a direction spec.
-- Returns 'Nothing' for non-direction tokens and for malformed directions.
parseDirectionToken :: Text -> Maybe BassDirectionSpec
parseDirectionToken tok =
  let low = T.toLower tok
      (core, optional) =
        if T.isSuffixOf "?" low
          then (T.init low, True)
          else (low, False)
      validStep n = n >= 1 && n <= 6
      parseDigit t = case T.unpack t of
        [c] | Char.isDigit c ->
          let n = Char.digitToInt c
          in if validStep n then Just n else Nothing
        _ -> Nothing
      stripKind t
        | Just r <- T.stripPrefix "rise" t = Just (RiseK, r)
        | Just r <- T.stripPrefix "fall" t = Just (FallK, r)
        | otherwise                        = Nothing
  in do
       (kind, rest) <- stripKind core
       (choices, selector) <-
         if T.null rest
           then Just ([1], BDFixed)
           else if T.isPrefixOf "<" rest && T.isSuffixOf ">" rest
             then
               let inner      = T.drop 1 (T.init rest)
                   hasComma   = T.any (== ',') inner
                   pieces     =
                     if hasComma
                       then concatMap T.words (T.splitOn "," inner)
                       else T.words inner
                   parsed     = mapM parseDigit pieces
               in case parsed of
                    Just ns@(_:_) ->
                      Just (ns, if hasComma then BDRandomPick else BDRotate)
                    _ -> Nothing
             else
               case parseDigit rest of
                 Just n  -> Just ([n], BDFixed)
                 Nothing -> Nothing
       pure (BassDirectionSpec kind choices selector optional)

-- |Extract a bass-direction spec from a roots filter string.
-- Returns the first token that parses as a direction; 'Nothing' otherwise.
--
-- Examples:
--   parseBassDirectionSpec "* fall"            -- Just (fixed Fall 1)
--   parseBassDirectionSpec "* rise1"           -- Just (fixed Rise 1) -- alias for "rise"
--   parseBassDirectionSpec "* rise3"           -- Just (fixed Rise 3)
--   parseBassDirectionSpec "* fall2?"          -- Just (fixed Fall 2, optional)
--   parseBassDirectionSpec "* fall\<3 2 1\>"     -- Just (rotate Fall [3,2,1])
--   parseBassDirectionSpec "* rise\<1,2\>"       -- Just (random Rise [1,2])
--   parseBassDirectionSpec "* rise\<1 2,3\>?"    -- Just (random Rise [1,2,3], optional)
--   parseBassDirectionSpec "C E G"             -- Nothing
parseBassDirectionSpec :: Text -> Maybe BassDirectionSpec
parseBassDirectionSpec input =
  case mapMaybe parseDirectionToken (splitBracketed input) of
    (s:_) -> Just s
    []    -> Nothing

-- |Strip the direction token from a roots filter string,
-- leaving only the pitch set specification for normal parsing.
--
-- Examples:
--   stripDirectionToken "* fall"               == "*"
--   stripDirectionToken "0# rise1"              == "0#"
--   stripDirectionToken "0# rise?"              == "0#"
--   stripDirectionToken "C E G fall\<2,3\>"      == "C E G"
--   stripDirectionToken "C E G"                 == "C E G"
stripDirectionToken :: Text -> Text
stripDirectionToken input =
  let tokens    = splitBracketed input
      isDir t   = case parseDirectionToken t of
                    Just _  -> True
                    Nothing -> False
      filtered  = filter (not . isDir) tokens
  in T.unwords filtered

-- |Find the Nth pitch class ABOVE the current one in the allowed set,
-- sorted by ascending circular distance. Step 1 = closest, 2 = skip one, etc.
-- When N exceeds the set size, wraps around using modular indexing.
-- If the set has only the current note, returns it (pedal).
--
-- Examples (with C major = {0,2,4,5,7,9,11}):
--   nthAbove 1 7 cMaj == 9   (G → A, closest)
--   nthAbove 3 0 cMaj == 5   (C → F, 3rd above)
--   nthAbove 1 11 cMaj == 0  (B → C, wraps mod-12)
--   nthAbove 1 0 {0} == 0    (pedal)
nthAbove :: Int -> Int -> IntSet.IntSet -> Int
nthAbove n current allowed =
  let others = IntSet.delete current allowed
  in if IntSet.null others
     then current  -- pedal: single-element set
     else let sorted = sortBy (\a b -> compare ((a - current) `mod` 12) ((b - current) `mod` 12))
                              (IntSet.toList others)
          in sorted !! ((n - 1) `mod` length sorted)

-- |Find the Nth pitch class BELOW the current one in the allowed set,
-- sorted by ascending circular distance downward. Step 1 = closest, etc.
-- When N exceeds the set size, wraps around using modular indexing.
-- If the set has only the current note, returns it (pedal).
--
-- Examples (with C major = {0,2,4,5,7,9,11}):
--   nthBelow 1 7 cMaj == 5   (G → F, closest)
--   nthBelow 3 7 cMaj == 2   (G → D, 3rd below)
--   nthBelow 1 0 cMaj == 11  (C → B, wraps mod-12)
--   nthBelow 1 5 {5} == 5    (pedal)
nthBelow :: Int -> Int -> IntSet.IntSet -> Int
nthBelow n current allowed =
  let others = IntSet.delete current allowed
  in if IntSet.null others
     then current  -- pedal: single-element set
     else let sorted = sortBy (\a b -> compare ((current - a) `mod` 12) ((current - b) `mod` 12))
                              (IntSet.toList others)
          in sorted !! ((n - 1) `mod` length sorted)

-- |Find the closest pitch class above. Equivalent to @nthAbove 1@.
closestAbove :: Int -> IntSet.IntSet -> Int
closestAbove = nthAbove 1

-- |Find the closest pitch class below. Equivalent to @nthBelow 1@.
closestBelow :: Int -> IntSet.IntSet -> Int
closestBelow = nthBelow 1
