{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Core.Filter
-- Description : Pitch class set filtering using legacy notation
-- 
-- This module implements the parsing and filtering system from the legacy
-- Overtone.hs, providing compatibility with the original command line app
-- and TidalCycles integration.
--
-- == Filter Notation (from original README)
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
--
-- * Key signature: @"bb"@, @"###"@, @"4b"@, @"0#"@
-- * Named key: @"C"@, @"F#m"@, @"Bb"@
-- * Wildcard: @"*"@ (no key filtering)
--
-- === Root Notes Filter
-- Limits bass notes to specified pitch classes or key.
--
-- * Pitches: @"E F# G"@
-- * Key signature: @"1b"@, @"#"@
-- * Wildcard: @"*"@ (all roots)
--
-- == Pitch Removal with '-' Operator
--
-- All parsing functions support pitch removal using the @-@ prefix:
--
-- * @"C E -E'"@ → (C overtones ∪ E overtones) \\ {E pitch}
-- * @"1b 2# -G"@ → (F major ∪ D major) \\ G major scale
-- * @"* -C' -F#'"@ → All pitches except C and F#
--
-- === Context-Specific Behavior
--
-- The @-@ operator treats tokens identically to positive tokens but subtracts:
--
-- * __Overtones__: @-G@ removes G overtones, @-G'@ removes G pitch
-- * __Key__: @-G@ removes G major scale, @-G'@ removes G pitch
-- * __Roots__: @-G@ removes G pitch, @-G'@ removes G major scale (INVERTED prime notation)
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
  
    -- * Internal (for testing)
  , parseOvertones'
  , parseKey'
  , parseFunds'
  , parseTuning'
  , partitionTokens
  , keyToPitchClasses
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (nub, sort, partition, (\\))
import           Data.Maybe (mapMaybe)

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
--   * Sharps: @"#"@, @"##"@, @"###"@, @"1#"@, @"2#"@, etc.
--   * Flats: @"b"@, @"bb"@, @"bbb"@, @"1b"@, @"2b"@, etc.
--   * Named: @"C"@, @"G"@, @"F#"@, @"Bb"@, @"Am"@, @"F#m"@
--   * Wildcard: @"*"@
--   * Removal: @"1b 2# -G"@ (union minus scale), @"* -C'"@ (all minus C)
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

-- |Parse a single token in key context
-- With prime: "G'" -> single pitch [7]
-- Without prime: "G" -> G major scale, "1b" -> F major, etc.
-- Wildcard token: "*" -> chromatic set
parseKeyToken :: Text -> [PitchClass]
parseKeyToken token
  | T.null token = []
  | token == "*" || token == "all" || token == "chr" = chromaticSet
  | "'" `T.isSuffixOf` token =
      -- Prime notation: single pitch class only
      case noteNameToPitchClass (T.init token) of
        Just pc -> [pc]
        Nothing -> []
  | otherwise =
      -- Try key signature first
      case parseKeySignature token of
        Just fifths -> keyPitches fifths
        Nothing ->
          -- Try as note name (resolves to key of that note)
          case noteNameToPitchClass token of
            Just pc ->
              -- Map pitch class to key: C=0 fifths, G=1, D=2, F=-1, etc.
              let fifths = case pc of
                    0 -> 0   -- C major
                    7 -> 1   -- G major (1#)
                    2 -> 2   -- D major (2#)
                    9 -> 3   -- A major (3#)
                    4 -> 4   -- E major (4#)
                    11 -> 5  -- B major (5#)
                    6 -> 6   -- F# major (6#)
                    1 -> -5  -- Db major (5b)
                    8 -> -4  -- Ab major (4b)
                    3 -> -3  -- Eb major (3b)
                    10 -> -2 -- Bb major (2b)
                    5 -> -1  -- F major (1b)
                    _ -> 0
              in keyPitches fifths
            Nothing -> []

-- |Parse key signature notation: "#", "##", "1#", "2b", "4b", etc.
parseKeySignature :: Text -> Maybe Int
parseKeySignature t
  -- Count sharps: "#" = 1, "##" = 2, "###" = 3
  | T.all (== '#') t && not (T.null t) = Just (T.length t)
  -- Count flats: "b" = -1, "bb" = -2, "bbb" = -3
  | T.all (== 'b') t && not (T.null t) = Just (negate $ T.length t)
  -- Numbered format: "1#", "2b", "4#", "0#", etc.
  | otherwise = parseNumberedKeySig t

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
--   * Key signature: @"1b"@, @"#"@ (roots from that key)
--   * Wildcard: @"*"@
--   * Removal: @"C G -G"@ (C and G minus G), @"* -E -A"@ (all except E,A)
--   * INVERTED prime: @"G"@ = single pitch [7], @"G'"@ = G major scale
parseFunds' :: Int -> Text -> [PitchClass]
parseFunds' _ input
  | isWildcard input = chromaticSet
  | otherwise =
      let (includes, excludes) = partitionTokens input
          includePcs = unique $ concatMap parseFundsToken includes
          excludePcs = unique $ concatMap parseFundsToken excludes
      in includePcs \\ excludePcs

-- |Parse a single token in fundamentals/roots context
-- INVERTED prime notation: 'G' = single pitch, 'G'' = G major scale
-- This makes roots default to individual pitches (more intuitive)
-- Wildcard token: "*" -> chromatic set
parseFundsToken :: Text -> [PitchClass]
parseFundsToken token
  | T.null token = []
  | token == "*" || token == "all" || token == "chr" = chromaticSet
  | "'" `T.isSuffixOf` token =
      -- INVERTED: Prime notation means KEY/SCALE in roots context
      let noteToken = T.init token
      in case noteNameToPitchClass noteToken of
           Just pc ->
             -- Map pitch class to key: C=0 fifths, G=1, D=2, F=-1, etc.
             let fifths = case pc of
                   0 -> 0   -- C major
                   7 -> 1   -- G major (1#)
                   2 -> 2   -- D major (2#)
                   9 -> 3   -- A major (3#)
                   4 -> 4   -- E major (4#)
                   11 -> 5  -- B major (5#)
                   6 -> 6   -- F# major (6#)
                   1 -> -5  -- Db major (5b)
                   8 -> -4  -- Ab major (4b)
                   3 -> -3  -- Eb major (3b)
                   10 -> -2 -- Bb major (2b)
                   5 -> -1  -- F major (1b)
                   _ -> 0
             in keyPitches fifths
           Nothing -> []
  -- Check for key signature patterns FIRST (before note names)
  -- This ensures "bb" = 2 flats (not Bb note), "###" = 3 sharps, etc.
  -- But "b" = B note (not 1 flat) since single letters are notes
  | T.length token > 1 && T.all (== '#') token = keyPitches (T.length token)
  | T.length token > 1 && T.all (== 'b') token = keyPitches (negate $ T.length token)
  -- Check for numbered key signatures like "1#", "2b"
  | ("#" `T.isSuffixOf` token || "b" `T.isSuffixOf` token) && T.length token > 1 =
      case parseKeySignature token of
        Just fifths -> keyPitches fifths
        Nothing ->
          -- If it doesn't parse as key signature, try as note (e.g., "eb" = Eb note)
          case noteNameToPitchClass token of
            Just pc -> [pc]
            Nothing -> []
  -- Default: single pitch (so "G" = pitch 7, "D" = pitch 2, "b" = pitch 11, etc.)
  | otherwise =
      case noteNameToPitchClass token of
        Just pc -> [pc]
        Nothing ->
          -- Last resort: try as key signature (e.g., single "#" = 1 sharp)
          case parseKeySignature token of
            Just fifths -> keyPitches fifths
            Nothing -> []

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
