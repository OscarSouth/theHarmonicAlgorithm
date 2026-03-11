{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Harmonic.Tidal.Interface
-- Description : TidalCycles interface for harmonic progressions
-- 
-- This module provides the bridge between the harmonic generation engine
-- and TidalCycles live coding. Key design principles:
--
-- 1. Pattern-based lookup with modulo wrap: Indices wrap around the
--    progression length, so `run 4` on a 16-bar progression loops the
--    first 4, and `iter` creates rotating windows.
--
-- 2. Preserve launcher ergonomics: The `arrange` function maintains
--    backward compatibility with the instrument launcher paradigm
--    (juno f s r d, moog f s r d, etc.)
--
-- 3. Voice extraction: Uses voicing paradigms from Arranger module
--    (flow, tall, slim, wide, lite) for different instrumental roles.

module Harmonic.Interface.Tidal.Bridge
  ( -- * Voice Functions
    VoiceFunction
  , rootNotes
  , bassNotes

    -- * Pattern Application
  , arrange
  , arrange'
  , applyProg
  , applyProg'
  , voiceRange

    -- * Per-Chord Pattern Distribution
  , expandBraces
  , bracket

    -- * Pattern-Based Lookup
  , lookupProgression
  , lookupChord
  , VoiceType(..)
  , voiceBy
  , harmony

    -- * Progression Overlap (Re-exports from Arranger)
  , overlapF    -- Forward overlap
  , overlapB    -- Backward overlap
  , overlap     -- Bidirectional overlap
  ) where

-- Phase B imports
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Interface.Tidal.Arranger as A

import Sound.Tidal.Context hiding (voice)  -- Hide to avoid conflict
import qualified Data.List as L
import Data.Foldable (toList)

-------------------------------------------------------------------------------
-- Voice Function Types (Using Phase B Progression)
-------------------------------------------------------------------------------

-- |Voice function type: extracts integer pitch sequences from progression
type VoiceFunction = P.Progression -> [[Int]]

-- |Extract root note only (for bass lines, lead melodies)
-- Renamed from 'root' to avoid conflict with Arranger exports
rootNotes :: VoiceFunction
rootNotes prog = 
  let cadenceStates = toList (P.unProgression prog)
  in map rootToInt cadenceStates
  where
    rootToInt :: H.CadenceState -> [Int]
    rootToInt cs = 
      let rootNoteName = H.stateCadenceRoot cs
          rootPc = Pitch.pitchClass rootNoteName
      in [Pitch.unPitchClass rootPc]

-- |Extract bass note (first interval in chord voicing)
-- Renamed from 'bass' to avoid conflict with Arranger exports
bassNotes :: VoiceFunction
bassNotes prog = map bassToInt (P.progChords prog)
  where
    bassToInt :: H.Chord -> [Int]
    bassToInt chord = 
      case H.chordIntervals chord of
        []    -> [0]
        (p:_) -> [fromIntegral p]

-------------------------------------------------------------------------------
-- Pattern Application (Legacy Compatible)
-------------------------------------------------------------------------------

-- |Filter pattern events by MIDI note range
voiceRange :: (Int, Int) -> Pattern Int -> Pattern Int
voiceRange (lo, hi) = filterValues (\v -> v >= lo && v <= hi)

-- |Apply progression to pattern with toScale and temporal stretching.
-- 
-- The key operation: for each chord in the progression, the input pattern
-- is mapped through that chord's scale (toScale). The patterns are then
-- concatenated with `cat` and slowed to span the desired cycle length.
applyProg :: VoiceFunction -> P.Progression -> Pattern Time -> Pattern Int -> Pattern ValueMap
applyProg voiceFunc prog len pat =
  slow (4 * len) (cat $ note <$>
    (`toScale` fast (4 * len) pat) <$>
    fmap fromIntegral <$> voiceFunc prog)

-- |Main arrange function combining voice extraction and pattern application.
-- 
-- This preserves the launcher paradigm:
--   juno f s r d = p "juno" $ f $ arrange A.flow s r (-9,9) ["pattern"]
--
-- The progression is applied with modulo wrap internally, so indices
-- exceeding the progression length wrap around.
arrange :: VoiceFunction 
        -> P.Progression 
        -> Pattern Time      -- ^ Repetitions/cycle length
        -> (Int, Int)        -- ^ MIDI note range filter
        -> [Pattern Int]     -- ^ Input patterns to harmonize
        -> Pattern ValueMap
arrange voiceFunc prog rep register pats =
  let stacked = stack pats
      ranged = voiceRange register stacked
   in applyProg voiceFunc prog rep ranged

-- |Apply progression with per-chord time rotation.
--
-- Like 'applyProg' but adds @rotL i@ per chord entry, counteracting
-- TidalCycles' @cat@ time normalization. This means multi-cycle patterns
-- (e.g., @\<a b c\>@) distribute different values to different chords
-- rather than showing the same value to all chords.
--
-- For single-cycle patterns, @rotL@ has no effect — behavior is identical
-- to 'applyProg'.
applyProg' :: VoiceFunction -> P.Progression -> Pattern Time -> Pattern Int -> Pattern ValueMap
applyProg' voiceFunc prog len pat =
  slow (4 * len) $ cat $
    zipWith (\i voicing ->
      note $ toScale (fmap fromIntegral voicing)
                     (pure (fromIntegral i) <~ fast (4 * len) pat)
    ) [0..] (voiceFunc prog)

-- |Arrange with per-chord pattern distribution.
--
-- Like 'arrange' but uses 'applyProg'' so that multi-cycle patterns
-- (e.g., angle brackets @\<\>@) distribute their values across chord
-- positions rather than showing the same value for every chord.
--
-- Usage:
--
-- @
-- arrange' flow s r (-9,9) [\"[0 \<2 1 [4 3]\>]/4\"]
-- -- chord 0 sees [0,2], chord 1 sees [0,1], chord 2 sees [0,[4 3]]
-- @
arrange' :: VoiceFunction
         -> P.Progression
         -> Pattern Time      -- ^ Repetitions/cycle length
         -> (Int, Int)        -- ^ MIDI note range filter
         -> [Pattern Int]     -- ^ Input patterns to harmonize
         -> Pattern ValueMap
arrange' voiceFunc prog rep register pats =
  let stacked = stack pats
      ranged = voiceRange register stacked
   in applyProg' voiceFunc prog rep ranged

-------------------------------------------------------------------------------
-- Per-Chord Pattern Distribution: expandBraces / bracket
-------------------------------------------------------------------------------

-- |Expand curly-brace groups in a TidalCycles mini-notation string into
-- a multi-cycle static expansion.
--
-- Since TidalCycles parses @{a b c}@ identically to @[a b c]@ (single-cycle),
-- @rotL@ alone can't distribute brace values across chords. This function
-- rewrites the string so that each brace value occupies its own cycle,
-- producing the multi-cycle structure that 'arrange'' can then distribute.
--
-- @
-- expandBraces \"[0 {1 2 3}]/4\"   == \"[0 1 0 2 0 3]/12\"
-- expandBraces \"[0 {1 2}]/4\"     == \"[0 1 0 2]/8\"
-- expandBraces \"1*4\"             == \"1*4\"   -- passthrough (no braces)
-- expandBraces \"[0 {1 2 3}]\"     == \"[0 1 0 2 0 3]\"  -- no divisor
-- @
expandBraces :: String -> String
expandBraces input =
  let (groups, template, divisor) = parseBraceGroups input
  in if null groups
     then input  -- passthrough: no braces
     else let reps = foldr1 lcm (map length groups)
              expanded = map (expandOne template groups) [0 .. reps - 1]
              inner = concatMap stripOuterBrackets expanded
              newDiv = case divisor of
                         Just d  -> Just (d * reps)
                         Nothing -> Nothing
          in formatExpanded inner newDiv template

-- |Parse brace groups from the input string.
-- Returns: (list of groups, template with placeholders, optional divisor)
parseBraceGroups :: String -> ([[String]], String, Maybe Int)
parseBraceGroups input =
  let (body, divisor) = splitDivisor input
      (groups, template) = extractGroups body 0 0
  in (groups, template, divisor)

-- |Split trailing /N divisor from the pattern body.
-- Handles nested brackets properly.
splitDivisor :: String -> (String, Maybe Int)
splitDivisor s =
  case findTopLevelSlash s of
    Nothing -> (s, Nothing)
    Just i  -> let body = take i s
                   rest = drop (i + 1) s
               in case reads rest of
                    [(n, "")] -> (body, Just n)
                    _         -> (s, Nothing)

-- |Find the index of a top-level '/' (not inside any brackets).
findTopLevelSlash :: String -> Maybe Int
findTopLevelSlash = go 0 0
  where
    go _ _ [] = Nothing
    go depth idx (c:cs)
      | c `elem` ['[', '(', '{', '<'] = go (depth + 1) (idx + 1) cs
      | c `elem` [']', ')', '}', '>'] = go (depth - 1) (idx + 1) cs
      | c == '/' && depth == 0         = Just idx
      | otherwise                      = go depth (idx + 1) cs

-- |Extract brace groups and build a template with numbered placeholders.
extractGroups :: String -> Int -> Int -> ([[String]], String)
extractGroups [] _ _ = ([], [])
extractGroups ('{':rest) groupIdx depth =
  let (content, after) = matchBrace rest 0
      elements = splitBraceContent content
      placeholder = '\x00' : show groupIdx ++ "\x00"
      (moreGroups, moreTemplate) = extractGroups after (groupIdx + 1) depth
  in (elements : moreGroups, placeholder ++ moreTemplate)
extractGroups (c:rest) groupIdx depth =
  let (moreGroups, moreTemplate) = extractGroups rest groupIdx depth
  in (moreGroups, c : moreTemplate)

-- |Match from after opening '{' to the corresponding '}'.
matchBrace :: String -> Int -> (String, String)
matchBrace [] _ = ([], [])
matchBrace ('}':rest) 0 = ([], rest)
matchBrace ('{':rest) d = let (inner, after) = matchBrace rest (d + 1) in ('{' : inner, after)
matchBrace ('}':rest) d = let (inner, after) = matchBrace rest (d - 1) in ('}' : inner, after)
matchBrace (c:rest) d = let (inner, after) = matchBrace rest d in (c : inner, after)

-- |Split brace content by whitespace, respecting nested brackets.
splitBraceContent :: String -> [String]
splitBraceContent = filter (not . null) . go 0 ""
  where
    go :: Int -> String -> String -> [String]
    go _ acc [] = [L.reverse acc]
    go 0 acc (' ':rest) = L.reverse acc : go 0 "" rest
    go depth acc (c:rest)
      | c `elem` ['[', '(', '{', '<'] = go (depth + 1) (c:acc) rest
      | c `elem` [']', ')', '}', '>'] = go (depth - 1) (c:acc) rest
      | otherwise                      = go depth (c:acc) rest

-- |Expand template for one repetition index, substituting brace group elements.
expandOne :: String -> [[String]] -> Int -> String
expandOne template groups repIdx = go template
  where
    go [] = []
    go ('\x00':rest) =
      let (numStr, after) = span (/= '\x00') rest
          groupIdx = read numStr :: Int
          group = groups !! groupIdx
          element = group !! (repIdx `mod` length group)
          afterPlaceholder = drop 1 after  -- skip closing \x00
      in element ++ go afterPlaceholder
    go (c:rest) = c : go rest

-- |Strip outer square brackets from a string, if present.
stripOuterBrackets :: String -> String
stripOuterBrackets ('[':rest) = case L.reverse rest of
  ']':inner -> ' ' : L.reverse inner
  _         -> '[' : rest
stripOuterBrackets s = s

-- |Format the expanded repetitions with optional divisor.
-- Uses the original template to determine whether to re-wrap in brackets.
formatExpanded :: String -> Maybe Int -> String -> String
formatExpanded inner Nothing  template
  | hasOuterBrackets template = "[" ++ L.dropWhile (== ' ') inner ++ "]"
  | otherwise                 = L.dropWhile (== ' ') inner
formatExpanded inner (Just d) template
  | hasOuterBrackets template = "[" ++ L.dropWhile (== ' ') inner ++ "]/" ++ show d
  | otherwise                 = L.dropWhile (== ' ') inner ++ "/" ++ show d

-- |Check if string has outer square brackets.
hasOuterBrackets :: String -> Bool
hasOuterBrackets ('[':rest) = not (null rest) && last rest == ']'
hasOuterBrackets _ = False

-- |Parse a mini-notation string with brace expansion into a TidalCycles
-- @Pattern Int@.
--
-- Combines 'expandBraces' with TidalCycles' 'parseBP_E' parser. Use with
-- 'arrange'' for per-chord distribution of brace groups:
--
-- @
-- arrange' flow s r (-9,9) [bracket \"[0 {1 2 3}]/4\"]
-- -- chord 0: [0,1], chord 1: [0,2], chord 2: [0,3]
-- @
bracket :: String -> Pattern Int
bracket = parseBP_E . expandBraces

-------------------------------------------------------------------------------
-- Pattern-Based Lookup (New Phase C Interface)
-------------------------------------------------------------------------------

-- |Voice type for extraction strategy
data VoiceType = Roots | Bass | Harmony | Voiced
  deriving (Show, Eq)

-- |Lookup a chord from a progression by index with modulo wrap.
-- 
-- This is the fundamental operation enabling pattern-based harmonic access.
-- The index wraps around the progression length:
--   lookupChord prog 0  -> first chord
--   lookupChord prog 16 -> chord at (16 mod len)
lookupChord :: P.Progression -> Int -> H.Chord
lookupChord prog idx =
  let len = P.progLength prog
      chords = P.progChords prog
      wrappedIdx = idx `mod` len
  in chords !! wrappedIdx

-- |Lookup progression as a pattern of indices.
-- 
-- Maps a pattern of integers to a pattern of pitch class lists.
-- Uses modulo wrap so `run 4` on a 16-chord progression loops first 4.
--
-- Example:
--   lookupProgression prog "<0 [1 2] 3>" 
--   -> Pattern of pitch class lists following that rhythm
lookupProgression :: P.Progression -> Pattern Int -> Pattern [Int]
lookupProgression prog idxPat =
  let len = P.progLength prog
      voicings = A.flow prog  -- Use Arranger's flow (voice-led)
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-- |Extract specific voice type from a progression pattern.
-- 
-- Takes a pattern of indices and extracts the appropriate voice:
--   Roots: Root note only
--   Bass: Lowest note of voicing
--   Harmony: All notes (lite - no voice leading)
--   Voiced: All notes with voice leading applied (flow paradigm)
voiceBy :: VoiceType -> P.Progression -> Pattern Int -> Pattern [Int]
voiceBy vtype prog idxPat =
  let voiceFunc = case vtype of
        Roots   -> rootNotes
        Bass    -> bassNotes
        Harmony -> A.lite    -- Raw intervals
        Voiced  -> A.flow    -- Voice-led
      voicings = voiceFunc prog
      len = length voicings
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-- |Convenience function: lookup progression and convert to note pattern.
-- 
-- This combines lookupProgression with note conversion for immediate use:
--   d1 $ note (harmony prog "<0 1 2 3>") # s "superpiano"
harmony :: P.Progression -> Pattern Int -> Pattern ValueMap
harmony prog idxPat =
  let voicings = lookupProgression prog idxPat
  in note $ fmap (fromIntegral . head) voicings  -- Take first note for simplicity

-------------------------------------------------------------------------------
-- Progression Overlap (Re-exports from Arranger)
-------------------------------------------------------------------------------

-- |Forward overlap: merge pitches from n bars ahead
-- Example: overlapF 1 prog merges each chord with the next chord's pitches
overlapF :: Int -> P.Progression -> P.Progression
overlapF = A.progOverlapF

-- |Backward overlap: merge pitches from n bars behind
overlapB :: Int -> P.Progression -> P.Progression
overlapB = A.progOverlapB

-- |Bidirectional overlap: merge pitches from n bars in both directions
overlap :: Int -> P.Progression -> P.Progression
overlap = A.progOverlap

