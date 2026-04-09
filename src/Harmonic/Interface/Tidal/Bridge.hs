{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Harmonic.Interface.Tidal.Bridge
-- Description : TidalCycles interface for harmonic progressions
--
-- Bridge between the harmonic generation engine and TidalCycles live coding.
-- Chord selection via mininotation patterns (@Pattern Int@).
--
-- Two arrangement strategies:
--
-- * 'arrange' — onset-join with kinetics range gating: each note maps
--   through the chord active at its onset time, masked by kinetics signal.
--
-- * 'arrange'' — squeeze with kinetics range gating: each chord slot
--   gets the full input pattern compressed to fit.
--
-- Both take a progression modifier @(P.Progression -> P.Progression)@
-- and read the base progression from @kProg k@ via @innerJoin@.

module Harmonic.Interface.Tidal.Bridge
  ( -- * Voice Functions
    VoiceFunction
  , voiceRange

    -- * Chord Selection Helpers
  , warp
  , rep

    -- * Arrangement
  , arrange       -- onset-join with kinetics
  , arrange'      -- squeeze with kinetics

    -- * Chord Lookup
  , lookupChordAt
  , lookupChord
  , lookupProgression

    -- * Voice Extraction
  , VoiceType(..)
  , voiceBy
  , harmony

    -- * Progression Overlap (Re-exports from Arranger)
  , overlapF
  , overlapB
  , overlap
  ) where

-- Phase B imports
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Interface.Tidal.Arranger as A
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK)

import Data.List (nub)
import Sound.Tidal.Context hiding (voice)

-------------------------------------------------------------------------------
-- Voice Function Types
-------------------------------------------------------------------------------

-- |Voice function type: extracts integer pitch sequences from progression
type VoiceFunction = P.Progression -> [[Int]]

-- |Filter pattern events by MIDI note range
voiceRange :: (Int, Int) -> Pattern Int -> Pattern Int
voiceRange (lo, hi) = filterValues (\v -> v >= lo && v <= hi)

-------------------------------------------------------------------------------
-- Chord Selection Helpers
-------------------------------------------------------------------------------

-- |Parse a mininotation chord selection pattern (bar-relative).
-- The @/N@ divisor specifies the number of bars the pattern spans.
--
-- @
-- let r = warp \"[1 2 3 4]/4\"   -- 4 chords over 4 bars (1 per bar)
-- let r = warp \"[1 2]/8\"       -- 2 chords over 8 bars (4 bars each)
-- @
warp :: String -> Pattern Int
warp s = slow 4 $ parseBP_E s

-- |Generate a sequential chord selection pattern from a progression.
-- Auto-derives length from the progression. Timing is bar-relative.
--
-- @
-- let r = rep s4 1     -- 4 chords over 4 bars (1 bar each)
-- let r = rep s4 0.5   -- 4 chords over 2 bars (half bar each)
-- @
rep :: P.Progression -> Pattern Time -> Pattern Int
rep prog repVal =
  let n = P.progLength prog
  in slow (fromIntegral n * repVal * 4) $ fastcat $ map pure [1..n]

-------------------------------------------------------------------------------
-- Arrangement: arrange (onset-join)
-------------------------------------------------------------------------------

-- |Map notes through chords using onset-time lookup, with kinetics range gating.
--
-- The base progression and chord selection are read from @IK@.
-- The modifier function transforms the progression (e.g. @overlapF 0@, @id@).
-- Events are masked by the kinetics signal: only active when kSignal is
-- within the @(lo, hi)@ range. Form-driven dynamics (@kDynamic@) are applied
-- automatically.
--
-- Parameter order: context first (kinetics range, IK, MIDI range), then
-- interactive (voice function, modifier, patterns).
arrange :: (Double, Double)                     -- ^ Kinetics range
        -> IK                                    -- ^ Performance context (kinetics + chord selection)
        -> (Int, Int)                            -- ^ MIDI note range filter
        -> VoiceFunction                         -- ^ Voice function (flow, root, etc.)
        -> (P.Progression -> P.Progression)      -- ^ Progression modifier (overlapF 0, id, etc.)
        -> [Pattern Int]                         -- ^ Input patterns to harmonize
        -> Pattern ValueMap
arrange (lo, hi) (kin, chordPat) register voiceFunc modifier pats =
  let -- Pre-compute note range filter ONCE (shared across all innerJoin invocations)
      ranged = voiceRange register (stack pats)
      -- Pre-compute voicings at construction time (runs ONCE when pattern is registered)
      allEvents = queryArc (kProg kin) (Arc 0 1000)
      uniqueProgs = nub (map value allEvents)
      cache = [ (p, let vs = voiceFunc (modifier p)
                        sc = map (map fromIntegral) vs :: [[Note]]
                        nc = length vs
                    in (sc, nc))
              | p <- uniqueProgs ]
      lookupCache prog = case lookup prog cache of
        Just hit -> hit
        Nothing  -> let vs = voiceFunc (modifier prog)
                    in (map (map fromIntegral) vs, length vs)
  in (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\prog ->
         arrangeLookup (lookupCache prog) chordPat ranged
       ) (kProg kin)

-- |Internal: onset-join arrangement logic (unchanged from original arrange).
arrangeCore :: VoiceFunction
            -> P.Progression
            -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
            -> (Int, Int)         -- ^ MIDI note range filter
            -> [Pattern Int]      -- ^ Input patterns to harmonize
            -> Pattern ValueMap
arrangeCore voiceFunc prog chordPat register pats
  | null voicings = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

          mapped = Pattern (\st ->
            let noteEvs = query ranged st
            in concatMap (\nEv -> case whole nEv of
              Nothing -> []
              Just wArc ->
                let onsetT  = start wArc
                    ci      = lookupChordAt onsetT chordIdx
                    sc      = scales !! (ci `mod` nChords)
                    noteVal = value nEv
                    scLen   = max 1 (length sc)
                    octave  = noteVal `div` scLen
                    idx     = noteVal `mod` scLen
                in [nEv { value = (sc !! idx) + fromIntegral (octave * 12) }]
              ) noteEvs
            ) Nothing Nothing

      in note mapped
  where
    voicings = voiceFunc prog
    scales   = map (map fromIntegral) voicings :: [[Note]]
    nChords  = length voicings
    stacked  = stack pats
    ranged   = voiceRange register stacked

-- |Cached onset-join: takes pre-computed (scales, nChords) and pre-built ranged pattern.
arrangeLookup :: ([[Note]], Int)
              -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
              -> Pattern Int        -- ^ Pre-computed range-filtered note pattern
              -> Pattern ValueMap
arrangeLookup (scales, nChords) chordPat ranged
  | nChords == 0 = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

          mapped = Pattern (\st ->
            let noteEvs = query ranged st
            in concatMap (\nEv -> case whole nEv of
              Nothing -> []
              Just wArc ->
                let onsetT  = start wArc
                    ci      = lookupChordAt onsetT chordIdx
                    sc      = scales !! (ci `mod` nChords)
                    noteVal = value nEv
                    scLen   = max 1 (length sc)
                    octave  = noteVal `div` scLen
                    idx     = noteVal `mod` scLen
                in [nEv { value = (sc !! idx) + fromIntegral (octave * 12) }]
              ) noteEvs
            ) Nothing Nothing

      in note mapped

-------------------------------------------------------------------------------
-- Arrangement: arrange' (squeeze)
-------------------------------------------------------------------------------

-- |Map notes through chords using squeeze, with kinetics range gating.
--
-- Same kinetics/modifier pattern as 'arrange', but uses squeeze strategy:
-- each chord slot gets the full input pattern compressed to fit.
arrange' :: (Double, Double)                     -- ^ Kinetics range
         -> IK                                    -- ^ Performance context
         -> (Int, Int)                            -- ^ MIDI note range filter
         -> VoiceFunction                         -- ^ Voice function
         -> (P.Progression -> P.Progression)      -- ^ Progression modifier
         -> [Pattern Int]                         -- ^ Input patterns to harmonize
         -> Pattern ValueMap
arrange' (lo, hi) (kin, chordPat) register voiceFunc modifier pats =
  let -- Pre-compute note range filter ONCE (shared across all innerJoin invocations)
      ranged = voiceRange register (stack pats)
      -- Pre-compute voicings at construction time (runs ONCE when pattern is registered)
      allEvents = queryArc (kProg kin) (Arc 0 1000)
      uniqueProgs = nub (map value allEvents)
      cache = [ (p, let vs = voiceFunc (modifier p)
                        sc = map (map fromIntegral) vs :: [[Note]]
                        nc = length vs
                    in (sc, nc))
              | p <- uniqueProgs ]
      lookupCache prog = case lookup prog cache of
        Just hit -> hit
        Nothing  -> let vs = voiceFunc (modifier prog)
                    in (map (map fromIntegral) vs, length vs)
  in (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\prog ->
         arrangeLookup' (lookupCache prog) chordPat ranged
       ) (kProg kin)

-- |Internal: squeeze arrangement logic (unchanged from original arrange').
arrangeCore' :: VoiceFunction
             -> P.Progression
             -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
             -> (Int, Int)         -- ^ MIDI note range filter
             -> [Pattern Int]      -- ^ Input patterns to harmonize
             -> Pattern ValueMap
arrangeCore' voiceFunc prog chordPat register pats
  | null voicings = silence
  | otherwise =
      let chordIdx  = fmap (\i -> (i - 1) `mod` nChords) chordPat
          chordPats = map (\sc -> note (toScale sc ranged)) scales
      in squeeze chordIdx chordPats
  where
    voicings = voiceFunc prog
    scales   = map (map fromIntegral) voicings :: [[Note]]
    nChords  = length voicings
    stacked  = stack pats
    ranged   = voiceRange register stacked

-- |Cached squeeze: takes pre-computed (scales, nChords) and pre-built ranged pattern.
arrangeLookup' :: ([[Note]], Int)
               -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
               -> Pattern Int        -- ^ Pre-computed range-filtered note pattern
               -> Pattern ValueMap
arrangeLookup' (scales, nChords) chordPat ranged
  | nChords == 0 = silence
  | otherwise =
      let chordIdx  = fmap (\i -> (i - 1) `mod` nChords) chordPat
          chordPats = map (\sc -> note (toScale sc ranged)) scales
      in squeeze chordIdx chordPats

-------------------------------------------------------------------------------
-- Chord Lookup
-------------------------------------------------------------------------------

-- |Point-query a chord selection pattern at a specific time.
-- Returns the chord index (0-indexed) active at time @t@.
-- Falls back to chord 0 if no events found.
lookupChordAt :: Time -> Pattern Int -> Int
lookupChordAt t cpat =
  case queryArc cpat (Arc t (t + 1/10000000)) of
    []    -> 0
    (e:_) -> value e

-- |Lookup a chord from a progression by index with modulo wrap.
lookupChord :: P.Progression -> Int -> H.Chord
lookupChord prog idx =
  let len = P.progLength prog
      chords = P.progChords prog
      wrappedIdx = idx `mod` len
  in chords !! wrappedIdx

-- |Lookup progression as a pattern of indices.
lookupProgression :: P.Progression -> Pattern Int -> Pattern [Int]
lookupProgression prog idxPat =
  let len = P.progLength prog
      voicings = A.flow prog
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-------------------------------------------------------------------------------
-- Voice Extraction
-------------------------------------------------------------------------------

-- |Voice type for extraction strategy
data VoiceType = Roots | Grid | Harmony | Voiced
  deriving (Show, Eq)

-- |Extract specific voice type from a progression pattern.
voiceBy :: VoiceType -> P.Progression -> Pattern Int -> Pattern [Int]
voiceBy vtype prog idxPat =
  let voiceFunc = case vtype of
        Roots   -> A.root
        Grid    -> A.grid
        Harmony -> A.lite
        Voiced  -> A.flow
      voicings = voiceFunc prog
      len = length voicings
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-- |Convenience function: lookup progression and convert to note pattern.
harmony :: P.Progression -> Pattern Int -> Pattern ValueMap
harmony prog idxPat =
  let voicings = lookupProgression prog idxPat
  in note $ fmap (fromIntegral . head) voicings

-------------------------------------------------------------------------------
-- Progression Overlap (Re-exports from Arranger)
-------------------------------------------------------------------------------

-- |Forward overlap: merge pitches from n bars ahead
overlapF :: Int -> P.Progression -> P.Progression
overlapF = A.progOverlapF

-- |Backward overlap: merge pitches from n bars behind
overlapB :: Int -> P.Progression -> P.Progression
overlapB = A.progOverlapB

-- |Bidirectional overlap: merge pitches from n bars in both directions
overlap :: Int -> P.Progression -> P.Progression
overlap = A.progOverlap
