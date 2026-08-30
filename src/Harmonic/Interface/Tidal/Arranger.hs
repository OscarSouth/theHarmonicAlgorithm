-- |
-- Module      : Harmonic.Interface.Tidal.Arranger
-- Description : Performance-oriented progression manipulation
-- 
-- Shorthand for manipulating progressions in a TidalCycles performance
-- context. All functions are designed for live-coding ergonomics: short
-- names, intuitive parameter order, and no @IO@.
--
-- Two families, both wrapping the more verbose
-- "Harmonic.Rules.Types.Progression" functions:
--
-- [Rearranging] 'rotate', 'excerpt', 'insert', 'switch', 'clone', 'extract',
-- 'transposeP', 'reverse', 'fuse', 'fuse2', 'interleave', 'expandP',
-- 'progOverlap', 'progOverlapF', 'progOverlapB'.
--
-- [Voicing] five strategies that turn a progression into concrete pitches —
-- 'grid', 'flow', 'lite', 'literal' and 'root'. 'grid' and 'flow' solve a
-- cyclic DP for smooth voice leading; the others are literal or bass-only.
--
-- Rearranging composes on the progression before it reaches a voicing:
--
-- @
-- s  \<- seek \"*\" $ len 8 $ entropy 0.4 $ gen
-- s' = fuse (excerpt 0 4 s) (rotate 2 s)
-- @
--
-- The voicing is then chosen per instrument at the point of play, so two
-- lines can read the same progression differently:
--
-- @
-- , cello      T (0,1) k vl flow Tenor8vb
-- , contrabass T (0,1) k vl grid Bass8vb
-- @

module Harmonic.Interface.Tidal.Arranger
  ( -- * Position/Range Operations
    rotate
  , excerpt
  , insert
  , switch
  , clone
  , extract

    -- * Transformation Operations
  , transposeP
  , Harmonic.Interface.Tidal.Arranger.reverse
  , fuse
  , fuse2
  , interleave
  , expandP

    -- * Overlap Operations (Progression-level)
  , progOverlap
  , progOverlapF
  , progOverlapB

    -- * Voicing Extractors (Voicing paradigms)
  , grid   -- Root locked in bass, smooth compact voice leading (cyclic DP)
  , flow   -- Any inversion allowed for smoothest voice leading (cyclic DP)
  , lite   -- Literal, no transformation
  , literal -- Alias for lite
  , root   -- Root note only (root pitch class per chord)
  , strataModeFlow  -- Bar-0-grounded pitch lattice for non-triad chroma layers

    -- * Explicit Progression Construction
  , fromChords      -- Construct Progression from pitch-class lists
  , prog            -- Legacy alias for fromChords

    -- * Scale Source (Switch Mechanism)
  , ScaleSource(..)
  , melodyStateFrom

    -- * Starting State Construction
  , lead
  , lead'
  , leadJ
  , parseLeadTokens
  , LeadToken(..)
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence ((><))
import Data.Foldable (toList)
import Data.List (sort, nub, sortBy)
import Data.Maybe (listToMaybe)
import Data.Char (toLower)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom, uniformRM, GenIO)

import Harmonic.Rules.Types.Progression
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Rules.Types.ProgressionContext (ProgressionContext, liftPC)
import Harmonic.Rules.Types.Harmony (Cadence(..), CadenceState(..), EnharmonicSpelling(..), toFunctionality, toFunctionalityChord, Movement(..), enharmonicFunc, inferSpelling, isAmbiguousPattern, initCadenceState, mkCadenceStatePCs, toMovement)
import qualified Harmonic.Rules.Constraints.Filter as Filter
import qualified Harmonic.Rules.Import.Jazz as J
import qualified Data.Text as T
import Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)
import Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import Harmonic.Rules.Types.Pitch (PitchClass(..), NoteName(..), pitchClass, mkPitchClass, unPitchClass, flat, sharp)
import Harmonic.Evaluation.Scoring.VoiceLeading (solveRoot, solveFlow, bassVoicing, normalizeByFirstRoot, initialCompact, alignVoices)
import Data.Function (on)
import Data.List (minimumBy)

-------------------------------------------------------------------------------
-- Position\/Range Operations
-------------------------------------------------------------------------------

-- |Rotate a progression by n bars (positive = left, negative = right).
-- Bar-aligned: provenance and family travel with the bars.
rotate :: Int -> ProgressionContext -> ProgressionContext
rotate n = PC.liftPCAligned (rotateSeq n)

-- |Extract bars start to end (1-indexed, inclusive).
-- Bar-aligned: provenance and family travel with the bars.
excerpt :: Int -> Int -> ProgressionContext -> ProgressionContext
excerpt s e = PC.liftPCAligned (excerptSeq s e)

-- |Insert a CadenceState at position (1-indexed), replacing the existing one.
-- Bar substitution: the foreign bar breaks strata\/polytonal invariants, so
-- provenance drops and those families downgrade.
insert :: CadenceState -> Int -> ProgressionContext -> ProgressionContext
insert cs pos = PC.liftPCSubst (insertProg cs pos)
  where
    insertProg :: CadenceState -> Int -> Progression -> Progression
    insertProg c p (Progression s)
      | Seq.null s = singleton c
      | p < 1 = Progression (c Seq.<| s)
      | p > Seq.length s = Progression (s Seq.|> c)
      | otherwise =
        let (before, rest) = Seq.splitAt (p - 1) s
        in case Seq.viewl rest of
             Seq.EmptyL -> Progression (before Seq.|> c)
             _ Seq.:< after -> Progression (before >< (c Seq.<| after))

-- |Switch two bars at positions m and n (1-indexed).
-- Treated as substitution (drops provenance, downgrades strata\/polytonal
-- families); promotable to the aligned tier via a lockstep 'Seq.update'
-- pair if per-bar identity is ever wanted here.
switch :: Int -> Int -> ProgressionContext -> ProgressionContext
switch m n = PC.liftPCSubst (switchProg m n)
  where
    switchProg :: Int -> Int -> Progression -> Progression
    switchProg a b progIn@(Progression s)
      | a == b = progIn
      | Seq.null s = progIn
      | otherwise =
        let len = Seq.length s
            m' = max 0 (min (len - 1) (a - 1))
            n' = max 0 (min (len - 1) (b - 1))
            csM = Seq.index s m'
            csN = Seq.index s n'
            s'  = Seq.update m' csN $ Seq.update n' csM s
        in Progression s'

-- |Clone bar m to position n (overwrites n with contents of m).
-- Treated as substitution — see 'switch'.
clone :: Int -> Int -> ProgressionContext -> ProgressionContext
clone m n = PC.liftPCSubst (cloneProg m n)
  where
    cloneProg :: Int -> Int -> Progression -> Progression
    cloneProg a b progIn@(Progression s)
      | a == b = progIn
      | Seq.null s = progIn
      | otherwise =
        let len = Seq.length s
            m' = max 0 (min (len - 1) (a - 1))
            n' = max 0 (min (len - 1) (b - 1))
            csM = Seq.index s m'
            s'  = Seq.update n' csM s
        in Progression s'

-- |Extract a single CadenceState at index (1-indexed, modulo wrap) from the triad layer
extract :: Int -> ProgressionContext -> CadenceState
extract n pc
  | len == 0  = error "extract: empty progression"
  | otherwise = case getCadenceState progArg idx of
      Just cs -> cs
      Nothing -> error "extract: internal error"
  where
    progArg = PC.triadLayer pc
    len  = progLength progArg
    idx  = ((n - 1) `mod` len) + 1  -- 1-indexed with modulo wrap

-------------------------------------------------------------------------------
-- Transformation Operations
-------------------------------------------------------------------------------

-- |Transpose a progression by n semitones. Strata provenance is anchored
-- to ABSOLUTE pitch classes (the octatripentatonic universe has no
-- transposition closure), so provenance and family survive only octave
-- shifts; any other interval drops them.
transposeP :: Int -> ProgressionContext -> ProgressionContext
transposeP n pc
  | n `mod` 12 == 0 =
      (liftPC (transposeProgression n) pc)
        { PC.pcProvenance = PC.pcProvenance pc
        , PC.pcFamily     = PC.pcFamily pc }
  | otherwise = liftPC (transposeProgression n) pc

-- |Reverse a progression. Bar-aligned: provenance and family travel.
reverse :: ProgressionContext -> ProgressionContext
reverse = PC.liftPCAligned Seq.reverse

-- |Fuse multiple progressions into one (concatenation)
fuse :: [ProgressionContext] -> ProgressionContext
fuse = mconcat

-- |Binary fuse for convenience in live coding
fuse2 :: ProgressionContext -> ProgressionContext -> ProgressionContext
fuse2 a b = a <> b

-- |Interleave two progressions (alternating chords)
-- Example: interleave [A,B,C] [X,Y,Z] = [A,X,B,Y,C,Z]
interleave :: ProgressionContext -> ProgressionContext -> ProgressionContext
interleave a b = PC.normalizeFamily PC.ProgressionContext
  { PC.triadLayer   = fuseProgression (PC.triadLayer a)  (PC.triadLayer b)
  , PC.strataLayer  = fuseProgression (PC.strataLayer a) (PC.strataLayer b)
  , PC.modeLayer    = fuseProgression (PC.modeLayer a)   (PC.modeLayer b)
  , PC.pcProvenance = interleaveSeq <$> PC.pcProvenance a <*> PC.pcProvenance b
  , PC.pcFamily     = if PC.pcFamily a == PC.pcFamily b then PC.pcFamily a else PC.FTriad
  }

-- |Expand a progression by repeating the WHOLE progression n times
-- (@expandP 2@ on C F G A yields C F G A C F G A).
-- Bar-aligned replication: provenance and family travel.
expandP :: Int -> ProgressionContext -> ProgressionContext
expandP n = PC.liftPCAligned (\sq -> if n <= 0 then Seq.empty else mconcat (replicate n sq))

-------------------------------------------------------------------------------
-- Overlap Operations
-- These create sustain\/legato effects by merging pitches from adjacent chords
-------------------------------------------------------------------------------

-- Shared overlap core: for every bar, union the ABSOLUTE pitch classes
-- of the bars inside the window, then store them relative to that bar's
-- own root. Pitch content is read straight from 'cadenceIntervals' — no
-- 'fromCadenceState'\/'toTriad' round trip. The old round trip stored
-- each bar's ABSOLUTE pitch classes back into 'cadenceIntervals', which
-- the voicing layer then transposed by the root a second time (every
-- non-C-rooted bar sounded shifted up by its own root), and 'toTriad'
-- reduced >3-note bars to a triad before merging (extended material lost its
-- fourth voice).
progOverlapWith :: (Int -> (Int, Int)) -> Progression -> Progression
progOverlapWith window (Progression sq)
  | Seq.null sq = Progression sq
  | otherwise =
      let states = toList sq
          len    = length states
          absAt j =
            let cs     = states !! j
                rootPC = unPitchClass (pitchClass (stateCadenceRoot cs))
            in [ (unPitchClass p + rootPC) `mod` 12
               | p <- cadenceIntervals (stateCadence cs) ]
          rebuilt i cs =
            let (lo, hi)  = window i
                rootPC    = unPitchClass (pitchClass (stateCadenceRoot cs))
                absUnion  = nub (concatMap absAt [max 0 lo .. min (len - 1) hi])
                rels      = sort (nub [ (a - rootPC) `mod` 12 | a <- absUnion ])
            in rebuildCadenceState (stateCadence cs) (stateCadenceRoot cs)
                                   (map fromIntegral rels)
      in Progression (Seq.fromList (zipWith rebuilt [0 ..] states))

-- |Bidirectional overlap: merge pitches from n bars in both directions
progOverlap :: Int -> Progression -> Progression
progOverlap range progArg
  | range <= 0 = progArg
  | otherwise  = progOverlapWith (\i -> (i - range, i + range)) progArg

-- |Forward-only overlap: merge pitches from n bars ahead
progOverlapF :: Int -> Progression -> Progression
progOverlapF range progArg
  | range <= 0 = progArg
  | otherwise  = progOverlapWith (\i -> (i, i + range)) progArg

-- |Backward-only overlap: merge pitches from n bars behind
progOverlapB :: Int -> Progression -> Progression
progOverlapB range progArg
  | range <= 0 = progArg
  | otherwise  = progOverlapWith (\i -> (i - range, i)) progArg

-- Helper: rebuild a CadenceState with new intervals
rebuildCadenceState :: Cadence -> NoteName -> [Integer] -> CadenceState
rebuildCadenceState cad rootName newIntervals =
  let -- Create a modified cadence with the new intervals (as PitchClasses)
      newPCs = map (\i -> mkPitchClass (fromIntegral i)) newIntervals
      -- Rename from the merged set: an overlapped bar sounds the union,
      -- so its displayed functionality must describe the union rather
      -- than the pre-merge chord.
      newName
        | length newPCs <= 3 = toFunctionality newPCs
        | otherwise          = toFunctionalityChord newPCs
      newCad = cad { cadenceIntervals = newPCs, cadenceFunctionality = newName }
      -- Infer spelling from the new absolute pitches
      rootPC = pitchClass rootName
      absolutePitches = map (\i -> (fromIntegral i + unPitchClass rootPC) `mod` 12) newIntervals
      spelling = inferSpelling absolutePitches
  in CadenceState newCad rootName spelling

-------------------------------------------------------------------------------
-- Voicing Extractors (Voicing paradigms)
-------------------------------------------------------------------------------

-- |GRID paradigm: Root locked in bass with smooth compact voice leading.
-- Uses cyclic DP to find globally optimal voicings.
-- First chord starts compact with root in bass; all subsequent chords
-- maintain root in bass with minimal voice movement.
grid :: Progression -> [[Int]]
grid progArg
  | hasBigChroma progArg = strataModeFlow progArg
  | otherwise =
      let intVoicings = map (map fromIntegral) $ literalVoicing' progArg
      in solveRoot intVoicings

-- |FLOW paradigm: Smoothest voice leading with any inversion allowed.
-- Uses cyclic DP to find globally optimal voicings.
-- Voice crossings permitted for optimal smoothness; bass doesn't need
-- to be the root if an inversion provides smoother voice leading.
flow :: Progression -> [[Int]]
flow progArg
  | hasBigChroma progArg = strataModeFlow progArg
  | otherwise =
      let intVoicings = map (map fromIntegral) $ literalVoicing' progArg
      in solveFlow intVoicings

-- |LITE paradigm: Literal voicings with first-root normalization.
-- Returns pitches as stored, but normalized so first chord's root is in [-12,-1].
-- No voice leading optimization applied (only octave normalization).
lite :: Progression -> [[Int]]
lite progArg = 
  let raw = map (map fromIntegral) $ literalVoicing' progArg
  in normalizeByFirstRoot raw

-- |ROOT paradigm: Root note only (root pitch class per chord).
-- Extracts the root note (first element, mod 12) from each chord.
-- Returns as single-element lists in [0,11] range.
root :: Progression -> [[Int]]
root progArg =
  let raw = map (map fromIntegral) $ literalVoicing' progArg
  in bassVoicing raw

-- |Alias for lite (legacy compatibility)
literal :: Progression -> [[Int]]
literal = lite

-- |STRATA-MODE-FLOW paradigm: a fixed pitch LATTICE grounded on bar 0.
-- Bar 0's chroma builds a compressed-ascending stack from its root
-- ('initialCompact' + 'normalizeByFirstRoot', root anchored in [-12, -1]);
-- every later bar keeps that lattice's slots in place and inflects each
-- slot to the current bar's chroma — the key signature changes, the hand
-- position does not. Pattern index @i@ addresses the i-th lattice slot,
-- so a held index pedals its pitch across key areas (inflecting by
-- accidental where the new set demands it) instead of transposing with
-- each bar's root, and increments of 1 still ascend by one set member.
--
-- Bar n (equal cardinality — the normal case: S layers uniformly 5-PC,
-- M layers uniformly 7-PC): the minimum-cost cyclic-monotone bijection
-- from the anchor lattice onto the bar's PC set. Each of the n cyclic
-- rotations of the bar's sorted PCs is realised monotonically against
-- the anchor (slot 0 nearest bar 0's slot 0, each later slot the
-- smallest pitch above its predecessor); the rotation minimising total
-- @|placed_MIDI - anchor_MIDI|@ wins (tie-break: smaller net signed
-- displacement). A bijection — not per-slot nearest-tone, which can
-- collapse two slots onto one pitch — so every bar stays distinct,
-- ascending, and surjective onto its chroma, and the octave-wrap
-- identity in "Harmonic.Interface.Tidal.Bridge" (index n = slot 0 + 12)
-- keeps working.
--
-- Anchoring every bar to bar 0 (rather than chaining to the previous
-- bar) bounds drift: each slot stays within a tritone of its anchor
-- pitch, the pattern wrap (bar N-1 → bar 0) returns home, and a bar
-- whose chroma equals bar 0's lands on bar 0's exact MIDI.
--
-- Bars whose cardinality differs from the anchor's (silent bars, 6-PC
-- override edges, hand-built mixed material arriving via @hasBigChroma@)
-- fall back to @shiftBar@ — slot identity is undefined across a
-- slot-count change. O(n²) per bar, n ≤ 7; eager forcing in
-- "Harmonic.Interface.Tidal.Bridge" hoists the work to REPL evaluation
-- time.
strataModeFlow :: Progression -> [[Int]]
strataModeFlow progArg =
  case toList (unProgression progArg) of
    []                  -> []
    (firstCS : restCSs) ->
      let firstPCs    = cadencePCs firstCS
          firstRootPC = case firstPCs of (p:_) -> p; [] -> 0
          v0          = initialCompact firstRootPC firstPCs
          voicings    = v0 : map (latticeBar v0) restCSs
      in normalizeByFirstRoot voicings

-- |Inflect the bar-0 lattice @v0@ onto one bar's chroma: the minimum-cost
-- cyclic-monotone bijection described at 'strataModeFlow'. Falls back to
-- @shiftBar@ when the bar's distinct-PC count differs from the lattice's.
latticeBar :: [Int] -> CadenceState -> [Int]
latticeBar v0 cs =
  let tgt = nub (sort (cadencePCs cs))
      n   = length v0
  in if null v0 || length tgt /= n
       then shiftBar v0 cs
       else
         let rotations  = [ take n (drop k (cycle tgt)) | k <- [0 .. n - 1] ]
             candidates = map (realizeAgainst v0) rotations
             cost v     = sum (zipWith (\y a -> abs (y - a)) v v0)
             net v      = abs (sum (zipWith (-) v v0))
             score v    = (cost v, net v)
         in minimumBy (compare `on` score) candidates

-- |Monotone realisation of an assigned PC sequence against the anchor:
-- slot 0 takes the pitch with its PC nearest the anchor's slot 0 (ties
-- resolve upward); each later slot takes the smallest pitch above its
-- predecessor carrying its PC. Output is strictly ascending and spans
-- less than an octave, like the anchor itself.
realizeAgainst :: [Int] -> [Int] -> [Int]
realizeAgainst v0 pcs = case (v0, pcs) of
  (a0 : _, p0 : ps) ->
    let d  = (p0 - a0) `mod` 12
        y0 = if d <= 6 then a0 + d else a0 + d - 12
        step prev p = prev + 1 + ((p - (prev + 1)) `mod` 12)
    in scanl step y0 ps
  _ -> []

-- |Build a bar's natural compressed-ascending voicing rooted on its own
-- harmonic root, then choose the uniform octave shift that maximises
-- exact-MIDI common tones with (then minimises distance to) the bar 0
-- anchor @v0@. The fallback path for bars whose cardinality differs from
-- the lattice's; @latticeBar@ handles the normal case.
shiftBar :: [Int] -> CadenceState -> [Int]
shiftBar v0 cs =
  let nextPCs    = cadencePCs cs
      nextRootPC = case nextPCs of (p:_) -> p; [] -> 0
      natural    = initialCompact nextRootPC nextPCs
      candidates = [ map (+ (k * 12)) natural | k <- [-3 .. 3] ]
      -- Primary metric: exact-MIDI common tones with the anchor — the
      -- pedal property (tones shared between bars hold their register,
      -- chosen rather than lucky under root motion). Tie-break: minimal
      -- aligned distance; 'alignVoices' handles bars whose cardinality
      -- differs from the anchor's (the old zipWith silently truncated).
      overlap v  = length (filter (`elem` v0) v)
      dist v     = let (a, b) = alignVoices v v0
                   in sum (zipWith (\x y -> abs (x - y)) a b)
      score v    = (negate (overlap v), dist v)
  in minimumBy (compare `on` score) candidates

-- |Scale-cluster routing guard for the cyclic DP. The cost driver is the
-- DENSITY of big bars, not their presence — the DP's per-edge work is the
-- product of adjacent bars' voicing candidates. Measured (bytecode,
-- 16 bars): uniform 5-PC 2.3 s (always allowed); two 6-PC bars among
-- 4-note harmony 1.85 s — CHEAPER than the allowed baseline, so a jazz
-- progression is no longer re-routed by one 13th chord; adjacent 7-PC
-- pair 10.3 s; uniform 6-PC 15.9 s; uniform 7-PC ≈ 107 s. Routing rule:
-- any 7-PC bar (a full mode — lattice semantics are the right tool for it
-- musically as well as computationally), or 6-PC bars exceeding a quarter
-- of the progression (cluster-dominant material). genP chroma layers
-- route by provenance in Bridge before reaching here.
hasBigChroma :: Progression -> Bool
hasBigChroma progArg =
  let sizes = map (length . cadenceIntervals . stateCadence)
                  (toList (unProgression progArg))
  in any (>= 7) sizes || 4 * length (filter (>= 6) sizes) > length sizes

-- |Read a CadenceState's absolute PCs in cadence-interval order (NOT sorted).
-- For genP strata\/mode layers (intervals start at 0 from harmonic root), this
-- yields [root, root+2nd, root+3rd, ...]. Head = the bar's root, which seeds
-- bar 0's lattice in 'strataModeFlow'; later bars sort and inflect, so a
-- pattern index tracks its lattice slot, not the bar root.
cadencePCs :: CadenceState -> [Int]
cadencePCs cs =
  let r    = unPitchClass (pitchClass (stateCadenceRoot cs))
      ints = map unPitchClass (cadenceIntervals (stateCadence cs))
  in [ (i + r) `mod` 12 | i <- ints ]

-- Helper to get literal voicings as Integer lists (internal use).
-- Reads cadence intervals directly so non-triad CadenceStates (5-PC strata,
-- 7-PC mode in genP-derived ProgressionContexts) survive without toTriad
-- reduction. For 3-PC triad cadences this produces the same PCs as the
-- legacy chordIntervals path.
literalVoicing' :: Progression -> [[Integer]]
literalVoicing' (Progression sq) =
  map cadenceVoicing (toList sq)
  where
    cadenceVoicing cs =
      let rootPC = unPitchClass (pitchClass (stateCadenceRoot cs))
          tones = cadenceIntervals (stateCadence cs)
          pcs   = map (\t -> (unPitchClass t + rootPC) `mod` 12) tones
      in map fromIntegral pcs

-------------------------------------------------------------------------------
-- Explicit Progression Construction
-------------------------------------------------------------------------------

-- |Name a chord from its zero-form intervals.
-- Uses legacy chord naming logic (toFunctionality for 3-note chords,
-- toFunctionalityChord for extended harmonies).
nameChord :: [Int] -> String
nameChord intervals
  | length intervals == 3 =
      toFunctionality (map mkPitchClass intervals)
  | otherwise =
      toFunctionalityChord (map mkPitchClass intervals)

-- |Construct a Progression from explicit pitch-class sets.
-- This is the main function for composing\/arranging workflow (not generation).
-- Takes an enharmonic spelling and a list of chord pitch-class sets,
-- returns a Progression ready for 'Harmonic.Interface.Tidal.Bridge.arrange'.
--
-- Example:
-- @
-- fromChords [[0,4,7], [5,9,0], [7,11,2]]
--   --> C major → F major → G major
-- @
fromChords :: [[Int]] -> ProgressionContext
fromChords = PC.fromProgression . fromChordsRaw

fromChordsRaw :: [[Int]] -> Progression
fromChordsRaw [] = mempty
fromChordsRaw chordSets = Progression (Seq.fromList cadenceStates)
  where
    -- Spelling continuity: while the root pitch class stands still, the
    -- spelling stands still. Per-bar inference alone can flip enharmonic
    -- side between bars that share a root when an upper tone changes;
    -- holding the side over a stationary root keeps one region of a
    -- progression on one accidental system. The first bar infers freely;
    -- later bars adopt the previous spelling when the root is unchanged
    -- or the pitch content is enharmonically ambiguous.
    cadenceStates = go Nothing chordSets
      where
        go _ [] = []
        go prev (pcs : rest) =
          let cs = toCadenceState prev pcs
              rootPC = (`mod` 12) (case pcs of { [] -> 0; (p : _) -> p })
          in cs : go (Just (rootPC, stateSpelling cs)) rest

    toCadenceState :: Maybe (Int, EnharmonicSpelling) -> [Int] -> CadenceState
    toCadenceState prev pcs =
      let rootInt = case pcs of { [] -> 0; (p : _) -> p }
          rootPC = mkPitchClass rootInt
          -- Dedup: pitch-class sets carry no duplicates (matches
          -- mkCadenceStatePCs; a duplicated PC would otherwise reach the
          -- voicing paths as a phantom voice).
          intervals = nub $ sort $ map (\p -> (p - rootInt) `mod` 12) pcs
          intervalPCs = map mkPitchClass intervals
          chordName = nameChord intervals
          -- Create Cadence with record syntax
          cadence = Cadence
            { cadenceFunctionality = chordName
            , cadenceMovement = Unison  -- Placeholder (no prior context)
            , cadenceIntervals = intervalPCs
            }
          absPCs = map (`mod` 12) pcs
          spelling = case prev of
            Just (prevRoot, prevSpelling)
              | prevRoot == rootInt `mod` 12       -> prevSpelling
              | isAmbiguousPattern absPCs          -> prevSpelling
            _                                      -> inferSpelling absPCs
          rootNote = enharmonicFunc spelling rootPC
       in CadenceState cadence rootNote spelling

-- |Legacy alias for fromChords (matches legacy prog function)
prog :: [[Int]] -> ProgressionContext
prog = fromChords

-------------------------------------------------------------------------------
-- Scale Source (Switch Mechanism)
-------------------------------------------------------------------------------

-- |Scale source for melody mapping.
-- Enables flexible melody construction by allowing harmony (with optional
-- overlap) to serve as the scale source instead of explicit scale definitions.
data ScaleSource
  = ExplicitScale [[Int]]           -- ^ User-defined scale per chord
  | HarmonyAsScale Progression      -- ^ Use harmony chords as scales
  | HarmonyWithOverlap Progression (Int -> Progression -> Progression)
    -- ^ Use harmony with overlap function applied

-- |Create melody state from scale source.
-- Converts a ScaleSource into a Progression suitable for melody arrangement.
melodyStateFrom :: ScaleSource -> Progression
melodyStateFrom (ExplicitScale scales) = fromChordsRaw scales
melodyStateFrom (HarmonyAsScale progArg) = progArg  -- Direct passthrough
melodyStateFrom (HarmonyWithOverlap progArg overlapFn) = overlapFn 1 progArg

-------------------------------------------------------------------------------
-- Starting State Construction
-------------------------------------------------------------------------------

-- All unique 3-note zero-form sets: [0, a, b] with 1 ≤ a < b ≤ 11 (55 total)
allTriadZeroForms :: [[Int]]
allTriadZeroForms = [[0, a, b] | a <- [1..10], b <- [a+1..11]]

-- Map from quality name → all sets producing that name, sorted by dissonance
qualityMap :: Map.Map String [[Int]]
qualityMap =
  Map.map (sortBy (comparing dissonanceScore))
  $ Map.fromListWith (++)
    [ (name, [zf])
    | zf <- allTriadZeroForms
    , let name = toFunctionality (map mkPitchClass zf)
    , not (null name)
    ]

-- User-friendly alias table: shorthand → interval set variants (most consonant first)
qualityAliases :: Map.Map String [[Int]]
qualityAliases = Map.fromList
  [ ("maj",  [[0,4,7]])
  , ("min",  [[0,3,7]])
  , ("dim",  [[0,3,6]])
  , ("aug",  [[0,4,8]])
  , ("7",    sortBy (comparing dissonanceScore) [[0,4,10], [0,7,10]])
  , ("dom7", sortBy (comparing dissonanceScore) [[0,4,10], [0,7,10]])
  , ("maj7", sortBy (comparing dissonanceScore) [[0,4,11], [0,7,11]])
  , ("min7", [[0,3,10]])
  , ("m7",   [[0,3,10]])
  , ("dim7", [[0,3,6]])
  , ("hdim", sortBy (comparing dissonanceScore) [[0,3,6], [0,3,10]])
  , ("sus2", [[0,2,7]])
  , ("sus4", [[0,5,7]])
  , ("6",    [[0,4,9]])
  , ("m6",   [[0,3,9]])
  ]

-- Note name parsing table: lowercase → canonical
noteNameTable :: [(String, String)]
noteNameTable =
  [ ("c","C"), ("db","Db"), ("c#","C#"), ("d","D")
  , ("eb","Eb"), ("d#","D#"), ("e","E"), ("f","F")
  , ("gb","Gb"), ("f#","F#"), ("g","G"), ("ab","Ab")
  , ("g#","G#"), ("a","A"), ("bb","Bb"), ("a#","A#")
  , ("b","B")
  ]

-- |Token type for 'parseLeadTokens'
data LeadToken = RootTok String | QualTok String | MoveTok Int
  deriving (Show, Eq)

-- Parse a movement token: "(N)" or "(-N)" → Just N
parseMovement :: String -> Maybe Int
parseMovement ('(':rest) =
  case Prelude.reverse rest of
    (')':inner) -> case reads (Prelude.reverse inner) :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing
    _ -> Nothing
parseMovement _ = Nothing

-- Classify a single token as root, movement, or quality
classifyToken :: String -> LeadToken
classifyToken tok
  | Just canonical <- lookup (map toLower tok) noteNameTable = RootTok canonical
  | Just n         <- parseMovement tok                       = MoveTok n
  | otherwise                                                 = QualTok tok

-- |Parse a lead string into a list of typed tokens.
-- Each space-separated token is independently classified as root, quality, or movement.
parseLeadTokens :: String -> [LeadToken]
parseLeadTokens = map classifyToken . words

-- Pick a variant from a sorted list, biased toward the most consonant
pickVariant :: GenIO -> String -> [[Int]] -> IO (String, [Int])
pickVariant gen label variants = do
  idx <- gammaIndexScaledWith gen 0.05 (length variants)
  pure (label, variants !! idx)

-- Resolve a quality string to (label, intervals), or fall through to random
resolveQuality :: GenIO -> Maybe String -> IO (String, [Int])
resolveQuality gen Nothing  = randomQuality gen
resolveQuality gen (Just q) = do
  let qLower = map toLower q
  case Map.lookup qLower qualityAliases of
    Just vs -> pickVariant gen q vs
    Nothing -> case Map.lookup qLower qualityMap of
      Just vs -> pickVariant gen q vs
      Nothing -> randomQuality gen

-- Select a random quality, biased toward consonant (low entropy gamma)
randomQuality :: GenIO -> IO (String, [Int])
randomQuality gen = do
  let entries = sortBy (comparing (\(_, vs) -> dissonanceScore (case vs of { (v : _) -> v; [] -> [] }))) (Map.toList qualityMap)
  idx <- gammaIndexScaledWith gen 0.2 (length entries)
  let (name, variants) = entries !! idx
  pickVariant gen name variants

-- Select a random root from the 12 chromatic notes (uniform)
randomRoot :: GenIO -> IO String
randomRoot gen = do
  let roots = ["C","C#","D","Eb","E","F","F#","G","Ab","A","Bb","B"]
  idx <- uniformRM (0, length roots - 1) gen
  pure (roots !! idx)

-- |Construct a 'CadenceState' from a human-readable string.
--
-- Parses root, quality, and movement from space-separated tokens.
-- Unspecified components fall through to randomness.
-- Prints "root quality" to the console after construction.
--
-- Examples:
-- @
-- start <- lead "E min (5)"  -- E minor, ascending 5th
-- start <- lead "E min"      -- E minor, random movement
-- start <- lead "min"        -- random root, minor quality, random movement
-- start <- lead "E"          -- E, random quality, random movement
-- start <- lead ""           -- fully random
-- start <- lead "(5)"        -- random root and quality, fixed movement 5
-- @
lead :: String -> IO CadenceState
lead input = do
  rng <- createSystemRandom
  let toks = parseLeadTokens input
      mRoot = listToMaybe [r | RootTok r <- toks]
      mQual = listToMaybe [q | QualTok q <- toks]
      mMove = listToMaybe [m | MoveTok m <- toks]
  rootStr          <- maybe (randomRoot rng) pure mRoot
  (qualLabel, ivs) <- resolveQuality rng mQual
  movement         <- maybe (uniformRM (-5, 6) rng) pure mMove
  let cs = initCadenceState movement rootStr ivs
  putStrLn $ rootStr ++ " " ++ qualLabel
  pure cs

-- |Construct a 'CadenceState' from an explicit list of note names —
-- the arbitrary-cardinality counterpart to 'lead'. The first note is the
-- root\/bass; the rest become root-relative intervals (any count, so
-- hand-built 4-note cues and beyond are first-class). Never truncates:
-- builds via 'mkCadenceStatePCs', so all pitch content survives into the
-- cue. Enharmonics follow the typed accidentals ("Eb" spells flat,
-- "D#" sharp; double accidentals accepted and resolved). An optional
-- @(N)@ token fixes the approach movement, otherwise it is randomized
-- exactly like 'lead'. Unrecognized tokens are reported and skipped;
-- with no valid notes at all, falls back to fully random 'lead'.
--
-- Examples:
-- @
-- start <- lead' "Eb Gb Bb Db"      -- Eb m7, random movement
-- start <- lead' "A C E G (5)"      -- A m7, ascending 5th approach
-- start <- lead' "C E G"            -- plain triad, same as lead "C maj"
-- @
lead' :: String -> IO CadenceState
lead' input = do
  rng <- createSystemRandom
  let toks       = words input
      mMove      = listToMaybe [ n | t <- toks, Just n <- [parseMovement t] ]
      noteToks   = [ t | t <- toks, parseMovement t == Nothing ]
      parsed     = [ (t, Filter.noteNameToPitchClass (T.pack t)) | t <- noteToks ]
      badToks    = [ t | (t, Nothing) <- parsed ]
      notes      = [ (t, p) | (t, Just p) <- parsed ]
  mapM_ (\t -> putStrLn ("lead': unrecognized note name '" ++ t ++ "' (skipped)")) badToks
  case notes of
    [] -> lead ""
    ((rootTok, rootPC) : _) -> do
      movement <- maybe (uniformRM (-5, 6) rng) pure mMove
      let rootInt   = fromIntegral rootPC :: Int
          -- typed accidental drives the root's enharmonic identity
          rootName  = if 'b' `elem` drop 1 (map toLower rootTok)
                        then flat (mkPitchClass rootInt) else sharp (mkPitchClass rootInt)
          intervals = [ (fromIntegral p - rootInt) `mod` 12 | (_, p) <- notes ]
          cs        = mkCadenceStatePCs rootName
                        (toMovement (P 0) (mkPitchClass movement)) intervals
      putStrLn $ show rootName ++ " " ++ cadenceFunctionality (stateCadence cs)
      pure cs

-- |Construct a 'CadenceState' from a leadsheet chord symbol — the jazz
-- counterpart to 'lead', for cueing 'Harmonic.Framework.Builder.genJ'.
-- Accepts the Bunks corpus grammar: root, quality, optional slash bass
-- ("Cm7", "EbM7", "G7b9#11", "Dm7\/G"). Qualities occupy the jazz
-- namespace ('Harmonic.Rules.Import.Jazz.qualityIntervals'), entirely
-- separate from 'lead''s triadic quality table, so triadic behaviour is
-- untouched. A notated slash bass is honoured exactly: unioned into the
-- pitch-class set and made the anchor, so "Dm7\/G" cues from G with the
-- full Dm7 sounding above it. An optional @(N)@ token fixes the approach
-- movement, otherwise it is randomized exactly like 'lead'; either way
-- the movement shapes bar 1's stored metadata only — a fresh walk
-- departs from the functionality's hub node regardless. An
-- unparseable symbol is reported and falls back to the corpus workhorse
-- C m7.
--
-- Examples:
-- @
-- start <- leadJ "Cm7"        -- C m7, random movement
-- start <- leadJ "Dm7\/G"     -- G anchor, Dm7 above (a 9sus4 set)
-- start <- leadJ "EbM7 (5)"   -- Eb maj7, ascending 5th approach
-- @
leadJ :: String -> IO CadenceState
leadJ input = do
  rng <- createSystemRandom
  let toks    = words input
      mMove   = listToMaybe [ n | t <- toks, Just n <- [parseMovement t] ]
      syms    = [ t | t <- toks, parseMovement t == Nothing ]
      moveTok = concat [ " (" ++ show n ++ ")" | Just n <- [mMove] ]
  case syms of
    (sym : _) | Right (J.Sounding jc) <- J.parseToken (T.pack sym) -> do
      movement <- maybe (uniformRM (-5, 6) rng) pure mMove
      -- the typed accidental of the anchor segment drives its spelling
      let anchorTok  = case break (== '/') sym of
            (_, '/' : b) -> b
            (r, _)       -> takeWhile (`elem` ("ABCDEFG#b" :: String)) (take 2 r)
          anchorName = if 'b' `elem` drop 1 anchorTok
                         then flat (J.jcAnchor jc) else sharp (J.jcAnchor jc)
          intervals  = J.jazzZeroForm jc
          cs0        = mkCadenceStatePCs anchorName
                         (toMovement (P 0) (mkPitchClass movement)) intervals
          name       = maybe (cadenceFunctionality (stateCadence cs0)) T.unpack
                         (J.jazzFunctionality intervals)
          cs         = cs0 { stateCadence =
                               (stateCadence cs0) { cadenceFunctionality = name } }
      putStrLn $ show anchorName ++ " " ++ name
      pure cs
    _ -> do
      let reason = case syms of
            []       -> "no chord symbol"
            (s0 : _) -> case J.parseToken (T.pack s0) of
              Left r  -> T.unpack (J.refusalReason r) ++ " in '" ++ s0 ++ "'"
              Right _ -> "NC is silence, not a chord"
      putStrLn $ "leadJ: " ++ reason ++ " — cueing C m7"
      leadJ ("Cm7" ++ moveTok)
