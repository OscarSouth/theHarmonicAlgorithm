-- |
-- Module      : Harmonic.Traversal.WalkingBass
-- Description : Three-pass walking-bass line generator with derived entropy
--
-- Produces a walking-bass line for a cyclic Progression as a pure function
-- of (progression, voiceFn) via three sequential passes:
--
--   1. Pass 1 — beat 1s. Every bar's beat 1 is a singleton PC derived from
--      the user-supplied VoiceFunction ('fund' or 'root'). Octave placement
--      is greedy left-to-right by smoothness to the previous beat 1, with a
--      soft direction-persistence bias, a half-weight loop-closure pull on
--      the final bar, and a root-fifth alternation option inside runs of
--      consecutive identical chords. Bar 0 anchors at the register centre.
--   2. Pass 2 — beat 3s. Per bar, choose a chord tone minimising smoothness
--      to b1_i plus smoothness to b1_{i+1} plus consonance-to-fund cost
--      (via 'beat3ConsTable'). Unison repeats carry a dedicated penalty
--      so a non-repeat chord tone wins when nearby. Symmetric chords
--      (dim / aug / dim7 / whole-tone) bypass the consonance term since no
--      chord tone is privileged in a rotation-invariant shape.
--   3. Pass 3 — beats 2 and 4. Per beat, choose from a (cyclic) local-scale
--      pool. Connector heuristics favour chord tones adjacent to the next
--      beat 1, penalise copying the next beat 1 outright on beat 4, reward
--      chromatic approaches to the next beat 1 regardless of scale/chord
--      membership (so root-as-leading-tone can win), resolve static
--      (b1==b3) bars via the chord's P5 or — on symmetric chords — any
--      non-root chord tone, and reward root-on-b4 (full weight) or P5-on-b4
--      (half weight) when that tone sits 1–2 semitones from the next b1.
--      If b3 already used the root / P5 at tone distance, the approach
--      bonus shifts to the chromatic in-between tone so the line avoids a
--      b3→b4 unison. Stranded connectors (neither flank within a whole
--      step) pay a sandwich penalty; quality-defining chord tones are
--      reserved for strong beats (only root/P5 earn the beat-4 chord-tone
--      bonus).
--
-- The entropy parameter that used to be user-facing is now derived from the
-- progression's root-motion angularity and chord-internal dissonance. Calm
-- diatonic progressions land near 0 (more settled repeats allowed); angular
-- tritone-heavy progressions land near 1 (fewer repeats, more chromatic
-- motion). Progression-level consonance ('progConsonance') independently
-- scales strong-beat strictness and connector tension licence. Pure-function
-- guarantees are preserved: same progression and voiceFn always produce the
-- same line.

{-# LANGUAGE MultiWayIf #-}
module Harmonic.Traversal.WalkingBass
  ( -- * Main entry
    walkLine
  , walkLineP
  , walkLineDyn
  , walkLinePDyn
  , ChromaSources(..)

    -- * Derived entropy (exported for tests / diagnostics)
  , progressionEntropy
  , progConsonance
  , inferKeyCentre

    -- * Utilities (exported for tests)
  , hashProgEntropy
  , closestLowMidi
  , closestMidMidi
  , lowestMidi
  , highestMidi
  , beatsPerBar
  , isSymmetricChord
  ) where

import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl', minimumBy)
import Data.Function (on)
import Data.Bits (xor)
import Data.Foldable (toList)
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import System.Random (mkStdGen, randoms)

import qualified Harmonic.Rules.Types.Pitch as Pt
import qualified Harmonic.Rules.Types.Harmony as Hm
import qualified Harmonic.Rules.Types.Progression as Pr
import Harmonic.Evaluation.Scoring.Dissonance (rootMotionScore, dissonanceScore)
import Harmonic.Interface.Tidal.Bridge (VoiceFunction)

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

lowestMidi, highestMidi, beatsPerBar, registerCenter :: Int
lowestMidi     = 28
highestMidi    = 48
beatsPerBar    = 4
registerCenter = 38

-- | Pass 3 repeat-gate costs (unchanged from v3).
kappaStaticBase, kappaStaticBlocked :: Int
kappaStaticBase    = 3
kappaStaticBlocked = 200

-- | Pass 3 scale-fit and chromatic-approach weights.
kappaChromatic, kappaChromaticBonus, kappaChromaticBonusBeat4 :: Int
kappaChromatic           = 8
kappaChromaticBonus      = 5
kappaChromaticBonusBeat4 = 10

-- | Pass 2 beat-3 consonance multiplier over 'beat3ConsTable'. Calibrated so
-- an adjacent 3rd overcomes a P5 whose combined smoothness cost is more than
-- ~6 semitones worse, while 7ths and tension tones stay reachable only when
-- the consonant options all demand large leaps.
kappaB3Consonance :: Int
kappaB3Consonance = 2

-- | Beat-3 anchor cost by interval above the bar's fundamental. The strong
-- beat wants the chord's most grounding tones: P5 first, then root/octave,
-- then the quality-defining 3rds; 7ths and colour tones cost enough that
-- they surface mainly when register pressure or an imminent chord change
-- leaves no cheap consonant option.
beat3ConsTable :: Int -> Int
beat3ConsTable iv = case iv `mod` 12 of
  7             -> 0    -- perfect fifth
  0             -> 2    -- root / octave
  3             -> 3    -- minor third
  4             -> 3    -- major third
  5             -> 5    -- fourth / eleventh
  10            -> 6    -- minor seventh
  11            -> 6    -- major seventh
  6             -> 12   -- tritone
  _             -> 9    -- seconds and sixths

-- | Pass 2 repeat penalty. Strong enough that a unison b1=b3 repeat loses to
-- the P5 (or another consonant chord tone) whenever one sits within a fifth,
-- keeping static-harmony bars in motion.
kappaPassiveRepeat :: Int
kappaPassiveRepeat = 9

-- | Pass 3 connector enrichment weights (new in iteration 4).
kappaDiatonicApproach, kappaChordToneBonus, kappaCopyNext, kappaStaticRecovery :: Int
kappaDiatonicApproach = 3   -- whole-step in-scale approach to target
kappaChordToneBonus   = 7   -- chord tone within 2 semitones of next beat 1 (beat 4 only)
kappaCopyNext         = 15  -- beat 4 MIDI equals next bar's beat 1 MIDI
kappaStaticRecovery   = 10  -- P5 of bar's fundamental when b1==b3 this bar

-- | Pass 3 approach bonus (iteration 6). Full strength for root, half for P5.
-- Fires on beat 4 when the target PC is 1–2 semitones from next b1, or when
-- b3 already used the target and m is the chromatic in-between. Calibrated
-- (32) to overcome squared-smoothness overshoot when the root sits 5 above
-- b3 at tone distance from next b1 (desc-tone middle bars).
kappaRootApproach :: Int
kappaRootApproach = 32

-- | Pass 1 dynamics-arc weight (in the doubled scoring units of
-- 'pass1Beat1s'). Per-bar dynamic levels are mean-centred over the walked
-- period: bars quieter than the piece's own mean bias the beat-1 contour
-- UPWARD, louder bars DOWNWARD, with penalty up to this weight on
-- opposing candidates at the dynamic extremes. Mean-centring makes a
-- constant dynamic exactly neutral, so the coupling shapes contour against
-- a piece's swell without dragging a flat-dynamic line anywhere. A bar
-- whose level falls 0.25+ below its predecessor instead RESETS beat 1 to
-- the lowest register instance (the line falls with the drop).
kappaDynArc :: Int
kappaDynArc = 4

-- | Pass 1 direction-persistence bonus (in the doubled scoring units of
-- 'pass1Beat1s'). A candidate continuing the previous beat-1 step's
-- direction by a step-or-third earns this small discount, so the beat-1
-- contour tends to run in lines rather than oscillate — while never
-- overriding a nearest candidate that is more than a third closer.
kappaB1Direction :: Int
kappaB1Direction = 2

-- | Pass 3 sandwich penalty: a connector with NEITHER flanking strong beat
-- within a whole step is a stranded tone; it pays this on top of its
-- smoothness cost. Willis's sandwich rule as a strong preference rather
-- than a hard constraint. Chord tones are exempt — a tone of the sounding
-- chord is never stranded.
kappaSandwich :: Int
kappaSandwich = 4

-- | Pass 3 beat-2 chord-tone bonus. Beat 2 prefers the bar's own chord
-- tones (the quality-defining 3rd especially) over scale or approach
-- tones, keeping the first half of the bar inside the sounding harmony.
kappaB2ChordTone :: Int
kappaB2ChordTone = 5

-- | Pass 3 dominant-fall bonus: on beat 4, the bar's ROOT when the next
-- bar's root lies a fourth above (dominant relation) — the classic V-I
-- fourth-fall approach, rewarded at close to the chromatic root-approach
-- weight so it can overcome the squared-smoothness cost of the leap.
kappaDominantFall :: Int
kappaDominantFall = 30

-- | Pass 2 anticipation penalty: beat 3 landing on exactly the next bar's
-- beat-1 MIDI robs the arrival; discouraged, not banned.
kappaB3Anticipate :: Int
kappaB3Anticipate = 3

-- | Pass 3 fourth-chain bonus. A connector that is a CHORD TONE a perfect
-- fourth/fifth from its left flank, and that resolves onward (by step,
-- chromatic, or another fourth), traces a cycle-of-fifths route to the
-- target. Tie-breaker weight only: squared smoothness prices the fourth
-- leap at 25+, so this decides between near-equal candidates but never
-- drives one. Full chains through beats 2-3-4 need beat 3 to participate,
-- which the pass ordering forbids — that belongs to a future bar-shape
-- (lane) pass.
kappaFourthChain :: Int
kappaFourthChain = 4

-- | Pass 3 mid-phrase damping. The beat-4 repeat-push (beat 4 repeating
-- beat 3, typically a doubled leading tone) is maximal tension: favoured
-- into a 4-bar phrase top, surcharged mid-phrase where it would make an
-- interior bar feel like a phrase start. The 4-bar grid anchors at the
-- (performed) progression's bar 0.
kappaPhraseMid :: Int
kappaPhraseMid = 6

-- | Pass 2 non-chord surcharge. Beat 3's pool admits regional-key tones
-- (the bar's stratum in the genP path) beyond the chord tones, but they
-- pay this on top of their 'beat3ConsTable' cost — surfacing only where
-- every consonant chord tone would force a leap or an anticipation.
kappaB3NonChord :: Int
kappaB3NonChord = 8

-- | Pass 1 duplicate-run weights. Within a run of consecutive identical
-- chords, an odd occurrence's beat 1 admits the fundamental's P5 as a soft
-- alternative: repeating the previous beat-1 MIDI pays 'kappaB1DupRepeat',
-- a P5 candidate pays 'kappaB1P5Option'. Calibrated so a P5 within a fifth
-- of the previous beat 1 wins (runs walk root-fifth-root-fifth) while a P5
-- only reachable by upward leap loses to the held root.
kappaB1DupRepeat, kappaB1P5Option :: Int
kappaB1DupRepeat = 10
kappaB1P5Option  = 2

-- | Pass 3 Minor Thirds Rule bonus. A minor-third gap between a connector's
-- flanks admits exactly two passing tones: the regional-key diatonic one
-- (strata tone in the genP path) earns the full bonus; the chromatic one an
-- entropy-scaled fraction, so low-entropy material prefers the diatonic fill
-- and high-entropy material may take the chromatic. A major-third gap
-- prefers its balanced whole-step passing tone at full bonus.
kappaMinorThird :: Int
kappaMinorThird = 4

-- | Pass 3 octatripentatonic tier weights ('walkLineP' only).
-- Strata candidates win the tier-fit term (most preferred), overlap is
-- neutral (matching legacy in-scale fit cost = 0), mode is a mild penalty
-- above neutral but still admissible (plays the role chromatic plays in
-- 'walkLine'). Calibrated so smoothness can override a tier-mismatch on
-- adjacent candidates, but a strata pick wins over an equally-smooth
-- overlap pick, and overlap wins over an equally-smooth mode pick.
kappaStrataPref, kappaModePenalty :: Int
kappaStrataPref  = 4   -- bonus (subtracted)
kappaModePenalty = 6   -- penalty (added)

-------------------------------------------------------------------------------
-- Seed derivation
-------------------------------------------------------------------------------

-- | Deterministic mixing of progression shape and entropy into a seed.
hashProgEntropy :: Pr.Progression -> Double -> Int
hashProgEntropy prog e = progHash `xor` entropyHash
  where
    css       = toList (Pr.unProgression prog)
    progHash  = foldl' step 0 css
    step h cs =
      let r   = Pt.unPitchClass (Pt.pitchClass (Hm.stateCadenceRoot cs))
          ivs = sum (map Pt.unPitchClass (Hm.cadenceIntervals (Hm.stateCadence cs)))
      in h * 31 + r * 13 + ivs
    entropyHash =
      let w = castDoubleToWord64 e :: Word64
      in fromIntegral (w `xor` (w `div` 4294967296))

-------------------------------------------------------------------------------
-- Per-bar metadata
-------------------------------------------------------------------------------

rootPCInt :: Hm.CadenceState -> Int
rootPCInt = Pt.unPitchClass . Pt.pitchClass . Hm.stateCadenceRoot

chordPCs :: Hm.CadenceState -> Set Int
chordPCs cs =
  let r   = rootPCInt cs
      ivs = map Pt.unPitchClass (Hm.cadenceIntervals (Hm.stateCadence cs))
  in Set.fromList [ (r + iv) `mod` 12 | iv <- ivs ]

-- | True iff the chord contains no perfect-fourth/fifth (5 or 7 semitones)
-- between any pair of tones. Covers diminished triads ([0,3,6]), augmented
-- triads ([0,4,8]), diminished sevenths ([0,3,6,9]), and whole-tone
-- hexachords — synthetic shapes with no privileged fifth, where every chord
-- tone is equally anchor-worthy.
isSymmetricChord :: Set Int -> Bool
isSymmetricChord s =
  Set.size s >= 3 &&
  all (\(a, b) -> let d = abs (a - b) `mod` 12
                  in d /= 5 && d /= 7)
      [ (a, b) | a <- Set.toList s, b <- Set.toList s, a < b ]

-- | Cyclic union of the previous, current, and next bar's chord-PC sets.
-- Loop-closure consistent: bar 0's prev is bar n-1; bar n-1's next is bar 0.
localScale :: V.Vector (Set Int) -> Int -> Set Int
localScale chordPCsV i =
  let n    = V.length chordPCsV
      prev = chordPCsV V.! ((i - 1) `mod` n)
      curr = chordPCsV V.!  i
      next = chordPCsV V.! ((i + 1) `mod` n)
  in prev `Set.union` curr `Set.union` next

-- | Lowest MIDI in [lowestMidi, highestMidi] whose pitch class equals 'pc'.
-- Always in range: the register spans more than an octave, so the lowest
-- PC-matching value sits at most 11 semitones above 'lowestMidi'.
closestLowMidi :: Int -> Int
closestLowMidi pc =
  let pc' = pc `mod` 12
  in lowestMidi + ((pc' - lowestMidi) `mod` 12)

-- | In-register instance of 'pc' nearest 'registerCenter' (lower on tie).
-- Anchoring bar 0 here gives the greedy beat-1 chain headroom in both
-- directions and evens out the single-instance asymmetry of the PCs that
-- occur only once in the register.
closestMidMidi :: Int -> Int
closestMidMidi pc =
  let lo    = closestLowMidi pc
      cands = takeWhile (<= highestMidi) [lo, lo + 12 ..]
  in minimumBy
       (\a b -> compare (abs (a - registerCenter), a)
                        (abs (b - registerCenter), b))
       cands

-- | All MIDI values in the register whose pitch class is in the given set.
midisIn :: Set Int -> V.Vector Int
midisIn s =
  V.fromList [ m | m <- [lowestMidi..highestMidi], (m `mod` 12) `Set.member` s ]

-------------------------------------------------------------------------------
-- Derived entropy
-------------------------------------------------------------------------------

-- | Entropy derived from the progression's harmonic character. Calm diatonic
-- progressions land near 0; angular / tritone-heavy progressions approach 1.
-- Deterministic: same progression always yields the same value.
progressionEntropy :: Pr.Progression -> Double
progressionEntropy prog
  | n == 0    = 0.0
  | otherwise = max 0 (min 1 (base + jitter))
  where
    bars     = V.fromList (toList (Pr.unProgression prog))
    n        = V.length bars

    -- Root-motion angularity: mean rootMotionScore across cyclic transitions.
    -- rootMotionScore range is [1, 6]; normalise to [0, 1].
    rootPCs  = V.map rootPCInt bars
    motions  = [ rootMotionScore
                   ((rootPCs V.! ((i + 1) `mod` n) - rootPCs V.! i) `mod` 12)
               | i <- [0 .. n - 1] ]
    meanMot  = fromIntegral (sum motions) / fromIntegral n :: Double
    normMot  = max 0 (min 1 ((meanMot - 1) / 5))

    -- Chord-internal dissonance: mean dissonanceScore over bars. Major and
    -- minor triads score 6; every seventh chord scores 19+ and saturates the
    -- /20 cap, so on all-tetrad progressions this term is a near-constant
    -- offset and root-motion angularity dominates the derived entropy.
    chordDiss = [ fromIntegral (dissonanceScore
                    (rootPCInt cs
                      : [ (rootPCInt cs + iv) `mod` 12
                        | iv <- map Pt.unPitchClass
                                  (Hm.cadenceIntervals (Hm.stateCadence cs)) ]))
                | cs <- V.toList bars ] :: [Double]
    meanDiss  = sum chordDiss / fromIntegral n
    normDiss  = min 1.0 (meanDiss / 20.0)

    base      = 0.70 * normMot + 0.30 * normDiss
    jitter    = (seededUniform (hashProgEntropy prog 0) 0 - 0.5) * 0.1

-- | Mean per-bar consonance of the progression in [0, 1], 1 = consonant.
-- 'dissonanceScore' grows with chord cardinality (major/minor triads score
-- 6 while the most consonant tetrads score 19), so each bar is normalised
-- against anchors for its own chord size before averaging — otherwise every
-- tetrad progression would read as maximally dissonant. Consumed by the
-- walk to scale strong-beat strictness and connector tension licence;
-- orthogonal to 'progressionEntropy', which is dominated by root motion.
progConsonance :: Pr.Progression -> Double
progConsonance prog
  | null css  = 1.0
  | otherwise = sum barVals / fromIntegral (length barVals)
  where
    css     = toList (Pr.unProgression prog)
    barVals = map barCons css
    -- Score the root-position interval set (root at 0), not absolute PCs:
    -- 'dissonanceScore' privileges a perfect fifth above its lowest tone,
    -- so an absolute-PC set would score the same chord differently
    -- depending on which pitch class happens to sort lowest.
    barCons c =
      let pcs = Set.toList (Set.fromList
                  [ Pt.unPitchClass iv `mod` 12
                  | iv <- Hm.cadenceIntervals (Hm.stateCadence c) ])
          d   = fromIntegral (dissonanceScore pcs) :: Double
      in case length pcs of
           n | n <= 2    -> 1.0
             | n == 3    -> clamp01 (1 - (d - 6)  / 26)
             | n == 4    -> clamp01 (1 - (d - 19) / 36)
             | otherwise -> clamp01 (1 - (d - 30) / 60)
    clamp01 = max 0 . min 1

-------------------------------------------------------------------------------
-- Regional key inference (walk-internal)
-------------------------------------------------------------------------------

-- | Per-bar regional key centre, inferred from chord qualities over a local
-- window.
--
-- BOUNDARY: this is a PURE, DERIVED quantity computed over a finished
-- progression, consumed only inside the walk's connector selection. The
-- generation system deliberately operates without key awareness — each
-- state is abstract and deterministic, upholding the Markov property —
-- so this function must never feed back into generation, become part of
-- any state, or appear in a generation-path signature.
--
-- Heuristics (weights in votes): a dominant-quality chord is V of its key;
-- a major-quality chord is I (strong) or IV (weak), plain major triads also
-- V (weak); a minor-quality chord is ii (strong), vi, or iii (weak);
-- half-diminished is vii. Relative major/minor are treated as one pool and
-- reported as the major-pool pitch class. Each bar takes the key with the
-- highest vote total over the surrounding window (cyclic, +/- 2 bars);
-- ties resolve to the lowest pitch class.
inferKeyCentre :: Pr.Progression -> [Pt.PitchClass]
inferKeyCentre prog =
  [ Pt.mkPitchClass (bestFor i) | i <- [0 .. n - 1] ]
  where
    bars = V.fromList (toList (Pr.unProgression prog))
    n    = V.length bars

    -- (key offset from chord root, votes) per quality class.
    votesFor cs =
      let r   = rootPCInt cs
          ivs = Set.fromList
                  [ Pt.unPitchClass iv `mod` 12
                  | iv <- Hm.cadenceIntervals (Hm.stateCadence cs) ]
          has = (`Set.member` ivs)
          offsets
            | has 4 && has 10            = [(5, 6)]                    -- V7
            | has 4 && has 11            = [(0, 4), (7, 2)]            -- Imaj7 / IVmaj7
            | has 3 && has 6             = [(1, 4)]                    -- vii (half-dim pool)
            | has 3 && has 10            = [(10, 4), (3, 3), (8, 2)]   -- ii / vi / iii
            | has 3                      = [(10, 4), (3, 3), (8, 2)]   -- minor triad
            | has 4                      = [(0, 4), (7, 2), (5, 2)]    -- major triad: I / IV / V
            | otherwise                  = []
      in [ ((r + off) `mod` 12, w) | (off, w) <- offsets ]

    -- Distinct bar indices only: on progressions shorter than the window
    -- the cyclic wrap must not count any bar's votes twice.
    windowVotes i =
      let idxs = Set.toList (Set.fromList [ (i + d) `mod` n | d <- [-2 .. 2] ])
      in concat [ votesFor (bars V.! j) | j <- idxs ]

    bestFor i =
      let votes     = windowVotes i
          total key = sum [ w | (k, w) <- votes, k == key ]
      in snd (minimum [ (negate (total key), key) | key <- [0 .. 11 :: Int] ])

-------------------------------------------------------------------------------
-- Pass 1 — Beat 1s (skeleton)
-------------------------------------------------------------------------------

-- | Extract per-bar beat-1 PC from the supplied voice function. Falls back
-- to the cadence-state root PC if the voice function returns [] for a bar.
beat1PCs :: VoiceFunction -> Pr.Progression -> V.Vector Int
beat1PCs voiceFn prog =
  let voicings = voiceFn prog
      barsL    = toList (Pr.unProgression prog)
      n        = length barsL
      pcAt i   =
        let cs = barsL !! i
        in if i < length voicings
           then case voicings !! i of
                  []    -> rootPCInt cs
                  (x:_) -> x `mod` 12
           else rootPCInt cs
  in V.fromList [ pcAt i | i <- [0 .. n - 1] ]

-- | Occurrence parity within runs of consecutive identical bars: True for
-- the 2nd, 4th, ... bar of a run of equal 'CadenceState's, False elsewhere.
-- Detection is acyclic (bar 0 always starts a run) so the loop wrap never
-- flips a line's opening beat 1.
dupOddFlags :: V.Vector Hm.CadenceState -> V.Vector Bool
dupOddFlags barsV = V.fromList (map odd (go Nothing (V.toList barsV)))
  where
    go _ [] = []
    go mPrev (c:cs') =
      let k = case mPrev of
                Just (p, kPrev) | p == c -> kPrev + 1
                _                        -> 0
      in k : go (Just (c, k)) cs'

-- | Place each bar's beat 1. Bar 0 anchors on the register-centre instance
-- of its PC; later bars pick greedily by closeness to the previous beat 1
-- (lower MIDI on tie). Scores are in half-semitone units so the final bar
-- can add a half-weight pull toward bar 0's beat 1, closing the register
-- loop instead of leaving the whole drift to the last seam. Odd occurrences
-- within duplicate runs admit the fundamental's P5 as a soft alternative
-- (see 'kappaB1DupRepeat' / 'kappaB1P5Option').
pass1Beat1s
  :: V.Vector Double   -- mean-centred dynamic bias per bar (0 = neutral)
  -> V.Vector Bool     -- dynamic drop-reset flags per bar
  -> V.Vector Int -> V.Vector Int -> V.Vector Bool -> V.Vector Int
pass1Beat1s biasV dropV pcs p5pcs dupOdd
  | V.null pcs = V.empty
  | otherwise  =
      let n  = V.length pcs
          b0 = closestMidMidi (pcs V.! 0)
          candidates i =
            let roots = [ (m, False)
                        | m <- V.toList (midisIn (Set.singleton (pcs V.! i))) ]
                p5s   = if dupOdd V.! i
                        then [ (m, True)
                             | m <- V.toList (midisIn (Set.singleton (p5pcs V.! i))) ]
                        else []
            in roots ++ p5s
          go i prev prevStep
            | i >= n    = []
            | dropV V.! i =
                -- Sudden dynamic drop: the line falls with it, resetting
                -- to the lowest viable instance of the bar's PC.
                let pick = closestLowMidi (pcs V.! i)
                in pick : go (i + 1) pick (pick - prev)
            | otherwise =
                let closure m = if i == n - 1 then abs (m - b0) else 0
                    continues m =
                      let step = m - prev
                      in prevStep /= 0 && step /= 0 && abs step <= 4
                         && signum step == signum prevStep
                    arcPen m =
                      let step = m - prev
                          b    = biasV V.! i
                      in if step /= 0 && b /= 0
                            && fromIntegral (signum step) == negate (signum b)
                         then round (fromIntegral kappaDynArc * 2 * abs b)
                         else 0
                    score (m, isP5) =
                      2 * abs (m - prev)
                      + closure m
                      + arcPen m
                      + (if m == prev && dupOdd V.! i
                         then 2 * kappaB1DupRepeat else 0)
                      + (if isP5 then 2 * kappaB1P5Option else 0)
                      - (if continues m then kappaB1Direction else 0)
                    pick = fst (minimumBy
                                 (\a b -> compare (score a, fst a)
                                                  (score b, fst b))
                                 (candidates i))
                in pick : go (i + 1) pick (pick - prev)
      in V.fromList (b0 : go 1 b0 0)

-- | Expand an optional per-bar dynamic vector into (mean-centred bias,
-- drop-reset flags), clamped and padded/truncated to n bars. 'Nothing'
-- yields all-neutral vectors — the walk is then byte-identical to the
-- dynamics-blind behaviour.
dynVectors :: Int -> Maybe [Double] -> (V.Vector Double, V.Vector Bool)
dynVectors n Nothing = (V.replicate n 0, V.replicate n False)
dynVectors n (Just ds) =
  let lvls  = V.fromList (take n (ds ++ repeat (if null ds then 0.5 else last ds)))
      mean  = V.sum lvls / fromIntegral (max 1 (V.length lvls))
      clamp = max (-0.5) . min 0.5
      biasV = V.map (\lvl -> clamp (mean - lvl)) lvls
      dropV = V.generate n (\i ->
                i > 0 && lvls V.! i <= lvls V.! (i - 1) - 0.25)
  in (biasV, dropV)

-------------------------------------------------------------------------------
-- Pass 2 — Beat 3s (re-anchor)
-------------------------------------------------------------------------------

-- | Per bar, pick the beat-3 MIDI minimising
--   (|m - b1_i| + |b1_{i+1} - m| + consonance-to-fund + repeat-penalty).
-- Linear (not quadratic) smoothness so moderate leaps to the P5 aren't
-- over-penalised. Consonance cost comes from 'beat3ConsTable' on the
-- interval above the fundamental, so the strong beat anchors on P5 / root /
-- 3rds and reaches tension tones only under register pressure. The repeat
-- penalty ('kappaPassiveRepeat') is stronger than Pass 3's connector repeat
-- cost so a non-repeat chord tone wins when nearby. For symmetric chords
-- (dim / aug / dim7 / whole-tone) the consonance term is neutralised because
-- no chord tone is privileged over the others.
pass2Beat3s :: Int -> V.Vector (Set Int) -> V.Vector (Set Int) -> V.Vector Int -> V.Vector Int -> V.Vector Int
pass2Beat3s consPct keyV chordPCsV b1s fundPCs =
  let n = V.length b1s
      pick i =
        let chord  = chordPCsV V.! i
            sym    = isSymmetricChord chord
            pool   = V.toList (midisIn (chord `Set.union` (keyV V.! i)))
            b1L    = b1s V.! i
            b1R    = b1s V.! ((i + 1) `mod` n)
            fundPC = fundPCs V.! i
            score m =
              let smL  = abs (m - b1L)
                  smR  = abs (b1R - m)
                  cons = if sym then 0
                         else (consPct * kappaB3Consonance
                               * beat3ConsTable ((m - fundPC) `mod` 12))
                              `div` 100
                  nchP = if (m `mod` 12) `Set.member` chord then 0
                         else kappaB3NonChord
                  repP = if m == b1L then kappaPassiveRepeat else 0
                  antP = if m == b1R then kappaB3Anticipate else 0
              in smL + smR + cons + nchP + repP + antP
        in case pool of
             [] -> b1L   -- degenerate bar (no chord tones): hold beat 1
             _  -> minimumBy (compare `on` score) pool
  in V.generate n pick

-------------------------------------------------------------------------------
-- Pass 3 — Beats 2 and 4 (connectors)
-------------------------------------------------------------------------------

data ConnectorPos = Beat2 | Beat4 deriving (Eq, Show)

-- | Deterministic Double in [0, 1) from (seed, position).
seededUniform :: Int -> Int -> Double
seededUniform seed pos =
  head (randoms (mkStdGen (seed `xor` (pos * 2654435761))) :: [Double])

-- | Repeat-rate probability: 0.20 at e=0, 0.05 at e=1 (clamped to [0,1]).
pRepeat :: Double -> Double
pRepeat e = 0.20 - 0.15 * max 0.0 (min 1.0 e)

-- | Controlled-repeat cost at a connector position.
repeatCostAt :: Int -> Double -> Int -> Int -> Int -> Int
repeatCostAt pos e seed m l
  | m /= l                              = 0
  | seededUniform seed pos < pRepeat e  = kappaStaticBase
  | otherwise                           = kappaStaticBlocked

-- | Connector candidate pool: local-scale tones union chromatic approaches
-- to the right-flank target (clipped to the register).
connectorPool :: Set Int -> Int -> [Int]
connectorPool scale target =
  let scaleMidis = V.toList (midisIn scale)
      chromas    = [ m | m <- [target - 1, target + 1]
                       , m >= lowestMidi, m <= highestMidi ]
  in Set.toList (Set.fromList (scaleMidis ++ chromas))

-- | Per-beat scoring. Beat 4 picks up extra bonuses (chord-tone near target,
-- stronger chromatic-leading-tone bonus, root / P5 approach) and a copy-next
-- penalty; both beats pick up a diatonic-approach bonus and a static-cell
-- recovery bonus. The chromatic-approach bonus applies to any candidate at
-- |m - r| == 1 — in-scale, in-chord, or chromatic. For symmetric chords the
-- static-cell recovery rewards any non-root chord tone (not just the phantom
-- P5). The approach bonus (iter 6) rewards the current bar's root on beat 4
-- when it sits 1 or 2 semitones from next b1 (half strength for the P5); if
-- b3 already used the root / P5, the bonus shifts to the chromatic
-- in-between tone so the line doesn't repeat itself into a static cell.
scoreConnector
  :: ConnectorPos
  -> Int            -- tension licence percentage (scales chromatic bonus)
  -> Set Int        -- regional-key major-scale PCs (Minor Thirds Rule)
  -> Set Int        -- localScale_i (cyclic)
  -> Set Int        -- chordPCs_i
  -> Int -> Int     -- L, R
  -> Bool           -- isStatic (b1 == b3 for this bar)
  -> Bool           -- isSymmetric (this bar's chord is rotation-invariant)
  -> Int            -- rootPC of this bar
  -> Int            -- p5PC of this bar
  -> Int            -- b3 MIDI of this bar
  -> Int -> Double -> Int -> Int -> Int
scoreConnector pos tensionPct keySet scale chord l r isStatic isSymmetric
               rootPC p5PC b3 posIdx e seed m =
  let smooth      = (m - l) * (m - l) + (r - m) * (r - m)
      inScale     = (m `mod` 12) `Set.member` scale
      inChord     = (m `mod` 12) `Set.member` chord
      scaleFit    = if inScale then 0 else kappaChromatic
      chromaticB  = if abs (m - r) == 1
                    then -((bonusK pos * tensionPct) `div` 100) else 0
      diatonicAp  = if abs (m - r) == 2 && inScale
                    then -kappaDiatonicApproach else 0
      -- Wasted-tone rule: weak beats reserve the quality-defining chord
      -- tones (3rds / 7ths) for strong beats; only root and P5 earn the
      -- beat-4 chord-tone approach bonus.
      chordToneB  = if pos == Beat4 && inChord && abs (m - r) `elem` [1, 2]
                       && (m `mod` 12) `elem` [rootPC, p5PC]
                    then -kappaChordToneBonus else 0
      b2ChordB    = if pos == Beat2 && inChord && m /= l
                    then -kappaB2ChordTone else 0
      sandwichPen = if not inChord && abs (m - l) > 2 && abs (m - r) > 2
                    then kappaSandwich else 0
      copyPen     = if pos == Beat4 && m == r
                    then kappaCopyNext else 0
      staticRec   = if isStatic && m /= l &&
                       ((m `mod` 12) == p5PC || (isSymmetric && inChord))
                    then -kappaStaticRecovery else 0
      approachB targetPC weight =
        if pos == Beat4 &&
           ( ((m `mod` 12) == targetPC &&
              abs (m - r) `elem` [1, 2] &&
              (b3 `mod` 12) /= targetPC)
           ||
             (abs (m - r) == 1 &&
              (b3 `mod` 12) == targetPC &&
              abs (l - r) == 2 &&
              (m - l) * (r - m) > 0) )
        then -weight else 0
      -- Root approach, P5 approach, and the dominant fall are alternative
      -- readings of one gesture: a candidate qualifying under several takes
      -- the strongest bonus only.
      dominantFallB = if pos == Beat4 && (m `mod` 12) == rootPC
                         && ((r - m) `mod` 12) == 5
                      then -kappaDominantFall else 0
      approachTotal = minimum
        [ approachB rootPC kappaRootApproach
        , approachB p5PC (kappaRootApproach `div` 2)
        , dominantFallB ]
      -- Minor Thirds Rule (see 'kappaMinorThird').
      gapLR    = abs (l - r)
      betweenLR = (m - l) * (r - m) > 0
      mtRule
        | gapLR == 3 && betweenLR =
            if (m `mod` 12) `Set.member` keySet
              then -kappaMinorThird
              else -((kappaMinorThird * round (100 * e)) `div` 100)
        | gapLR == 4 && betweenLR && abs (m - l) == 2 = -kappaMinorThird
        | otherwise = 0
      fourthChainB = if inChord && m /= r && abs (m - l) `elem` [5, 7]
                        && (abs (m - r) <= 2 || abs (m - r) `elem` [5, 7])
                     then -kappaFourthChain else 0
      repC        = let base      = repeatCostAt posIdx e seed m l
                        phrasePos = (posIdx `div` 2) `mod` 4
                    in if pos == Beat4 && m == l
                       then if phrasePos == 3
                            then kappaStaticBase
                            else base + kappaPhraseMid
                       else base
  in smooth + scaleFit + chromaticB + diatonicAp + chordToneB + b2ChordB
           + sandwichPen + copyPen + staticRec + approachTotal + mtRule
           + fourthChainB + repC
  where
    bonusK Beat2 = kappaChromaticBonus
    bonusK Beat4 = kappaChromaticBonusBeat4

-- | Fill beats 2 and 4 per bar.
pass3Connectors
  :: Int                  -- tension licence percentage
  -> V.Vector (Set Int)   -- regional-key major-scale PCs per bar
  -> V.Vector (Set Int)   -- local scales
  -> V.Vector (Set Int)   -- chord PCs
  -> V.Vector Int         -- b1s
  -> V.Vector Int         -- b3s
  -> V.Vector Int         -- fund PCs (for P5 recovery)
  -> Int -> Double
  -> (V.Vector Int, V.Vector Int)
pass3Connectors tensionPct keySetsV localsV chordsV b1s b3s fundPCs seed e =
  let n = V.length b1s
      isStaticAt i = b1s V.! i == b3s V.! i
      isSymAt i    = isSymmetricChord (chordsV V.! i)
      rootPCAt i   = fundPCs V.! i
      p5PCAt i     = (fundPCs V.! i + 7) `mod` 12
      b3At i       = b3s V.! i
      chooseBeat2 i =
        let scale = localsV V.! i
            chord = chordsV V.! i
            l     = b1s V.! i
            r     = b3s V.! i
            pool  = connectorPool scale r
            sc    = scoreConnector Beat2 tensionPct (keySetsV V.! i) scale chord l r
                                   (isStaticAt i) (isSymAt i)
                                   (rootPCAt i) (p5PCAt i) (b3At i)
                                   (2 * i) e seed
        in case pool of
             [] -> l
             _  -> minimumBy (compare `on` sc) pool
      chooseBeat4 i =
        let scale = localsV V.! i
            chord = chordsV V.! i
            l     = b3s V.! i
            r     = b1s V.! ((i + 1) `mod` n)
            pool  = connectorPool scale r
            sc    = scoreConnector Beat4 tensionPct (keySetsV V.! i) scale chord l r
                                   (isStaticAt i) (isSymAt i)
                                   (rootPCAt i) (p5PCAt i) (b3At i)
                                   (2 * i + 1) e seed
        in case pool of
             [] -> l
             _  -> minimumBy (compare `on` sc) pool
  in (V.generate n chooseBeat2, V.generate n chooseBeat4)

-------------------------------------------------------------------------------
-- Main entry
-------------------------------------------------------------------------------

-- | Generate a walking-bass line. Entropy is derived from the progression's
-- harmonic character; the caller supplies only the progression and a voice
-- function ('fund' or 'root') defining each bar's beat 1.
walkLine :: VoiceFunction -> Pr.Progression -> [[Int]]
walkLine = walkLineDyn Nothing

-- | 'walkLine' with an optional per-bar dynamic vector coupling the beat-1
-- register arc to the piece's dynamics (see 'kappaDynArc'). 'Nothing' is
-- byte-identical to 'walkLine'.
walkLineDyn :: Maybe [Double] -> VoiceFunction -> Pr.Progression -> [[Int]]
walkLineDyn mDyn voiceFn prog
  | nBars == 0 = []
  | otherwise  = [ [b1s V.! i, b2s V.! i, b3s V.! i, b4s V.! i]
                 | i <- [0 .. nBars - 1] ]
  where
    e         = progressionEntropy prog
    seed      = hashProgEntropy prog e

    -- Progression-level consonance scales the walk's character: consonant
    -- material anchors harder (stricter beat-3 table) and licenses less
    -- chromatic tension in the connectors; dissonant material the reverse.
    -- The band is deliberately narrow so scaling shades the line's colour
    -- without erasing strong-beat variety at either extreme.
    consPct    = 70 + round (60 * progConsonance prog) :: Int
    tensionPct = 200 - consPct

    barsV     = V.fromList (toList (Pr.unProgression prog))
    nBars     = V.length barsV

    chordPCsV = V.map chordPCs barsV
    localsV   = V.generate nBars (localScale chordPCsV)

    -- Pass-2 consonance target is the cadence-state fundamental regardless
    -- of voice function supplied for beat 1.
    fundPCs   = V.map rootPCInt barsV

    pcs1      = beat1PCs voiceFn prog
    p5PCs     = V.map (\r -> (r + 7) `mod` 12) fundPCs
    dupOdd    = dupOddFlags barsV
    (biasV, dropV) = dynVectors nBars mDyn
    b1s       = pass1Beat1s biasV dropV pcs1 p5PCs dupOdd
    -- Per-bar tonal palette (derived, walk-internal). Minor-turnaround
    -- bars modulate internally, so chord QUALITY overrides the regional
    -- key: a half-diminished bar takes the harmonic minor of the tonic a
    -- whole step below (ii of minor); an altered dominant (b9/#9) takes
    -- its altered scale (melodic minor a semitone up); a plain dominant
    -- resolving up a fourth to a minor chord takes the target's harmonic
    -- minor. All other bars take the major scale of the regional centre
    -- from 'inferKeyCentre'. Purely local and deterministic — the Markov
    -- boundary of 'inferKeyCentre' applies to the palettes too.
    keySetsV  = V.fromList
      [ barPalette i k
      | (i, k) <- zip [0 ..] (inferKeyCentre prog) ]
    barPalette i k =
      let cs      = barsV V.! i
          r       = rootPCInt cs
          ivs     = Set.fromList
                      [ Pt.unPitchClass iv `mod` 12
                      | iv <- Hm.cadenceIntervals (Hm.stateCadence cs) ]
          has     = (`Set.member` ivs)
          nextCs  = barsV V.! ((i + 1) `mod` nBars)
          nextR   = rootPCInt nextCs
          nextIvs = Set.fromList
                      [ Pt.unPitchClass iv `mod` 12
                      | iv <- Hm.cadenceIntervals (Hm.stateCadence nextCs) ]
          scaleAt base steps = Set.fromList [ (base + st) `mod` 12 | st <- steps ]
          harmMinor t = scaleAt t [0, 2, 3, 5, 7, 8, 11]
          altered     = scaleAt r [0, 1, 3, 4, 6, 8, 10]
          domToMinor  = has 4 && has 10
                        && (nextR - r) `mod` 12 == 5
                        && 3 `Set.member` nextIvs
      in if | has 3 && has 6            -> harmMinor ((r - 2) `mod` 12)
            | has 4 && has 10
                && (has 1 || has 3)     -> altered
            | domToMinor                -> harmMinor ((r + 5) `mod` 12)
            | otherwise                 -> scaleAt (Pt.unPitchClass k)
                                             [0, 2, 4, 5, 7, 9, 11]

    b3s       = pass2Beat3s consPct keySetsV chordPCsV b1s fundPCs
    (b2s, b4s) = pass3Connectors tensionPct keySetsV localsV chordPCsV b1s b3s fundPCs seed e

-------------------------------------------------------------------------------
-- Octatripentatonic-aware variant
-------------------------------------------------------------------------------

-- | Per-bar chroma sources for the 'walkLineP' Pass-3 connector pool.
-- 'csStrata' is the bar's full 5-PC strata chroma; 'csMode' is the full
-- 7-PC mode chroma. Both are supplied by the caller (typically derived from
-- 'PC.strataLayer' / 'PC.modeLayer' of a genP-origin ProgressionContext).
data ChromaSources = ChromaSources
  { csStrata :: !(Set Int)
  , csMode   :: !(Set Int)
  } deriving (Eq, Show)

-- | Octatripentatonic-aware connector pool. Replaces the chromatic ±1
-- candidates of 'connectorPool' with strata / mode chroma — chromatic
-- approaches that aren't independently in strata, overlap, mode, or chord
-- are excluded entirely (no chromatic lines in genP context).
connectorPoolP :: Set Int -> Set Int -> Set Int -> Set Int -> [Int]
connectorPoolP strata overlap mode chord =
  let allPCs = strata `Set.union` overlap `Set.union` mode `Set.union` chord
  in V.toList (midisIn allPCs)

-- | Tier-aware scoring for the genP path. Mirrors 'scoreConnector' but:
--   * the leading-tone bonus applies only to in-pool candidates (the pool
--     admits no chromatic outsiders, so a semitone approach is always a
--     strata / overlap / mode tone — purity is preserved).
--   * replaces the binary 'kappaChromatic' fit penalty with a three-tier
--     preference: strata (bonus), overlap (neutral), mode (mild penalty).
--   * keeps every other term unchanged (smoothness, diatonic approach,
--     chord-tone bonus on beat 4, copy-next penalty, static recovery,
--     root/P5 approach, repeat cost).
scoreConnectorP
  :: ConnectorPos
  -> Int            -- tension licence percentage (scales chromatic bonus)
  -> Set Int        -- strata PCs (5)
  -> Set Int        -- overlap (localScale) PCs
  -> Set Int        -- mode PCs (7)
  -> Set Int        -- chord PCs (3)
  -> Int -> Int     -- L, R
  -> Bool           -- isStatic
  -> Bool           -- isSymmetric
  -> Int            -- rootPC
  -> Int            -- p5PC
  -> Int            -- b3 MIDI
  -> Int -> Double -> Int -> Int -> Int
scoreConnectorP pos tensionPct strata overlap mode chord l r isStatic isSymmetric
                rootPC p5PC b3 posIdx e seed m =
  let smooth      = (m - l) * (m - l) + (r - m) * (r - m)
      pcMod       = m `mod` 12
      inStrata    = pcMod `Set.member` strata
      inOverlap   = pcMod `Set.member` overlap
      inMode      = pcMod `Set.member` mode
      inChord     = pcMod `Set.member` chord
      inAny       = inStrata || inOverlap || inMode || inChord
      -- Chord tones are subsumed by strata (pcStrictContainment guarantees
      -- chord ⊆ strata ⊆ mode for every genP bar), so three tiers suffice:
      -- own-stratum tone, neighbour-triad tone outside the stratum, and
      -- partner-contributed mode tone.
      tierFit
        | inStrata  = -kappaStrataPref     -- bonus for the bar's own stratum
        | inOverlap = 0                    -- neighbour-triad tone, neutral
        | otherwise = kappaModePenalty     -- mode-only (partner stratum) tone
      chromaticB  = if abs (m - r) == 1
                    then -((bonusK pos * tensionPct) `div` 100) else 0
      diatonicAp  = if abs (m - r) == 2 && inAny
                    then -kappaDiatonicApproach else 0
      -- Wasted-tone rule: weak beats reserve the quality-defining chord
      -- tones (3rds / 7ths) for strong beats; only root and P5 earn the
      -- beat-4 chord-tone approach bonus.
      chordToneB  = if pos == Beat4 && inChord && abs (m - r) `elem` [1, 2]
                       && (m `mod` 12) `elem` [rootPC, p5PC]
                    then -kappaChordToneBonus else 0
      b2ChordB    = if pos == Beat2 && inChord && m /= l
                    then -kappaB2ChordTone else 0
      sandwichPen = if not inChord && abs (m - l) > 2 && abs (m - r) > 2
                    then kappaSandwich else 0
      copyPen     = if pos == Beat4 && m == r
                    then kappaCopyNext else 0
      staticRec   = if isStatic && m /= l &&
                       ((m `mod` 12) == p5PC || (isSymmetric && inChord))
                    then -kappaStaticRecovery else 0
      approachB targetPC weight =
        if pos == Beat4 &&
           ( ((m `mod` 12) == targetPC &&
              abs (m - r) `elem` [1, 2] &&
              (b3 `mod` 12) /= targetPC)
           ||
             (abs (m - r) == 1 &&
              (b3 `mod` 12) == targetPC &&
              abs (l - r) == 2 &&
              (m - l) * (r - m) > 0) )
        then -weight else 0
      -- Root approach, P5 approach, and the dominant fall are alternative
      -- readings of one gesture: a candidate qualifying under several takes
      -- the strongest bonus only.
      dominantFallB = if pos == Beat4 && (m `mod` 12) == rootPC
                         && ((r - m) `mod` 12) == 5
                      then -kappaDominantFall else 0
      approachTotal = minimum
        [ approachB rootPC kappaRootApproach
        , approachB p5PC (kappaRootApproach `div` 2)
        , dominantFallB ]
      -- Minor Thirds Rule, strata vocabulary: the bar's own stratum plays
      -- the diatonic role; anything else in the pool is the tension fill.
      gapLR    = abs (l - r)
      betweenLR = (m - l) * (r - m) > 0
      mtRule
        | gapLR == 3 && betweenLR =
            if inStrata
              then -kappaMinorThird
              else -((kappaMinorThird * round (100 * e)) `div` 100)
        | gapLR == 4 && betweenLR && abs (m - l) == 2 = -kappaMinorThird
        | otherwise = 0
      fourthChainB = if inChord && m /= r && abs (m - l) `elem` [5, 7]
                        && (abs (m - r) <= 2 || abs (m - r) `elem` [5, 7])
                     then -kappaFourthChain else 0
      repC        = let base      = repeatCostAt posIdx e seed m l
                        phrasePos = (posIdx `div` 2) `mod` 4
                    in if pos == Beat4 && m == l
                       then if phrasePos == 3
                            then kappaStaticBase
                            else base + kappaPhraseMid
                       else base
  in smooth + tierFit + chromaticB + diatonicAp + chordToneB + b2ChordB
           + sandwichPen + copyPen + staticRec + approachTotal + mtRule
           + fourthChainB + repC
  where
    bonusK Beat2 = kappaChromaticBonus
    bonusK Beat4 = kappaChromaticBonusBeat4

-- | Octatripentatonic Pass 3. Same shape as 'pass3Connectors' but with
-- per-bar 'ChromaSources' driving the candidate pool and tier scoring.
pass3ConnectorsP
  :: Int                        -- tension licence percentage
  -> V.Vector (Set Int)         -- local scales (overlap)
  -> V.Vector (Set Int)         -- chord PCs
  -> V.Vector (ChromaSources)   -- per-bar (strata, mode)
  -> V.Vector Int               -- b1s
  -> V.Vector Int               -- b3s
  -> V.Vector Int               -- fund PCs (for P5 recovery)
  -> Int -> Double
  -> (V.Vector Int, V.Vector Int)
pass3ConnectorsP tensionPct localsV chordsV chromasV b1s b3s fundPCs seed e =
  let n = V.length b1s
      isStaticAt i = b1s V.! i == b3s V.! i
      isSymAt i    = isSymmetricChord (chordsV V.! i)
      rootPCAt i   = fundPCs V.! i
      p5PCAt i     = (fundPCs V.! i + 7) `mod` 12
      b3At i       = b3s V.! i
      strataAt i   = csStrata (chromasV V.! i)
      modeAt i     = csMode   (chromasV V.! i)
      chooseBeat2 i =
        let overlap = localsV V.! i
            chord   = chordsV V.! i
            strata  = strataAt i
            mode    = modeAt i
            l       = b1s V.! i
            r       = b3s V.! i
            pool    = connectorPoolP strata overlap mode chord
            sc      = scoreConnectorP Beat2 tensionPct strata overlap mode chord l r
                                      (isStaticAt i) (isSymAt i)
                                      (rootPCAt i) (p5PCAt i) (b3At i)
                                      (2 * i) e seed
        in case pool of
             [] -> l
             _  -> minimumBy (compare `on` sc) pool
      chooseBeat4 i =
        let overlap = localsV V.! i
            chord   = chordsV V.! i
            strata  = strataAt i
            mode    = modeAt i
            l       = b3s V.! i
            r       = b1s V.! ((i + 1) `mod` n)
            pool    = connectorPoolP strata overlap mode chord
            sc      = scoreConnectorP Beat4 tensionPct strata overlap mode chord l r
                                      (isStaticAt i) (isSymAt i)
                                      (rootPCAt i) (p5PCAt i) (b3At i)
                                      (2 * i + 1) e seed
        in case pool of
             [] -> l
             _  -> minimumBy (compare `on` sc) pool
  in (V.generate n chooseBeat2, V.generate n chooseBeat4)

-- | Octatripentatonic-aware walking-bass line. Pass 1 (beat 1s from voiceFn)
-- and Pass 2 (beat 3s) are unchanged from 'walkLine'; Pass 3 (beats 2 & 4)
-- swaps the chromatic-±1 candidate path for tier-scored strata / overlap /
-- mode candidates supplied per bar via 'ChromaSources'. Caller is responsible
-- for matching @length chromas@ to @progLength prog@ (mismatch falls back to
-- the legacy 'walkLine' path for safety).
walkLineP :: VoiceFunction -> Pr.Progression -> [ChromaSources] -> [[Int]]
walkLineP = walkLinePDyn Nothing

-- | 'walkLineP' with the optional dynamic vector of 'walkLineDyn'.
walkLinePDyn :: Maybe [Double] -> VoiceFunction -> Pr.Progression -> [ChromaSources] -> [[Int]]
walkLinePDyn mDyn voiceFn prog chromas
  | nBars == 0                         = []
  | length chromas /= nBars            = walkLineDyn mDyn voiceFn prog
  | otherwise =
      [ [b1s V.! i, b2s V.! i, b3s V.! i, b4s V.! i]
      | i <- [0 .. nBars - 1] ]
  where
    e         = progressionEntropy prog
    seed      = hashProgEntropy prog e

    -- Progression-level consonance scales the walk's character: consonant
    -- material anchors harder (stricter beat-3 table) and licenses less
    -- chromatic tension in the connectors; dissonant material the reverse.
    consPct    = 70 + round (60 * progConsonance prog) :: Int
    tensionPct = 200 - consPct

    barsV     = V.fromList (toList (Pr.unProgression prog))
    nBars     = V.length barsV

    chordPCsV = V.map chordPCs barsV
    localsV   = V.generate nBars (localScale chordPCsV)
    chromasV  = V.fromList chromas

    fundPCs   = V.map rootPCInt barsV

    pcs1      = beat1PCs voiceFn prog
    p5PCs     = V.map (\r -> (r + 7) `mod` 12) fundPCs
    dupOdd    = dupOddFlags barsV
    (biasV, dropV) = dynVectors nBars mDyn
    b1s       = pass1Beat1s biasV dropV pcs1 p5PCs dupOdd
    strataV   = V.map csStrata chromasV
    b3s       = pass2Beat3s consPct strataV chordPCsV b1s fundPCs
    (b2s, b4s) = pass3ConnectorsP tensionPct localsV chordPCsV chromasV b1s b3s fundPCs seed e
