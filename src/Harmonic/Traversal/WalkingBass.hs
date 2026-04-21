-- |
-- Module      : Harmonic.Traversal.WalkingBass
-- Description : Three-pass walking-bass line generator with derived entropy
--
-- Produces a walking-bass line for a cyclic Progression as a pure function
-- of (progression, voiceFn) via three sequential passes:
--
--   1. Pass 1 — beat 1s. Every bar's beat 1 is a singleton PC derived from
--      the user-supplied VoiceFunction ('fund' or 'root'). Octave placement
--      is greedy left-to-right by smoothness to the previous beat 1.
--   2. Pass 2 — beat 3s. Per bar, choose a chord tone minimising smoothness
--      to b1_i plus smoothness to b1_{i+1} plus consonance-to-fund cost
--      (via 'rootMotionScore'). Unison repeats carry a dedicated penalty
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
--      b3→b4 unison.
--
-- The entropy parameter that used to be user-facing is now derived from the
-- progression's root-motion angularity and chord-internal dissonance. Calm
-- diatonic progressions land near 0 (more settled repeats allowed); angular
-- tritone-heavy progressions land near 1 (fewer repeats, more chromatic
-- motion). Pure-function guarantees are preserved: same progression and
-- voiceFn always produce the same line.

module Harmonic.Traversal.WalkingBass
  ( -- * Main entry
    walkLine

    -- * Derived entropy (exported for tests / diagnostics)
  , progressionEntropy

    -- * Utilities (exported for tests)
  , hashProgEntropy
  , closestLowMidi
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

lowestMidi, highestMidi, beatsPerBar :: Int
lowestMidi  = 28
highestMidi = 48
beatsPerBar = 4

-- | Pass 3 repeat-gate costs (unchanged from v3).
kappaStaticBase, kappaStaticBlocked :: Int
kappaStaticBase    = 3
kappaStaticBlocked = 200

-- | Pass 3 scale-fit and chromatic-approach weights.
kappaChromatic, kappaChromaticBonus, kappaChromaticBonusBeat4 :: Int
kappaChromatic           = 8
kappaChromaticBonus      = 5
kappaChromaticBonusBeat4 = 10

-- | Pass 2 consonance-to-fund weight (unchanged from v3).
kappaConsonance :: Int
kappaConsonance = 4

-- | Pass 2 repeat penalty — raised from v3's kappaStaticBase=3 so a non-repeat
-- chord tone within ~4 semitones wins over a unison repeat.
kappaPassiveRepeat :: Int
kappaPassiveRepeat = 6

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

-------------------------------------------------------------------------------
-- Config (internal)
-------------------------------------------------------------------------------

data WalkConfig = WalkConfig
  { wcEntropy :: !Double
  , wcSeed    :: !Int
  } deriving (Show, Eq)

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
closestLowMidi :: Int -> Int
closestLowMidi pc =
  let pc'   = pc `mod` 12
      start = lowestMidi + ((pc' - lowestMidi) `mod` 12)
  in if start > highestMidi then start - 12 else start

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

    -- Chord-internal dissonance: mean dissonanceScore over bars. Triads sit
    -- around 2–3; altered / tritone-heavy chords climb into 10+. Cap at 20.
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

-- | Place each bar's beat 1 greedily. Bar 0 uses 'closestLowMidi'; later bars
-- pick the PC-matching MIDI closest to the previous beat 1 (lower MIDI on tie).
pass1Beat1s :: V.Vector Int -> V.Vector Int
pass1Beat1s pcs
  | V.null pcs = V.empty
  | otherwise  =
      let n  = V.length pcs
          b0 = closestLowMidi (pcs V.! 0)
          go i prev
            | i >= n    = []
            | otherwise =
                let pool = V.toList (midisIn (Set.singleton (pcs V.! i)))
                    pick = minimumBy
                             (\a b -> compare (abs (a - prev), a)
                                              (abs (b - prev), b))
                             pool
                in pick : go (i + 1) pick
      in V.fromList (b0 : go 1 b0)

-------------------------------------------------------------------------------
-- Pass 2 — Beat 3s (re-anchor)
-------------------------------------------------------------------------------

-- | Per bar, pick the chord-tone MIDI minimising
--   (|m - b1_i| + |b1_{i+1} - m| + consonance-to-fund + repeat-penalty).
-- Linear (not quadratic) smoothness so moderate leaps to the P5 aren't
-- over-penalised. The repeat penalty ('kappaPassiveRepeat') is stronger than
-- Pass 3's connector repeat cost so a non-repeat chord tone wins when nearby.
-- For symmetric chords (dim / aug / dim7 / whole-tone) the consonance term is
-- neutralised because no chord tone is privileged over the others.
pass2Beat3s :: V.Vector (Set Int) -> V.Vector Int -> V.Vector Int -> V.Vector Int
pass2Beat3s chordPCsV b1s fundPCs =
  let n = V.length b1s
      pick i =
        let chord  = chordPCsV V.! i
            sym    = isSymmetricChord chord
            pool   = V.toList (midisIn chord)
            b1L    = b1s V.! i
            b1R    = b1s V.! ((i + 1) `mod` n)
            fundPC = fundPCs V.! i
            score m =
              let smL  = abs (m - b1L)
                  smR  = abs (b1R - m)
                  cons = if sym then 0
                         else kappaConsonance
                              * fromIntegral
                                  (rootMotionScore ((m - fundPC) `mod` 12))
                  repP = if m == b1L then kappaPassiveRepeat else 0
              in smL + smR + cons + repP
        in minimumBy (compare `on` score) pool
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
  -> Set Int        -- localScale_i (cyclic)
  -> Set Int        -- chordPCs_i
  -> Int -> Int     -- L, R
  -> Bool           -- isStatic (b1 == b3 for this bar)
  -> Int            -- p5Midi of this bar's fundamental
  -> Bool           -- isSymmetric (this bar's chord is rotation-invariant)
  -> Int            -- rootPC of this bar
  -> Int            -- p5PC of this bar
  -> Int            -- b3 MIDI of this bar
  -> Int -> Double -> Int -> Int -> Int
scoreConnector pos scale chord l r isStatic p5m isSymmetric
               rootPC p5PC b3 posIdx e seed m =
  let smooth      = (m - l) * (m - l) + (r - m) * (r - m)
      inScale     = (m `mod` 12) `Set.member` scale
      inChord     = (m `mod` 12) `Set.member` chord
      scaleFit    = if inScale then 0 else kappaChromatic
      chromaticB  = if abs (m - r) == 1
                    then -(bonusK pos) else 0
      diatonicAp  = if abs (m - r) == 2 && inScale
                    then -kappaDiatonicApproach else 0
      chordToneB  = if pos == Beat4 && inChord && abs (m - r) <= 2
                    then -kappaChordToneBonus else 0
      copyPen     = if pos == Beat4 && m == r
                    then kappaCopyNext else 0
      staticRec   = if isStatic && m /= l &&
                       (m == p5m || (isSymmetric && inChord))
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
      rootApproachB = approachB rootPC kappaRootApproach
      p5ApproachB   = approachB p5PC   (kappaRootApproach `div` 2)
      evenness    = abs ((m - l) - (r - m))
      repC        = repeatCostAt posIdx e seed m l
  in smooth + scaleFit + chromaticB + diatonicAp + chordToneB
           + copyPen + staticRec + rootApproachB + p5ApproachB
           + evenness + repC
  where
    bonusK Beat2 = kappaChromaticBonus
    bonusK Beat4 = kappaChromaticBonusBeat4

-- | Fill beats 2 and 4 per bar.
pass3Connectors
  :: V.Vector (Set Int)   -- local scales
  -> V.Vector (Set Int)   -- chord PCs
  -> V.Vector Int         -- b1s
  -> V.Vector Int         -- b3s
  -> V.Vector Int         -- fund PCs (for P5 recovery)
  -> Int -> Double
  -> (V.Vector Int, V.Vector Int)
pass3Connectors localsV chordsV b1s b3s fundPCs seed e =
  let n = V.length b1s
      isStaticAt i = b1s V.! i == b3s V.! i
      p5MidiAt i   = closestLowMidi ((fundPCs V.! i + 7) `mod` 12)
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
            sc    = scoreConnector Beat2 scale chord l r
                                   (isStaticAt i) (p5MidiAt i) (isSymAt i)
                                   (rootPCAt i) (p5PCAt i) (b3At i)
                                   (2 * i) e seed
        in minimumBy (compare `on` sc) pool
      chooseBeat4 i =
        let scale = localsV V.! i
            chord = chordsV V.! i
            l     = b3s V.! i
            r     = b1s V.! ((i + 1) `mod` n)
            pool  = connectorPool scale r
            sc    = scoreConnector Beat4 scale chord l r
                                   (isStaticAt i) (p5MidiAt i) (isSymAt i)
                                   (rootPCAt i) (p5PCAt i) (b3At i)
                                   (2 * i + 1) e seed
        in minimumBy (compare `on` sc) pool
  in (V.generate n chooseBeat2, V.generate n chooseBeat4)

-------------------------------------------------------------------------------
-- Main entry
-------------------------------------------------------------------------------

-- | Generate a walking-bass line. Entropy is derived from the progression's
-- harmonic character; the caller supplies only the progression and a voice
-- function ('fund' or 'root') defining each bar's beat 1.
walkLine :: VoiceFunction -> Pr.Progression -> [[Int]]
walkLine voiceFn prog
  | nBars == 0 = []
  | otherwise  = [ [b1s V.! i, b2s V.! i, b3s V.! i, b4s V.! i]
                 | i <- [0 .. nBars - 1] ]
  where
    e         = progressionEntropy prog
    seed      = hashProgEntropy prog e

    barsV     = V.fromList (toList (Pr.unProgression prog))
    nBars     = V.length barsV

    chordPCsV = V.map chordPCs barsV
    localsV   = V.generate nBars (localScale chordPCsV)

    -- Pass-2 consonance target is the cadence-state fundamental regardless
    -- of voice function supplied for beat 1.
    fundPCs   = V.map rootPCInt barsV

    pcs1      = beat1PCs voiceFn prog
    b1s       = pass1Beat1s pcs1
    b3s       = pass2Beat3s chordPCsV b1s fundPCs
    (b2s, b4s) = pass3Connectors localsV chordPCsV b1s b3s fundPCs seed e
