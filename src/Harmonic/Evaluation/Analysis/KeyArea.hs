-- |
-- Module      : Harmonic.Evaluation.Analysis.KeyArea
-- Description : Whole-progression key-area detection and chordscale layers
--
-- Chord-scale analysis over a finished progression: a cyclic Viterbi walk
-- over a 24-key lattice (12 major + 12 /composite/ minor areas) assigns
-- every bar a key area and a scale form, from which the M (mode, 7-PC) and
-- S (anhemitonic pentatonic, 5-PC) layers of gen \/ genJ contexts are
-- derived ('chordscale').
--
-- A minor key is COMPOSITE: one tonic with three interchangeable forms —
-- natural (the relative-major set), harmonic minor, melodic minor — chosen
-- per bar by chord fit. A minor ii-V-i therefore reads as ONE key area:
-- the iiO7 and V7b9 bars take the harmonic form (Locrian ♮6 \/ Phrygian
-- dominant), the tonic bars the melodic\/natural forms. Melodic minor is
-- never an independent key whose ii-V-i sits on its own scale degrees.
--
-- BOUNDARY: analysis is a PURE, DERIVED annotation computed over a finished
-- progression. The generation system deliberately operates without key
-- awareness — each state is abstract and deterministic, upholding the
-- Markov property — so nothing here may feed back into generation, become
-- part of a generation state, or appear in a generation-path signature.
-- Producers attach the RESULTS to the finished context ('chordscale');
-- the walking bass reads the same detector ('barPalettes'). Under identity
-- chord selection the walk and the layers therefore agree; under a
-- reordering selection the walk re-analyses the PERFORMED bar sequence —
-- deliberately — and its lines also add chromatic approach tones beyond
-- any stored set.
--
-- All numeric constants are probe-calibrated — see
-- @archive\/analysis\/keyarea.md@ \/ @penta.md@ (2026-08-29): the lambda \/
-- bonus sweep on live gen\/genJ output plus hand-encoded cyclic standards
-- froze @kaLambda = 6@, @kaDomBonus = 3@, @kaTonicBonus = 1.5@ (97-98%
-- chord-in-key coverage at ~3.8-bar mean segments) and @kaPentaLambda = 4@
-- (~3.3 pentatonic switches per progression, 80% guide-tone coverage, zero
-- avoid-note hits).
module Harmonic.Evaluation.Analysis.KeyArea
  ( -- * Types
    KeyQuality(..)
  , KeyArea(..)
  , KeyForm(..)
  , ModeTier(..)
  , BarAnalysis(..)
  , showKeyArea

    -- * Analysis
  , analyzeProgression
  , barPalettes

    -- * Layer derivation
  , chordscale

    -- * Calibrated constants
  , kaLambda
  , kaDomBonus
  , kaTonicBonus
  , kaPentaLambda
  ) where

import           Data.Foldable (toList)
import           Data.List (maximumBy, nub, sort)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Vector as V

import qualified Harmonic.Rules.Import.Jazz as J
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pt
import qualified Harmonic.Rules.Types.Progression as Pr
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc

-------------------------------------------------------------------------------
-- Key lattice
-------------------------------------------------------------------------------

-- |Major, or composite minor (natural + harmonic + melodic forms).
data KeyQuality = MajorKey | MinorKey
  deriving (Eq, Ord, Show, Enum, Bounded)

-- |A key area: tonic pitch class + quality.
data KeyArea = KeyArea
  { kaTonic   :: !Int
  , kaQuality :: !KeyQuality
  } deriving (Eq, Ord, Show)

-- |Which scale form realises a bar inside its key area.
data KeyForm = MajForm | NatForm | HarmForm | MelForm
  deriving (Eq, Ord, Show, Enum, Bounded)

-- |Which tier of the override ladder produced a bar's M set: the key form
-- itself, the seeded special cases, the generic 28-mode search, or the
-- best-coverage gap fallback (chord not fully contained).
data ModeTier = TierForm | TierSeed | TierSearch | TierGap
  deriving (Eq, Ord, Show, Enum, Bounded)

-- |Per-bar analysis result.
data BarAnalysis = BarAnalysis
  { baKey        :: !KeyArea
  , baForm       :: !KeyForm
  , baModeSet    :: !(Set Int)        -- ^ absolute M-layer pitch classes
  , baMode       :: !(Maybe Sc.Mode)  -- ^ classified at the harmonic root
  , baTier       :: !ModeTier
  , baPentaRoot  :: !Int              -- ^ root of the anhemitonic pentatonic
  , baPentaInKey :: !Bool             -- ^ pentatonic sits inside the M set
  , baBoundary   :: !Bool             -- ^ key differs from the previous bar (cyclic)
  } deriving (Eq, Show)

-- |Render a key area the way musicians say it: @\"Bb\"@, @\"Gm\"@.
showKeyArea :: KeyArea -> String
showKeyArea (KeyArea t q) =
  show (Pt.flat (Pt.mkPitchClass t)) ++ (case q of MajorKey -> ""; MinorKey -> "m")

allKeys :: [KeyArea]
allKeys = [ KeyArea t q | q <- [MajorKey, MinorKey], t <- [0 .. 11] ]

scaleSetAt :: Int -> [Int] -> Set Int
scaleSetAt t steps = Set.fromList [ (t + s) `mod` 12 | s <- steps ]

majSteps, harmSteps, melSteps, altSteps :: [Int]
majSteps  = [0, 2, 4, 5, 7, 9, 11]
harmSteps = [0, 2, 3, 5, 7, 8, 11]
melSteps  = [0, 2, 3, 5, 7, 9, 11]
altSteps  = [0, 1, 3, 4, 6, 8, 10]

-- |The form sets of a key area, in preference order.
keyForms :: KeyArea -> [(KeyForm, Set Int)]
keyForms (KeyArea t MajorKey) = [(MajForm, scaleSetAt t majSteps)]
keyForms (KeyArea t MinorKey) =
  [ (NatForm,  scaleSetAt ((t + 3) `mod` 12) majSteps)
  , (HarmForm, scaleSetAt t harmSteps)
  , (MelForm,  scaleSetAt t melSteps)
  ]

-------------------------------------------------------------------------------
-- Bar facts
-------------------------------------------------------------------------------

data BarFacts = BarFacts
  { bfRoot    :: !Int        -- harmonic root (post inversion detection)
  , bfAbs     :: !(Set Int)  -- absolute chord pitch classes
  , bfZero    :: ![Int]      -- zero-form above the harmonic root
  , bfZeroSet :: !(Set Int)  -- the same, as a set (hot in the DP inner loop)
  , bfVocab   :: !J.BassVocab
  }

-- Harmonic root via the chord namer (the bass of an inverted gen triad is
-- NOT the root); bassVocabFor is total over any zero-form set, so one
-- fact extractor serves classical triads and jazz extensions alike.
barFacts :: H.CadenceState -> BarFacts
barFacts cs =
  let bassPC = Pt.unPitchClass (Pt.pitchClass (H.stateCadenceRoot cs))
      absPCs = Set.fromList [ (bassPC + Pt.unPitchClass iv) `mod` 12
                            | iv <- H.cadenceIntervals (H.stateCadence cs) ]
      harmR  = Pt.unPitchClass (Pt.pitchClass (H.chordNoteName (H.fromCadenceState cs)))
      zf     = sort [ (p - harmR) `mod` 12 | p <- Set.toList absPCs ]
  in BarFacts harmR absPCs zf (Set.fromList zf) (J.bassVocabFor zf)

absOf :: Int -> [Int] -> [Int]
absOf r = map (\i -> (r + i) `mod` 12)

-------------------------------------------------------------------------------
-- Emission: how well a bar sits in a key
-------------------------------------------------------------------------------

-- Weighted membership of the bar's tones in one candidate form set. The
-- BassVocab tiers grade structural importance (target > strong > passing);
-- notated colour tones still count — a V7b9's b9 is the harmonic-minor
-- marker, not noise — but at the lightest weight.
membScore :: BarFacts -> Set Int -> Double
membScore bf _form
  -- A silent bar (no chord tones) is key-neutral: without this guard the
  -- fabricated default vocabulary (root C, fifth G) would cast real votes
  -- and a rest could tug the whole key path. DP continuity carries the
  -- neighbouring key across instead.
  | Set.null (bfAbs bf) = 0
membScore bf form =
  let v        = bfVocab bf
      tgt      = nub (absOf (bfRoot bf) (J.bvTarget v))
      strong   = nub (absOf (bfRoot bf) (J.bvStrong v)) `minus` tgt
      passing  = nub (absOf (bfRoot bf) (J.bvPassing v)) `minus` (tgt ++ strong)
      colour   = Set.toList (bfAbs bf) `minus` (tgt ++ strong ++ passing)
      minus xs ys = [ x | x <- xs, x `notElem` ys ]
      score w wOut ps = sum [ if p `Set.member` form then w else wOut | p <- ps ]
  in score 3 (-4) tgt + score 2 (-2) strong + score 1 (-0.5) passing
     + score 1 (-1) colour

-- Functional-harmony votes: the retired walk votesFor table re-based onto
-- the 24-key lattice with first-class minor evidence (an altered dominant
-- is V of a MINOR tonic; a half-diminished is ii of minor before vii of
-- major; mM7 \/ m6 mark a tonic minor; a plain major triad is also V of a
-- harmonic-minor key). Triadic tones state a chord's identity; the tones
-- ABOVE the triad state which key it belongs to, so extension evidence is
-- read before the plain seventh-and-triad cases.
votes :: BarFacts -> [(KeyArea, Double)]
votes bf =
  let ivs = Set.fromList (bfZero bf)
      has = (`Set.member` ivs)
      altered   = has 1 || has 8 || (has 3 && has 4)
      unaltered = has 2 || has 9
      r = bfRoot bf
      k q off = KeyArea ((r + off) `mod` 12) q
      offsets
        | has 4 && has 10 && altered = [ (k MinorKey 5, 6), (k MajorKey 5, 2) ]
        | has 4 && has 10 && unaltered && not (has 6) =
            [ (k MajorKey 5, 7), (k MinorKey 5, 2) ]
        | has 4 && has 10            = [ (k MajorKey 5, 6), (k MinorKey 5, 3) ]
        | has 4 && has 11 && has 6 && has 7 = [ (k MajorKey 0, 5) ]
        | has 4 && has 11            = [ (k MajorKey 0, 4), (k MajorKey 7, 2)
                                       , (k MinorKey 9, 2) ]
        | has 3 && has 11            = [ (k MinorKey 0, 6) ]
        | has 3 && has 9 && not (has 10) = [ (k MinorKey 0, 5), (k MajorKey 10, 2) ]
        | has 3 && has 6 && not (has 4) = [ (k MinorKey 10, 5), (k MajorKey 1, 3) ]
        | has 3                      = [ (k MajorKey 10, 4), (k MajorKey 3, 3)
                                       , (k MajorKey 8, 2), (k MinorKey 0, 3)
                                       , (k MinorKey 7, 1) ]
        | has 4                      = [ (k MajorKey 0, 4), (k MajorKey 7, 2)
                                       , (k MajorKey 5, 2), (k MinorKey 5, 2) ]
        | otherwise                  = []
  in offsets

-- Best form for the bar within one key, plus this key's functional votes.
emission :: Map.Map KeyArea Double -> BarFacts -> KeyArea -> (Double, KeyForm)
emission voteMap bf key =
  let (best, fm) = maximumBy (comparing fst)
                     [ (membScore bf s, f) | (f, s) <- keyForms key ]
  in (best + Map.findWithDefault 0 key voteMap, fm)

-------------------------------------------------------------------------------
-- Transition cost and the cyclic Viterbi
-------------------------------------------------------------------------------

-- |Key-switch penalty (probe-calibrated, see module header).
kaLambda :: Double
kaLambda = 6

-- |Boundary bonus when the bar BEFORE the switch is dominant-functioning
-- of the new key (V7 shell a P5 above the tonic, or a dim\/half-dim shell
-- a semitone below it) — modulations announced by their dominant switch
-- cheaper.
kaDomBonus :: Double
kaDomBonus = 3

-- |Boundary bonus when the arrival bar is tonic-functioning in the new key.
kaTonicBonus :: Double
kaTonicBonus = 1.5

isDomOf :: BarFacts -> KeyArea -> Bool
isDomOf bf (KeyArea t _) =
  let ivs = bfZeroSet bf
      off = (bfRoot bf - t) `mod` 12
  in (off == 7 && 4 `Set.member` ivs && 10 `Set.member` ivs)
     || (off == 11 && 3 `Set.member` ivs && 6 `Set.member` ivs)

isTonicOf :: BarFacts -> KeyArea -> Bool
isTonicOf bf (KeyArea t q) =
  bfRoot bf == t && case q of
    MajorKey -> 4 `Set.member` bfZeroSet bf
    MinorKey -> 3 `Set.member` bfZeroSet bf

-- The switch cost depends only on the TARGET key (and the two bars around
-- the boundary), never on the source key or the Viterbi conditioning
-- start — so it is computed once per bar step as a target-indexed map and
-- shared across the whole inner loop and all 24 conditioned runs.
switchCostTo :: BarFacts -> BarFacts -> Map.Map KeyArea Double
switchCostTo prevBf curBf = Map.fromList
  [ (k', max (kaLambda / 4) (kaLambda - bonus k')) | k' <- allKeys ]
  where
    bonus k' = (if isDomOf prevBf k' then kaDomBonus else 0)
             + (if isTonicOf curBf k' then kaTonicBonus else 0)

-- Exact cyclic Viterbi: condition on bar 0's key, run the linear DP, close
-- the wrap edge, take the global optimum. Ties resolve deterministically
-- (lowest tonic pitch class, major before minor).
bestKeyPath :: [BarFacts] -> [(KeyArea, KeyForm)]
bestKeyPath []  = []
bestKeyPath bfs =
  let vms  = map (Map.fromListWith (+) . votes) bfs
      emit = [ Map.fromList [ (key, emission vm bf key) | key <- allKeys ]
             | (bf, vm) <- zip bfs vms ]
      n    = length bfs
      bfV  = V.fromList bfs
      emV  = V.fromList emit
      -- Per-step target-indexed switch costs, shared across all 24 runs.
      costV = V.fromList
        [ switchCostTo (bfV V.! (i - 1)) (bfV V.! i) | i <- [1 .. n - 1] ]
      wrapCost = switchCostTo (bfV V.! (n - 1)) (bfV V.! 0)
      run k0 =
        let e0 = fst (emV V.! 0 Map.! k0)
            step (scores, bps) i =
              let costTo = costV V.! (i - 1)
                  scores' = Map.fromList
                    [ (k', cand)
                    | k' <- allKeys
                    , let e   = fst (emV V.! i Map.! k')
                          c   = costTo Map.! k'
                          cand = maximumBy (comparing fst)
                            [ ( scores Map.! k + e - (if k == k' then 0 else c)
                              , k )
                            | k <- allKeys ]
                    ]
              in ( Map.map fst scores', Map.insert i (Map.map snd scores') bps )
            init0 = ( Map.fromList
                        [ (k, if k == k0 then e0 else -1 / 0) | k <- allKeys ]
                    , Map.empty )
            (final, paths) = foldl' step init0 [1 .. n - 1]
            wrap k = final Map.! k - (if k == k0 then 0 else wrapCost Map.! k0)
            kLast  = maximumBy (comparing wrap) allKeys
            walkBack i k acc
              | i == 0    = k : acc
              | otherwise = walkBack (i - 1) ((paths Map.! i) Map.! k) (k : acc)
        in (wrap kLast, if n == 1 then [k0] else walkBack (n - 1) kLast [])
      (_, keys) = maximumBy (comparing fst) (map run allKeys)
  in [ (key, snd (emV V.! i Map.! key)) | (i, key) <- zip [0 ..] keys ]

-------------------------------------------------------------------------------
-- M set: the key form, or the override ladder
-------------------------------------------------------------------------------

allModeSetsAt :: Int -> [(Sc.ModeQuality, Set Int)]
allModeSetsAt r =
  [ ( q
    , Set.fromList (map Pt.unPitchClass (Sc.modeChroma (Sc.Mode q (Pt.mkPitchClass r)))) )
  | q <- [minBound .. maxBound] ]

-- The bar's 7-PC mode set. When the chord sits inside its key form, the
-- form IS the mode set (chord-scale theory: one parent scale per key
-- area, expressed as the mode on each bar's root). Otherwise the ladder:
-- seeded special cases (half-diminished outside a minor area -> harmonic
-- minor a whole step below; altered dominant -> its altered scale), then
-- a generic search of the 28-quality vocabulary rooted at the harmonic
-- root (chord contained, fewest tones foreign to the key form), then the
-- best-coverage gap fallback. Override sets are always built FROM mode
-- templates, so they can never escape the vocabulary.
mSetFor :: BarFacts -> (KeyArea, KeyForm) -> (Set Int, ModeTier)
mSetFor bf (key, formNm) =
  let formSet = maybe Set.empty id (lookup formNm (keyForms key))
      chord   = bfAbs bf
      ivs     = Set.fromList (bfZero bf)
      has     = (`Set.member` ivs)
      r       = bfRoot bf
      seeded
        | has 3 && has 6 && not (has 4) = Just (scaleSetAt ((r - 2) `mod` 12) harmSteps)
        | has 4 && has 10 && (has 1 || has 3) = Just (scaleSetAt r altSteps)
        | otherwise = Nothing
      fits s = chord `Set.isSubsetOf` s
      foreignTo s = Set.size (s `Set.difference` formSet)
      searched = case filter (fits . snd) (allModeSetsAt r) of
                   [] -> Nothing
                   xs -> Just (snd (minimumOn (foreignTo . snd) xs))
      coverage s = length [ p | p <- absOf r (J.bvTarget (bfVocab bf)
                                              ++ J.bvStrong (bfVocab bf))
                              , p `Set.member` s ]
      gapBest = snd (maximumBy (comparing (coverage . snd)) (allModeSetsAt r))
  in if fits formSet then (formSet, TierForm)
     else case seeded of
            Just s | fits s -> (s, TierSeed)
            _ -> case searched of
                   Just s  -> (s, TierSearch)
                   Nothing -> (gapBest, TierGap)
  where minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)

-------------------------------------------------------------------------------
-- S set: the pentatonic pass
-------------------------------------------------------------------------------

-- |Pentatonic-switch penalty (probe-calibrated, see module header).
kaPentaLambda :: Double
kaPentaLambda = 4

-- The S vocabulary is exactly 'Sc.MajorPenta' — the one anhemitonic
-- pentatonic set class (its rotations include the minor pentatonic).
pentaSteps :: [Int]
pentaSteps = map Pt.unPitchClass (Sc.familyChroma Sc.MajorPenta)

pentaSet :: Int -> Set Int
pentaSet r = Set.fromList [ (r + s) `mod` 12 | s <- pentaSteps ]

-- Guide-tone coverage first, avoid tones hard-penalised, foreign tones
-- (outside the bar's M set) priced but not forbidden — an out-of-key
-- pentatonic wins only where the harmony demands it (chromaticism enabled
-- by the pentatonic's melodic self-sufficiency).
pentaEmission :: BarFacts -> Set Int -> Int -> Double
pentaEmission bf mset pr =
  let p       = pentaSet pr
      v       = bfVocab bf
      tgt     = nub (absOf (bfRoot bf) (J.bvTarget v))
      strong  = nub (absOf (bfRoot bf) (J.bvStrong v)) `minus` tgt
      avoid   = nub (absOf (bfRoot bf) (J.bvAvoid v))
      minus xs ys = [ x | x <- xs, x `notElem` ys ]
      count ps = fromIntegral (length [ x | x <- ps, x `Set.member` p ])
      foreignN = fromIntegral (Set.size (p `Set.difference` mset))
  in 3 * count tgt + 2 * count strong - 3 * count avoid - 2 * foreignN

pentaPath :: [BarFacts] -> [Set Int] -> [Int]
pentaPath [] _ = []
pentaPath bfs msets =
  let n    = length bfs
      emit = V.fromList
               [ Map.fromList [ (pr, pentaEmission bf ms pr) | pr <- [0 .. 11] ]
               | (bf, ms) <- zip bfs msets ]
      run p0 =
        let step (scores, bps) i =
              let scores' = Map.fromList
                    [ (p', cand)
                    | p' <- [0 .. 11]
                    , let cand = maximumBy (comparing fst)
                            [ ( scores Map.! p + emit V.! i Map.! p'
                                - (if p == p' then 0 else kaPentaLambda), p )
                            | p <- [0 .. 11] ] ]
              in (Map.map fst scores', Map.insert i (Map.map snd scores') bps)
            init0 = ( Map.fromList
                        [ (p, if p == p0 then emit V.! 0 Map.! p0 else -1 / 0)
                        | p <- [0 .. 11] ]
                    , Map.empty )
            (final, paths) = foldl' step init0 [1 .. n - 1]
            wrap p = final Map.! p - (if p == p0 then 0 else kaPentaLambda)
            pLast  = maximumBy (comparing wrap) [0 .. 11]
            walkBack i p acc
              | i == 0    = p : acc
              | otherwise = walkBack (i - 1) ((paths Map.! i) Map.! p) (p : acc)
        in (wrap pLast, if n == 1 then [p0] else walkBack (n - 1) pLast [])
      (_, best) = maximumBy (comparing fst) (map run [0 .. 11])
  in best

-------------------------------------------------------------------------------
-- Public analysis
-------------------------------------------------------------------------------

-- |Analyze a finished progression: one 'BarAnalysis' per bar. Pure and
-- deterministic; cyclic (the wrap edge counts, so rotating the bars
-- rotates the answers).
analyzeProgression :: Pr.Progression -> [BarAnalysis]
analyzeProgression prog =
  let bars = toList (Pr.unProgression prog)
      bfs  = map barFacts bars
      keys = bestKeyPath bfs
      mres = [ mSetFor bf ka | (bf, ka) <- zip bfs keys ]
      ps   = pentaPath bfs (map fst mres)
      n    = length bars
      keyAt i = fst (keys !! (i `mod` n))
  in [ BarAnalysis
         { baKey        = key
         , baForm       = fm
         , baModeSet    = mset
         , baMode       = Sc.classifyModeAt (bfRoot bf)
                            (map Pt.mkPitchClass (Set.toList mset))
         , baTier       = tier
         , baPentaRoot  = pr
         , baPentaInKey = pentaSet pr `Set.isSubsetOf` mset
         , baBoundary   = n > 1 && key /= keyAt (i - 1 + n)
         }
     | (i, bf, (key, fm), (mset, tier), pr)
         <- zip5 [0 ..] bfs keys mres ps
     ]
  where
    zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a, b, c, d, e) : zip5 as bs cs ds es
    zip5 _ _ _ _ _ = []

-- |Per-bar 7-PC palettes for the walking bass — the M sets of
-- 'analyzeProgression', in the exact shape the walk consumes.
barPalettes :: Pr.Progression -> V.Vector (Set Int)
barPalettes = V.fromList . map baModeSet . analyzeProgression

-------------------------------------------------------------------------------
-- Layer derivation
-------------------------------------------------------------------------------

-- |Fill the S (pentatonic) and M (mode) layers of a context from the
-- key-area analysis of its triad layer. Identity on 'PC.FStrata' and
-- 'PC.FPoly' contexts, whose layers are already meaningful. Layer bars are
-- built exactly like genP's aux layers ('H.Unison' movement, chroma
-- expressed as intervals from the bar root), so they voice through the
-- same chroma engine ('Harmonic.Interface.Tidal.Arranger.strataModeFlow')
-- and print as mode names via the
-- 7-PC display path. The M bar is rooted on the bar's harmonic root (the
-- mode OF the bar — every tier of the override ladder contains it); the S
-- bar is rooted on the PENTATONIC's own root — a pentatonic legitimately
-- excludes the chord root (the pent-on-the-fifth over a maj7 does so by
-- design), and the set is named from its own root. (Bar roots seed only
-- bar 0's lattice in the chroma engine; pattern indices then track their
-- lattice slot, not the bar root.)
-- Provenance stays 'Nothing' (provenance is strata-specific and gates
-- strata regen \/ scoring).
--
-- The derived bars are forced to normal form as part of the result's
-- WHNF: 'PC.ProgressionContext' fields are lazy, and the first thing to
-- inspect an unforced layer can be the derived 'Eq' inside an arrange \/
-- lineHarmony cache lookup — i.e. the audio thread. Generated contexts
-- are forced when the generator prints them, so their analysis always
-- lands at generation time; a hand-applied 'chordscale' evaluates at its
-- own first use — print it (or run
-- 'Harmonic.Interface.Tidal.ChordscaleT.chordscaleReport' on it) at build time to
-- keep the work off the performance path.
chordscale :: PC.ProgressionContext -> PC.ProgressionContext
chordscale ctx
  | PC.pcFamily ctx `elem` [PC.FStrata, PC.FPoly] = ctx
  | Pr.progLength (PC.triadLayer ctx) == 0        = ctx
  | otherwise =
      let bars = toList (Pr.unProgression (PC.triadLayer ctx))
          anns = analyzeProgression (PC.triadLayer ctx)
          mk rootN set =
            let rootPC = Pt.unPitchClass (Pt.pitchClass rootN)
                ivs    = sort (nub [ (p - rootPC) `mod` 12 | p <- Set.toList set ])
            in H.mkCadenceStatePCs rootN H.Unison ivs
          spellAs cs = case H.stateSpelling cs of
            H.FlatSpelling -> Pt.flat . Pt.mkPitchClass
            _              -> Pt.sharp . Pt.mkPitchClass
          -- The M bar roots on the harmonic root; when that root is not a
          -- member of the set (a silent bar's default root, never a real
          -- override tier), fall to the set's lowest member — the
          -- constructor force-inserts interval 0, so an outside root would
          -- silently grow the bar to 8 tones.
          mRootFor cs a
            | bfRoot facts `Set.member` baModeSet a =
                H.chordNoteName (H.fromCadenceState cs)
            | otherwise = spellAs cs (Set.findMin (baModeSet a))
            where facts = barFacts cs
          sBars = [ mk (spellAs cs (baPentaRoot a)) (pentaSet (baPentaRoot a))
                  | (cs, a) <- zip bars anns ]
          mBars = [ mk (mRootFor cs a) (baModeSet a)
                  | (cs, a) <- zip bars anns ]
          forceBars = foldr (\cs acc -> forceCS cs `seq` acc) ()
          forceCS cs =
            let cad = H.stateCadence cs
            in length (H.cadenceFunctionality cad)
               `seq` foldr seq () (H.cadenceIntervals cad)
               `seq` H.stateCadenceRoot cs
               `seq` H.stateSpelling cs
          sLayer = Pr.fromCadenceStates sBars
          mLayer = Pr.fromCadenceStates mBars
      in forceBars sBars `seq` forceBars mBars
         `seq` Pr.progLength sLayer `seq` Pr.progLength mLayer
         `seq` ctx { PC.strataLayer = sLayer, PC.modeLayer = mLayer }
