-- |
-- Module      : Harmonic.Framework.Builder.PolyGen
-- Description : The genE paradigm — polytonal three-layer generation
--
-- The foundation progression (T layer) is a plain 'Harmonic.Framework.Builder.gen'
-- walk, byte-identical to it: same chain builder, same R constraints, same
-- entropy dial. Two partner triad chains (S\/M layers) are then walked over
-- the finished foundation, one bar at a time. Each partner draws from a
-- fresh transition list fetched from ITS OWN previous state — every partner
-- bar is a corpus-valid continuation of its own layer history — filtered to
-- the polytonal overlap rules against that bar's foundation triad:
--
-- * each partner shares exactly 2 pitch classes with the foundation bar;
-- * the three triads union to exactly 5 pitch classes.
--
-- Those two rules admit exactly two geometries per bar, and the traversal
-- chooses freely between them: COMMON-DYAD (all three triads share one
-- dyad; every layer pair sounds 4 tones) and BASE-ANCHORED (the partners
-- share different dyads of the foundation; T+S and T+M sound 4 tones, S+M
-- sounds the full pentad). The hub-tone shape (three different dyads
-- through one tone) unions to 4 and is excluded.
--
-- Partners honour the harmonic-space constraints only — key, allowed
-- roots, overtones — through the same R predicate as the walk
-- ('Core.matchesContextWithTarget' with no bass target). Root-motion
-- direction specs, drift, pedal and inversion spacing bind the foundation
-- alone: the foundation owns the bass whenever it is present, and the
-- partner layers stay free to diverge.
--
-- Selection: jointly valid (S, M) pairs are ranked by summed own-list rank
-- and drawn with ONE entropy-scaled gamma draw over the pair pool (the
-- two-stage per-layer alternative saturates the entropy dial at the ~8-
-- candidate second pool; the joint pool sits at ~100-324 where the dial is
-- monotone — measured in archive\/analysis\/poly_seq.md). Supply relaxes
-- down tiers when a list runs dry: one side from the space-constrained
-- pure enumeration over all 220 absolute 3-PC sets, then both, then the
-- unconstrained enumeration — the last is total, so partner selection can
-- never fail (the study measured the list tier alone at 100% over 2,100
-- live steps; archive\/analysis\/poly_chain.md).
--
-- S\/M identity is assigned once, after generation: the chain with the
-- lower whole-layer dissonance total becomes S. Per-bar assignment would
-- swap chain membership at ~45% of bar boundaries and destroy the very
-- layer identity the chains provide.
module Harmonic.Framework.Builder.PolyGen
  ( runPolyGen
  , runPolyGenFrom
  ) where

import           Data.Bits ((.&.), (.|.), popCount, setBit, testBit)
import           Data.Foldable (toList)
import           Data.List (intercalate, sort, sortBy)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Control.Monad (when)
import           System.Random.MWC (GenIO, createSystemRandom)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import           Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import           Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)

import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Core
                   ( TransitionSource, sourceFor, buildChainWith
                   , chainToProgression, extractCadence
                   , matchesContextWithTarget )
import           Harmonic.Framework.Builder.StrataGen (mkStarterDiag)

-------------------------------------------------------------------------------
-- Pitch-class set machinery
-------------------------------------------------------------------------------

-- Absolute pitch classes of a bar as a 12-bit mask.
absMask :: H.CadenceState -> Int
absMask cs =
  let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
  in foldl' setBit 0 [ (r + P.unPitchClass iv) `mod` 12
                     | iv <- H.cadenceIntervals (H.stateCadence cs) ]

pcsOf :: Int -> [Int]
pcsOf m = [ p | p <- [0 .. 11], testBit m p ]

-- Every absolute 3-PC set — the total partner universe behind the
-- enumeration tiers.
allTriadMasks :: [Int]
allTriadMasks = [ m | m <- [7 .. 4095], popCount m == 3 ]

-- Both admitted geometries in one predicate: each partner already shares
-- exactly 2 tones with the foundation, so union 5 admits common-dyad
-- (S∩M = 2, the shared dyad) and base-anchored (S∩M = 1, the hub tone)
-- while excluding hub-tone triples and coincident partners (union 4).
unionOK :: Int -> Int -> Int -> Bool
unionOK t s m = popCount (t .|. s .|. m) == 5

-- Harmonic root of an unrooted PC set: build from the lowest tone and let
-- inversion detection name the true root (deterministic on rotation ties).
harmonicRoot :: Int -> Int
harmonicRoot m =
  let pcs = pcsOf m
      low = minimum pcs
      cs  = H.mkCadenceStatePCs (P.flat (P.mkPitchClass low)) H.Unison
              [ (p - low) `mod` 12 | p <- pcs ]
  in P.unPitchClass (P.pitchClass (H.chordNoteName (H.fromCadenceState cs)))

-------------------------------------------------------------------------------
-- Partner chain steps
-------------------------------------------------------------------------------

-- Advance a partner chain onto a candidate cadence: root moves by the
-- cadence's movement, spelling carries over (partner chains keep a stable
-- enharmonic side rather than re-inferring per bar).
advCand :: H.CadenceState -> H.Cadence -> H.CadenceState
advCand prev cad =
  let r' = P.pitchClass (H.stateCadenceRoot prev) + H.fromMovement (H.cadenceMovement cad)
      sp = H.stateSpelling prev
  in H.CadenceState cad (H.enharmonicFunc sp r') sp

-- A ranked eligible continuation: rank in the (corpus-sorted) source it
-- came from, and the advanced state.
type Elig = (Int, (Int, H.CadenceState))   -- (mask, (rank, state))

-- Eligible continuations from a partner's own transition list: advanced
-- set shares exactly 2 tones with the foundation bar, differs from it, and
-- passes the harmonic-space R predicate (no bass target — direction specs
-- never bind partners). Deduped by advanced set, best list rank kept.
listEligible :: ParsedContext -> Int -> H.CadenceState -> [(H.Cadence, Double)] -> [Elig]
listEligible pctx tMask prev ts =
  Map.toList $ Map.fromListWith (\a b -> if fst a <= fst b then a else b)
    [ (mk, (rank, st))
    | (rank, (cad, _)) <- zip [0 ..] ts
    , matchesContextWithTarget Nothing pctx prev cad
    , let st = advCand prev cad
          mk = absMask st
    , mk /= tMask
    -- Each layer is a triad by the overlap algebra. The enumeration tiers
    -- are 3-PC by construction; the list tier inherits whatever the corpus
    -- holds, so the contract is asserted rather than assumed.
    , popCount mk == 3
    , popCount (mk .&. tMask) == 2 ]

-- Eligible continuations from the pure enumeration: every 3-PC set sharing
-- exactly 2 tones with the foundation bar, rooted on its own detected
-- harmonic root, reached by the real root movement from the partner's
-- previous bar. Ranked consonant-first AFTER any list candidates
-- (@baseRank@ = the list length), so list material always outranks
-- enumerated material at equal pool tier. The space flag applies the same
-- R predicate as the list tier; the unconstrained tier drops it and is
-- total — partner selection can never fail.
enumEligible :: Bool -> ParsedContext -> Int -> Int -> H.CadenceState -> [Elig]
enumEligible space pctx baseRank tMask prev =
  [ (mk, (baseRank + i, advCand prev cad))
  | (i, (mk, cad)) <- zip [0 ..] ranked
  , not space || matchesContextWithTarget Nothing pctx prev cad ]
  where
    prevRootPC = P.pitchClass (H.stateCadenceRoot prev)
    candOf mk =
      let root = harmonicRoot mk
          zf   = sort [ (p - root) `mod` 12 | p <- pcsOf mk ]
          zfP  = map P.mkPitchClass zf
          mv   = H.toMovement prevRootPC (P.mkPitchClass root)
      in (mk, H.Cadence (H.corpusFunctionality zfP) mv zfP)
    ranked = sortBy (comparing (dissOfMask . fst))
               [ candOf mk | mk <- allTriadMasks
                           , mk /= tMask
                           , popCount (mk .&. tMask) == 2 ]

-- Stored-zero-form dissonance of a set read from its detected root.
dissOfMask :: Int -> Integer
dissOfMask m =
  let r = harmonicRoot m
  in dissonanceScore (sort [ (p - r) `mod` 12 | p <- pcsOf m ])

-- Jointly valid (candidate-for-chain-1, candidate-for-chain-2) pairs,
-- ranked by summed rank so the corpus ordering shapes the pool the gamma
-- draw explores.
jointPairs :: Int -> [Elig] -> [Elig] -> [((Int, H.CadenceState), (Int, H.CadenceState))]
jointPairs tMask e1 e2 =
  sortBy (comparing (\((r1, _), (r2, _)) -> r1 + r2))
    [ (c1, c2)
    | (m1, c1) <- e1, (m2, c2) <- e2
    , unionOK tMask m1 m2 ]

-- Raw per-bar selection facts, chain-labelled (S\/M assignment happens at
-- the end); the renderers consume these through 'PolyDiag'.
data RawStep = RawStep
  { rsTier  :: String
  , rsPoolK :: Int
  , rsRank1 :: Maybe Int   -- chain-1 candidate's own-list rank (Nothing = enumerated)
  , rsRank2 :: Maybe Int
  }

-- One partner step: both chains fetch their own lists, tiers relax from
-- list×list through enumeration until a pool exists (the unconstrained
-- floor is total).
stepPartners :: GenIO -> TransitionSource -> ParsedContext -> Double
             -> H.CadenceState -> (H.CadenceState, H.CadenceState)
             -> IO (H.CadenceState, H.CadenceState, RawStep)
stepPartners rng source pctx ent tBar (prev1, prev2) = do
  ts1 <- source (T.pack (show (H.stateCadence prev1)))
  ts2 <- source (T.pack (show (H.stateCadence prev2)))
  let tMask = absMask tBar
      l1 = listEligible pctx tMask prev1 ts1
      l2 = listEligible pctx tMask prev2 ts2
      e1 = enumEligible True  pctx (length ts1) tMask prev1
      e2 = enumEligible True  pctx (length ts2) tMask prev2
      u1 = enumEligible False pctx (length ts1) tMask prev1
      u2 = enumEligible False pctx (length ts2) tMask prev2
      ladder = [ ("list",      jointPairs tMask l1 l2)
               , ("list+enum", jointPairs tMask l1 e2)
               , ("list+enum", jointPairs tMask e1 l2)
               , ("enum",      jointPairs tMask e1 e2)
               , ("free",      jointPairs tMask u1 u2) ]
      listRank listed r = if r < listed then Just r else Nothing
  case filter (not . null . snd) ladder of
    ((tier, pool) : _) -> do
      idx <- gammaIndexScaledWith rng ent (length pool)
      let ((r1, st1), (r2, st2)) = pool !! idx
      pure ( st1, st2
           , RawStep tier (length pool)
                     (listRank (length ts1) r1) (listRank (length ts2) r2) )
    [] -> error "genE: partner pool empty at the unconstrained tier — unreachable (the enumeration is total)"

-- Walk both partner chains across the finished foundation. Bar 0 partners
-- come from the enumeration (a cue has no transition list), drawn from the
-- dissonance-ranked space-constrained pool.
partnerPass :: GenIO -> TransitionSource -> ParsedContext -> Double
            -> [H.CadenceState]
            -> IO ([H.CadenceState], [H.CadenceState], [RawStep])
partnerPass _ _ _ _ [] = pure ([], [], [])
partnerPass rng source pctx ent (cueBar : rest) = do
  let tMask0 = absMask cueBar
      e0     = enumEligible True pctx 0 tMask0 cueBar
      u0     = enumEligible False pctx 0 tMask0 cueBar
      pool0  = case jointPairs tMask0 e0 e0 of
                 [] -> jointPairs tMask0 u0 u0
                 ps -> ps
  idx <- gammaIndexScaledWith rng ent (length pool0)
  let ((_, s0), (_, m0)) = pool0 !! idx
      raw0 = RawStep "enum" (length pool0) Nothing Nothing
      go _ [] acc = pure (reverse acc)
      go prevs (tBar : more) acc = do
        (st1, st2, raw) <- stepPartners rng source pctx ent tBar prevs
        go (st1, st2) more ((st1, st2, raw) : acc)
  steps <- go (s0, m0) rest []
  pure ( s0 : [ a | (a, _, _) <- steps ]
       , m0 : [ b | (_, b, _) <- steps ]
       , raw0 : [ r | (_, _, r) <- steps ] )

-- Whole-layer ordering: the chain with the lower dissonance total becomes
-- S. Ties (0-2% of runs) break on canonical zero-forms, then roots —
-- deterministic, no musical claim. The flag reports whether the chains
-- swapped, so per-bar diagnostics can relabel their chain-bound fields.
orderChains :: [H.CadenceState] -> [H.CadenceState]
            -> ([H.CadenceState], [H.CadenceState], Bool)
orderChains c1 c2 =
  if keyOf c1 <= keyOf c2 then (c1, c2, False) else (c2, c1, True)
  where
    keyOf ch = ( sum (map barDiss ch)
               , map (canon . absMask) ch
               , map (P.unPitchClass . P.pitchClass . H.stateCadenceRoot) ch )
    barDiss cs = dissonanceScore
      (map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs)))
    canon m = minimum [ sort [ (p - t) `mod` 12 | p <- pcsOf m ] | t <- pcsOf m ]

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------

diagLevel :: Verbosity -> Maybe Int
diagLevel Silent   = Nothing
diagLevel Standard = Just 1
diagLevel Verbose  = Just 2

-- |Execute a 'PolyMode' config: foundation walk (byte-identical to 'gen'),
-- then the partner pass, then S\/M assignment.
runPolyGen :: GenConfig -> IO (PC.ProgressionContext, GenerationDiagnostics)
runPolyGen gc = do
  start <- _gcCue gc
  when (length (H.cadenceIntervals (H.stateCadence start)) /= 3) $
    error "genE cues are exactly 3 tones — each layer is a triad by the overlap algebra; richer structures come from combining layers (TS/TM/SM/TSM), not from the cue"
  let pctx = parseContextOnce (_gcTonal gc)
      n    = _gcLen gc
  rng    <- createSystemRandom
  source <- sourceFor (T.pack (_gcSeek gc))
  (chain, stepDiags) <- buildChainWith source rng
                          (diagLevel (_gcVerbosity gc)) (_gcEntropy gc)
                          (_gcTonal gc) (const pctx) start (n - 1)
  (p1, p2, raws) <- partnerPass rng source pctx (_gcEntropy gc) chain
  let (sChain, mChain, swapped) = orderChains p1 p2
      prog = chainToProgression chain
      pcx = PC.ProgressionContext
        { PC.triadLayer   = prog
        , PC.strataLayer  = Prog.fromCadenceStates sChain
        , PC.modeLayer    = Prog.fromCadenceStates mChain
        , PC.pcProvenance = Nothing
        , PC.pcFamily     = PC.FPoly
        }
      -- Diagnostics carry one entry per bar (starter row for the cue),
      -- each with the bar's PolyDiag; the renderers relabel nothing —
      -- chain-bound fields are already S/M-ordered here.
      -- A one-bar walk takes no steps, so there is no step diagnostic to
      -- hang the bar's PolyDiag on and genE'' prints no per-bar table at
      -- len 1. Degenerate case — the grids still print.
      polySteps = case stepDiags of
        [] -> []
        _  -> [ d { sdStepNumber = i, sdPoly = Just pd }
              | (i, d, pd) <- zip3 [1 ..] (mkStarterDiag start : stepDiags)
                                   (polyDiagsFor pcx swapped raws) ]
      diag = GenerationDiagnostics
        { gdStartCadence = show (extractCadence start)
        , gdStartRoot    = show (H.stateCadenceRoot start)
        , gdRequestedLen = n
        , gdActualLen    = Prog.progLength prog
        , gdEntropy      = _gcEntropy gc
        , gdSteps        = polySteps
        , gdProgression  = prog
        , gdJazzTrace    = []
        }
  pure (pcx, diag)

-- |Regenerate a contiguous range of bars within an existing polytonal
-- context. The foundation range regenerates exactly like a 'gen' regen
-- (cue = the bar before the range, inferred by 'genFrom'); both partner
-- chains regenerate over it, seeded from the KEPT partner bars before the
-- range, so every regenerated partner bar continues its own layer's
-- history. The source's S\/M labelling is preserved — a partial regen
-- never reorders chains (that would relabel the kept bars).
--
-- Seam: when the regen does not cover the whole progression, the final
-- regenerated bar's joint pool is additionally filtered to pairs whose
-- partner states can continue onto the kept next partner bars as real
-- graph edges — relaxed when empty (the study measured list-tier supply
-- at 100%, so the filter is a preference, not a wall).
runPolyGenFrom :: PC.ProgressionContext -> Int -> Int -> GenConfig
               -> IO (PC.ProgressionContext, GenerationDiagnostics)
runPolyGenFrom srcPC s _e gc = do
  start <- _gcCue gc
  let pctx  = parseContextOnce (_gcTonal gc)
      srcN  = PC.pcLength srcPC
      rSize = _gcLen gc
      effE  = ((s - 1 + rSize - 1) `mod` srcN) + 1
      cuePos = ((s - 2) `mod` srcN) + 1
      barAt lyr i = case Prog.getCadenceState (lyr srcPC) i of
        Just cs -> cs
        Nothing -> error "genFrom (poly): source bar out of range"
      sSeed = barAt PC.strataLayer cuePos
      mSeed = barAt PC.modeLayer cuePos
      -- Kept partner bars after the seam (Nothing on a full-cycle regen).
      keptNext
        | rSize >= srcN = Nothing
        | otherwise =
            let nextPos = (effE `mod` srcN) + 1
            in Just (barAt PC.strataLayer nextPos, barAt PC.modeLayer nextPos)
  rng    <- createSystemRandom
  source <- sourceFor (T.pack (_gcSeek gc))
  (chain, stepDiags) <- buildChainWith source rng
                          (diagLevel (_gcVerbosity gc)) (_gcEntropy gc)
                          (_gcTonal gc) (const pctx) start rSize
  let newFound = drop 1 chain
      -- Offline (empty list) can't verify continuity — accept rather than
      -- spin the retry budget on an unverifiable preference.
      canContinue next ts =
        null ts || show (H.stateCadence next) `elem` [ show cad | (cad, _) <- ts ]
      go _ [] acc = pure (reverse acc)
      go prevs (tBar : more) acc = do
        (st1, st2, raw) <- stepPartners rng source pctx (_gcEntropy gc) tBar prevs
        -- Seam preference on the final regenerated bar: keep drawing pairs
        -- until one reaches the kept next partner bars, bounded by the
        -- pool-shaped retry budget; fall back to the unfiltered draw.
        (st1', st2', raw') <-
          case (more, keptNext) of
            ([], Just (sNext, mNext)) -> do
              let retry 0 best = pure best
                  retry k best@(b1, b2, _) = do
                    tsS <- source (T.pack (show (H.stateCadence b1)))
                    tsM <- source (T.pack (show (H.stateCadence b2)))
                    if canContinue sNext tsS && canContinue mNext tsM
                      then pure best
                      else do
                        cand <- stepPartners rng source pctx (_gcEntropy gc) tBar prevs
                        retry (k - 1 :: Int) cand
              retry 8 (st1, st2, raw)
            _ -> pure (st1, st2, raw)
        go (st1', st2') more ((st1', st2', raw') : acc)
  steps <- go (sSeed, mSeed) newFound []
  let newS = [ a | (a, _, _) <- steps ]
      newM = [ b | (_, b, _) <- steps ]
      raws = RawStep "seed" 0 Nothing Nothing : [ r | (_, _, r) <- steps ]
      triad'  = Prog.spliceProgression (PC.triadLayer srcPC)  s effE newFound
      strata' = Prog.spliceProgression (PC.strataLayer srcPC) s effE newS
      mode'   = Prog.spliceProgression (PC.modeLayer srcPC)   s effE newM
      pcx = PC.ProgressionContext triad' strata' mode' Nothing PC.FPoly
      -- Trace context: seed bar + regenerated bars, in walk order.
      insPC = PC.ProgressionContext
        { PC.triadLayer   = Prog.fromCadenceStates chain
        , PC.strataLayer  = Prog.fromCadenceStates (sSeed : newS)
        , PC.modeLayer    = Prog.fromCadenceStates (mSeed : newM)
        , PC.pcProvenance = Nothing
        , PC.pcFamily     = PC.FPoly
        }
      polySteps = case stepDiags of
        [] -> []
        _  -> [ d { sdStepNumber = i, sdPoly = Just pd }
              | (i, d, pd) <- zip3 [1 ..] (mkStarterDiag start : stepDiags)
                                   (polyDiagsFor insPC False raws) ]
      diag = GenerationDiagnostics
        { gdStartCadence = show (extractCadence start)
        , gdStartRoot    = show (H.stateCadenceRoot start)
        , gdRequestedLen = rSize + 1
        , gdActualLen    = length chain
        , gdEntropy      = _gcEntropy gc
        , gdSteps        = polySteps
        , gdProgression  = Prog.fromCadenceStates chain
        , gdJazzTrace    = []
        }
  pure (pcx, diag)

-- Per-bar 'PolyDiag' records for a finished polytonal context: names are
-- rendered with each bar's own spelling; chain-bound facts (list ranks)
-- follow the final S/M assignment.
polyDiagsFor :: PC.ProgressionContext -> Bool -> [RawStep] -> [PolyDiag]
polyDiagsFor pcx swapped raws =
  [ PolyDiag
      { pdGeometry = case popCount (absMask sBar .&. absMask mBar) of
          2 -> "common-dyad"
          _ -> "base-anchored"
      , pdTier  = rsTier raw
      , pdPoolK = rsPoolK raw
      , pdSRank = if swapped then rsRank2 raw else rsRank1 raw
      , pdMRank = if swapped then rsRank1 raw else rsRank2 raw
      , pdSName = nameBar sBar
      , pdMName = nameBar mBar
      , pdDyad  = renderDyad tBar (absMask tBar .&. absMask sBar .&. absMask mBar)
      , pdPairTS = nameBar tsBar
      , pdPairTM = nameBar tmBar
      , pdPairSM = nameBar smBar
      , pdPentad = nameBar tsmBar
      }
  | (tBar, sBar, mBar, tsBar, tmBar, smBar, tsmBar, raw) <-
      zip8 (bars PC.T) (bars PC.S) (bars PC.M)
           (bars PC.TS) (bars PC.TM) (bars PC.SM) (bars PC.TSM) raws ]
  where
    bars sel = toList (Prog.unProgression (PC.layer sel pcx))
    nameBar cs = Prog.showHarmony (H.enharmonicFunc (H.stateSpelling cs)) cs
    renderDyad tBar m =
      intercalate "+" [ show (H.enharmonicFunc (H.stateSpelling tBar)
                                (P.mkPitchClass p))
                      | p <- pcsOf m ]
    zip8 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
      (a, b, c, d, e, f, g, h) : zip8 as bs cs ds es fs gs hs
    zip8 _ _ _ _ _ _ _ _ = []
