-- |
-- Module      : Harmonic.Framework.Builder.Modifiers
-- Description : The pure 'GenConfig' surface — entrypoints and modifiers
--
-- Every name here is a pure record construction or update over
-- 'GenConfig': the family entrypoints (@gen@\/@genE@\/@genP@\/@genJ@ and
-- their verbosity primes), the regeneration entrypoints ('genFrom'
-- family), and the modifier chain ('cue', 'len', 'entropy', 'tonal',
-- 'attempt', …). No IO beyond the deferred cue draw, no music theory —
-- execution lives behind 'Harmonic.Framework.Builder.seek' in the facade.

module Harmonic.Framework.Builder.Modifiers
  ( defaultGenConfig
  , gen, gen', gen''
  , genGrid
  , genGrid'
  , genGrid''
  , genE, genE', genE''
  , genJ, genJ', genJ''
  , genFrom, genFrom', genFrom''
  , cue
  , len
  , entropy
  , steer
  , attempt
  , viability
  , tonal
  , relStrata
  , absStrata
  , sameBoost, flipBoost, triBoost
  , genP, genP', genP''
  , genI, genII, genIII, genIV, genV, genVI, genVII, genVIII, genIX, genX, genXI
  , genI', genII', genIII', genIV', genV', genVI', genVII', genVIII', genIX', genX', genXI'
  , genI'', genII'', genIII'', genIV'', genV'', genVI'', genVII'', genVIII'', genIX'', genX'', genXI''
  ) where

import           System.Random.MWC (createSystemRandom, uniformRM)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc

import           Harmonic.Framework.Builder.Types

-- |Default generation configuration.
--
-- @
-- cue:     random root, major triad
-- len:     4
-- seek:    "*" (all composers)
-- entropy: 0.2
-- tonal:   hContext (chromatic)
-- @
defaultGenConfig :: GenConfig
defaultGenConfig = GenConfig
  { _gcCue         = defaultCue
  , _gcCueExplicit = False
  , _gcLen         = 4
  , _gcSeek        = "*"
  , _gcEntropy     = 0.2
  , _gcTonal       = hContext
  , _gcVerbosity   = Silent
  , _gcMode        = Fresh
  , _gcLenOverride = Nothing
  , _gcRelStrata   = Nothing
  , _gcAbsStrata   = Nothing
  -- Plan defaults: same-strata 0.90, flip-flop 0.80, same-tristrata 0.70.
  -- Values < 1.0 multiply @badness@ down (favouring the candidate); 1.0 is
  -- the no-op. The product caps at 0.70 * 0.80 * 0.90 ≈ 0.50, giving
  -- ≤2× favouring — overpowerable by strong graph-side confidence.
  , _gcBoostSame   = 0.90
  , _gcBoostFlip   = 0.80
  , _gcBoostTri    = 0.70
  , _gcSteer       = 3.0
  , _gcMaxAttempts  = 1
  , _gcViableTarget = 1
  -- Calibrated from a 30-sample online probe (gen, 8 bars, entropy 0.4,
  -- seek "*"): totalScore observed at min 0.375, median ~0.59, max 0.725
  -- on the mode-validity-gated weight scale (the probe ran on the old
  -- 0.2-mv scale — values remapped exactly via (old − 0.2) / 0.8 when mv
  -- was demoted to gate-only). T=0.5 catches the same bottom ~20% of
  -- attempts (fallback-driven or tritone-leap runs), keeping
  -- 'attempt 3 12' reliable. Tune with the 'viability' modifier.
  , _gcViabilityFloor = 0.5
  }
  where
    defaultCue = do
      rng <- createSystemRandom
      rootIdx <- uniformRM (0 :: Int, 11) rng
      let rootName = H.enharmonicFunc H.FlatSpelling (P.mkPitchClass rootIdx)
      pure $ H.initCadenceState 0 (show rootName) [0, 4, 7]

-- |Generation config with header + grid output (default).
--
-- @
-- s <- seek "*" $ gen
-- s <- seek "*" $ cue start $ tonal ctx $ len 4 $ entropy 0.3 $ gen
-- @
gen :: GenConfig
gen = defaultGenConfig

-- |Generation config with compact musical summary.
gen' :: GenConfig
gen' = defaultGenConfig { _gcVerbosity = Standard }

-- |Generation config with verbose diagnostic traces.
gen'' :: GenConfig
gen'' = defaultGenConfig { _gcVerbosity = Verbose }

-- |Static grid: repeats the cue chord for 'len' bars. No database access.
--
-- @s <- seek "*" $ cue start $ len 4 $ genGrid@
genGrid :: GenConfig
genGrid = defaultGenConfig { _gcMode = GridMode }

-- |'genGrid' with compact musical summary.
genGrid' :: GenConfig
genGrid' = genGrid { _gcVerbosity = Standard }

-- |'genGrid' with verbose diagnostic traces.
genGrid'' :: GenConfig
genGrid'' = genGrid { _gcVerbosity = Verbose }

-- |The genE paradigm: polytonal three-layer generation. The T layer is a
-- foundation walk byte-identical to 'gen' (all R constraints apply to it
-- alone — the foundation owns the bass); the S\/M layers are partner triad
-- chains, each a corpus-valid walk of its own, sharing exactly 2 pitch
-- classes with the foundation per bar and unioning to exactly 5. The
-- traversal chooses freely per bar between the two admitted geometries —
-- common-dyad (every layer pair sounds 4 tones) and base-anchored (pairs
-- with the foundation sound 4, S+M sounds the pentad). Partners honour
-- key\/roots\/overtones but never direction specs or strata machinery.
-- Combine layers at the pattern surface via the 'PC.Layer' selectors
-- (@TS@\/@TM@\/@SM@\/@TSM@\/@PT@). Cues are triadic.
--
-- @s <- seek "*" $ len 8 $ entropy 0.3 $ genE'@
genE :: GenConfig
genE = defaultGenConfig { _gcMode = PolyMode }

-- |'genE' with compact musical summary (per-bar layer table).
genE' :: GenConfig
genE' = genE { _gcVerbosity = Standard }

-- |'genE' with verbose diagnostic traces (adds partner pool\/tier trace).
genE'' :: GenConfig
genE'' = genE { _gcVerbosity = Verbose }

-- |Generate over the jazz (Change) graph: the genJ family. Same
-- modifier chain as 'gen' (@seek "*" \$ cue start \$ len 8 \$ entropy 0.3 \$ genJ@);
-- the seek spec resolves against BOTH corpora (see
-- "Harmonic.Framework.Builder.JazzGen" for the full semantics: jazz
-- names blend, classical names steer via the 'steer' dial, @"*"@ walks
-- the whole corpus, @"none"@ is refused — the jazz graph has no offline
-- mode). Progressions carry variable-arity chords (3-6 tones) straight
-- from the corpus vocabulary, stamped 'PC.FJazz' so 'genFrom'
-- regenerates them jazz-natively and 'attempt' ranks them against the
-- @Change@ graph. 'tonal' constraints apply through the same R filter
-- as every family; a step they empty is relaxed with a notice (there is
-- deliberately no fallback pool).
genJ :: GenConfig
genJ = defaultGenConfig { _gcMode = JazzMode }

-- |'genJ' with Standard per-step trace (walk steps, pool sizes, picks).
genJ' :: GenConfig
genJ' = genJ { _gcVerbosity = Standard }

-- |'genJ' with Verbose trace (adds seek resolution and steer notices).
genJ'' :: GenConfig
genJ'' = genJ { _gcVerbosity = Verbose }

-- |Regenerate a range of bars within an existing progression.
-- The cue is inferred from the bar before the start position (wrapping).
-- A 'cue' override applies to every family EXCEPT 'PC.FStrata': the strata
-- regen must seed from the source's own bar to keep the walk-graph seam
-- valid, so it ignores the override.
--
-- FAMILY-AWARE: regeneration produces states of the family the source
-- progression already is — families never mix. The one downgrade is
-- deliberate: hand-built extended ('PC.FExtended') sources regenerate as
-- triads (with a printed notice), so the result re-infers as mixed
-- material rather than pretending the fusion family still exists.
--
-- * @pcProvenance = Just _@ — strata-aware path: regenerates all three
--   layers + provenance in lockstep, with one-step lookahead at the
--   @e → e+1@ seam to keep the spliced bar sequence walk-graph valid
--   under 'Harmonic.Framework.Builder.Strata.allowedNext'.
-- * 'PC.FPoly' (genE source) — polytonal path: regenerates the foundation
--   range plus both partner chains, seeded from the kept partner bars
--   before the range; the S\/M labelling of the source is preserved
--   (chains are never reordered by a partial regen).
-- * uniform 4-note hand-built material — regenerated bars come back as
--   plain triads with a printed notice.
-- * uniform 3-note (gen source) — plain triad regen.
-- * hand-mixed cardinalities — regenerated as plain triads with a printed
--   notice (hand-mixed material is the human aberration channel; regen
--   does not amplify it).
--
-- @s' <- seek "*" $ entropy 0.3 $ genFrom s 2 3@
-- @s' <- seek "*" $ cue start $ genFrom s 2 3    -- override inferred cue@
-- @s' <- seek "*" $ len 6 $ genFrom s 2 3        -- expand range@
-- @s' <- seek "*" $ genFrom'  s 2 3              -- Standard per-step trace@
-- @s' <- seek "*" $ genFrom'' s 2 3              -- Verbose trace (+ scoreboard with 'attempt')@
genFrom :: PC.ProgressionContext -> Int -> Int -> GenConfig
genFrom pc s e = defaultGenConfig
  { _gcCue  = inferCue
  , _gcLen  = rSize
  , _gcMode = case PC.pcFamily pc of
      PC.FStrata -> FromProgPC pc s e
      PC.FJazz   -> FromProgJ pc s e
      PC.FPoly   -> FromProgPoly pc s e
      _          -> FromProg (PC.triadLayer pc) s e
  }
  where
    triad = PC.triadLayer pc
    n0 = Prog.progLength triad
    -- A failed genP run returns an empty context; regenerating it would
    -- divide by zero in the wrap arithmetic below. Refuse loudly instead.
    n = if n0 == 0
          then error "genFrom: source progression is empty (a failed generation?) — nothing to regenerate"
          else n0
    rSize = if s <= e then e - s + 1 else n - s + 1 + e
    cuePos = ((s - 2) `mod` n) + 1  -- 1-indexed, wraps to N when s=1
    inferCue = case Prog.getCadenceState triad cuePos of
      Just cs -> pure cs
      Nothing -> _gcCue defaultGenConfig

-- |Standard-verbosity alias of 'genFrom'. Mirrors @gen'@, @genP'@ and @genI'@.
genFrom' :: PC.ProgressionContext -> Int -> Int -> GenConfig
genFrom' pc s e = (genFrom pc s e) { _gcVerbosity = Standard }

-- |Verbose-verbosity alias of 'genFrom'. Mirrors @gen''@, @genP''@ and @genI''@.
genFrom'' :: PC.ProgressionContext -> Int -> Int -> GenConfig
genFrom'' pc s e = (genFrom pc s e) { _gcVerbosity = Verbose }


-------------------------------------------------------------------------------
-- Generation Modifiers
-------------------------------------------------------------------------------

-- |Set starting state.
--
-- @s <- seek "*" $ cue start $ gen@
cue :: H.CadenceState -> GenConfig -> GenConfig
cue start gc = gc { _gcCue = pure start, _gcCueExplicit = True }

-- |Set progression length (number of chords).
--
-- @s <- seek "*" $ len 8 $ gen@
len :: Int -> GenConfig -> GenConfig
len n gc = gc { _gcLen = n, _gcLenOverride = Nothing }


-- |Set entropy (>= 0) — the gamma sampler targets rank @entropy * 10@
-- in the scored pool, so 0 usually takes the top candidate, 0.5 wanders
-- around the 5th, 1 around the 10th; values above 1 reach deeper still.
--
-- @s <- seek "*" $ entropy 0.5 $ gen@
entropy :: Double -> GenConfig -> GenConfig
entropy e gc = gc { _gcEntropy = e }

-- |Set the genJ classical-steer boost strength (default 3.0). Applies
-- only when the seek spec names classical composers: a jazz candidate
-- containing one of the steer blend's top recommended triads has its
-- score multiplied by up to @(1 + strength)@. 0 disables steering
-- influence entirely. Initial calibration — tune by ear.
--
-- @s <- seek "debussy" $ steer 6 $ len 8 $ genJ'@
steer :: Double -> GenConfig -> GenConfig
steer x gc = gc { _gcSteer = max 0 x }

-- |Run multi-attempt rank-and-select generation: produce up to @maxAttempts@
-- candidate progressions, stop early once @viableTarget@ viable attempts
-- (all bars 'Harmonic.Rules.Types.Scale.ModeOk') have been collected, then return the highest-scoring
-- one. Scoring blends root motion, voice leading, and mode validity via
-- 'PS.defaultWeightsOffline'.
--
-- @s <- seek "*" $ attempt 3 24 $ entropy 0.4 $ gen@   -- best of up to 24
--
-- Polytonal ('Harmonic.Framework.Builder.genE') attempts rank differently,
-- because they are three progressions rather than one: each layer is scored
-- in its own right (half the weight on the foundation, a quarter on each
-- partner) and a divergence axis rewards the layers standing apart — the
-- point of the family. See 'PS.PolyScore'. Raising @maxAttempts@ raises both
-- the quality and the divergence of what comes back.
--
-- Defaults are @attempt 1 1@ — i.e. the modifier is a no-op when omitted,
-- preserving legacy single-pass behaviour.
attempt :: Int -> Int -> GenConfig -> GenConfig
attempt viableTarget maxAttempts gc = gc
  { _gcViableTarget = max 1 viableTarget
  , _gcMaxAttempts  = max 1 maxAttempts
  }

-- |Set the viability quality floor used by 'attempt'. An attempt is
-- /viable/ iff @psModeValidity >= 1.0@ (structural invariant) and
-- @totalScore >= floor@. Default is @0.5@; passing @0.0@ recovers the
-- original structural-only viability.
--
-- @s <- seek "*" $ viability 0.65 $ attempt 3 24 $ gen@
--
-- Tune downward if @attempt N K@ frequently fails to collect N viable
-- within K (raise K or lower the floor); tune upward if K is being hit
-- consistently with mediocre-quality picks (lower K or raise the floor).
viability :: Double -> GenConfig -> GenConfig
viability t gc = gc { _gcViabilityFloor = max 0 t }

-- |Set harmonic context (R constraints).
--
-- @s <- seek "*" $ tonal (hcKey "0#" $ hContext) $ gen@
tonal :: HarmonicContext -> GenConfig -> GenConfig
tonal ctx gc = gc { _gcTonal = ctx }

-- |Per-bar position within the dynamically-changing active tristrata. Elements
-- @∈ {1,2,3}@ cycle circularly. Sets '_gcLenOverride' to the parsed list length
-- so '_gcLen' doesn't need to be set explicitly; 'len' applied later clears the
-- override (last-writer-wins).
--
-- @s <- seek "none" $ relStrata "1 1 2 2 3 3" $ genVI@  -- 6 bars
relStrata :: String -> GenConfig -> GenConfig
relStrata s gc =
  let ns = Sc.parseRelStrata s
  in gc { _gcRelStrata = Just ns
        , _gcLenOverride = if null ns then Nothing else Just (length ns)
        }

-- |Per-bar absolute strata label across all tristratas. Elements are Roman
-- numerals @I..XI@ cycling circularly. Sets '_gcLenOverride' to the parsed
-- list length.
--
-- @s <- seek "none" $ absStrata "I V X" $ genI@  -- 3 bars
absStrata :: String -> GenConfig -> GenConfig
absStrata s gc =
  let ss = Sc.parseAbsStrata s
  in gc { _gcAbsStrata = Just ss
        , _gcLenOverride = if null ss then Nothing else Just (length ss)
        }

-- |Override the same-strata continuity boost multiplier. Values below 1.0
-- favour candidates whose strata matches the previous bar's. Default 0.90.
-- Pass 1.0 to disable the bias.
--
-- @s <- seek "none" $ sameBoost 0.5 $ genVI@  -- strong same-strata pull
sameBoost :: Double -> GenConfig -> GenConfig
sameBoost x gc = gc { _gcBoostSame = x }

-- |Override the flip-flop boost multiplier (candidates matching the
-- grandparent strata when the current /= previous). Default 0.80.
flipBoost :: Double -> GenConfig -> GenConfig
flipBoost x gc = gc { _gcBoostFlip = x }

-- |Override the same-tristrata continuity boost multiplier. Values below
-- 1.0 favour candidates whose active tristrata matches the previous bar's.
-- Default 0.70 (strongest of the three).
triBoost :: Double -> GenConfig -> GenConfig
triBoost x gc = gc { _gcBoostTri = x }

-------------------------------------------------------------------------------
-- genP Paradigm (strata-first traversal)
-------------------------------------------------------------------------------

-- |Strata-first generation entrypoint. Seeded by a 'Sc.StrataLabel'; produces
-- a 'PC.ProgressionContext' with distinct triad, strata, and mode layers and
-- @pcProvenance = Just …@.
--
-- Uncued, the starting chord is drawn from inside the stratum itself — a
-- five-tone set admits few triads, and the ordinary whole-corpus cue almost
-- never lands in one. A cue you pass with @cue@ is honoured as given; one
-- that escapes the stratum is reported with the triads that would have
-- fitted, rather than silently replaced.
--
-- @s <- seek "none" $ len 6 $ genP VI@
-- @s <- seek "none" $ cue start $ len 6 $ genP VI@
genP :: Sc.StrataLabel -> GenConfig
genP s = defaultGenConfig { _gcMode = StrataMode s }

-- |Standard-verbosity variant of 'genP'.
genP' :: Sc.StrataLabel -> GenConfig
genP' s = (genP s) { _gcVerbosity = Standard }

-- |Verbose-verbosity variant of 'genP'.
genP'' :: Sc.StrataLabel -> GenConfig
genP'' s = (genP s) { _gcVerbosity = Verbose }

-- | Silent-verbosity 'genP' aliases, one per Roman numeral — @genI@ pins the
-- starting tristrata to @I@, @genII@ to @II@, and so on through @genXI@.
--
-- @s \<- seek \"*\" $ attempt 3 12 $ entropy 0.4 $ genI@
--
-- Three verbosities throughout, by the usual prime convention: @genI@ silent,
-- @genI'@ standard, @genI''@ verbose.
genI, genII, genIII, genIV, genV, genVI, genVII, genVIII, genIX, genX, genXI :: GenConfig
genI     = genP Sc.I
genII    = genP Sc.II
genIII   = genP Sc.III
genIV    = genP Sc.IV
genV     = genP Sc.V
genVI    = genP Sc.VI
genVII   = genP Sc.VII
genVIII  = genP Sc.VIII
genIX    = genP Sc.IX
genX     = genP Sc.X
genXI    = genP Sc.XI

-- | Standard-verbosity Roman numeral aliases: per-step musical context plus
-- the grid. See 'genI'.
genI', genII', genIII', genIV', genV', genVI', genVII', genVIII', genIX', genX', genXI' :: GenConfig
genI'    = genP' Sc.I
genII'   = genP' Sc.II
genIII'  = genP' Sc.III
genIV'   = genP' Sc.IV
genV'    = genP' Sc.V
genVI'   = genP' Sc.VI
genVII'  = genP' Sc.VII
genVIII' = genP' Sc.VIII
genIX'   = genP' Sc.IX
genX'    = genP' Sc.X
genXI'   = genP' Sc.XI

-- | Verbose-verbosity Roman numeral aliases: full traces, the grid, and the
-- multi-attempt scoreboard when paired with @attempt@. See 'genI'.
genI'', genII'', genIII'', genIV'', genV'', genVI'', genVII'', genVIII'', genIX'', genX'', genXI'' :: GenConfig
genI''    = genP'' Sc.I
genII''   = genP'' Sc.II
genIII''  = genP'' Sc.III
genIV''   = genP'' Sc.IV
genV''    = genP'' Sc.V
genVI''   = genP'' Sc.VI
genVII''  = genP'' Sc.VII
genVIII'' = genP'' Sc.VIII
genIX''   = genP'' Sc.IX
genX''    = genP'' Sc.X
genXI''   = genP'' Sc.XI
