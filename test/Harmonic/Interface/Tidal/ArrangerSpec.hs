-- |
-- Module      : Harmonic.Interface.Tidal.ArrangerSpec
-- Description : Tests for lead token parsing and starting state construction

module Harmonic.Interface.Tidal.ArrangerSpec (spec) where

import Test.Hspec
import Harmonic.Interface.Tidal.Arranger (parseLeadTokens, LeadToken(..), lead, lead', leadJ, strataModeFlow, flow, grid, lite, root, progOverlapF, progOverlapB, rotate, excerpt, insert, switch, clone, fuse, interleave, expandP, transposeP, fromChords)
import Harmonic.Rules.Types.Harmony (CadenceState(..), Cadence(..), Movement(..), stateCadenceRoot, stateCadence, stateSpelling, cadenceIntervals, cadenceFunctionality, cadenceMovement, EnharmonicSpelling(..), mkCadenceStatePCs)
import Harmonic.Rules.Types.Progression (fromCadenceStates, unProgression)
import Harmonic.Rules.Types.ProgressionContext (triadLayer)
import qualified Data.Foldable
import Data.List (sort, nub)
import Harmonic.Rules.Types.Pitch (NoteName(..), PitchClass(..), pitchClass)

spec :: Spec
spec = do
  describe "parseLeadTokens" $ do
    it "parses root from 'E min (5)'" $
      [r | RootTok r <- parseLeadTokens "E min (5)"] `shouldBe` ["E"]

    it "parses quality from 'E min (5)'" $
      [q | QualTok q <- parseLeadTokens "E min (5)"] `shouldBe` ["min"]

    it "parses movement from 'E min (5)'" $
      [m | MoveTok m <- parseLeadTokens "E min (5)"] `shouldBe` [5]

    it "parses negative movement from 'E min (-3)'" $
      [m | MoveTok m <- parseLeadTokens "E min (-3)"] `shouldBe` [-3]

    it "parses movement-only '(5)'" $
      [m | MoveTok m <- parseLeadTokens "(5)"] `shouldBe` [5]

    it "parses root-only 'C#'" $
      [r | RootTok r <- parseLeadTokens "C#"] `shouldBe` ["C#"]

    it "parses flat roots canonically" $
      [r | RootTok r <- parseLeadTokens "Bb"] `shouldBe` ["Bb"]

    it "parses root case-insensitively" $
      [r | RootTok r <- parseLeadTokens "eb"] `shouldBe` ["Eb"]

    it "unrecognised token becomes QualTok" $
      [q | QualTok q <- parseLeadTokens "E xyz (5)"] `shouldBe` ["xyz"]

    it "empty string gives no tokens" $
      parseLeadTokens "" `shouldBe` []

    it "quality-only 'min' gives no root or movement" $ do
      [r | RootTok r <- parseLeadTokens "min"] `shouldBe` []
      [m | MoveTok m <- parseLeadTokens "min"] `shouldBe` []
      [q | QualTok q <- parseLeadTokens "min"] `shouldBe` ["min"]

  describe "lead" $ do
    it "lead 'E min (5)' produces E root" $ do
      cs <- lead "E min (5)"
      stateCadenceRoot cs `shouldBe` E

    it "lead 'C maj' produces C root" $ do
      cs <- lead "C maj"
      stateCadenceRoot cs `shouldBe` C

    it "lead empty string completes without error" $ do
      _ <- lead ""
      pure ()

  describe "lead' (note-name list cue)" $ do
    it "builds Eb m7 from 'Eb Gb Bb Db' with flat spelling" $ do
      cs <- lead' "Eb Gb Bb Db"
      stateCadenceRoot cs `shouldBe` Eb
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]
      cadenceFunctionality (stateCadence cs) `shouldBe` "m7"
      stateSpelling cs `shouldBe` FlatSpelling

    it "honors the movement token" $ do
      cs <- lead' "A C E G (5)"
      stateCadenceRoot cs `shouldBe` A
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]
      cadenceMovement (stateCadence cs) `shouldBe` Asc (P 5)

    it "names a triad input with the triad namer (lead parity)" $ do
      cs <- lead' "C E G"
      stateCadenceRoot cs `shouldBe` C
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 4, P 7]
      cadenceFunctionality (stateCadence cs) `shouldBe` "maj"

    it "skips unrecognized tokens gracefully" $ do
      cs <- lead' "Eb xyz Gb Bb Db"
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]

    it "sharp accidental implies sharp spelling" $ do
      cs <- lead' "F# A# C# E"
      stateCadenceRoot cs `shouldBe` F'
      stateSpelling cs `shouldBe` SharpSpelling

    it "falls back to random lead when no valid notes" $ do
      _ <- lead' "xyz qqq"
      pure ()

  describe "rearranging family (ProgressionContext combinators)" $ do
    let pcOf = fromChords
        rootsOf pc' = map stateCadenceRoot
                        (Data.Foldable.toList (unProgression (triadLayer pc')))
        p4 = pcOf [[0,4,7], [5,9,0], [7,11,2], [9,0,4]]  -- C F G Am

    it "rotate n shifts bars cyclically and preserves length" $ do
      length (rootsOf (rotate 1 p4)) `shouldBe` 4
      rootsOf (rotate 1 p4) `shouldBe` (drop 1 (rootsOf p4) ++ take 1 (rootsOf p4))

    it "rotate by the length is the identity on roots" $
      rootsOf (rotate 4 p4) `shouldBe` rootsOf p4

    it "excerpt s e keeps exactly the requested bars" $
      rootsOf (excerpt 2 3 p4) `shouldBe` take 2 (drop 1 (rootsOf p4))

    it "insert replaces the bar at the position, length unchanged" $ do
      cs <- lead "D min (0)"
      let out = rootsOf (insert cs 2 p4)
      length out `shouldBe` 4
      out !! 1 `shouldBe` stateCadenceRoot cs
      [out !! i | i <- [0, 2, 3]] `shouldBe` [rootsOf p4 !! i | i <- [0, 2, 3]]

    it "switch m n swaps two bars" $ do
      let out = rootsOf (switch 1 3 p4)
          orig = rootsOf p4
      out `shouldBe` [orig !! 2, orig !! 1, orig !! 0, orig !! 3]

    it "clone m n copies bar m over bar n" $ do
      let out = rootsOf (clone 1 4 p4)
          orig = rootsOf p4
      out `shouldBe` [orig !! 0, orig !! 1, orig !! 2, orig !! 0]

    it "fuse concatenates in order" $ do
      let a = pcOf [[0,4,7]]
          b = pcOf [[5,9,0]]
      rootsOf (fuse [a, b]) `shouldBe` (rootsOf a ++ rootsOf b)

    it "interleave alternates bars from each source" $ do
      let a = pcOf [[0,4,7], [7,11,2]]
          b = pcOf [[5,9,0], [9,0,4]]
          orig x = rootsOf x
      rootsOf (interleave a b)
        `shouldBe` [orig a !! 0, orig b !! 0, orig a !! 1, orig b !! 1]

    it "expandP n repeats the whole progression n times" $ do
      let out = rootsOf (expandP 2 p4)
      out `shouldBe` (rootsOf p4 ++ rootsOf p4)

    it "transposeP preserves interval structure (pitch content shifts)" $ do
      let ivsOf' pc' = map (cadenceIntervals . stateCadence)
                         (Data.Foldable.toList (unProgression (triadLayer pc')))
      ivsOf' (transposeP 2 p4) `shouldBe` ivsOf' p4
      length (rootsOf (transposeP 2 p4)) `shouldBe` 4

  describe "voicing strategies (structure properties)" $ do
    let prog = triadLayer (fromChords [[0,4,7], [5,9,0], [7,11,2]])

    it "grid: one voicing per bar, root pitch class in the bass" $ do
      let vs = grid prog
      length vs `shouldBe` 3
      [ v `mod` 12 | (v:_) <- vs ] `shouldBe` [0, 5, 7]

    it "flow: one voicing per bar, pitch-class content preserved" $ do
      let vs = flow prog
      length vs `shouldBe` 3
      map (sort . nub . map (`mod` 12)) vs `shouldBe`
        [[0,4,7], [0,5,9], [2,7,11]]

    it "lite: literal intervals, register-normalized only" $ do
      let vs = lite prog
      length vs `shouldBe` 3
      map (sort . nub . map (`mod` 12)) vs `shouldBe`
        [[0,4,7], [0,5,9], [2,7,11]]

    it "root: one pitch class per bar (bass lines)" $ do
      let vs = root prog
      map length vs `shouldBe` [1, 1, 1]
      [ v `mod` 12 | [v] <- vs ] `shouldBe` [0, 5, 7]

  describe "progOverlap family (windowed pitch union)" $ do
    let mkBar root ivs = mkCadenceStatePCs root Unison ivs
        -- G maj -> C maj -> F maj (each stored as zero-form [0,4,7])
        gcf = fromCadenceStates [mkBar G [0,4,7], mkBar C [0,4,7], mkBar F [0,4,7]]
        pcInt (P n) = n
        ivsOf cs = map pcInt (cadenceIntervals (stateCadence cs))
        absOf cs = sort (nub [ (i + pcInt (pitchClass (stateCadenceRoot cs))) `mod` 12
                             | i <- ivsOf cs ])
        barsOf = Data.Foldable.toList . unProgression

    it "keeps every bar's own pitch classes (no self-transposition)" $ do
      let out = barsOf (progOverlapF 1 gcf)
      -- G maj's absolute PCs {7, 11, 2} must survive in its overlapped bar
      mapM_ (\pc -> absOf (head out) `shouldSatisfy` (pc `elem`)) [7, 11, 2]

    it "overlapF 1 unions the next bar's absolute content (G+C)" $ do
      let out = barsOf (progOverlapF 1 gcf)
      absOf (head out) `shouldBe` sort (nub ([7, 11, 2] ++ [0, 4, 7]))

    it "a 4-note bar keeps its fourth voice through overlap" $ do
      let m7  = fromCadenceStates [mkBar C [0,3,7,10], mkBar C [0,3,7,10]]
          out = barsOf (progOverlapF 1 m7)
      sort (nub (ivsOf (head out))) `shouldBe` [0, 3, 7, 10]

    it "overlapF 0 is the identity" $ do
      map ivsOf (barsOf (progOverlapF 0 gcf)) `shouldBe` map ivsOf (barsOf gcf)

    it "backward overlap pulls from behind only" $ do
      let out = barsOf (progOverlapB 1 gcf)
      -- bar 1 (index 0) has nothing behind: unchanged
      absOf (head out) `shouldBe` sort [7, 11, 2]

  describe "leadJ (leadsheet chord-symbol cue)" $ do
    it "builds C m7 from 'Cm7' with the jazz namer" $ do
      cs <- leadJ "Cm7"
      stateCadenceRoot cs `shouldBe` C
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]
      cadenceFunctionality (stateCadence cs) `shouldBe` "m7"

    it "honours a notated slash bass as the anchor" $ do
      cs <- leadJ "Dm7/G (5)"
      stateCadenceRoot cs `shouldBe` G
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 2, P 5, P 7, P 10]
      cadenceFunctionality (stateCadence cs) `shouldBe` "9sus4"
      cadenceMovement (stateCadence cs) `shouldBe` Asc (P 5)

    it "flat accidental implies flat spelling" $ do
      cs <- leadJ "EbM7"
      stateCadenceRoot cs `shouldBe` Eb
      stateSpelling cs `shouldBe` FlatSpelling
      cadenceFunctionality (stateCadence cs) `shouldBe` "M7"

    it "falls back to C m7 on an unparseable symbol, keeping the movement" $ do
      cs <- leadJ "Xq9 (3)"
      stateCadenceRoot cs `shouldBe` C
      cadenceFunctionality (stateCadence cs) `shouldBe` "m7"
      cadenceMovement (stateCadence cs) `shouldBe` Asc (P 3)

    it "treats NC as unparseable (silence is not a chord)" $ do
      cs <- leadJ "NC"
      cadenceFunctionality (stateCadence cs) `shouldBe` "m7"

  describe "strataModeFlow pedal behavior + big-chroma routing" $ do
    let csOf root ivs = mkCadenceStatePCs root Unison ivs
        pOf = fromCadenceStates

    it "static root: shared tones hold exact MIDI across a mode change" $ do
      let voiced = strataModeFlow (pOf [csOf E [0,2,3,5,7,9,10], csOf E [0,2,4,5,7,9,10]])
      voiced `shouldBe` [[-8,-6,-5,-3,-1,1,2],[-8,-6,-4,-3,-1,1,2]]

    it "static root: 5-PC strata pair pedals shared tones" $ do
      let voiced = strataModeFlow (pOf [csOf E [0,2,3,7,10], csOf E [0,2,3,7,9]])
      take 4 (head voiced) `shouldBe` take 4 (voiced !! 1)

    it "root motion picks the max-common-tone octave" $ do
      let voiced = strataModeFlow (pOf [csOf E [0,2,3,5,7,9,10], csOf A [0,2,4,5,7,9,10]])
          shared = length (filter (`elem` head voiced) (voiced !! 1))
      shared `shouldSatisfy` (>= 4)

    it "flow routes >=6-PC bars to the chroma engine" $ do
      let big = pOf [csOf E [0,2,3,5,7,9,10], csOf G [0,2,4,5,7,9,10]]
      flow big `shouldBe` strataModeFlow big
      grid big `shouldBe` strataModeFlow big

    it "flow keeps the DP for harmony-sized bars (mixed 3/4 seam)" $ do
      let seam = pOf [csOf C [0,4,7], csOf C [0,4,7,10], csOf F [0,4,7]]
      (flow seam !! 1) `shouldBe` [-12,-8,-5,-2]

  describe "enharmonic spelling continuity (2026-08-20 curation)" $ do
    it "fromChords holds the spelling side while the root stands still" $ do
      -- the sixteen_bars case: Gb 6b5 -> maj7b5 -> maj7 (root PC 6 held)
      let pc = fromChords [[1,5,6,8],[6,10,0,3],[6,10,0,5],[6,10,1,5]]
          sps = [ stateSpelling cs
                | cs <- Data.Foldable.toList (unProgression (triadLayer pc)) ]
      drop 1 sps `shouldBe` [FlatSpelling, FlatSpelling, FlatSpelling]
