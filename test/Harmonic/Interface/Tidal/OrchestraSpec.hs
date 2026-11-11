-- |
-- Module      : Harmonic.Interface.Tidal.OrchestraSpec
-- Description : Tests for Orchestra module (Voice, VoiceLines, clip, articulations)

module Harmonic.Interface.Tidal.OrchestraSpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import Harmonic.Interface.Tidal.Orchestra
import Harmonic.Interface.Tidal.Devices.JV1010 (jv1010)
import Harmonic.Interface.Tidal.Form (Kinetics(..), lK)
import Harmonic.Interface.Tidal.Score (inKey)
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Interface.Tidal.Arranger as A
import Data.List (nub)
import Harmonic.Rules.Types.ProgressionContext (Layer(..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Sound.Tidal.Context hiding (clip)

-------------------------------------------------------------------------------
-- Test Fixtures
-------------------------------------------------------------------------------

mkCadenceState :: Pitch.NoteName -> String -> [Integer] -> H.CadenceState
mkCadenceState rootNote func intervals =
  let pcs = map (Pitch.mkPitchClass . fromIntegral) (take 3 intervals)
      cadence = H.Cadence func H.Unison pcs
  in H.CadenceState cadence rootNote H.FlatSpelling

testProgA :: P.Progression
testProgA = P.Progression $ Seq.fromList
  [ mkCadenceState Pitch.C "maj" [0, 4, 7]
  , mkCadenceState Pitch.G "maj" [0, 4, 7]
  ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- |Query a Pattern Int at the first cycle and extract values
queryIntPat :: Pattern Int -> [Int]
queryIntPat pat = map value $ queryArc pat (Arc 0 1)

-- |Compare two Pattern Int by querying them
patsEqual :: Pattern Int -> Pattern Int -> Bool
patsEqual a b = queryIntPat a == queryIntPat b

extractNote :: ValueMap -> Maybe Double
extractNote vm = case Map.lookup "note" vm of
  Just (VN n) -> Just (unNote n)
  Just (VF v) -> Just v
  _           -> Nothing

extractAmp :: ValueMap -> Maybe Double
extractAmp vm = case Map.lookup "amp" vm of
  Just (VF v) -> Just v
  Just (VN n) -> Just (unNote n)
  _           -> Nothing

queryNotes :: ControlPattern -> Arc -> [Double]
queryNotes pat arc =
  [ noteVal
  | ev <- queryArc pat arc
  , Just noteVal <- [extractNote (value ev)]
  ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Voice and VoiceLines" $ do
    it "vlGet Soprano returns soprano pattern" $
      patsEqual (vlGet Soprano voiceLines) (soprano voiceLines) `shouldBe` True

    it "vlGet covers all voices" $ do
      patsEqual (vlGet Alto voiceLines) (alto voiceLines) `shouldBe` True
      patsEqual (vlGet Tenor voiceLines) (tenor voiceLines) `shouldBe` True
      patsEqual (vlGet Bass voiceLines) (bass voiceLines) `shouldBe` True

    it "octave variants map to base voice" $ do
      patsEqual (vlGet Soprano8va voiceLines) (soprano voiceLines) `shouldBe` True
      patsEqual (vlGet Tenor8vb voiceLines) (tenor voiceLines) `shouldBe` True
      patsEqual (vlGet Bass15vb voiceLines) (bass voiceLines) `shouldBe` True

  describe "voiceOct" $ do
    it "loco voices return 0" $ do
      voiceOct Soprano `shouldBe` 0
      voiceOct Alto `shouldBe` 0
      voiceOct Tenor `shouldBe` 0
      voiceOct Bass `shouldBe` 0

    it "8va returns 1" $
      voiceOct Soprano8va `shouldBe` 1

    it "8vb returns -1" $
      voiceOct Tenor8vb `shouldBe` (-1)

    it "15va returns 2" $
      voiceOct Alto15va `shouldBe` 2

    it "15vb returns -2" $
      voiceOct Bass15vb `shouldBe` (-2)

  describe "clip" $ do
    it "passes notes within range" $ do
      let pat = note 60 :: ControlPattern
          clipped = clip (48, 72) pat
          evs = queryArc clipped (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "filters notes outside range" $ do
      let pat = note 80 :: ControlPattern
          clipped = clip (48, 72) pat
          evs = queryArc clipped (Arc 0 1)
      length evs `shouldBe` 0

    it "passes events without note field" $ do
      let pat = s "thru" :: ControlPattern
          clipped = clip (48, 72) pat
          evs = queryArc clipped (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

  describe "Articulations (JV-1010)" $ do
    it "articulation values are distinct channel patterns" $ do
      let extract p = queryArc p (Arc 0 1)
          pEvs  = extract (pizz jv1010)
          sEvs  = extract (spicc jv1010)
          mEvs  = extract (marc jv1010)
          lEvs  = extract (legg jv1010)
          aEvs  = extract (arco jv1010)
      -- All should produce events
      length pEvs `shouldSatisfy` (> 0)
      length sEvs `shouldSatisfy` (> 0)
      length mEvs `shouldSatisfy` (> 0)
      length lEvs `shouldSatisfy` (> 0)
      length aEvs `shouldSatisfy` (> 0)
      -- They should be distinct
      map value pEvs `shouldNotBe` map value sEvs
      map value pEvs `shouldNotBe` map value mEvs
      map value sEvs `shouldNotBe` map value lEvs

  describe "divisi tiers (primed voices)" $ do
    it "primed voice reads the primed field, not the base" $ do
      patsEqual (vlGet Soprano'  voiceLines) (soprano'  voiceLines) `shouldBe` True
      patsEqual (vlGet Soprano'' voiceLines) (soprano'' voiceLines) `shouldBe` True
      patsEqual (vlGet Bass'     voiceLines) (bass'     voiceLines) `shouldBe` True

    it "primed voice keeps the base octave (Bass8vb' = bass' at -1)" $ do
      voiceOct Bass8vb' `shouldBe` (-1)
      voiceTier Bass8vb' `shouldBe` 1
      patsEqual (vlGet Bass8vb' voiceLines) (bass' voiceLines) `shouldBe` True

    it "voiceBase / voiceTier decompose a primed voice" $ do
      voiceBase Soprano8va'' `shouldBe` Soprano8va
      voiceTier Soprano8va'' `shouldBe` 2

    it "default primes stack degrees above the base" $ do
      queryIntPat (vlGet Soprano   voiceLines) `shouldBe` [3]
      queryIntPat (vlGet Soprano'  voiceLines) `shouldBe` [4]
      queryIntPat (vlGet Soprano'' voiceLines) `shouldBe` [5]

  describe "divisi N (auto-stack wrapper)" $ do
    -- fake instrument: emits its voice's enum index as a note, ignoring the rest
    let fakeInstr :: Instrument
        fakeInstr _ _ _ _ _ v = note (fromIntegral (fromEnum v)) # amp 1
        run p = queryArc p (Arc 0 1)

    it "stacks N desks reading successive tiers" $ do
      let pat = divisi 3 fakeInstr T (0,1) undefined voiceLines undefined Soprano
          notes = [ n | ev <- run pat, Just n <- [extractNote (value ev)] ]
      -- Soprano=0, Soprano'=20, Soprano''=40
      notes `shouldMatchList` [0, 20, 40]

    it "scales amp by 1/sqrt N" $ do
      let pat = divisi 3 fakeInstr T (0,1) undefined voiceLines undefined Soprano
          amps = [ a | ev <- run pat, Just a <- [extractAmp (value ev)] ]
      amps `shouldSatisfy` all (\a -> abs (a - 1 / sqrt 3) < 1e-9)

    it "clamps the tier for N past the top (divisi 4 doubles the top desk, no crash)" $ do
      let pat = divisi 4 fakeInstr T (0,1) undefined voiceLines undefined Soprano
          notes = [ n | ev <- run pat, Just n <- [extractNote (value ev)] ]
      -- Soprano=0, Soprano'=20, Soprano''=40, 4th desk clamps to Soprano''
      notes `shouldMatchList` [0, 20, 40, 40]

    it "clamps starting from an already-primed voice (divisi 2 on Soprano'')" $ do
      let pat = divisi 2 fakeInstr T (0,1) undefined voiceLines undefined Soprano''
          notes = [ n | ev <- run pat, Just n <- [extractNote (value ev)] ]
      notes `shouldMatchList` [40, 40]

    it "divisi 1 is the plain instrument (no vel scaling)" $ do
      let pat = divisi 1 fakeInstr T (0,1) undefined voiceLines undefined Soprano
          notes = [ n | ev <- run pat, Just n <- [extractNote (value ev)] ]
      notes `shouldMatchList` [0]

  describe "divisi volume tags" $ do
    it "divisi2 sets amp to 1/sqrt 2" $ do
      let pat = note 60 # divisi2 :: ControlPattern
          amps = [ a | ev <- queryArc pat (Arc 0 1), Just a <- [extractAmp (value ev)] ]
      amps `shouldSatisfy` all (\a -> abs (a - 1 / sqrt 2) < 1e-9)

    it "divisi3 sets amp to 1/sqrt 3" $ do
      let pat = note 60 # divisi3 :: ControlPattern
          amps = [ a | ev <- queryArc pat (Arc 0 1), Just a <- [extractAmp (value ev)] ]
      amps `shouldSatisfy` all (\a -> abs (a - 1 / sqrt 3) < 1e-9)

  describe "Unpitched percussion" $ do
    it "bassdrum produces events on struct true" $ do
      let pat = bassdrum jv1010 "t" :: ControlPattern
          evs = queryArc pat (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "tamtam produces events on struct true" $ do
      let pat = tamtam jv1010 "t" :: ControlPattern
          evs = queryArc pat (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

  describe "JV-1010 map (pinned)" $ do
    let pcx  = PC.fromProgression testProgA
        kin  = Kinetics (pure 1.0) (pure 1.0) (pure pcx) [pcx] 0 0 (const True)
        k    = (kin, parseBP_E "1" :: Pattern Int)
        num v = case v of { VF x -> Just x; VN x -> Just (realToFrac (unNote x)); VI x -> Just (fromIntegral x); _ -> Nothing }
        chans p = nub [ c | ev <- queryArc p (Arc 0 4), Just c <- [Map.lookup "midichan" (value ev) >>= num] ]
        notes p = queryNotes p (Arc 0 4)
        play instr v = instr jv1010 T (0,1) k voiceLines A.flow v
        -- (instrument, voice that lands inside its range, channel, bounds)
        pins = [ ("flute",      play flute      Soprano,  0, (-12, 26))
               , ("oboe",       play oboe       Soprano,  1, ( -2, 33))
               , ("clarinet",   play clarinet   Alto,     2, (-22, 34))
               , ("bassoon",    play bassoon    Bass8vb,  3, (-28, 15))
               , ("horn",       play horn       Tenor,    4, (-29, 17))
               , ("trombone",   play trombone   Bass,     5, (-28, 17))
               , ("basstrom",   play basstrom   Bass15vb, 5, (-39, -5))
               , ("harp",       play harp       Soprano,  6, (-29, 42))
               , ("timpani",    play timpani    Bass,     7, (-22,  0))
               , ("violin1",    play violin1    Soprano, 15, ( -5, 45))
               , ("violin2",    play violin2    Soprano, 15, ( -5, 45))
               , ("viola",      play viola      Alto,    15, (-12, 28))
               , ("cello",      play cello      Bass,    15, (-24, 24))
               , ("contrabass", play contrabass Bass8vb, 15, (-36,  0))
               ]

    it "every instrument sounds, on its channel, inside its range" $
      [ (nm, chans p, all (\n -> n >= fromIntegral lo && n <= fromIntegral hi) (notes p), not (null (notes p)))
      | (nm, p, _, (lo, hi)) <- pins ]
        `shouldBe` [ (nm, [c], True, True) | (nm, _, c, _) <- pins ]

    it "articulations re-route the strings: pizz 12, spicc 13, marc 14, legg 15, arco 16" $
      [ chans (play violin1 Soprano # art jv1010) | art <- [pizz, spicc, marc, legg, arco] ]
        `shouldBe` [[11], [12], [13], [14], [15]]

    it "bass drum is MIDI 36 on channel 9; tam-tam MIDI 31 on channel 11" $ do
      -- 'midinote' lands in the note field, offset by 60
      let hit p = [ (extractNote (value ev), Map.lookup "midichan" (value ev) >>= num) | ev <- queryArc p (Arc 0 1) ]
      hit (bassdrum jv1010 "t") `shouldBe` [(Just (-24), Just 8)]
      hit (tamtam jv1010 "t")   `shouldBe` [(Just (-29), Just 10)]

    it "the record holds the map the instruments read" $ do
      map ($ jv1010) [rigFlute, rigOboe, rigClarinet, rigBassoon, rigHorn, rigTrombone, rigBasstrom, rigHarp, rigTimpani]
        `shouldBe` [1, 2, 3, 4, 5, 6, 6, 7, 8]
      map ($ jv1010) [rigViolin1, rigViolin2, rigViola, rigCello, rigContrabass]
        `shouldBe` [16, 16, 16, 16, 16]
      rigContinuo jv1010 `shouldBe` 7
      rigSetup jv1010 `shouldSatisfy` null

  describe "noArt" $
    it "is the identity under #" $ do
      let base = note 1 # amp 0.5 :: ControlPattern
      map value (queryArc (base # noArt) (Arc 0 1)) `shouldBe` map value (queryArc base (Arc 0 1))

  describe "score mode (primed instruments)" $ do
    let chanOf v = case v of { VF x -> Just x; VN x -> Just (realToFrac (unNote x)); VI x -> Just (fromIntegral x); _ -> Nothing }
        evs p = [ (extractNote (value ev), extractAmp (value ev), Map.lookup "midichan" (value ev) >>= chanOf)
                | ev <- queryArc p (Arc 0 1) ]
        keyC = lK 1 1 (inKey Pitch.C "major") (pure 1)

    it "reads degrees against the IK's progression, the mark as amp, on the rig's channel" $
      evs (flute' jv1010 M 1 "p" keyC "0 7") `shouldBe` [(Just 0, Just 0.40, Just 0), (Just 12, Just 0.40, Just 0)]

    it "samples a pattern of marks at each onset" $
      map (\(_, a, _) -> a) (evs (violin1' jv1010 M 1 "p mf" keyC "0 0 0 0"))
        `shouldBe` [Just 0.40, Just 0.40, Just 0.55, Just 0.55]

    it "scales by the form's dynamic like arrange" $
      map (\(_, a, _) -> a) (evs (flute' jv1010 M 1 "p" (lK 1 0.5 (inKey Pitch.C "major") (pure 1)) "0"))
        `shouldBe` [Just 0.20]

    it "clips to the instrument's range after the octave" $ do
      evs (flute' jv1010 M 0 "p" keyC "0") `shouldSatisfy` ((== 1) . length)     -- C3 is the flute's floor
      evs (flute' jv1010 M (-1) "p" keyC "0") `shouldBe` []                      -- C2 is below it
      evs (contrabass' jv1010 M 1 "p" keyC "1") `shouldBe` []                     -- D4 is above the bass's ceiling

    it "answers to the articulation tags" $
      map (\(_, _, c) -> c) (evs (cello' jv1010 T 0 "p" keyC "0" # pizz jv1010)) `shouldBe` [Just 11]

    it "T and T' agree on where degree 0 sits (the bar root at the bass anchor)" $ do
      let pcx = PC.fromProgression testProgA
          kin = Kinetics (pure 1.0) (pure 1.0) (pure pcx) [pcx] 0 0 (const True)
          k   = (kin, pure 1 :: Pattern Int)
          plain  = queryNotes (cello jv1010 T (0,1) k voiceLines { bass = "0" } A.grid Bass) (Arc 0 1)
          primed = queryNotes (cello' jv1010 T 0 "p" k "0") (Arc 0 1)
      take 1 primed `shouldBe` take 1 plain
