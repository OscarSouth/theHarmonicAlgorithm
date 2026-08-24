-- |
-- Module      : Harmonic.Rules.Import.MergeSpec
-- Description : Composer normalisation and transition-merge contracts
module Harmonic.Rules.Import.MergeSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import Harmonic.Rules.Import.Merge
import Harmonic.Rules.Import.Types
import qualified Harmonic.Rules.Types.Harmony as H

edgeAB, edgeBC :: (H.Cadence, H.Cadence)
edgeAB = ( H.toCadence (H.flatTriad [0,4,7], H.flatTriad [0,4,7])
         , H.toCadence (H.flatTriad [0,4,7], H.flatTriad [2,7,11]) )
edgeBC = ( H.toCadence (H.flatTriad [0,4,7], H.flatTriad [2,7,11])
         , H.toCadence (H.flatTriad [2,7,11], H.flatTriad [5,9,0]) )

spec :: Spec
spec = do
  describe "slug (composer-key normaliser)" $ do
    it "lowercases and maps spaces to underscores" $ do
      slug "Anna Amalia" `shouldBe` "anna_amalia"
      slug "Strauss II" `shouldBe` "strauss_ii"
    it "does not trim whitespace (trailing space becomes trailing underscore)" $
      slug "Hummel " `shouldBe` "hummel_"
    it "keeps spaces as underscores where the R normaliser strips them" $
      -- R's normalize_composer strips ALL non-alnums ("cpebach"); slug
      -- maps spaces to underscores ("cpe_bach"). Curation lists must be
      -- built against slug's output, never the R rule.
      slug "C.P.E. Bach" `shouldBe` "cpe_bach"

  describe "normalizeComposers" $
    it "merges raw names that normalise to the same key" $ do
      let sliceA = ChordSlice [0,4,7] 0
          dataset = Map.fromList
            [ ("Bach",  Map.fromList [("p1", [sliceA])])
            , ("BACH",  Map.fromList [("p2", [sliceA])])
            ]
          normalized = normalizeComposers dataset
      Map.keys normalized `shouldBe` ["bach"]
      Map.size (normalized Map.! "bach") `shouldBe` 2

  describe "filterComposers" $ do
    let sliceA = ChordSlice [0,4,7] 0
        pieces = Map.fromList [("p1", [sliceA]), ("p2", [sliceA])]
        dataset = Map.fromList [("bach", pieces), ("strauss_ii", pieces)]
    it "keeps allow-listed composers and reports the dropped with piece counts" $ do
      let (kept, dropped) = filterComposers ["bach"] [] dataset
      Map.keys kept `shouldBe` ["bach"]
      dropped `shouldBe` [("strauss_ii", 2)]
    it "empty allow-list admits everything" $ do
      let (kept, dropped) = filterComposers [] [] dataset
      Map.size kept `shouldBe` 2
      dropped `shouldBe` []
    it "exclude-list drops and reports" $ do
      let (kept, dropped) = filterComposers [] ["bach"] dataset
      Map.keys kept `shouldBe` ["strauss_ii"]
      dropped `shouldBe` [("bach", 2)]

  describe "mergeComposerTransitions" $ do
    it "unions weights per edge across composers, sparsely (no zero padding)" $ do
      let maps = Map.fromList
            [ ("bach",    Map.fromList [(edgeAB, 0.6), (edgeBC, 0.4)])
            , ("debussy", Map.fromList [(edgeAB, 1.0)])
            ]
          out = mergeComposerTransitions maps
      length out `shouldBe` 2
      let lookupEdge e = [ w | (f, t, w) <- out, (f, t) == e ]
      lookupEdge edgeAB `shouldBe` [Map.fromList [("bach", 0.6), ("debussy", 1.0)]]
      -- SPARSE: debussy carries no key on the edge it never took
      lookupEdge edgeBC `shouldBe` [Map.fromList [("bach", 0.4)]]
    it "excludes edges whose total weight is zero" $ do
      let maps = Map.fromList [("bach", Map.fromList [(edgeAB, 0.0)])]
      mergeComposerTransitions maps `shouldBe` []
