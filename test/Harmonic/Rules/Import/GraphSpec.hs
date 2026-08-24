{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Harmonic.Rules.Import.GraphSpec
-- Description : Write/read contract for the batched edge writer (offline)
module Harmonic.Rules.Import.GraphSpec (spec) where

import Test.Hspec
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Harmonic.Rules.Import.Graph
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Evaluation.Database.Query as Q

cad :: [Int] -> [Int] -> H.Cadence
cad from to = H.toCadence (H.flatTriad from, H.flatTriad to)

field :: A.Value -> T.Text -> Maybe A.Value
field (A.Object o) k = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "edgeRow (write/read contract, no database)" $ do
  let from    = cad [0,4,7] [0,4,7]
      to      = cad [0,4,7] [2,7,11]
      weights = Map.fromList [("bach", 0.6), ("debussy", 1.0)]
      row     = edgeRow (from, to, weights)

  it "keys nodes by the cadence show string" $ do
    field row "fromShow" `shouldBe` Just (A.String (T.pack (show from)))
    field row "toShow"   `shouldBe` Just (A.String (T.pack (show to)))

  it "confidence is the sum of all composer weights (the r.confidence invariant)" $
    -- fetchTransitionsAggregate's fast path depends on
    -- r.confidence == sum(weights); this pins it at the write seam.
    field row "confidence" `shouldBe` Just (A.toJSON (1.6 :: Double))

  it "weights round-trip through the read side's parseWeightsJson" $ do
    let Just (A.String w) = field row "weights"
    Q.parseWeightsJson w `shouldBe` weights

  it "a composer key needing escaping survives the parameter path intact" $ do
    -- Values travel as JSON parameters, never spliced into Cypher, so
    -- quotes and backslashes in a key are inert.
    let spicy = Map.fromList [("o'weird \\ composer", 2.5)]
        Just (A.String w) = field (edgeRow (from, to, spicy)) "weights"
    Q.parseWeightsJson w `shouldBe` spicy

  it "batchCypher unwinds $rows and never splices values" $ do
    batchCypher `shouldSatisfy` T.isInfixOf "UNWIND $rows"
    batchCypher `shouldSatisfy` (not . T.isInfixOf "'")
