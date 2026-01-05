{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Harmonic.Ingestion.CSV where

import           Harmonic.Ingestion.Types
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import           Data.Csv ((.:))
import           Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.List (foldl', sortOn)
import           Text.Read (readMaybe)

-- YCACL artifact rows exported via scripts/export_ycacl.R
data YCACLRow = YCACLRow
  { yrComposer    :: !T.Text
  , yrPiece       :: !T.Text
  , yrOrder       :: !Int
  , yrPitches     :: ![Int]
  , yrFundamental :: !Int
  } deriving (Show, Eq)

instance Csv.FromNamedRecord YCACLRow where
  parseNamedRecord m = do
    composer <- m .: "composer"
    piece    <- m .: "piece"
    orderVal <- m .: "order"
    pitchTxt :: T.Text <- m .: "pitches"
    pitches  <- parsePitchList pitchTxt
    fund     <- m .: "fundamental"
    pure (YCACLRow composer piece orderVal pitches fund)
    where
      parsePitchList txt =
        let tokens = T.words txt
        in mapM toInt tokens
      toInt chunk =
        case readMaybe (T.unpack chunk) of
          Just n  -> pure n
          Nothing -> mzero

type ComposerId = T.Text
type PieceId = T.Text
type YCACLData = Map.Map ComposerId (Map.Map PieceId [ChordSlice])

-- |Load YCACL artifact (composer, piece, order, pitches, fundamental) into nested maps.
loadYCACLData :: FilePath -> IO YCACLData
loadYCACLData fp = do
  csvData <- BL.readFile fp
  case Csv.decodeByName csvData of
    Left err -> do
      putStrLn $ "YCACL artifact parse error: " ++ err
      pure Map.empty
    Right (_, rows) ->
      let grouped = foldl' accumulate Map.empty (V.toList rows)
       in pure $ fmap (fmap finalize) grouped
  where
    accumulate acc row =
      -- Each CSV row already has de-duplicated pitches and a trusted
      -- fundamental pitch class from the exporter; we simply preserve
      -- ordering information so pieces can be replayed in sequence.
      let composer = yrComposer row
          piece    = yrPiece row
          chord    = yrPitches row
          fund     = yrFundamental row
          slice    = ChordSlice chord fund
          orderVal = yrOrder row
          updatePiece Nothing      = Just [(orderVal, slice)]
          updatePiece (Just items) = Just ((orderVal, slice):items)
          updateComposer Nothing   = Just (Map.singleton piece [(orderVal, slice)])
          updateComposer (Just pieceMap) = Just (Map.alter updatePiece piece pieceMap)
       in Map.alter updateComposer composer acc
    finalize entries =
      -- Rows were appended as we streamed the CSV, so we re-sort by the
      -- original `order` column before dropping the index and returning
      -- the slice payloads.
      let ordered = sortOn fst entries
       in map snd ordered
