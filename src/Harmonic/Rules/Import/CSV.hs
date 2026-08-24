-- |
-- Module      : Harmonic.Rules.Import.CSV
-- Description : CSV parsing for YCACL corpus ingestion
--
-- Parses Yale Classical Archives Corpus data from CSV format into
-- typed Haskell records for downstream transformation and graph storage.

module Harmonic.Rules.Import.CSV (
    -- * Corpus rows
    YCACLRow(..),

    -- * Nested corpus map
    ComposerId, PieceId, YCACLData,

    -- * Loading
    loadYCACLData,
) where

import           Harmonic.Rules.Import.Types
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import           Data.Csv ((.:))
import           Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.List (sortOn)
import           Text.Read (readMaybe)

-- | One row of the YCACL artifact exported by @scripts\/export_ycacl.R@.
--
-- The CSV carries pitches as a space-separated string in a single column; the
-- 'Csv.FromNamedRecord' instance splits and parses that into 'yrPitches'.
data YCACLRow = YCACLRow
  { yrComposer    :: !T.Text   -- ^ composer name as it appears in the corpus
  , yrPiece       :: !T.Text   -- ^ piece identifier, unique within a composer
  , yrOrder       :: !Int      -- ^ position of this slice within the piece
  , yrPitches     :: ![Int]    -- ^ pitch classes (0-11) sounding at this slice
  , yrFundamental :: !Int      -- ^ fundamental detected for the slice
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

-- | Composer name, used as the outer key of 'YCACLData'. Case is preserved
-- here; composer /matching/ is case-insensitive and happens downstream.
type ComposerId = T.Text

-- | Piece identifier, unique within one composer.
type PieceId = T.Text

-- | The whole corpus, grouped composer then piece, with each piece reduced to
-- its ordered list of slices.
type YCACLData = Map.Map ComposerId (Map.Map PieceId [ChordSlice])

-- |Load YCACL artifact (composer, piece, order, pitches, fundamental) into nested maps.
loadYCACLData :: FilePath -> IO YCACLData
loadYCACLData fp = do
  csvData <- BL.readFile fp
  case Csv.decodeByName csvData of
    Left err ->
      error $ "YCACL artifact parse error: " ++ err
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
