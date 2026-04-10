{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Framework.Builder.Portmanteau
-- Description : Portmanteau name generation from composer blend strings
--
-- Generates blended composer display names by extracting weighted
-- portions from beginning/middle/end of each composer name based
-- on their position and weight in the blend string.

module Harmonic.Framework.Builder.Portmanteau
  ( parseComposersWithOrder
  , makePortmanteau
  , extractByPosition
  , takeFromBeginning
  , takeFromEnd
  , takeFromMiddle
  ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe (mapMaybe)

-- | Parse composer string preserving input order
-- Returns list of (name, normalized weight) tuples in order of appearance
parseComposersWithOrder :: Text -> [(Text, Double)]
parseComposersWithOrder input
  | T.null input || input == "*" = []
  | otherwise = normalizeList parsed
  where
    tokens = filter (not . T.null) $ T.split isSeparator input
    parsed = mapMaybe parseToken tokens

    isSeparator c = c == ' ' || c == ','

    parseToken :: Text -> Maybe (Text, Double)
    parseToken tok =
      case T.splitOn ":" tok of
        [name]       -> Just (T.strip name, 1.0)
        [name, wStr] ->
          let weight = parseWeight (T.strip wStr)
           in Just (T.strip name, weight)
        _            -> Nothing

    parseWeight wStr =
      case reads (T.unpack wStr) of
        [(d, "")] -> d
        _         -> 1.0

    -- Normalize weights to sum to 1.0, preserving order
    normalizeList :: [(Text, Double)] -> [(Text, Double)]
    normalizeList pairs
      | total <= 0 = pairs
      | otherwise  = map (\(n, w) -> (n, w / total)) pairs
      where
        total = sum (map snd pairs)

-- | Generate portmanteau from composer string (preserving input order)
-- Takes weighted portions from beginning/middle/end based on POSITION
-- Returns Nothing for "*", empty input, or "none" (offline mode)
makePortmanteau :: Text -> Maybe Text
makePortmanteau input
  | T.toLower input == "none" = Nothing
  | otherwise = case parseComposersWithOrder input of
      []               -> Nothing
      [(name, _)]      -> Just (T.toTitle name)
      composers
        | any ((== "*") . fst) composers -> Nothing
        | otherwise -> Just . T.toTitle . T.concat $ buildFragments composers
  where
    buildFragments composers = zipWith extractPart [0..] composers
      where
        total = length composers
        extractPart idx (name, weight) = extractByPosition idx total name weight

-- | Extract characters from name based on position in list
extractByPosition :: Int -> Int -> Text -> Double -> Text
extractByPosition idx total name weight
  | idx == 0           = takeFromBeginning name weight  -- First
  | idx == total - 1   = takeFromEnd name weight        -- Last
  | otherwise          = takeFromMiddle name weight     -- Middle

takeFromBeginning :: Text -> Double -> Text
takeFromBeginning name weight =
  let chars = max 1 (ceiling (fromIntegral (T.length name) * weight))
   in T.take chars name

takeFromEnd :: Text -> Double -> Text
takeFromEnd name weight =
  let len = T.length name
      chars = max 1 (ceiling (fromIntegral len * weight))
   in T.drop (len - chars) name

takeFromMiddle :: Text -> Double -> Text
takeFromMiddle name weight =
  let len = T.length name
      chars = max 1 (ceiling (fromIntegral len * weight))
      start = (len - chars) `div` 2  -- Center, favor earlier chars
   in T.take chars (T.drop start name)
