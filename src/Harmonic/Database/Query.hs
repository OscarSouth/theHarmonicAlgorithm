{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Database.Query
-- Description : Read-only interface to Neo4j for cadence graph traversal
-- 
-- This module implements the Evaluation (E) component of the Creative Systems
-- Framework. It fetches transition probabilities from Neo4j and resolves
-- composer-weighted scores for candidate cadences.
--
-- The database is treated as abstract/pitch-agnostic. Root notes and voicings
-- are computed at runtime from user-defined starting conditions.

module Harmonic.Database.Query
  ( -- * Composer Weight Parsing
    ComposerWeights
  , parseComposerWeights
  , normalizeWeights
  
    -- * Graph Queries
  , fetchTransitions
  , fetchHomingCandidates
  
    -- * Weight Resolution
  , resolveWeights
  , applyComposerBlend
  ) where

import qualified Database.Bolt as Bolt
import           Data.Default (def)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Char (isSpace, isDigit)
import           Data.List (sortBy)
import           Data.Ord (Down(..))
import           Data.Function (on)
import           Control.Monad (forM)
import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON(..), Value(..), (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL

import qualified Harmonic.Core.Harmony as H

-- | Map from composer name to weight (e.g., "bach" -> 0.7)
type ComposerWeights = Map Text Double

-------------------------------------------------------------------------------
-- Composer Weight Parsing
-------------------------------------------------------------------------------

-- |Parse a composer selection string into normalized weights.
-- 
-- Supported formats:
--   "bach debussy"           -> equal weights, normalized to sum 1.0
--   "bach:30 debussy:70"     -> weighted, normalized (30/100 = 0.3, 70/100 = 0.7)
--   "bach:0.3, debussy:0.7"  -> already normalized (or re-normalized if needed)
--
-- Examples:
--   parseComposerWeights "bach debussy" 
--     == Map.fromList [("bach", 0.5), ("debussy", 0.5)]
--   parseComposerWeights "bach:30 debussy:70" 
--     == Map.fromList [("bach", 0.3), ("debussy", 0.7)]
parseComposerWeights :: Text -> ComposerWeights
parseComposerWeights input =
  let tokens = filter (not . T.null) $ T.split isSeparator input
      parsed = mapMaybe parseToken tokens
   in normalizeWeights $ Map.fromList parsed
  where
    isSeparator c = c == ' ' || c == ','
    
    parseToken :: Text -> Maybe (Text, Double)
    parseToken tok =
      case T.splitOn ":" tok of
        [name]        -> Just (T.strip name, 1.0)  -- Equal weight
        [name, wStr]  -> 
          let weight = parseWeight (T.strip wStr)
           in Just (T.strip name, weight)
        _             -> Nothing
    
    parseWeight :: Text -> Double
    parseWeight wStr =
      let str = T.unpack wStr
       in case reads str of
            [(d, "")] -> d
            _         -> 1.0  -- Default to 1.0 if parse fails

-- |Normalize weights so they sum to 1.0
normalizeWeights :: ComposerWeights -> ComposerWeights
normalizeWeights weights
  | total <= 0 = weights
  | otherwise  = Map.map (/ total) weights
  where
    total = sum (Map.elems weights)

-------------------------------------------------------------------------------
-- Graph Queries
-------------------------------------------------------------------------------

-- |Fetch all outgoing transitions from a cadence node.
-- 
-- Returns: List of (Cadence, ComposerWeights) pairs for all [:NEXT] edges.
-- The Cadence is reconstructed from Neo4j node properties (movement, chord).
--
-- Query: MATCH (c:Cadence {show: $show})-[r:NEXT]->(n:Cadence) 
--        RETURN n.movement, n.chord, r.weights
fetchTransitions :: Text -> Bolt.BoltActionT IO [(H.Cadence, ComposerWeights)]
fetchTransitions cadenceShow = do
  let query = T.unlines
        [ "MATCH (c:Cadence {show: $show})-[r:NEXT]->(n:Cadence)"
        , "RETURN n.movement AS movement, n.chord AS chord, r.weights AS weights"
        ]
      params = Map.fromList [("show", Bolt.T cadenceShow)]
  
  records <- Bolt.queryP query params
  pure $ mapMaybe parseRecord records
  where
    parseRecord :: Bolt.Record -> Maybe (H.Cadence, ComposerWeights)
    parseRecord record = do
      mvmtVal <- Map.lookup "movement" record
      chordVal <- Map.lookup "chord" record
      weightsVal <- Map.lookup "weights" record
      
      mvmtStr <- extractText mvmtVal
      chordStr <- extractText chordVal
      weightsStr <- extractText weightsVal
      
      let cadence = H.constructCadence (T.unpack mvmtStr, T.unpack chordStr)
      let weights = parseWeightsJson weightsStr
      
      pure (cadence, weights)

-- |Fetch cadences that have a high transition probability INTO the target.
-- Used for homing logic: finding "feeder" cadences that lead to the start.
--
-- Query: MATCH (n:Cadence)-[r:NEXT]->(target:Cadence {show: $show})
--        RETURN n.show, r.confidence
fetchHomingCandidates :: Text -> Bolt.BoltActionT IO [(Text, Double)]
fetchHomingCandidates targetShow = do
  let query = T.unlines
        [ "MATCH (n:Cadence)-[r:NEXT]->(target:Cadence {show: $show})"
        , "RETURN n.show AS fromShow, r.confidence AS confidence"
        , "ORDER BY r.confidence DESC"
        ]
      params = Map.fromList [("show", Bolt.T targetShow)]
  
  records <- Bolt.queryP query params
  pure $ mapMaybe parseHomingRecord records
  where
    parseHomingRecord :: Bolt.Record -> Maybe (Text, Double)
    parseHomingRecord record = do
      showVal <- Map.lookup "fromShow" record
      confVal <- Map.lookup "confidence" record
      
      showStr <- extractText showVal
      conf <- extractDouble confVal
      
      pure (showStr, conf)

-------------------------------------------------------------------------------
-- Weight Resolution
-------------------------------------------------------------------------------

-- |Resolve transition weights using the active composer blend.
-- 
-- For each candidate (Cadence, ComposerWeights), compute a single score
-- by multiplying each composer's edge weight by the user's blend weight
-- and summing.
--
-- Example:
--   candidate weights: {"bach": 5, "debussy": 3}
--   user blend: {"bach": 0.7, "debussy": 0.3}
--   score = 5 * 0.7 + 3 * 0.3 = 4.4
resolveWeights :: ComposerWeights -> [(H.Cadence, ComposerWeights)] -> [(H.Cadence, Double)]
resolveWeights blend candidates =
  let scored = map (scoreCandidate blend) candidates
      sorted = sortBy (compare `on` (Down . snd)) scored  -- Highest first
   in sorted
  where
    scoreCandidate :: ComposerWeights -> (H.Cadence, ComposerWeights) -> (H.Cadence, Double)
    scoreCandidate userBlend (cadence, edgeWeights) =
      let score = sum 
            [ userWeight * fromMaybe 0 (Map.lookup composer edgeWeights)
            | (composer, userWeight) <- Map.toList userBlend
            ]
       in (cadence, score)

-- |Apply composer blend to filter transitions, keeping only those with score > 0
applyComposerBlend :: ComposerWeights -> [(H.Cadence, ComposerWeights)] -> [(H.Cadence, Double)]
applyComposerBlend blend = filter ((> 0) . snd) . resolveWeights blend

-------------------------------------------------------------------------------
-- JSON Parsing Helpers
-------------------------------------------------------------------------------

-- |Parse the weights JSON string from Neo4j.
-- Format: '{"Debussy":3.0,"Stravinsky":2.0}'
parseWeightsJson :: Text -> ComposerWeights
parseWeightsJson jsonStr =
  case Aeson.decode (BL.fromStrict $ TE.encodeUtf8 jsonStr) of
    Just (Object obj) -> 
      Map.fromList [(Key.toText k, extractNum v) | (k, v) <- KM.toList obj]
    _ -> Map.empty
  where
    extractNum (Number n) = realToFrac n
    extractNum _          = 0

-- |Convert Aeson Key to Text
-- (Aeson uses Key type for object keys in newer versions)

-------------------------------------------------------------------------------
-- Bolt Value Extractors
-------------------------------------------------------------------------------

extractText :: Bolt.Value -> Maybe Text
extractText (Bolt.T t) = Just t
extractText _          = Nothing

extractDouble :: Bolt.Value -> Maybe Double
extractDouble (Bolt.F d) = Just d
extractDouble (Bolt.I i) = Just (fromIntegral i)
extractDouble _          = Nothing
