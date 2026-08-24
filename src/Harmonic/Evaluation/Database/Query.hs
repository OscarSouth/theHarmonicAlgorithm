{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Evaluation.Database.Query
-- Description : Read-only interface to Neo4j for cadence graph traversal
--
-- This module implements the Evaluation (E) component of the Creative Systems
-- Framework. It fetches transition probabilities from Neo4j and resolves
-- composer-weighted scores for candidate cadences.
--
-- The database is treated as abstract\/pitch-agnostic. Root notes and voicings
-- are computed at runtime from user-defined starting conditions.

module Harmonic.Evaluation.Database.Query
  ( -- * Composer Weight Parsing
    ComposerWeights
  , parseComposerWeights
  , normalizeWeights
  
    -- * Graph Queries
  , fetchTransitions
  , fetchTransitionsAggregate

    -- * Weight Resolution
  , resolveWeights
  , applyComposerBlend
  ) where

import qualified Data.Aeson as A
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
import           Data.Scientific (toRealFloat)

import           Harmonic.Database (DbActionT, runQueryP)
import qualified Harmonic.Rules.Types.Harmony as H

-- | Map from composer name to weight (e.g., "bach" -> 0.7)
type ComposerWeights = Map Text Double

-------------------------------------------------------------------------------
-- Composer Weight Parsing
-------------------------------------------------------------------------------

-- |Parse a composer selection string into normalized weights.
--
-- Composer names are matched case-insensitively against the corpus —
-- @"Bach"@, @"bach"@, @"BACH"@, @"bAcH"@ all collapse to the same key.
-- Names are lower-cased here at parse time; 'resolveWeights' lowercases
-- corpus edge keys at lookup time so the match is robust regardless of
-- the case convention used during corpus ingestion.
--
-- Supported formats:
--   "bach debussy"           -> equal weights, normalized to sum 1.0
--   "Bach:30 Debussy:70"     -> weighted, normalized; case-insensitive
--   "bach:0.3, debussy:0.7"  -> already normalized (or re-normalized if needed)
--
-- Examples:
--   parseComposerWeights "bach debussy"
--     == Map.fromList [("bach", 0.5), ("debussy", 0.5)]
--   parseComposerWeights "Bach:30 DEBUSSY:70"
--     == Map.fromList [("bach", 0.3), ("debussy", 0.7)]
parseComposerWeights :: Text -> ComposerWeights
parseComposerWeights input
  | T.strip input == "*" = Map.empty  -- wildcard: empty → aggregate path
  | otherwise =
      let tokens = filter (not . T.null) $ T.split isSeparator input
          parsed = mapMaybe parseToken tokens
       in normalizeWeights $ Map.fromListWith (+) parsed
  where
    isSeparator c = c == ' ' || c == ','

    parseToken :: Text -> Maybe (Text, Double)
    parseToken tok =
      case T.splitOn ":" tok of
        [name]        -> Just (T.toLower (T.strip name), 1.0)  -- Equal weight
        [name, wStr]  ->
          let weight = parseWeight (T.strip wStr)
           in Just (T.toLower (T.strip name), weight)
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
fetchTransitions :: Text -> DbActionT [(H.Cadence, ComposerWeights)]
fetchTransitions cadenceShow = do
  let query = T.unlines
        [ "MATCH (c:Cadence {show: $show})-[r:NEXT]->(n:Cadence)"
        , "RETURN n.movement AS movement, n.chord AS chord, r.weights AS weights"
        ]
      params = Map.fromList [("show", A.String cadenceShow)]

  records <- runQueryP query params
  pure $ mapMaybe parseRecord records
  where
    parseRecord :: Map Text A.Value -> Maybe (H.Cadence, ComposerWeights)
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

-- |Fetch outgoing transitions with the pre-aggregated corpus score.
--
-- Fast path for the wildcard composer spec (@"*"@, i.e. an empty blend):
-- every edge stores @r.confidence@, written at ingestion as exactly
-- @sum (Map.elems weights)@ ("Harmonic.Rules.Import.Graph"), which is the
-- score 'resolveWeights' computes for an empty blend. Projecting it directly
-- skips the @r.weights@ JSON payload (all corpus composers per edge, ~1MB\/
-- fetch on high-degree nodes) — measured ~30x faster and ~115x less
-- allocation per generation step.
--
-- Result contract matches @'applyComposerBlend' Map.empty <$> 'fetchTransitions'@
-- exactly: positive scores only (always true by the ingestion invariant —
-- zero-total edges are never written), sorted highest first.
--
-- Query: MATCH (c:Cadence {show: $show})-[r:NEXT]->(n:Cadence)
--        RETURN n.movement, n.chord, r.confidence
fetchTransitionsAggregate :: Text -> DbActionT [(H.Cadence, Double)]
fetchTransitionsAggregate cadenceShow = do
  let query = T.unlines
        [ "MATCH (c:Cadence {show: $show})-[r:NEXT]->(n:Cadence)"
        , "RETURN n.movement AS movement, n.chord AS chord, r.confidence AS confidence"
        ]
      params = Map.fromList [("show", A.String cadenceShow)]

  records <- runQueryP query params
  pure $ sortBy (compare `on` (Down . snd))
       $ filter ((> 0) . snd)
       $ mapMaybe parseRecord records
  where
    parseRecord :: Map Text A.Value -> Maybe (H.Cadence, Double)
    parseRecord record = do
      mvmtVal <- Map.lookup "movement" record
      chordVal <- Map.lookup "chord" record
      confVal <- Map.lookup "confidence" record

      mvmtStr <- extractText mvmtVal
      chordStr <- extractText chordVal
      conf <- extractDouble confVal

      let cadence = H.constructCadence (T.unpack mvmtStr, T.unpack chordStr)

      pure (cadence, conf)

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
    scoreCandidate userBlend (cadence, edgeWeights)
      | Map.null userBlend =
          -- Wildcard "*": use aggregate (sum of all composer weights = r.confidence equivalent)
          (cadence, sum (Map.elems edgeWeights))
      | otherwise =
          -- Case-fold both sides so the match is robust regardless of corpus
          -- case convention (current corpus is lowercase; doc example showed
          -- capitalised) and regardless of how the user constructed the blend
          -- (parseComposerWeights lower-cases at parse time, but a directly-
          -- constructed Map bypassing the parser may have arbitrary case).
          -- Sum on collision so two case-variant keys merge rather than drop.
          let edgeLower = Map.fromListWith (+)
                            [ (T.toLower k, v) | (k, v) <- Map.toList edgeWeights ]
              score = sum
                [ userWeight * fromMaybe 0 (Map.lookup (T.toLower composer) edgeLower)
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
-- Result Value Extractors
-------------------------------------------------------------------------------

extractText :: A.Value -> Maybe Text
extractText (A.String t) = Just t
extractText _            = Nothing

-- | JSON numbers carry both stored FLOATs and INTEGERs, so this covers an
-- integral-stored confidence from any past ingestion as well.
extractDouble :: A.Value -> Maybe Double
extractDouble (A.Number n) = Just (toRealFloat n)
extractDouble _            = Nothing
