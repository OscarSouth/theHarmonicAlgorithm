-- |
-- Module      : Harmonic.Rules.Import.Merge
-- Description : Composer normalisation, curation, and transition merging
--
-- The stages of the ingestion pipeline that sit between the per-piece
-- cadence expansion ("Harmonic.Rules.Import.Transform") and the graph
-- write ("Harmonic.Rules.Import.Graph"): normalising raw composer names
-- to corpus keys, applying the curated allow-list, and merging
-- per-composer transition maps into per-edge composer weight maps.
--
-- == The composer-key contract
--
-- 'slug' is the ingester's normaliser. The exporter
-- (@scripts\/export_ycacl.R@, @normalize_composer@) must apply the SAME
-- rule: any curation list matched against keys produced under a
-- different normalisation silently drops every composer whose two keys
-- disagree. Any change here must be mirrored in the exporter.
module Harmonic.Rules.Import.Merge (
    ComposerPieces,
    slug,
    normalizeComposers,
    filterComposers,
    mergeComposerTransitions,
) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isAlphaNum)
import           Data.List (foldl')

import           Harmonic.Rules.Import.CSV (YCACLData)
import           Harmonic.Rules.Import.Types (ChordSlice)
import           Harmonic.Rules.Import.Graph (ComposerWeights)
import           Harmonic.Evaluation.Analysis.Markov (Edge)
import qualified Harmonic.Rules.Types.Harmony as H

-- | Composer catalogues keyed by normalised ('slug') composer name.
type ComposerPieces = Map Text (Map Text [ChordSlice])

-- | Normalise a raw corpus composer string to a graph composer key:
-- lowercase, keep alphanumerics and spaces, then map spaces (and any
-- other retained non-alphanumeric) to @_@.
--
-- This is the key under which every edge weight is stored and fetched
-- (@r.weights@ in Neo4j, parsed by "Harmonic.Evaluation.Database.Query").
slug :: Text -> Text
slug = sanitize . T.toLower
  where
    sanitize = T.map replaceChar . T.filter validChar
    validChar c = isAlphaNum c || c == ' '
    replaceChar c
      | isAlphaNum c = c
      | otherwise    = '_'

-- | Re-key the parsed corpus by 'slug', merging composers whose raw
-- names normalise identically.
normalizeComposers :: YCACLData -> ComposerPieces
normalizeComposers dataset = foldl' insertComposer Map.empty (Map.toList dataset)
  where
    insertComposer acc (composer, pieces) =
      let key = slug composer
       in Map.insertWith (Map.unionWith (++)) key pieces acc

-- | Apply curation: an allow-list (empty admits everything) and an
-- exclude-list. Returns the kept catalogues and the dropped keys (with
-- piece counts), so the caller can REPORT every refusal instead of
-- losing composers silently.
filterComposers :: [Text] -> [Text] -> ComposerPieces -> (ComposerPieces, [(Text, Int)])
filterComposers include exclude dataset =
  let admits key = (null include || key `elem` include)
                   && key `notElem` exclude
      (kept, dropped) = Map.partitionWithKey (\k _ -> admits k) dataset
   in (kept, [ (k, Map.size pieces) | (k, pieces) <- Map.toList dropped ])

-- | Merge per-composer transition probability maps into one map per
-- edge, keyed by composer. Edges whose total weight is zero are
-- excluded. Weights are SPARSE: a composer absent from an edge's map
-- carries implicit weight 0 — the read side
-- ('Harmonic.Evaluation.Database.Query.resolveWeights') reads a missing
-- key as 0, so dense zero-padding would only inflate the store.
mergeComposerTransitions
  :: Map Text (Map Edge Double)
  -> [(H.Cadence, H.Cadence, ComposerWeights)]
mergeComposerTransitions transitionMaps =
  let merged = Map.foldlWithKey' accumulate Map.empty transitionMaps
   in [ (from, to, weights)
      | ((from, to), weights) <- Map.toList merged
      , sum (Map.elems weights) > 0
      ]
  where
    accumulate acc composer edgeMap =
      foldl' (insertWeight composer) acc (Map.toList edgeMap)

    insertWeight composer acc (edge, weight) =
      Map.insertWith (Map.unionWith (+)) edge (Map.singleton composer weight) acc
