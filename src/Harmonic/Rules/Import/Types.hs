-- |
-- Module      : Harmonic.Rules.Import.Types
-- Description : Shared record types for the YCACL ingestion pipeline
--
-- The intermediate vocabulary passed between the ingestion stages:
-- "Harmonic.Rules.Import.CSV" parses the corpus export into these,
-- "Harmonic.Rules.Import.Transform" folds them into cadences, and
-- "Harmonic.Rules.Import.Graph" writes the result to Neo4j.
--
-- Ingestion-only. Nothing here is needed to generate or play music.

module Harmonic.Rules.Import.Types (
    ChordSlice(..),
) where

-- | A single YCACL slice: every pitch sounding at one vertical moment,
-- together with the fundamental detected for it by the corpus export.
--
-- Pitches are pitch classes 0-11: the R exporter
-- (@scripts\/export_ycacl.R@) already reduces to Z12 before writing the
-- artefact.
data ChordSlice = ChordSlice
  { slicePitches     :: [Int]   -- ^ every pitch in the slice, as pitch classes 0-11
  , sliceFundamental :: Int     -- ^ fundamental detected for the slice
  } deriving (Show, Eq)
