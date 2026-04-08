{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Config
-- Description : Connection constants and file paths for Neo4j and corpus data
--
-- Centralised configuration for Neo4j database connection (URI, credentials)
-- and YCACL corpus file paths used during data ingestion.

module Harmonic.Config where

import Data.Text (Text)

neo4jUri :: Text
neo4jUri = "bolt://localhost:7687"

neo4jUser :: Text
neo4jUser = "neo4j"

neo4jPassword :: Text
neo4jPassword = "password"

bachDataPath :: FilePath
bachDataPath = "../data/jsbach_chorals_harmony.data"

ycaclArtifactPath :: FilePath
ycaclArtifactPath = "data/ycacl_sequences.csv"
