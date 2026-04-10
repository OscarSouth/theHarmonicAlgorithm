{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Config
-- Description : Connection constants and file paths for Neo4j and corpus data
--
-- Centralised configuration for Neo4j database connection (URI, credentials)
-- and YCACL corpus file paths used during data ingestion.
--
-- theHarmonicAlgorithm is a local-first tool. Neo4j runs on localhost via
-- docker-compose, which configures the same credentials shown here. There is
-- no network exposure — these are docker defaults, not secrets.
-- To start Neo4j: @docker compose up -d neo4j@

module Harmonic.Config where

import Data.Text (Text)

-- | Neo4j bolt URI. Matches the docker-compose service on localhost.
neo4jUri :: Text
neo4jUri = "bolt://localhost:7687"

-- | Neo4j username. Matches @NEO4J_AUTH@ in docker-compose.yml.
neo4jUser :: Text
neo4jUser = "neo4j"

-- | Neo4j password. Matches @NEO4J_AUTH@ in docker-compose.yml.
neo4jPassword :: Text
neo4jPassword = "password"

bachDataPath :: FilePath
bachDataPath = "../data/jsbach_chorals_harmony.data"

ycaclArtifactPath :: FilePath
ycaclArtifactPath = "data/ycacl_sequences.csv"
