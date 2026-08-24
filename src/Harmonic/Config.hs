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

module Harmonic.Config (
    -- * Neo4j connection
    neo4jUri, neo4jUser, neo4jPassword,

    -- * Corpus paths
    ycaclArtifactPath,
) where

import Data.Text (Text)

-- | Neo4j HTTP base URI (the Query API lives under it). Matches the
-- docker-compose service on localhost. Override per-process with the
-- @HA_NEO4J_URL@ environment variable.
neo4jUri :: Text
neo4jUri = "http://localhost:7474"

-- | Neo4j username. Matches @NEO4J_AUTH@ in docker-compose.yml.
neo4jUser :: Text
neo4jUser = "neo4j"

-- | Neo4j password. Matches @NEO4J_AUTH@ in docker-compose.yml.
neo4jPassword :: Text
neo4jPassword = "password"

-- | Path to the derived YCACL corpus artefact consumed by @stack run@.
-- Produced by @scripts\/export_ycacl.R@; not distributed with the
-- repository (@data\/artefacts/@ is gitignored — the file is ~345 MB).
ycaclArtifactPath :: FilePath
ycaclArtifactPath = "data/artefacts/ycacl_sequences.csv"
