{-# LANGUAGE OverloadedStrings #-}
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
