module Chorale where

import MusicData
import Utility

import qualified Data.List as List ( sort, sortBy )
import Data.Function ( on )
























-- |test dataset of 'pitch' data
jsData ::[[Integer]] 
jsData = [[2, 3, 7, 10],
          [0, 5, 9],
          [3, 7, 10],
          [0, 3, 7, 9, 10],
          [2, 5, 10],
          [2, 5, 9, 10],
          [3, 7, 10],
          [0, 3, 7, 10],
          [0, 5, 10],
          [0, 3, 5, 9]]

-- |test dataset of 'fundamental' data
jsData' :: [String]
jsData' = ["D#", "F", "G", "A", "Bb", "Bb", "Eb", "Eb", "F", "F"]

-- |test dataset of ingested data
jsData'' :: [[Integer]]
jsData'' = mostConsonant <$> possibleTriads' jsData' jsData