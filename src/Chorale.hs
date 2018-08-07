module Chorale where

import MusicData
import Utility

import qualified Data.List as List ( sort, sortBy )
import Data.Function ( on )

-- |helper function for reading in NoteName data
readNoteName  :: String -> NoteName
readNoteName s = read $ replace "#" "'" s

-- |mapping from sets of fundamentals and overtones into lists of viable triads
bachTriads         :: (Eq a, Num a) => a -> [String] -> [[Integer]] -> [[[Integer]]]
bachTriads n rs ps  =
  let fund = (\x -> [x]) . i . readNoteName <$> rs
   in zipWith (overtoneSets n) fund ps

-- |mapping from interval vector to degree of dissonance
dissonanceLevel           :: [Integer] -> (Integer, [Integer])
dissonanceLevel xs
  | countElem iVect 0 == 5 = (27, xs)
  | elem (7+head xs) xs    = (subtract 1 $ sum $ zipWith (*) dissVect iVect, xs)
  | otherwise              = (sum $ zipWith (*) dissVect iVect, xs)
    where
      iVect                = intervalVector xs 
      dissVect             = [16,8,4,2,1,24] -- based on work of Paul Hindemith

-- |mapping from a nested list of integers to the most consonant pitchclass set
mostConsonant         :: [[Integer]] -> [Integer]
mostConsonant xs       = triadChoice . sortFst $ dissonanceLevel <$> xs
  where triadChoice xs = (snd . head . sortFst) xs
        sortFst xs     = List.sortBy (compare `on` fst) xs



















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
jsData'' = mostConsonant <$> bachTriads 3 jsData' jsData