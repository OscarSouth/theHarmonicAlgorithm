module Chorale where

import MusicData
import Utility

import qualified Data.List as List ( sort )

-- |helper function for reading in NoteName data
readNoteName  :: String -> NoteName
readNoteName s = read $ replace "#" "'" s

-- |mapping from sets of fundamentals and overtones into viable composite sets
overtoneSets      :: (Num a, Num b, Eq a, Eq b, Ord b) => a -> [b] -> [b] -> [[b]]
overtoneSets n r p = [ i:j | i <- r, 
                       j <- List.sort <$> (choose $ n-1) p, 
                       not $ i `elem` j]

-- |mapping from sets of fundamentals and overtones into lists of viable triads
bachTriads        :: (Num a, Eq a) => a -> [String] -> [[Integer]] -> [[[Integer]]]
bachTriads n r ps  =
  let funds = (\x -> [x]) . i . readNoteName <$> r
   in zipWith (overtoneSets n) funds ps

























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