module Overtone where

import           Utility
import           MusicData

import qualified Data.Char as Char (toLower, isNumber)
import qualified Data.List as List (isInfixOf)

parseOvertones' :: (Num a, Integral a) => Int -> String -> [a]
parseOvertones' n str = --pitchList n str
  unique $ pitchList n str []
  where
    pitchList n str =
      let
        xs = words $ Char.toLower <$> str
        o k = (`mod` 12) . (+k) <$> (take n [0,7,4,10,2])
        k n = ((`mod`12) . (+n*5 `mod` 12) <$> [0,2,4,5,7,9,11])
        ys ?? z = if any (`elem` (fmap Char.toLower <$> ys)) $ xs
                  then (mappend z)--(mappend z) 
                  else (mappend mempty)--mempty
        chain =
          [["*","all","chr"]?? [0..11]
          ,["c","b#","dbb"]?? o 0 
          ,["c#","db","b*","b##"]?? o 1
          ,["d","c*","c##","ebb"]?? o 2
          ,["d#","eb","fbb"]?? o 3
          ,["e","d*","d##","fb"]?? o 4
          ,["f","e#","gbb"]?? o 5
          ,["f#","gb","e*","e##"]?? o 6
          ,["g","f*","f##","abb"]?? o 7
          ,["g#","ab"]?? o 8
          ,["a","g*","g##","bbb"]?? o 9
          ,["a#","bb","cbb"]?? o 10
          ,["b","a*","a##","cb"]?? o 11
          ,["c'","b#'","dbb'"]?? [0]
          ,["c#'","db'","b*'","b##'"]?? [1]
          ,["d'","c*'","c##'","ebb'"]?? [2]
          ,["d#'","eb'","fbb'"]?? [3]
          ,["e'","d*'","d##'","fb'"]?? [4]
          ,["f'","e#'","gbb'"]?? [5]
          ,["f#'","gb'","e*'","e##'"]?? [6]
          ,["g'","f*'","f##'","abb'"]?? [7]
          ,["g#'","ab'"]?? [8]
          ,["a'","g*'","g##'","bbb'"]?? [9]
          ,["a#'","bb'","cbb'"]?? [10]
          ,["b'","a*'","a##'","cb'"]?? [11]
          ,["0b","0#"]?? k 0
          ,["1b","11#"]?? k 1
          ,["2b","10#"]?? k 2
          ,["3b","9#"]?? k 3
          ,["4b","8#"]?? k 4
          ,["5b","7#"]?? k 5
          ,["6b","6#"]?? k 6
          ,["7b","5#"]?? k 7
          ,["8b","4#"]?? k 8
          ,["9b","3#"]?? k 9
          ,["10b","2#"]?? k 10
          ,["11b","1#"]?? k 11
          ]
      in foldr (.) id chain

parseOvertones :: (Num a, Integral a) => String -> [a]
parseOvertones = parseOvertones' 3

theHarmonicAlgorithm :: (MusicData a, Num b, Integral b) => 
                        (PitchClass -> NoteName) -> 
                        b -> 
                        [a] -> 
                        String -> 
                        [Chord]
theHarmonicAlgorithm f n rs str = 
  let integralSets = overtoneSets n (i' rs) $ parseOvertones str
   in toTriad f <$> integralSets


