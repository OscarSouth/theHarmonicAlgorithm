module Overtone where

import           MusicData
import           Utility

import qualified Data.Char as Char (isNumber, toLower)
import qualified Data.List as List (isInfixOf)

parseTuning' :: (Num a, Integral a) => Int -> String -> [a]
parseTuning' n str = unique $ pitchList n str []
  where
    pitchList n str =
      let
        xs = words $ Char.toLower <$> str
        o k = (`mod` 12) . (+k) <$> (take n [0,7,4,10,2])
        ys ?? z = if any (`elem` ys) $ xs
                  then (mappend z)
                  else (mappend mempty)
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
          ]
      in foldr (.) id chain

parseKey' :: (Num a, Integral a) => Int -> String -> [a]
parseKey' n str = unique $ pitchList n str []
  where
    pitchList n str =
      let
        xs = words $ Char.toLower <$> str
        k n = ((`mod`12) . (+n*5 `mod` 12) <$> [0,2,4,5,7,9,11])
        ys ?? z = if any (`elem` ys) $ xs
                  then (mappend z)
                  else (mappend mempty)
        chain =
          [["*","all","chr"]?? [0..11]
          ,["0b","0#","0","c","b#","am"]?? k 0
          ,["1b","b","11#","###########","f","e#","dm"]?? k 1
          ,["2b","bb","10#","##########","a#","gm"]?? k 2
          ,["3b","bbb","9#","#########","eb","d#","cm","b#m"]?? k 3
          ,["4b","bbbb","8#","########","ab","g#","fm","e#m"]?? k 4
          ,["5b","bbbbb","7#","#######","db","c#","bbm","a#m"]?? k 5
          ,["6b","bbbbbb","6#","######","gb","f#","ebm","d#m"]?? k 6
          ,["7b","bbbbbbb","5#","#####","cb","b","abm","g#m"]?? k 7
          ,["8b","bbbbbbbb","4#","####","fb","e","dbm","c#m"]?? k 8
          ,["9b","bbbbbbbbb","3#","###","a","gbm","f#m"]?? k 9
          ,["10b","bbbbbbbbbb","2#","##","d","cbm","bm"]?? k 10
          ,["11b","bbbbbbbbbbb","1#","#","g","fbm","em"]?? k 11
          ]
      in foldr (.) id chain

parseFunds' :: (Num a, Integral a) => Int -> String -> [a]
parseFunds' n str = unique $ pitchList n str []
  where
    pitchList n str =
      let
        xs = words $ Char.toLower <$> str
        k n = ((`mod`12) . (+n*5 `mod` 12) <$> [0,2,4,5,7,9,11])
        ys ?? z = if any (`elem` ys) $ xs
                  then (mappend z)
                  else (mappend mempty)
        chain =
          [["*","all","chr"]?? [0..11]
          ,["c","b#","dbb"]?? [0]
          ,["c#","db","b*","b##"]?? [1]
          ,["d","c*","c##","ebb"]?? [2]
          ,["d#","eb","fbb"]?? [3]
          ,["e","d*","d##","fb"]?? [4]
          ,["f","e#","gbb"]?? [5]
          ,["f#","gb","e*","e##"]?? [6]
          ,["g","f*","f##","abb"]?? [7]
          ,["g#","ab"]?? [8]
          ,["a","g*","g##","bbb"]?? [9]
          ,["a#","bb","cbb"]?? [10]
          ,["b","a*","a##","cb"]?? [11]
          ,["0b","0#","0"]?? k 0
          ,["1b","11#","###########"]?? k 1
          ,["2b","bb","10#","##########"]?? k 2
          ,["3b","bbb","9#","#########"]?? k 3
          ,["4b","bbbb","8#","########"]?? k 4
          ,["5b","bbbbb","7#","#######"]?? k 5
          ,["6b","bbbbbb","6#","######"]?? k 6
          ,["7b","bbbbbbb","5#","#####"]?? k 7
          ,["8b","bbbbbbbb","4#","####"]?? k 8
          ,["9b","bbbbbbbbb","3#","###"]?? k 9
          ,["10b","bbbbbbbbbb","2#","##"]?? k 10
          ,["11b","bbbbbbbbbbb","1#","#"]?? k 11
          ]
      in foldr (.) id chain

parseOvertones' :: (Num a, Integral a) => Int -> String -> [a]
parseOvertones' n str = unique $ pitchList n str []
  where
    pitchList n str =
      let
        xs = words $ Char.toLower <$> str
        o k = (`mod` 12) . (+k) <$> (take n [0,7,4,10,2])
        k n = ((`mod`12) . (+n*5 `mod` 12) <$> [0,2,4,5,7,9,11])
        ys ?? z = if any (`elem` ys) $ xs
                  then (mappend z)
                  else (mappend mempty)
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
          ,["0b","0#","0"]?? k 0
          ,["1b","b","11#","###########"]?? k 1
          ,["2b","bb","10#","##########"]?? k 2
          ,["3b","bbb","9#","#########"]?? k 3
          ,["4b","bbbb","8#","########"]?? k 4
          ,["5b","bbbbb","7#","#######"]?? k 5
          ,["6b","bbbbbb","6#","######"]?? k 6
          ,["7b","bbbbbbb","5#","#####"]?? k 7
          ,["8b","bbbbbbbb","4#","####"]?? k 8
          ,["9b","bbbbbbbbb","3#","###"]?? k 9
          ,["10b","bbbbbbbbbb","2#","##"]?? k 10
          ,["11b","bbbbbbbbbbb","1#","#"]?? k 11
          ]
      in foldr (.) id chain

parseOvertones :: (Num a, Integral a) => String -> [a]
parseOvertones = parseOvertones' 3

parseTuning :: (Num a, Integral a) => String -> [a]
parseTuning = parseTuning' 3

parseKey :: (Num a, Integral a) => String -> [a]
parseKey = parseKey' 3

parseFunds :: (Num a, Integral a) => String -> [a]
parseFunds = parseFunds' 3

theHarmonicAlgorithm :: (MusicData a, Num b, Integral b) =>
                        (PitchClass -> NoteName) ->
                        b -> [a] -> String -> [Chord]
theHarmonicAlgorithm f n roots str =
  let integralSets = overtoneSets n (i' roots) $ parseOvertones str
   in toTriad f <$> integralSets

theHarmonicAlgorithm' :: (Num a, Integral a) =>
                        a -> [a] -> [a] -> (PitchClass -> NoteName) -> [Chord]
theHarmonicAlgorithm' n roots overtones f =
  let integralSets = overtoneSets n roots overtones
   in toTriad f <$> integralSets