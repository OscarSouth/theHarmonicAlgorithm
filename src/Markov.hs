module Markov where

import Utility
import MusicData
import Chorale

import Data.Map (Map)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set ( toList, fromList, size)

import Data.Maybe ( fromMaybe )
import Data.Map.Merge.Lazy
import Numeric.LinearAlgebra
import Data.Ord ( comparing )

-- |representation of harmonic quality
-- data Chord = Maj
--            | Min
--            | Dim
--            deriving (Show, Eq, Ord)

-- |representation of a harmonic cadence
-- newtype Cadence = Cadence Chord  deriving (Show, Eq, Ord)

-- |representation of bigrams and trigrams containing deterministic cadences
type Trigram = ((Cadence, Cadence), Cadence)
type Bigram  = (Cadence, Cadence)

-- |representation of counts for each trigram
type TransitionCounts = Map Trigram Double

-- |representations of the Markov transition matrix
type TransitionMatrix = Matrix R

-- |map containing 'current state' bigrams and list of 'next' probabilities
type MarkovMap = Map Bigram [(Cadence, Double)]

-- |mapping from list of events into list of trigrams
trigrams                    :: [Cadence] -> [Trigram]
trigrams (x:xs)
  | length (x:xs) < 3        = []
  | otherwise                = trigram (x:xs) : trigrams xs
    where trigram (x:y:z:zs) = (\a b c -> ((a, b), c)) x y z

-- |mapping from input data into all theoretically possible trigrams
threes   :: [Cadence] -> [Trigram]
threes xs = 
  let cs  = unique xs
   in [ ((a, b), c) | a <- cs, b <- cs, c <- cs ]

-- |mapping from input data into all possible trigrams with counts of zero
zeroCounts             :: [Cadence] -> TransitionCounts
zeroCounts xs           = 
  let mapInsert acc key = Map.insert key 0 acc
   in foldl mapInsert Map.empty $ threes xs

-- |mapping from input data to counts of all occurring transitions
cadenceCounts        :: [Cadence] -> TransitionCounts
cadenceCounts xs      = 
  let mInsert acc key = Map.insertWith (+) key 1 acc
   in foldl mInsert Map.empty $ trigrams xs

-- |mapping from input to counts of cadences, including 'stationary' movements
transitionCounts       :: [Cadence] -> TransitionCounts
transitionCounts xs     = mergeMaps (foldl mInsert cadences (Map.keys diff)) zeros
  where diff            = Map.difference zeros cadences
        zeros           = zeroCounts xs
        cadences        = cadenceCounts xs
        mergeMaps m1 m2 = merge preserveMissing preserveMissing 
                          (zipWithMatched (\k x y -> x)) m1 m2
        keys k          = [ (fst k, nxt) | nxt <- unique xs ]
        newKey k        = (fst k, snd $ fst k)
        member k        = sequenceA [ f ks | f <- [Map.member], ks <- keys k ] (cadences)
        mInsert acc key
          | all (\x -> x == False) (member key) == True =
              Map.insert (newKey key) 1 acc
          | otherwise   =
              Map.insert key (fromMaybe 0 $ Map.lookup key $ cadences) acc

-- |helper function for probabilityList which generates probability sublists
transitionProbs      :: [Cadence] -> [Double] -> [[Double]]
transitionProbs _ []  = []
transitionProbs xs ys =
  let j               = Set.size $ Set.fromList xs
      i               = j^2
      xx              = take j ys
      recurse         = drop j ys
   in fmap (/ sum xx) xx : transitionProbs xs recurse

-- |mapping from list of Cadences into list of transitions with probabilities
probabilityMap   :: [Cadence] -> Map Trigram Double
probabilityMap xs = Map.fromList $ zip (Map.keys $ zeroCounts xs) $ concat 
                     . transitionProbs xs $ Map.elems $ transitionCounts xs

-- |mapping from list of cadences into transition matrix
transitionMatrix   :: [Cadence] -> TransitionMatrix
transitionMatrix xs = 
  let i             = j^2
      j             = Set.size $ Set.fromList xs
   in (i><j) $ Map.elems $ probabilityMap xs :: Matrix R

-- |mapping from list of cadences into map with possible next probabilities
markovMap              :: [Cadence] -> MarkovMap
markovMap xs            = foldl mInsert Map.empty $ threes xs
  where mInsert acc key = Map.insertWith (++) (fst key) (pList key) acc
        pList key       = [(snd key, fromMaybe 0 $ Map.lookup key pMap)]
        pMap            = probabilityMap xs

showCadences       :: [Cadence] -> String
showCadences []     = []
showCadences (c:cs) = showc ++ ('\n' : showcs)
   where showc      = (show c)
         showcs     = showCadences cs

printCadences   :: [Cadence] -> IO ()
printCadences xs = putStr $ showCadences xs

-- chords = take 50 $ cycle [Cadence Maj, Cadence Min, Cadence Dim]









































-- -- |temporary deterministic test datasets
-- tsData :: [Cadence]
-- tsData  = [ Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Min
--           , Cadence Min
--           , Cadence Maj
--           ]

-- tsData' :: [Cadence]
-- tsData'  = [ Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Dim
--           , Cadence Maj
--           , Cadence Min
--           , Cadence Min
--           , Cadence Maj
--           , Cadence Dim
--           , Cadence Dim
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Min
--           , Cadence Min
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Min
--           , Cadence Maj
--           , Cadence Min
--           , Cadence Min
--           , Cadence Dim
--           , Cadence Dim
--           , Cadence Dim
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Maj
--           , Cadence Dim
--           , Cadence Maj
--           ]
