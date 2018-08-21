module Markov where

import           MusicData
import           Utility

import           Data.Map              (Map)
import           Numeric.LinearAlgebra (Matrix, R, (><))

import qualified Data.Map              as Map (empty, insertWith, lookup)
import qualified Data.Map.Merge.Strict as Map' (merge, preserveMissing,
                                                zipWithMatched)
import qualified Data.Map.Strict       as Strict (Map)
import qualified Data.Map.Strict       as Map' (difference, elems, empty,
                                                fromList, insert, insertWith,
                                                keys, lookup, member, toList)
import qualified Data.Maybe            as Maybe (fromMaybe)
import qualified Data.Set              as Set (fromList, size, toList)

-- |representation of bigrams and containing deterministic cadence sequences
type Bigram  = (Cadence, Cadence)

-- |representation of counts for each trigram
type TransitionCounts = Strict.Map Bigram Double

-- |representations of the Markov transition matrix
type TransitionMatrix = Matrix R

-- |representation of markov transition matrix as key-value pairs
type MarkovMap = Map Cadence [(Cadence, Double)]

-- |mapping from list of events into list of existing preceding bigrams
bigrams :: [a] -> [(a, a)]
bigrams (x:xs)
  | length (x:xs) < 2 = []
  | otherwise = bigram (x:xs) : bigrams xs
  where bigram (x:y:ys) = (\a b -> (a, b)) x y

-- |lifted 'shortcut' `toCadence` which operates on a list of integer lists
toCadences    :: (Integral a, Num a) => [[a]] -> [Cadence]
toCadences xs = toCadence <$> (bigrams $ flatTriad <$> xs)

-- |mapping from input data into all theoretically possible bigrams
pairs   :: [Cadence] -> [Bigram]
pairs xs =
  let cs  = unique xs
   in [ (x, y) | x <- cs, y <- cs ]

-- |mapping from input data into all possible trigrams with counts of zero
zeroCounts             :: [Cadence] -> TransitionCounts
zeroCounts xs           =
  let mInsert acc key = Map'.insert key 0 acc
   in foldl mInsert Map'.empty $ pairs xs

-- |mapping from input data to counts of all occurring transitions
cadenceCounts        :: [Cadence] -> TransitionCounts
cadenceCounts xs      =
  let mInsert acc key = Map'.insertWith (+) key 1 acc
   in foldl mInsert Map'.empty $ bigrams xs

-- |mapping from input to counts of cadences, including 'stationary' movements
transitionCounts       :: [Cadence] -> TransitionCounts
transitionCounts xs     = mergeMaps (foldl mInsert cadences (Map'.keys diff)) zeros
  where diff            = Map'.difference zeros cadences
        zeros           = zeroCounts xs
        cadences        = cadenceCounts xs
        mergeMaps m1 m2 = Map'.merge Map'.preserveMissing Map'.preserveMissing
                          (Map'.zipWithMatched (\k x y -> x)) m1 m2
        keys k          = [ (fst k, nxt) | nxt <- unique xs ]
        newKey k        = (fst k, fst k)
        member k        = sequenceA [ f ks | f <- [Map'.member], ks <- keys k ] (cadences)
        mInsert acc key
          | all (\x -> x == False) (member key) == True =
              Map'.insert (newKey key) 1 acc
          | otherwise   =
              Map'.insert key (Maybe.fromMaybe 0 $ Map'.lookup key $ cadences) acc

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
probabilityMap   :: [Cadence] -> Map Bigram Double
probabilityMap xs = Map'.fromList $ zip (Map'.keys $ zeroCounts xs) $ concat
                     . transitionProbs xs $ Map'.elems $ transitionCounts xs

-- |mapping from list of cadences into transition matrix
transitionMatrix   :: [Cadence] -> TransitionMatrix
transitionMatrix xs =
  let n             = Set.size $ Set.fromList xs
   in (n><n) $ Map'.elems $ probabilityMap xs :: Matrix R

-- |mapping from list of cadences into map with possible next probabilities
markovMap              :: [Cadence] -> MarkovMap
markovMap xs            = foldl mInsert Map.empty $ pairs xs
  where mInsert acc key = Map.insertWith (++) (fst key) (pList key) acc
        pList key       = [(snd key, Maybe.fromMaybe 0 $ Map.lookup key pMap)]
        pMap            = probabilityMap xs