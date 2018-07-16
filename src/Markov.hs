module Markov where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe
import Data.Map.Merge.Lazy
import Numeric.LinearAlgebra

-- |representation of harmonic quality
data Chord = Maj
           | Min
           | Dim
           deriving (Show,Eq,Ord)

-- |representation of root motion
data Movement = Unison
              | Second
              | Fourth
              deriving (Show,Eq,Ord)

-- |representation of a harmonic cadence
newtype Cadence = Cadence Chord deriving (Show,Eq,Ord)

-- |representation of bigrams and trigrams containing deterministic cadences
type Trigram = ((Cadence,Cadence),Cadence)

-- |representation of counts for each trigram
type TransitionCounts = M.Map Trigram Double

-- |representations of the Markov transition matrix
type TransitionMatrix = Matrix R

-- |function to reduce data down to ordered list of unique instances
unique   :: [Cadence] -> [Cadence]
unique = S.toList . S.fromList

-- |mapping from list of events into list of trigrams
trigrams :: [Cadence] -> [Trigram]
trigrams (x:xs)
  | length (x:xs) < 3 = []
  | otherwise = trigram (x:xs) : trigrams xs
    where trigram (x:y:z:zs) = (\a b c -> ((a, b), c)) x y z

-- |mapping from input data into all theoretically possible trigrams
threes   :: [Cadence] -> [Trigram]
threes xs = 
  let cs = S.toList $ S.fromList xs
   in [ ((a,b),c) | a <- cs, b <- cs, c <- cs ]

-- |mapping from input data into all possible trigrams with counts of zero
zeroCounts   :: [Cadence] -> TransitionCounts
zeroCounts xs = 
  let mapInsert acc key = M.insert key 0 acc
   in foldl mapInsert M.empty $ threes xs

-- |mapping from input data to counts of all occurring transitions
cadenceCounts   :: [Cadence] -> TransitionCounts
cadenceCounts xs = 
  let mapInsert acc key = M.insertWith (+) key 1 acc
   in foldl mapInsert M.empty $ trigrams xs

-- |mapping from input to counts of cadences, including 'stationary' movements
transitionCounts   :: [Cadence] -> TransitionCounts
transitionCounts ts = mergeMaps (foldl mInsert cadences (M.keys diff)) zeros
  where diff = M.difference zeros cadences
        zeros = zeroCounts ts
        cadences = cadenceCounts ts
        set = unique ts
        mergeMaps m1 m2 = merge preserveMissing preserveMissing (zipWithMatched (\k x y -> x)) m1 m2
        keys k = [ (fst k, nxt) | nxt <- set ]
        newKey k = (fst k, snd $ fst k)
        member k = sequenceA [ f ks | f <- [M.member], ks <- keys k ] (cadences)
        mInsert acc key
          | all (\x -> x == False) (member key) == True = M.insert (newKey key) 1 acc -- if all members return False
          | otherwise = M.insert key (fromMaybe 0 (M.lookup key $ cadences)) acc -- if at least one member returns True

-- |helper function for transitionMatrix which generates probability pairs
transitionProbs     :: [Cadence] -> [Double] -> [[Double]]
transitionProbs _ [] = []
transitionProbs xs ys =
  let j = S.size $ S.fromList xs
      i = j^2
      xx = take j ys
      recurse = drop j ys
   in fmap (/ sum xx) xx : transitionProbs xs recurse

-- |mapping from deterministic list of events into transition matrix
transitionMatrix :: [Cadence] -> TransitionMatrix
transitionMatrix xs = 
  let ps = concat . transitionProbs xs $ M.elems $ transitionCounts xs
      j = S.size $ S.fromList xs
      i = j^2
   in (i><j) $ ps :: Matrix R




























-- |temporary deterministic test datasets
tsData :: [Cadence]
tsData  = [ Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Min
          , Cadence Min
          , Cadence Maj
          ]

tsData' :: [Cadence]
tsData'  = [ Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Dim
          , Cadence Maj
          , Cadence Min
          , Cadence Min
          , Cadence Maj
          , Cadence Dim
          , Cadence Dim
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Min
          , Cadence Min
          , Cadence Maj
          , Cadence Maj
          , Cadence Min
          , Cadence Maj
          , Cadence Min
          , Cadence Min
          , Cadence Dim
          , Cadence Dim
          , Cadence Dim
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Dim
          , Cadence Maj
          ]
