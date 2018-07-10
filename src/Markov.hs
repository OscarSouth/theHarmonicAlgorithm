module Markov where

import Utility
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe

import Numeric.LinearAlgebra

-- |representation of structure harmonic quality
data Chord = Maj
           | Min
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
type Bigram  = (Cadence,Cadence)

-- |representation of counts for each trigram
type TransitionCounts = M.Map Trigram Double

-- |representation of current state and probabilities of sucessive states
type State = M.Map Cadence Double
type Coord = (Int,Int)
-- type MatrixLoc = M.Map Bigram Coord
type TransitionMap = M.Map Bigram (State,Coord)

-- |representations of the Markov transition and state matrixes
type TransitionMatrix = Matrix R
-- type StateMatrix      = M.Map 

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
tsData' = [ Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Min
          , Cadence Min
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
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
          , Cadence Min
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Maj
          , Cadence Min
          , Cadence Min
          , Cadence Maj
          ]

-- |mapping from list of events into list of trigrams
trigrams :: [Cadence] -> [Trigram]
trigrams (x:xs)
  | length (x:xs) < 3 = []
  | otherwise = trigram (x:xs) : trigrams xs
    where trigram (x:y:z:zs) = (\a b c -> ((a, b), c)) x y z

-- |mapping from input data to counts of all occurring transitions
cadenceCounts   :: [Cadence] -> TransitionCounts
cadenceCounts xs = 
  let mapInsert acc key = M.insertWith (+) key 1 acc
   in foldl mapInsert M.empty $ trigrams xs

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

--------------------------------------------------------------------------------

-- for each member of list (from map)
-- build a list of all transition counts for events with same preceding bigram
-- if sum of list elements is 0, set ``snd`` element's count to 1
-- recurse (fold?) through list

-- -- zeroCases   :: TransitionCounts -> TransitionCounts
-- -- zeroCases [] = []
-- zeroCases (x:xs)
--   -- | sum xs == 0 = set ``snd`` value to 1.0
--   | otherwise = (a,b,c)
--     where 
--           
--           (a,b,c) = (fst $ fst x, snd $ fst x, snd x)
--           -- b = snd x
--           -- b = L.elemIndices x (x:xs)


-- zeroCases mp = 
--   let (x:xs) = M.toList mp
--       (a,b,c) = (fst $ fst x, snd $ fst x, snd x)
--       -- mapInsert acc key = 
--         case key of (a,b)
        
        
--       --   M.insertWith (f) key (v) acc
--    in (a,b)
--   --  foldr mapInsert M.empty ls
--    --foldl mapInsert (zeroCounts xs) $ trigrams xs


-- zeroCases :: [Cadence] -> TransitionMap -> TransitionMap
-- zeroCases ts mp = --foldr mapInsert M.empty ls
--     where (x:xs) = M.toList mp
--           (a,b,c) = (fst $ fst x, snd $ fst x, snd x)
--           ps = unique ts
--           cs a ps = [ (bg,st) | bg <- [a], st <- ps ]
--           rs a ps = [ (M.lookup ks m) | ks <- cs a ps, m <- [mp] ]
--           mapInsert acc key
--             | (sum . catMaybes $ rs a ps) == 0 = --M.insertWith (f) key (v) acc
--             | otherwise = --M.insertWith (f) key (v) acc




-- temp x xs = x `L.elemIndices` xs

-- temp' = M.toList

--------------------------------------------------------------------------------

-- |mapping from input data to fully covered counts of transition events
transitionCounts   :: [Cadence] -> TransitionCounts
transitionCounts xs = 
  let mapInsert acc key = M.insertWith (+) key 1 acc
   in foldl mapInsert (zeroCounts xs) $ trigrams xs

-- |helper function for transitionMatrix which generates probability pairs
stateProbabilities     :: [Cadence] -> [Double] -> [[Double]]
stateProbabilities _ [] = []
stateProbabilities xs ys 
  | sum xx == 0 = xx : stateProbabilities xs recurse
  | otherwise = fmap (/ sum xx) xx : stateProbabilities xs recurse
    where j = S.size $ S.fromList xs
          i = j^2
          xx = take j ys
          recurse = drop j ys

-- |mapping from deterministic list of events into transition matrix
transitionMatrix :: [Cadence] -> TransitionMatrix
transitionMatrix xs = 
  let ps = concat $ stateProbabilities xs $ M.elems $ transitionCounts xs
      j = S.size $ S.fromList xs
      i = j^2
   in (i><j) $ ps :: Matrix R

-- |mapping from deterministic list of events into transition map
-- transitionMap :: [Cadence] -> TransitionMap

-- |map to extract transition probabilities from matrix
-- probabilityLoc :: Bigram -> Coord

-- |mapping from list of events into list of existing preceding bigrams
bigrams :: [Cadence] -> [Bigram]
bigrams (x:xs)
  | length (x:xs) < 3 = []
  | otherwise = bigram (x:xs) : bigrams xs
    where bigram (x:y:z:ys) = (\a b -> (a, b)) x y

-- |mapping from list of events into theoretically possible preceding bigrams
pairs   :: [Cadence] -> [Bigram]
pairs xs = 
  let cs = S.toList $ S.fromList xs
   in [ (a,b) | a <- cs, b <- cs ]

unique   :: [Cadence] -> [Cadence]
unique = S.toList . S.fromList

-- slice m = m ?? (Pos (idxs[0]), Pos (idxs[0,1]))
-- p = m `atIndex` (1,1)
-- stateMatrix = (1><2) [0.5,0.5] :: Matrix R
