module Markov where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe
import Data.Map.Merge.Lazy

import Numeric.LinearAlgebra

------------------- #####|rewrite with lists of three variations

-- |representation of structure harmonic quality
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
-- type Bigram  = (Cadence,Cadence)

-- |representation of counts for each trigram
type TransitionCounts = M.Map Trigram Double

-- |representation of current state and probabilities of sucessive states
-- type State = M.Map Cadence Double
-- type Coord = (Int,Int)
-- type MatrixLoc = M.Map Bigram Coord
-- type TransitionMap = M.Map Bigram (State,Coord)

-- |representations of the Markov transition and state matrixes
type TransitionMatrix = Matrix R
-- type StateMatrix      = M.Map 



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

-- -- zeroCases :: [Cadence] -> ([Cadence] -> TransitionCounts) -> TransitionCounts
-- -- zeroCases ts tf = cs (fst . fst $ ls!!1) ps -- mapInsert rs (ls!!1) -- foldl mapInsert M.empty ls
-- --     where mp = tf ts
-- --           ls = M.toList $ mp
-- --           -- (a,b,c) = (fst $ fst key, snd $ fst key, snd key)
-- --           newKey k = (fst $ fst k, snd $ fst $ fst k)
-- --           newKeyOpp k = (fst $ fst k, snd $ fst $ snd k)
-- --           ps = unique ts
-- --           cs a ps = [ (bg,st) | bg <- [fst a], st <- ps ]
-- --           rs a ps = [ (M.lookup ks m) | ks <- cs a ps, m <- [mp] ]
-- --           -- mapInsert acc key = sequenceA [(M.insert (newKey key) 1),(M.insert (newKeyOpp key) 2)] acc
-- --             -- | (sum . catMaybes $ rs (fst key) ps) == 0 = sequenceA [(M.insert (newKey key) 1),(M.insert (newKeyOpp key) 2)] acc -- M.insert (newKey key) 1 acc 
-- --             -- | otherwise = M.insert (fst key) (fromMaybe 0 (M.lookup (fst key) mp)) acc

-- -- temp = sequenceA (M.fromList [(M.insert "a" 1),(M.insert "b" 2)])

-- test = sequenceA $ M.fromList [(0,(>4)),(1,(<10)),(2,(odd))]
-- test' m = M.fromList m

-- testr = sequenceA [(M.insert "a" 1),(M.insert "b" 2)]
-- testr' = M.empty

-- mergeMap a b = M.union a b

-- ($||$) :: (a -> a -> b) -> [a] -> b
-- f $||$ (x:y:_) = f x y


-- -- ^ get missing entry to go in correctly

-- -- |mapping from input data to fully covered counts of transition events
-- transitionCounts   :: [Cadence] -> TransitionCounts
-- transitionCounts xs = 
--   let mapInsert acc key = M.insertWith (+) key 1 acc
--    in foldl mapInsert (zeroCounts xs) $ trigrams xs

-- |helper function for transitionMatrix which generates probability pairs
transitionProbs     :: [Cadence] -> [Double] -> [[Double]]
transitionProbs _ [] = []
transitionProbs xs ys = fmap (/ sum xx) xx : transitionProbs xs recurse
    where j = S.size $ S.fromList xs
          i = j^2
          xx = take j ys
          recurse = drop j ys


-- |mapping from deterministic list of events into transition matrix
transitionMatrix :: [Cadence] -> TransitionMatrix
transitionMatrix xs = 
  let ps = concat . transitionProbs xs $ M.elems $ transitionCounts xs
      j = S.size $ S.fromList xs
      i = j^2
   in (i><j) $ ps :: Matrix R

-- -- test = stateProbabilities tsData $ M.elems $ zeroCases tsData transitionCounts


-- -- |mapping from deterministic list of events into transition map
-- -- transitionMap :: [Cadence] -> TransitionMap

-- -- |map to extract transition probabilities from matrix
-- -- probabilityLoc :: Bigram -> Coord

-- -- |mapping from list of events into list of existing preceding bigrams
-- bigrams :: [Cadence] -> [Bigram]
-- bigrams (x:xs)
--   | length (x:xs) < 3 = []
--   | otherwise = bigram (x:xs) : bigrams xs
--     where bigram (x:y:z:ys) = (\a b -> (a, b)) x y

-- -- |mapping from list of events into theoretically possible preceding bigrams
-- pairs   :: [Cadence] -> [Bigram]
-- pairs xs = 
--   let cs = S.toList $ S.fromList xs
--    in [ (a,b) | a <- cs, b <- cs ]



-- -- slice m = m ?? (Pos (idxs[0]), Pos (idxs[0,1]))
-- -- p = m `atIndex` (1,1)
-- -- stateMatrix = (1><2) [0.5,0.5] :: Matrix R






















































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