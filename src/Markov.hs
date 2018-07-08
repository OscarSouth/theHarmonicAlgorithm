module Markov where

import Utility
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Numeric.LinearAlgebra

-- |matrix creation function should be (no. of states >< no of bigrams / 2)
m = tr $ (4><2) $ ratioList tsData :: Matrix R
-- n' = m ?? (Pos (idxs[3]), Pos (idxs[3]))
-- p = m `atIndex` (1,1)

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
type CadenceCounts = M.Map Trigram Double

-- |representation of present state
-- newtype PresentState = State Chord deriving (Show,Eq,Ord)

-- |representations of the Markov transition and state matrixes
-- type TransitionMatrix = M.Map 
-- type StateMatrix      = M.Map 

-- |temporary deterministic test dataset
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

-- |mapping from list of events into list of trigrams
trigrams :: [Cadence] -> [Trigram]
trigrams (x:xs)
  | length (x:xs) < 3 = []
  | otherwise = trigram (x:xs) : trigrams xs
    where trigram (x:y:z:zs) = (\a b c -> ((a, b), c)) x y z

-- |### needs to be modified to accomodate for all theoretical transitions
cadenceCounts   :: [Cadence] -> CadenceCounts
cadenceCounts xs = 
  let addOrInc acc k = M.insertWith (+) k 1 acc
   in foldl addOrInc M.empty $ trigrams xs

-- |#### needs to be changed to show correct probabilities
ratioList   :: [Cadence] -> [Double]
ratioList xs = 
  let ns = M.elems $ cadenceCounts xs
      nSum = sum ns
      addRatio x acc = (\x acc -> (x / nSum):acc) x acc
   in foldr addRatio [] ns

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

diff xs = S.difference (S.fromList $ pairs xs) (S.fromList $ bigrams xs)

-- |make transition matrix generate correctly


-- diff xs = S.fromList $ bigrams xs