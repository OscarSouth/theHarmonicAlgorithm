module Utility where

import Data.Map (fromListWith, toList)
import           Data.Set (Set)
import qualified Data.Set as Set (fromList, toList)

-- |nCr utility function
choose         :: (Num a, Eq a) => a -> [b] -> [[b]]
choose 0 _      = [[]]
choose k []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

-- |helper function to replace
begins                           :: Eq a => [a] -> [a] -> Maybe [a]
begins string []         = Just string
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _        _        = Nothing

-- |replace occurences in a String
replace               :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to string =
  case begins string from of
      Just remains -> to ++ remains
      Nothing      -> case string of
                        []     -> []
                        x : xs -> x : replace from to xs

-- |function to reduce data down to ordered list of unique instances
unique :: Ord a => [a] -> [a]
unique  = Set.toList . Set.fromList

-- |function to count elements in a list
countElem     :: Eq a => [a] -> a -> Int
countElem xs x = (length . filter (== x)) xs

-- |mapping from a list to association list of counts of elements
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])