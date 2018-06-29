module Utility where

-- |Quicksort utility function
sort       :: Ord a => [a] -> [a]
sort []     = []
sort (p:xs) = (sort lesser) ++ [p] ++ (sort greater)
  where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs

-- |nCr utility function
choose     :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose k [] = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs