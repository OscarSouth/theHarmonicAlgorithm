module Generate where

import Utility

-- |Generate list of tuples of containing all possible pitch combinations
overtoneSets  :: Int -> [[Int]]
overtoneSets n = [i:j | i <- [0..11], 
                        j <- map sort $ (choose $ n-1) [0..11], 
                        not $ i `elem` j]

-- |Transform list of overtone sets into zero forms
zeroForms    :: [[Int]] -> [[Int]]
zeroForms xss = map zeroForm xss

-- |Transform overtone set into zero form
zeroForm       :: [Int] -> [Int]
zeroForm (x:xs) = sort $ [(zeroTrans x) i | i <- (x:xs)]

-- |Transpose and sort an overtone set
zeroTrans             :: Int -> Int -> Int
zeroTrans x y | x <= y = y-x
              | x > y  = y+12-x