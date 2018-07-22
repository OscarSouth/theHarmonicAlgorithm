module Overtone where

import Data.List ( sort )

-- |nCr utility function
choose     :: Integer -> [a] -> [[a]]
choose 0 _  = [[]]
choose k [] = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

-- |Generate list containing all possible pitchclass set combinations
overtoneSets  :: Integer -> [[Integer]]
overtoneSets n = [ i:j | i <- [0..11], 
                         j <- map sort $ (choose $ n-1) [0..11], 
                         not $ i `elem` j]

-- -- |Transform list of overtone sets into zero forms
-- zeroForms    :: [[Int]] -> [[Int]]
-- zeroForms xss = map zeroForm xss

-- -- |Transform overtone set into zero form
-- zeroForm       :: [Int] -> [Int]
-- zeroForm (x:xs) = sort $ [(zeroTrans x) i | i <- (x:xs)]

-- -- |Transpose and sort an overtone set
-- zeroTrans             :: Int -> Int -> Int
-- zeroTrans x y | x <= y = y-x
--               | x > y  = y+12-x