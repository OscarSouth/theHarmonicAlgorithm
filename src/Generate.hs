module Generate where

anotherFunc :: String
anotherFunc = "anotherFunc"

-- |Generate list of tuples of containing all possible pitch combinations
overtoneSets  :: Int -> [[Int]]
overtoneSets n = [i:j | i <- [0..11], j <- map qsort $ (choose $ n-1) [0..11], not $ i `elem` j]

-- |Transform list of overtone sets into zero forms
zeroForms    :: [[Int]] -> [[Int]]
zeroForms xss = map zeroForm xss

-- |Transform overtone set into zero form
zeroForm       :: [Int] -> [Int]
zeroForm (x:xs) = qsort $ [(zeroTrans x) i | i <- (x:xs)]

-- |Transpose and sort an overtone set
zeroTrans             :: Int -> Int -> Int
zeroTrans x y | x <= y = y-x
              | x > y  = y+12-x

-- |Quicksort utility function
qsort       :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = (qsort lesser) ++ [p] ++ (qsort greater)
  where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs

-- |nCr utility function
choose     :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose k [] = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs  
