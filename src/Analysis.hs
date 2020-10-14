module Analysis where

import           Utility
import           MusicData
import           Overtone

import           Data.Function (on)
import qualified Data.List     as List (sortBy, sort, intersect, isSubsequenceOf, isInfixOf, concat)
import qualified Data.Set      as Set (fromList)

-- VOCABULARY FUNCTIONS

pentaChords :: (Integral a, Num a) => ([a], [a]) -> [[a]]
pentaChords (up, rs) = 
  let upper = replicate (length rs) up
   in zipWith (:) rs upper

tetra0 = ([0,2,4,7], [0,5,10,8,6,11,9])
tetra1 = ([0,2,5,7], [0,10,3,8,11,4,9])
tetra2 = ([0,3,5,8], [0,10,1,6,11,9,2,7])
tetra3 = ([0,3,5,7], [0,10,8,1,11,9,2])

penta0 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords tetra0)
penta1 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords tetra1)
penta2 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords tetra2)
penta3 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords tetra3)

vocabulary :: (Integral a, Num a) => [[a]]
vocabulary =
  let major         = i' <$> inversions [0,2,4,5,7,9,11]
      melodicMinor  = i' <$> inversions [0,2,3,5,7,9,11]
      harmonicMinor = i' <$> inversions [0,2,3,5,7,8,11]
      harmonicMajor = i' <$> inversions [0,2,4,5,7,8,11]
   in []
      ++ major 
      ++ melodicMinor 
      ++ harmonicMinor
      ++ harmonicMajor

vocabulary' :: (Integral a, Num a) => [[a]]
vocabulary' = fromChord <$> []
              ++ concat penta0
              ++ concat penta1
              ++ concat penta2
              ++ concat penta3

oktet0 = ([0,4,5,7],[0,10,8,1,11,9,2])
oktet1 = ([0,1,3,7],[0,5,10,8,6,4,9])
oktet2 = ([0,2,6,7],[0,10,3,11,4,9])
oktet3 = ([0,4,5,9],[0,10,8,1,11,2,7])
oktet4 = ([0,1,5,6],[0,10,3,8,9])

okpen0 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords oktet0)
okpen1 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords oktet1)
okpen2 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords oktet2)
okpen3 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords oktet3)
okpen4 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords oktet4)

okinawan :: (Integral a, Num a) => [[a]]
okinawan = fromChord <$> []
           ++ concat okpen0
           ++ concat okpen1
           ++ concat okpen2
           ++ concat okpen3
           ++ concat okpen4

iwtet0 = ([0,1,5,6],[0,3,8,9,10])
iwtet1 = ([0,4,5,9],[0,1,2,7,8,10,11])
iwtet2 = ([0,1,5,7],[0,3,4,8,9,10])
iwtet3 = ([0,4,6,7],[0,1,2,3,9,10,11])
iwtet4 = ([0,2,3,7],[0,5,6,8,9,10,11])

iwpen0 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet0)
iwpen1 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet1)
iwpen2 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet2)
iwpen3 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet3)
iwpen4 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet4)

iwato :: (Integral a, Num a) => [[a]]
iwato = fromChord <$> []
           ++ concat iwpen0
           ++ concat iwpen1
           ++ concat iwpen2
           ++ concat iwpen3
           ++ concat iwpen4

kutet0 = ([0,2,3,7],[0,5,6,8,9,10,11])
kutet1 = ([0,1,5,7],[0,3,4,8,9,10])
kutet2 = ([0,4,6,9],[0,1,2,3,7,8,10,11])
kutet3 = ([0,2,5,7],[0,3,4,8,9,10,11])
kutet4 = ([0,3,5,6],[0,1,2,8,9,10,11])

kupen0 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet0)
kupen1 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet1)
kupen2 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet2)
kupen3 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet3)
kupen4 = (fmap flatChord) <$> (simpleInversions') <$> (pentaChords iwtet4)

kumoi :: (Integral a, Num a) => [[a]]
kumoi = fromChord <$> []
           ++ concat kupen0
           ++ concat kupen1
           ++ concat kupen2
           ++ concat kupen3
           ++ concat kupen4

penta :: Num a => Int -> Int -> Int -> [a]
penta tt rt iv
  | tt == 0 = fromIntegral <$> simpleInversions' (pentaChords tetra0!!rt)!!iv
  | tt == 1 = fromIntegral <$> simpleInversions' (pentaChords tetra1!!rt)!!iv
  | tt == 2 = fromIntegral <$> simpleInversions' (pentaChords tetra2!!rt)!!iv
  | tt == 3 = fromIntegral <$> simpleInversions' (pentaChords tetra3!!rt)!!iv
  | otherwise = []

allPenta' :: [[Integer]]
allPenta' = 
  let one   = simpleInversions' <$> pentaChords tetra0
      two   = simpleInversions' <$> pentaChords tetra1
      three = simpleInversions' <$> pentaChords tetra2
      four  = simpleInversions' <$> pentaChords tetra3
   in  (concat (one ++ two ++ three ++ four))

allOkina' :: [[Integer]]
allOkina' = 
  let one   = simpleInversions' <$> pentaChords oktet0
      two   = simpleInversions' <$> pentaChords oktet1
      three = simpleInversions' <$> pentaChords oktet2
      four  = simpleInversions' <$> pentaChords oktet3
      five  = simpleInversions' <$> pentaChords oktet4
   in  (concat (one ++ two ++ three ++ four ++ five))

allIwato' :: [[Integer]]
allIwato' = 
  let one   = simpleInversions' <$> pentaChords iwtet0
      two   = simpleInversions' <$> pentaChords iwtet1
      three = simpleInversions' <$> pentaChords iwtet2
      four  = simpleInversions' <$> pentaChords iwtet3
      five  = simpleInversions' <$> pentaChords iwtet4
   in  (concat (one ++ two ++ three ++ four ++ five))

allKumoi' :: [[Integer]]
allKumoi' = 
  let one   = simpleInversions' <$> pentaChords kutet0
      two   = simpleInversions' <$> pentaChords kutet1
      three = simpleInversions' <$> pentaChords kutet2
      four  = simpleInversions' <$> pentaChords kutet3
      five  = simpleInversions' <$> pentaChords kutet4
   in  (concat (one ++ two ++ three ++ four ++ five))

allPenta  :: (Num a, Integral a) => [[a]]
allPenta   =
  let f xs = (sequence ((+) <$> xs)) <$> [0..11]
   in filter (\xs -> length xs >= 5) (unique $ i' . pcSet <$> concat (f <$> allPenta'))

allOkina  :: (Num a, Integral a) => [[a]]
allOkina   =
  let f xs = (sequence ((+) <$> xs)) <$> [0..11]
   in filter (\xs -> length xs >= 5) (unique $ i' . pcSet <$> concat (f <$> allOkina'))

allIwato  :: (Num a, Integral a) => [[a]]
allIwato   =
  let f xs = (sequence ((+) <$> xs)) <$> [0..11]
   in filter (\xs -> length xs >= 5) (unique $ i' . pcSet <$> concat (f <$> allIwato'))

allKumoi  :: (Num a, Integral a) => [[a]]
allKumoi   =
  let f xs = (sequence ((+) <$> xs)) <$> [0..11]
   in filter (\xs -> length xs >= 5) (unique $ i' . pcSet <$> concat (f <$> allKumoi'))

pVocab  :: (Num a, Integral a) => [[a]]
pVocab = allPenta ++ allOkina ++ allIwato ++ allKumoi

(<?) :: (Integral a, Num a) => [a] -> [a] -> Bool
(<?) k p
  | k == [] || p == [] = False
  | otherwise          = List.intersect p k == p

(?>) :: (Integral a, Num a) => [a] -> [a] -> Bool
(?>) p k = k <? p

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length xs /= length set
  where set = Set.fromList xs 

triadSets ps = 
  let results = overtoneSets 3 ps ps
   in unique $ filter (not . hasDuplicates) results

chordSets ps = 
  let results = overtoneSets 4 ps ps
   in unique $ filter (not . hasDuplicates) results

triadSets' tt rt = 
  let ps = penta tt rt 0
      results = overtoneSets 3 ps ps
   in unique $ filter (not . hasDuplicates) results

consonance = List.sortBy (compare `on` (\x ->  fst . dissonanceLevel $ x))

chr               :: Int -> Int -> [(((Integer, [Integer]), [Integer]), [Char],
                                     ((Integer, [Integer]), [Integer]))]
chr n k            = 
  let combinations = ([ (xs, ys) | xs <- unique (i' . normalForm <$> allPenta), ys <- allPenta ]) 
      filt         = filter (\(xs,ys) -> (length (pcSet (xs ++ ys)) == n)) combinations
      sorted       =  List.sortBy (compare `on` (\(xs,ys) -> (fst $ dissonanceLevel xs) + 
                                                             (fst $ dissonanceLevel ys))) filt
   in (\(xs,ys) -> 
     ((dissonanceLevel xs, intervalVector xs), " --> ", 
      (dissonanceLevel ys, intervalVector ys))
     ) <$> (take k $ sorted)

chr'              :: Int -> [Integer] -> Int -> [(((Integer, [Integer]), [Integer]), [Char],
                                           ((Integer, [Integer]), [Integer]))]
chr' n p k         = 
  let combinations = [ (xs, ys) | xs <- allPenta, ys <- allPenta,
                       (length (pcSet (xs ++ ys)) == n) && ((i' . pcSet $ p) == xs)]
      sorted       = List.sortBy (compare `on` (\(_,ys) -> fst $ dissonanceLevel ys)) combinations
   in (\(xs,ys) -> 
     ((dissonanceLevel xs, intervalVector xs), " --> ", 
      (dissonanceLevel ys, intervalVector ys))
     ) <$> (take k $ sorted)

-- FUNCTION WHICH RETURNS LIST OF 4 CYCLIC PENTATONICS OVER A PEDAL TONE
-- -- CONTAINING A TOTAL OF n SIMILAR TONES PER TRANSITION
-- -- CONTAINING A TOTAL OF n TONES OVERALL

prog3 n1 n2 k = take k $ sorted
  where
    vocab     = unique (i' . zeroForm <$> allPenta)
    voca2     = allPenta
    tsns      = [ (dissonanceLevel xs,
                  dissonanceLevel ys,
                  dissonanceLevel zs) | 
                  xs <- vocab, ys <- voca2, zs <- voca2, 
                  ((length (pcSet (xs ++ ys)) == n1) &&
                    (length (pcSet (xs ++ zs)) == n1) &&
                    (length (pcSet (ys ++ zs)) == n1)) &&
                  (length (pcSet (xs ++ ys ++ zs)) == n2) &&
                  ((xs /= ys) && (xs /= zs) && (ys /= zs)) &&
                  ((elem 0 xs) && (elem 0 ys) && (elem 0 zs))
                  ]
    sorted    = show <$> List.sortBy (compare `on` (\(dx,dy,dz) -> (fst dx) + 
                                                                   (fst dy) +
                                                                   (fst dz))) tsns 

pentaPatterns xs = 
  let patterns   = filter (xs ?>) (unique (i' . zeroForm <$> allPenta ++ allOkina ++ allIwato ++ allKumoi))
      sorted     = List.sort <$> (List.sortBy (compare `on` (\xs -> dissonanceLevel xs)) patterns)
   in zip (dissonanceLevel <$> sorted) (intervalVector <$> sorted) 

pentaHarmonies xs  = 
  let toPrint = snd . fst <$> pentaPatterns xs
   in zip (triadSets (head toPrint)) (flatTriad <$> triadSets (head toPrint))

-- two different pitches per transition
-- each pair fits into different diatonic scale
-- three different diatonic scales

type Analysis = [((Integer, Integer, Integer),
                 ([Integer], [Integer], [Integer]),
                 ([Integer], [Integer], [Integer]), (Int, [Integer]),
                 ((Integer, [Integer]), [Integer]),
                 ([Integer], [Integer], [Integer]))]

-- prog3ecbc t1 t2 t3 t4 t5 = sorted
prog3ecbc = sorted
  where
    tA   = parseOvertones "G D F A"
    tB   = parseOvertones "G D F Bb"
    vocab     = unique $
                  (List.sort <$> allPenta ++ allOkina ++ allIwato ++ allKumoi)
                  -- (List.sort <$> allPenta ++ allKumoi)
                  -- (List.sort <$> allPentaChr)
    dsls      = dissonanceLevel <$> (List.sort <$> vocab) 
    tsns      =  unique [ ((fst xs, fst ys, fst zs), (snd xs, snd ys, snd zs),
                    (intervalVector $ snd xs, intervalVector $ snd ys, intervalVector $ snd zs),
                    (length $ pcSet (snd xs ++ snd ys ++ snd zs),
                      [ x | x <- [0..11], x `notElem` (i <$> pcSet (snd xs ++ snd ys ++ snd zs)) ] ),
                    (dissonanceLevel (List.sort (i <$> pcSet (snd xs ++ snd ys ++ snd zs))),
                      intervalVector (i <$> pcSet (snd xs ++ snd ys ++ snd zs))),
                      (i' $ pcSet (List.sort $ snd xs ++ snd ys),
                        i' $ pcSet (List.sort $ snd ys ++ snd zs),
                        i' $ pcSet (List.sort $ snd zs ++ snd xs))
                  ) | 
                  xs <- dsls, ys <- dsls, zs <- dsls, 
                  (
                    (
                      (xs /= ys) && (xs /= zs) && (ys /= zs)
                    ) &&
                    (
                        (fst xs <= fst ys) && (fst ys <= fst zs)
                    ) &&
                      ((((snd xs ++ snd ys) /= (snd ys ++ snd zs)) && 
                          ((snd xs ++ snd ys) /= (snd zs ++ snd ys))) && 
                      (((snd ys ++ snd zs) /= (snd zs ++ snd xs)) && 
                          ((snd ys ++ snd zs) /= (snd xs ++ snd zs))) && 
                      (((snd zs ++ snd xs) /= (snd xs ++ snd ys)) && 
                          ((snd zs ++ snd xs) /= (snd ys ++ snd xs)))
                    ) &&
                    (
                      (pcSet (List.sort $ snd xs ++ snd ys) `elem` vocabulary) &&
                      (pcSet (List.sort $ snd ys ++ snd zs) `elem` vocabulary) &&
                      (pcSet (List.sort $ snd zs ++ snd xs) `elem` vocabulary)
                    ) && 
                    (
                      (((snd xs ++ snd ys) /= (snd xs ++ snd zs)) &&
                        ((snd xs ++ snd ys) /= (snd ys ++ snd zs))) &&
                      (((snd ys ++ snd zs) /= (snd ys ++ snd xs)) &&
                        ((snd ys ++ snd zs) /= (snd zs ++ snd xs))) &&
                      (((snd zs ++ snd xs) /= (snd zs ++ snd ys)) &&
                        ((snd zs ++ snd xs) /= (snd xs ++ snd ys)))
                    ) &&
                    (
                      (pcSet (List.sort $ snd xs ++ snd ys) /= pcSet (List.sort $ snd ys ++ snd zs)) && 
                      (pcSet (List.sort $ snd ys ++ snd zs) /= pcSet (List.sort $ snd zs ++ snd xs)) && 
                      (pcSet (List.sort $ snd zs ++ snd xs) /= pcSet (List.sort $ snd xs ++ snd ys))
                    ) &&
                    (
                      ((snd xs ?> tA) || (snd xs ?> tB)) && 
                      ((snd ys ?> tA) || (snd ys ?> tB)) && 
                      ((snd zs ?> tA) || (snd zs ?> tB))
                    )
                  )
                  ]
    sets =  
      ((\(_,(xs,ys,zs),_,_,_,_) -> xs ++ ys) <$> tsns) ++
      ((\(_,(xs,ys,zs),_,_,_,_) -> ys ++ zs) <$> tsns)
    rmDups analysis = 
        (\(_,(xs,ys,zs),_,_,_,_) -> ((ys ++ xs) `notElem` sets) && ((zs ++ ys) `notElem` sets)) analysis
    filtered  = filter rmDups tsns
    sorted    = List.sortBy (compare `on` (\((x,y,z),_,_,_,_,_) -> x+y+z)) filtered 

-- sets+dissonanceLevels, intervalVectors, tone count+omitted

-- mapM_ (putStrLn . show) $ filter (\(x,_,_) -> (fst (dissonanceLevel x)) <= 9) (zip3 (triadSets [0,3,7,8,10]) (fmap (flat . pc) <$> triadSets [0,3,7,8,10]) (flatTriad <$> triadSets [0,3,7,8,10]))
-- mapM_ (putStrLn . show) (zip4 (flatTriad <$> triadSets [0,3,7,8,10]) (dissonanceLevel <$> triadSets [0,3,7,8,10]) (fmap (flat . pc) <$> triadSets [0,3,7,8,10]) (intervalVector <$> triadSets [0,3,7,8,10]))

-- analyse = 

fullSet3title :: String
fullSet3title =
  "\
  \------------------\n\
  \---- FULL SET ----\n\
  \------------------\n\
  \"

pentatonicSet1title :: String
pentatonicSet1title =
  "\
  \------------------\n\
  \-- PENTATONIC 1 --\n\
  \------------------\n\
  \"

pentatonicSet2title :: String
pentatonicSet2title =
  "\
  \------------------\n\
  \-- PENTATONIC 2 --\n\
  \------------------\n\
  \"
  
pentatonicSet3title :: String
pentatonicSet3title =
  "\
  \------------------\n\
  \-- PENTATONIC 3 --\n\
  \------------------\n\
  \"
  
diatonicSet12title :: String
diatonicSet12title =
  "\
  \------------------\n\
  \-- DIATONIC 1+2 --\n\
  \------------------\n\
  \"
  
diatonicSet23title :: String
diatonicSet23title =
  "\
  \------------------\n\
  \-- DIATONIC 2+3 --\n\
  \------------------\n\
  \"
  
diatonicSet31title :: String
diatonicSet31title =
  "\
  \------------------\n\
  \-- DIATONIC 3+1 --\n\
  \------------------\n\
  \"

generateScale (ps1,ps2,ps3) 
  | (length pitches == 7) = pitches
  | otherwise = []
  where 
    tonic       = ps1
    dominant    = i' ((7+) <$> pcSet ps2)
    subdominant = i' ((5+) <$> pcSet ps3)
    pitches     = List.sort $ (i' . pcSet) (tonic ++ dominant ++ subdominant)

-- WHY IS THIS TAKING SO LONG TO COMPUTE? OPTIMISE NORMAL FORM FUNCTION
generateScales = normalised
  where
    sets = [(tonic,dominant,subdominant) | 
              tonic       <- [[0,4,7],[0,3,7]], 
              dominant    <- [[0,4,7],[0,3,7]],
              subdominant <- [[0,4,7],[0,3,7]]]
    filtered = filter (\xs -> (length xs /= 0)) (generateScale <$> sets)
    normalised = i' <$> unique (normalForm <$> filtered)

vocab'' = unique $ i' <$> concat (inversions <$> generateScales)
pVocab'' = unique (i' <$> concat (inversions <$> (i' . zeroForm <$> choose 5 [1..11])))

chrCluster xs' = not (chrClusters xs') -- filter chrClusters xs
  where
    chrClusters xs = any (`List.isSubsequenceOf` xs) ([0,10,11] : [0,1,11] : (sequence ((+) <$> [0,1,2]) <$> [0..9]))

allModes = unique $ filter chrCluster vocab''
allPenta'' chr = List.sort (filter (\xs -> length (filter (\x -> x == 1) (last <$> (i' . zeroForm <$> (choose 2 xs)))) == chr) (unique $ filter chrCluster (i' . normalForm <$> pVocab'')))

allPentaChr :: (Num a, Integral a) => [[a]]
allPentaChr =
  let f xs  = (sequence ((+) <$> xs)) <$> [0..11]
   in filter ((\xs -> (length xs >= 5))) (unique $ i' . pcSet <$> concat (f <$> (allPenta'' 0 ++ allPenta'' 1)))

-- chrCluster' xs' = not (chrClusters xs') -- filter chrClusters xs
--   where
--     chrClusters xs = any (`List.isSubsequenceOf` xs) ([0,10,11] : [0,1,11] : (sequence ((+) <$> [0,1,2]) <$> [0..9]))
--     intervals xs   = 

-- toPenta :: Integral a => [a] -> [String]
-- toPenta pcs  = 
--   unique ((\x -> 
--   if any (x `List.isInfixOf`) allPenta'
--     then ((show (flat $ pc (head x))) ++ "_major," ++
--       show ((flat . pc) <$> ((+(head x)) <$> (i' . zeroForm $ x))))
--     else if any (x `List.isInfixOf`) allOkina'
--       then ((show (flat $ pc (head x))) ++ "_okina," ++
--         show ((flat . pc) <$> ((+(head x)) <$> (i' . zeroForm $ x))))
--       else if any (x `List.isInfixOf`) allIwato'
--         then ((show (flat $ pc (head x))) ++ "_iwato," ++
--           show ((flat . pc) <$> ((+(head x)) <$> (i' . zeroForm $ x))))
--         else if any (x `List.isInfixOf`) allKumoi'
--           then ((show (flat $ pc (head x))) ++ "_kumoi," ++
--             show ((flat . pc) <$> ((+(head x)) <$> (i' . zeroForm $ x))))
--           else ("n/a," ++
--             show ((flat . pc) <$> ((+(head x)) <$> (i' . zeroForm $ x))))
--   ) <$> pcs : [])
