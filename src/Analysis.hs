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

prog3ecbc = sorted
  where
    tA   = parseOvertones "G D F A"
    tB   = parseOvertones "G D F Bb"
    tC   = [0..11]
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
                      (any (?> tA) (choose 5 (snd xs)) || any (?> tB) (choose 5 (snd xs))) && 
                      (any (?> tA) (choose 4 (snd ys)) || any (?> tB) (choose 4 (snd ys))) && 
                      (any (?> tA) (choose 3 (snd zs)) || any (?> tB) (choose 3 (snd zs)))
                    ) &&
                    (
                      ((pitchClass <$> [D,F,A]) ?> pcSet (snd xs ++ snd ys ++ snd zs)) &&
                      ((pitchClass <$> [A,C,E]) ?> pcSet (snd xs ++ snd ys ++ snd zs))
                    ) &&
                    (
                      (any (?> tA) (snd <$> [xs,ys])) &&
                      (any (?> tB) (snd <$> [xs,ys])) 
                      -- (any ([6] ?>) (snd <$> [xs,ys,zs])) &&
                      -- (any ([1] ?>) (snd <$> [xs,ys,zs]))
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


-- copied out from Main.hs

-- main = do
  -- return ()

  -- let analysis = prog3ecbc
  -- let contents = show <$> analysis
  -- let totalTones123 = (\(_,_,_,(x,_),_,_) -> show x) <$> analysis
  -- let omittedTones123 = (\(_,_,_,(_,x),_,_) -> (show x) ++ "\n" ++ (show (flat . pc <$> x))) <$> analysis
  -- let includedTones123 = (\(_,_,_,_,((_,x),_),_) -> (show x) ++ "\n" ++ (show (flat . pc <$> x))) <$> analysis
  -- let intervalVector123 = (\(_,_,_,_,(_,x),_) -> show x) <$> analysis
  -- let pentatonicSet1 = (\(_,(x,_,_),_,_,_,_) -> (show x) ++ "\n" ++ (show (flat . pc <$> x))) <$> analysis
  -- let dissonanceLevel1 = (\((x,_,_),_,_,_,_,_) -> show x) <$> analysis
  -- let tunings1 = (\(_,(x,_,_),_,_,_,_) -> show (if x ?> parseOvertones "G D F A" then "DGDGA" else "n/a", if x ?> parseOvertones "G D F Bb" then "DGDABb" else "n/a")) <$> analysis
  -- let intervalVector1 = (\(_,_,(x,_,_),_,_,_) -> show x) <$> analysis
  -- let dissonanceLevel1 = (\((x,_,_),_,_,_,_,_) -> show x) <$> analysis
  -- let basePentas1 = (\(_,(x,_,_),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (basePenta x)) <$> analysis
  -- let availableTriads1 = (\(_,(x,_,_),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (filter (not . ("Inv" `List.isInfixOf`)) (show <$> (List.sortBy (compare `on` (\(_,_,(x,_),_) -> x)) $ List.zip4 (flatTriad <$> triadSets x) (fmap (flat . pc) <$> triadSets x) (dissonanceLevel <$> triadSets x) (intervalVector <$> triadSets x))))) <$> analysis
  -- let availableChords1 = (\(_,(x,_,_),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (show <$> (List.sortBy (compare `on` (\(_,_,(x,_),_) -> x)) $ List.zip4 (flatChord <$> chordSets x) (fmap (flat . pc) <$> chordSets x) (dissonanceLevel <$> chordSets x) (intervalVector <$> chordSets x)))) <$> analysis
  -- let diatonicModes12 = (\(_,_,_,_,_,(x,_,_)) -> filter (not . (`elem` "()")) $ unlines (show <$> (zip (toMode flat <$> simpleInversions x) ((fmap (flat . pc)) <$> simpleInversions x)))) <$> analysis
  -- let intervalVector12 = (\(_,_,_,_,_,(x,_,_)) -> show $ intervalVector x) <$> analysis
  -- let pentatonicSet2 = (\(_,(_,x,_),_,_,_,_) -> (show x) ++ "\n" ++ (show (flat . pc <$> x))) <$> analysis
  -- let dissonanceLevel2 = (\((_,x,_),_,_,_,_,_) -> show x) <$> analysis
  -- let tunings2 = (\(_,(_,x,_),_,_,_,_) -> show (if x ?> parseOvertones "G D F A" then "DGDGA" else "N/A", if x ?> parseOvertones "G D F Bb" then "DGDABb" else "n/a")) <$> analysis
  -- let intervalVector2 = (\(_,_,(_,x,_),_,_,_) -> show x) <$> analysis
  -- let dissonanceLevel2 = (\((_,x,_),_,_,_,_,_) -> show x) <$> analysis
  -- let basePentas2 = (\(_,(_,x,_),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (basePenta x)) <$> analysis
  -- let availableTriads2 = (\(_,(_,x,_),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (filter (not . ("Inv" `List.isInfixOf`)) (show <$> (List.sortBy (compare `on` (\(_,_,(x,_),_) -> x)) $ List.zip4 (flatTriad <$> triadSets x) (fmap (flat . pc) <$> triadSets x) (dissonanceLevel <$> triadSets x) (intervalVector <$> triadSets x))))) <$> analysis
  -- let availableChords2 = (\(_,(_,x,_),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (show <$> (List.sortBy (compare `on` (\(_,_,(x,_),_) -> x)) $ List.zip4 (flatChord <$> chordSets x) (fmap (flat . pc) <$> chordSets x) (dissonanceLevel <$> chordSets x) (intervalVector <$> chordSets x)))) <$> analysis
  -- let diatonicModes23 = (\(_,_,_,_,_,(_,x,_)) -> filter (not . (`elem` "()")) $ unlines (show <$> (zip (toMode flat <$> simpleInversions x) ((fmap (flat . pc)) <$> simpleInversions x)))) <$> analysis
  -- let intervalVector23 = (\(_,_,_,_,_,(_,x,_)) -> show $ intervalVector x) <$> analysis
  -- let pentatonicSet3 = (\(_,(_,_,x),_,_,_,_) -> (show x) ++ "\n" ++ (show (flat . pc <$> x))) <$> analysis
  -- let dissonanceLevel3 = (\((_,_,x),_,_,_,_,_) -> show x) <$> analysis
  -- let tunings3 = (\(_,(_,_,x),_,_,_,_) -> show (if x ?> parseOvertones "G D F A" then "DGDGA" else "N/A", if x ?> parseOvertones "G D F Bb" then "DGDABb" else "n/a")) <$> analysis
  -- let intervalVector3 = (\(_,_,(_,_,x),_,_,_) -> show x) <$> analysis
  -- let dissonanceLevel3 = (\((_,_,x),_,_,_,_,_) -> show x) <$> analysis
  -- let basePentas3 = (\(_,(_,_,x),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (basePenta x)) <$> analysis
  -- let availableTriads3 = (\(_,(_,_,x),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (filter (not . ("Inv" `List.isInfixOf`)) (show <$> (List.sortBy (compare `on` (\(_,_,(x,_),_) -> x)) $ List.zip4 (flatTriad <$> triadSets x) (fmap (flat . pc) <$> triadSets x) (dissonanceLevel <$> triadSets x) (intervalVector <$> triadSets x))))) <$> analysis
  -- let availableChords3 = (\(_,(_,_,x),_,_,_,_) -> filter (not . (`elem` "()")) $ unlines (show <$> (List.sortBy (compare `on` (\(_,_,(x,_),_) -> x)) $ List.zip4 (flatChord <$> chordSets x) (fmap (flat . pc) <$> chordSets x) (dissonanceLevel <$> chordSets x) (intervalVector <$> chordSets x)))) <$> analysis
  -- let diatonicModes31 = (\(_,_,_,_,_,(_,_,x)) -> filter (not . (`elem` "()")) $ unlines (show <$> (zip (toMode flat <$> simpleInversions x) ((fmap (flat . pc)) <$> simpleInversions x)))) <$> analysis
  -- let intervalVector31 = (\(_,_,_,_,_,(_,_,x)) -> show $ intervalVector x) <$> analysis
  -- writeFile "output/0ecbc_patternsALL.txt" (unlines (show <$> zip [1..] analysis))
  -- let filePath = "/home/oscarsouth/.stack/global-project/output/" 
  -- mapM_ (\(ns,xs) -> writeFile ("output/analysis_" ++ ns ++ ".txt") fullSet3title) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- total tones 1+2+3\n" ++ xs)) (zip (show <$> [1..]) totalTones123)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- omitted tones 1+2+3\n" ++ xs)) (zip (show <$> [1..]) omittedTones123)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- included tones 1+2+3\n" ++ xs)) (zip (show <$> [1..]) includedTones123)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- interval vector 1+2+3\n" ++ xs)) (zip (show <$> [1..]) intervalVector123)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n" ++ pentatonicSet1title)) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- pentatonic set 1\n" ++ xs)) (zip (show <$> [1..]) pentatonicSet1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- dissonance level 1\n" ++ xs)) (zip (show <$> [1..]) dissonanceLevel1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- tunings 1\n" ++ xs)) (zip (show <$> [1..]) tunings1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- interval vector 1\n" ++ xs)) (zip (show <$> [1..]) intervalVector1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- related pentatonic scales (through substitution of one note)\n" ++ xs)) (zip (show <$> [1..]) basePentas1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- available triads (name, pitches, dissonance level, pitchclasses, interval vector\n" ++ xs)) (zip (show <$> [1..]) availableTriads1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- available 4 note chords (name, pitches, dissonance level, pitchclasses, interval vector\n" ++ xs)) (zip (show <$> [1..]) availableChords1)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n" ++ diatonicSet12title)) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- diatonic modes 1+2\n" ++ xs)) (zip (show <$> [1..]) diatonicModes12)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- interval vector 1\n" ++ xs)) (zip (show <$> [1..]) intervalVector12)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n" ++ pentatonicSet2title)) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- pentatonic set 2\n" ++ xs)) (zip (show <$> [1..]) pentatonicSet2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- dissonance level 2\n" ++ xs)) (zip (show <$> [1..]) dissonanceLevel2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- tunings 2\n" ++ xs)) (zip (show <$> [1..]) tunings2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- interval vector 2\n" ++ xs)) (zip (show <$> [1..]) intervalVector2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- related pentatonic scales (through substitution of one note)\n" ++ xs)) (zip (show <$> [1..]) basePentas2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- available triads (name, pitches, dissonance level, pitchclasses, interval vector\n" ++ xs)) (zip (show <$> [1..]) availableTriads2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- available 4 note chords (name, pitches, dissonance level, pitchclasses, interval vector\n" ++ xs)) (zip (show <$> [1..]) availableChords2)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n" ++ diatonicSet23title)) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- diatonic modes 2+3\n" ++ xs)) (zip (show <$> [1..]) diatonicModes23)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- interval vector 2\n" ++ xs)) (zip (show <$> [1..]) intervalVector23)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n" ++ pentatonicSet3title)) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- pentatonic set 3\n" ++ xs)) (zip (show <$> [1..]) pentatonicSet3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- dissonance level 3\n" ++ xs)) (zip (show <$> [1..]) dissonanceLevel3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- tunings 3\n" ++ xs)) (zip (show <$> [1..]) tunings3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- interval vector 3\n" ++ xs)) (zip (show <$> [1..]) intervalVector3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n\n-- related pentatonic scales (through substitution of one note)\n" ++ xs)) (zip (show <$> [1..]) basePentas3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- available triads (name, pitches, dissonance level, pitchclasses, interval vector\n" ++ xs)) (zip (show <$> [1..]) availableTriads3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- available 4 note chords (name, pitches, dissonance level, pitchclasses, interval vector\n" ++ xs)) (zip (show <$> [1..]) availableChords3)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n" ++ diatonicSet31title)) (zip (show <$> [1..]) contents)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- diatonic modes 3+1\n" ++ xs)) (zip (show <$> [1..]) diatonicModes31)
  -- mapM_ (\(ns,xs) -> appendFile ("output/analysis_" ++ ns ++ ".txt") ("\n-- interval vector 3+1\n" ++ xs)) (zip (show <$> [1..]) intervalVector31)
