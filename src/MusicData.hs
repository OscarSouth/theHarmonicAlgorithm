{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module MusicData where

import           Utility

import           Data.Function   (on)
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import           GHC.Base        (modInt, quotInt, remInt)
import           GHC.Real        ((%))
import           Data.List.Split (splitOn)

import qualified Data.Char     as Char (isAlphaNum)
import qualified Data.List     as List (concat, isInfixOf, reverse, sort,
                                        sortBy, intersect)
import qualified Data.Set      as Set (fromList, toList)

-- |newtype defining PitchClass
newtype PitchClass = P Int deriving (Ord, Eq, Show, Read)

-- |Bounded instance definition for PitchClass
instance Bounded PitchClass where
  minBound = P 0
  maxBound = P 11

-- |Num instance definition for PitchClass, defining PitchClass arithmetic
instance Num PitchClass where
  (+) (P n1) (P n2) = P (n1 + n2) `mod` P 12
  (-) (P n1) (P n2) = P (n1 - n2) `mod` P 12
  (*) (P n1) (P n2) = P (n1 * n2) `mod` P 12
  negate            = id
  fromInteger n     = P $ fromInteger n `mod` 12
  abs               = id
  signum a          = 1

-- |Integral instance definition for PitchClass with more PitchClass arithmetic laws
instance Integral PitchClass where
  toInteger = toInteger . fromEnum
  a `mod` b
    | b == 0                     = error "divide by zero"
    | b == (-1)                  = 0
    | otherwise                  = P $ a' `modInt` b'
      where toInt                = fromIntegral . toInteger
            (a', b')             = (toInt a, toInt b)
  a `quotRem` b
    | b == 0                     = error "divide by zero"
    | b == (-1) && a == minBound = (error "divide by zero", P 0)
    | otherwise                  = (P $ a' `quotInt` b', P $ a' `remInt` b' )
      where toInt                = fromIntegral . toInteger
            (a', b')             = (toInt a, toInt b)

-- |Real instance definition for PitchClass, required for Integral instance
instance Real PitchClass where
  toRational n = toInteger n % 1

-- |Enum instance definition for PitchClass, define 'infinite' PitchClass rotation
instance Enum PitchClass where
  succ n
    | n == maxBound = minBound
    | otherwise     = n + 1
  pred n
    | n == minBound = maxBound
    | otherwise     = n - 1
  toEnum n          = P $ n `mod` 12
  fromEnum n        = case n of {P v -> v}

-- | MusicData class definition
class Ord a => MusicData a where
  pitchClass :: a -> PitchClass -- mappings into pitch classes
  sharp :: a -> NoteName -- mappings into sharp note names
  flat :: a -> NoteName -- mappings into flat note names
  (<+>) :: a -> Integer -> PitchClass -- addition between MusicData & Integer
  (<->) :: a -> Integer -> PitchClass -- subtraction between MusicData & Integer
  i :: Num b => a -> b -- polymorphic mapping from MusicData into numeric types
  (+\) :: a -> Integer -> NoteName -- addition by Integer to flat representations
  (-\) :: a -> Integer -> NoteName -- subtraction by Integer to flat representations
  (+#) :: a -> Integer -> NoteName -- addition by Integer to sharp representations
  (-#) :: a -> Integer -> NoteName -- subtraction by Integer to sharp representations
  i      = fromIntegral . toInteger . pitchClass -- automatic derivation
  (+\) a = flat . (<+>) a --                     |
  (-\) a = flat . (<->) a --                     |
  (+#) a = sharp . (<+>) a --                    |
  (-#) a = sharp . (<->) a --                    v

-- |data type defining set all (standard) enharmonic note names
data NoteName = C
              | C'
              | Db
              | D
              | D'
              | Eb
              | E
              | F
              | F'
              | Gb
              | G
              | G'
              | Ab
              | A
              | A'
              | Bb
              | B deriving (Ord, Eq, Read)

-- |custom Show instance for NoteName
instance Show NoteName where
  show C  = "C"
  show C' = "C#"
  show Db = "Db"
  show D  = "D"
  show D' = "D#"
  show Eb = "Eb"
  show E  = "E"
  show F  = "F"
  show F' = "F#"
  show Gb = "Gb"
  show G  = "G"
  show G' = "G#"
  show Ab = "Ab"
  show A  = "A"
  show A' = "A#"
  show Bb = "Bb"
  show B  = "B"

-- |helper function for reading in NoteName data
readNoteName  :: String -> NoteName
readNoteName s = read $ replace "#" "'" s

-- | MusicData instance for NoteName
instance MusicData NoteName where
  pitchClass n
    | n == C    = 0
    | n == C'   = 1
    | n == Db   = 1
    | n == D    = 2
    | n == D'   = 3
    | n == Eb   = 3
    | n == E    = 4
    | n == F    = 5
    | n == F'   = 6
    | n == Gb   = 6
    | n == G    = 7
    | n == G'   = 8
    | n == Ab   = 8
    | n == A    = 9
    | n == A'   = 10
    | n == Bb   = 10
    | n == B    = 11
  sharp n
    | n == Db   = C'
    | n == Eb   = D'
    | n == Gb   = F'
    | n == Ab   = G'
    | n == Bb   = A'
    | otherwise = n
  flat n
    | n == C'   = Db
    | n == D'   = Eb
    | n == F'   = Gb
    | n == G'   = Ab
    | n == A'   = Bb
    | otherwise = n
  a <+> b       = pitchClass a + fromInteger b
  a <-> b       = pitchClass a + fromInteger b

-- | MusicData instance for PitchClass
instance MusicData PitchClass where
  pitchClass  = id
  sharp n
    | n == 0  = C
    | n == 1  = C'
    | n == 2  = D
    | n == 3  = D'
    | n == 4  = E
    | n == 5  = F
    | n == 6  = F'
    | n == 7  = G
    | n == 8  = G'
    | n == 9  = A
    | n == 10 = A'
    | n == 11 = B
  flat n
    | n == 0  = C
    | n == 1  = Db
    | n == 2  = D
    | n == 3  = Eb
    | n == 4  = E
    | n == 5  = F
    | n == 6  = Gb
    | n == 7  = G
    | n == 8  = Ab
    | n == 9  = A
    | n == 10 = Bb
    | n == 11 = B
  a <+> b     = a + fromInteger b
  a <-> b     = a - fromInteger b

-- working towards addition and subtraction that maintains key

-- |convert any integral into a PitchClass
pc :: (Integral a, Num a) => a -> PitchClass
pc  = fromInteger . fromIntegral

-- |put a list of integers into a PitchClass set (represented as a list)
pcSet   :: (Integral a, Num a) => [a] -> [PitchClass]
pcSet xs = unique $ pc <$> xs

-- |'prime' version for work with MusicData typeclass
pcSet'   :: MusicData a => [a] -> [PitchClass]
pcSet' xs = pcSet $ i <$> xs

-- |transform list of integers into 'zero' form of the PitchClass set
zeroForm       :: (Integral a, Num a) => [a] -> [PitchClass]
zeroForm (x:xs) =
  let ps = (subtract x) <$> x:xs
   in List.sort (unique $ pc <$> ps)

-- |'prime' version for work with MusicData typeclass
zeroForm'   :: MusicData a => [a] -> [PitchClass]
zeroForm' xs = zeroForm $ i <$> xs

-- ** zeroForm CAN GIVE A DIFFERENT RESULT TO zeroForm' $ pcSet AS pcSet MAY REORDER THE SET **

-- |'double prime' version for work purely with integral numbers **DOES NOT TRIM
zeroForm''       :: (Num a, Integral a) => [a] -> [a]
zeroForm'' (x:xs) = List.sort $ [(zeroTrans x) i | i <- (x:xs)]
  where zeroTrans x y | x <= y = y-x
                      | x > y  = y+12-x

-- |minimal inversions function which simply returns all rotations of a list
simpleInversions :: Ord a => [a] -> [[a]]
simpleInversions (p:ps) =
  let xs = p:(List.sort ps)
      l = [0 .. length xs - 1]
      shift n xs = zipWith const (drop n $ cycle xs) xs
      lAppend acc key = acc ++ [shift key xs]
      sortPs (z:zs) = z:(List.sort zs) 
      result = foldl lAppend [] l
   in sortPs <$> result

simpleInversions' :: Ord a => [a] -> [[a]]
simpleInversions' xs = 
  let l = [0 .. length xs - 1] 
      shift n xs = zipWith const (drop n $ cycle xs) xs
      lAppend acc key = acc ++ [shift key xs]
   in foldl lAppend [] l

-- |creates a list of inversions in zero form
inversions :: (Integral a, Num a) => [a] -> [[PitchClass]]
inversions xs = zeroForm <$> simpleInversions xs

-- |'prime' version for work with MusicData typeclass
inversions' :: MusicData a => [a] -> [[PitchClass]]
inversions' xs = zeroForm' <$> simpleInversions xs

-- |mapping from integer pitchclass set to the normal (most compact) form
normalForm   :: (Integral a, Num a) => [a] -> [PitchClass]
normalForm xs 
  | length pitches == 1 = [P 0]
  | length pitches == 2 = fromInteger <$> (head $ lst xs)
  | length pitches == 3 = fromInteger <$> (head $ sfl xs)
  | length pitches == 4 = fromInteger <$> (head $ tfl xs)
  | length pitches == 5 = fromInteger <$> (head $ ffl xs)
  | length pitches == 6 = fromInteger <$> (head $ vfl xs)
  | otherwise           = fromInteger <$> (head $ fin xs)
  where
    pitches = i' . pcSet $ xs
    invs xs = fmap i <$> inversions xs
    lst xs  = filter (\x ->
      last x == 
      (minimum $ last <$> invs xs)) $ invs xs
    sfl xs  = filter (\x ->
      (last . init) x == 
      (minimum $ last . init <$> lst xs)) $ lst xs
    tfl xs  = filter (\x ->
      (last . init . init) x ==
      (minimum $ last . init . init <$> sfl xs)) $ sfl xs
    ffl xs  = filter (\x ->
      (last . init . init . init) x ==
      (minimum $ last . init . init . init <$> tfl xs)) $ tfl xs
    vfl xs  = filter (\x ->
      (last . init . init . init . init) x ==
      (minimum $ last . init . init . init . init <$> ffl xs)) $ ffl xs
    fin xs  = filter (\x ->
      (last . init . init . init . init) x ==
      (minimum $ last . init . init . init . init <$> vfl xs)) $ vfl xs

-- |'prime' version for work with MusicData typeclass
normalForm'   :: MusicData a => [a] -> [PitchClass]
normalForm' xs = normalForm $ i <$> xs

-- |mapping from integer pitchclass set to the prime form
primeForm        :: (Integral a, Num a) => [a] -> [PitchClass]
primeForm xs      = fromInteger <$> is xs
  where
    is xs         = prime $ cmpt xs : (cmpt $ (`subtract` 12) <$> cmpt xs) : []
    prime xs      = head $ List.sortBy (compare `on` sum) xs
    cmpt xs       = i <$> normalForm xs

-- |'prime' version for work with MusicData typeclass
primeForm' :: MusicData a => [a] -> [PitchClass]
primeForm' xs = primeForm $ i <$> xs

-- |quick function to convert MusicData set objects into integer versions
i'   :: (MusicData a, Num b) => [a] -> [b]
i' xs = fromInteger <$> i <$> xs

-- |mapping from list of integers into interval vector
intervalVector''         :: (Integral a, Num a) => [a] -> [Integer]
intervalVector'' xs       = toInteger . vectCounts <$> [1..6]
  where
    diffTriangle []     = []
    diffTriangle (x:xs) = (modCorrect . (subtract x) <$> xs) : diffTriangle xs
    vectCounts          = countElem . List.concat . diffTriangle $ primeForm xs
    modCorrect x
      | x <= 6          = x
      | otherwise       = x - 2*(x-6)

-- |'prime' version for work with MusicData typeclass
intervalVector'   :: MusicData a => [a] -> [Integer]
intervalVector' xs = intervalVector $ i <$> xs

-- |implementation using the prime form NEEDS TO BE CONFIRMED TO WORK WITHOUT
intervalVector         :: (Integral a, Num a) => [a] -> [Integer]
intervalVector xs       = toInteger . vectCounts <$> [1..6]
  where
    diffTriangle []     = []
    diffTriangle (x:xs) = (modCorrect . (subtract x) <$> xs) : diffTriangle xs
    vectCounts          = countElem . List.concat . diffTriangle $ zeroForm xs
    modCorrect x
      | x <= 6          = x
      | otherwise       = x - 2*(x-6)

-- |mapping from sets of fundamentals and overtones into viable composite sets
overtoneSets        :: (Num a, Eq a, Eq b, Ord b) => a -> [b] -> [b] -> [[b]]
overtoneSets n rs ps = [ i:j | i <- rs,
                       j <- List.sort <$> (choose $ n-1) ps,
                       not $ i `elem` j]

-- |mapping from sets of fundamental and overtones into list of viable triads
possibleTriads     :: (Integral a, Num a) => NoteName -> [a] -> [[a]]
possibleTriads r ps =
  let fund = (\x -> [x]) . i $ r
   in overtoneSets 3 fund ps

-- |mapping from sets of fundamentals and overtones into lists of viable triads
possibleTriads'      :: (Integral a, Num a) => [String] -> [[a]] -> [[[a]]]
possibleTriads' rs ps =
  let fund = (\x -> [x]) . i . readNoteName <$> rs
   in zipWith (overtoneSets 3) fund ps

-- |mapping from tuple of fundamental and overtones into list of viable triads
possibleTriads''     :: (Integral a, Num a) => (String, [a]) -> [[a]]
possibleTriads'' (r, ps) =
  let fund = (\x -> [x]) . i . readNoteName $ r
   in overtoneSets 3 fund ps

-- |mapping of integral set to tuple containing degree of dissonance and original input 
dissonanceLevel           :: (Integral a, Num a) => [a] -> (Integer, [a])
dissonanceLevel xs
  | countElem iVect 0 == 5 = (27, xs)
  | elem (7+head xs) xs    = (subtract 1 $ sum $ zipWith (*) dissVect iVect, xs)
  | otherwise              = (sum $ zipWith (*) dissVect iVect, xs)
    where
      iVect                = intervalVector xs
      dissVect             = [16,8,4,2,1,24] -- based on work of Paul Hindemith

-- |mapping from a nested list of integers to the most consonant pitchclass set
mostConsonant         ::  (Integral a, Num a) => [[a]] -> [a]
mostConsonant xs       = triadChoice . sortFst $ dissonanceLevel <$> xs
  where triadChoice xs = (snd . head . sortFst) xs
        sortFst xs     = List.sortBy (compare `on` fst) xs

-- |synonym representation of harmonic functionality as a String
type Functionality = String

-- |type synonym for a 'static' musical pitch structure of tones over a root
data Chord = Chord ((NoteName, Functionality), [Integer]) deriving (Eq, Ord)

-- |hides underlying Chord data and presents in a human readable way
instance Show Chord where
  show (Chord ((a,b),c)) 
    | b == "N/A"   = show "N/A" 
    | otherwise    = show a ++ "_" ++ b

-- |mapping from integer list to tuple of root and chord name
toTriad :: (Integral a, Num a) => (PitchClass -> NoteName) -> [a] -> Chord
toTriad f ps@(fund:tones)
  | length (pcSet xs) > 3 = toTriad f $ mostConsonant $ possibleTriads (f . pc $ fund) tones
  | any (`elem` [[P 0, P 3, P 7], [P 0, P 2, P 7],[P 0, P 3, P 6]]) [primeForm xs] =
    Chord ((fst $ inv, (nameFunc normalForm xs "") ++ (snd $ inv)),
    (`mod` 12) . fromIntegral <$> xs)
  | otherwise = 
   Chord ((f . pc $ head xs, nameFunc zeroForm xs ""),
    (`mod` 12) . fromIntegral <$> xs)
  where
    xs = (+fund) <$> (i' (head ps' : (List.sort $ tail ps'))) where ps' = zeroForm ps
    invs = inversions xs -- (head ps : (List.reverse . List.sort $ tail ps))
    inv
      | head invs == [P 0, P 4, P 7] || head invs == [P 0, P 3, P 7] = (f . pc $ xs!!0, "")
      | head invs == [P 0, P 3, P 8] = (f . pc $ xs!!2, "_1stInv")
      | head invs == [P 0, P 4, P 9] = (f . pc $ xs!!2, "_1stInv")
      | head invs == [P 0, P 5, P 9] = (f . pc $ xs!!1, "_2ndInv")
      | head invs == [P 0, P 5, P 8] = (f . pc $ xs!!1, "_2ndInv")
      | head invs == [P 0, P 5, P 7] = (f . pc $ xs!!0, "")
      | head invs == [P 0, P 5, P 10] = (f . pc $ xs!!1, "_2ndInv")
      | head invs == [P 0, P 2, P 7] = (f . pc $ xs!!2, "_1stInv")
      | head invs == [P 0, P 3, P 6] = (f . pc $ xs!!0, "")
      | head invs == [P 0, P 6, P 9] = (f . pc $ xs!!1, "_2ndInv")
      | head invs == [P 0, P 3, P 9] = (f . pc $ xs!!2, "_1stInv")
      | otherwise = (f . pc $ xs!!0, "*INVERSION_ERROR*")
    nameFunc f xs =
      let
        zs = i <$> f xs
        chain =
          [if (elem 4 zs && all (`notElem` zs) [3,10,11]) && notElem 8 zs
            then ("maj"++) else (""++)
          ,if (elem 3 zs && notElem 4 zs) && notElem 6 zs
            then ("min"++) else (""++)
          ,if elem 9 zs then ("6"++) else (""++)
          ,if elem 10 zs && notElem 5 zs then ("7"++) else (""++)
          ,if elem 11 zs then ("maj7"++) else (""++)
          ,if all (`elem` zs) [7,8] then ("b13"++) else (""++)
          ,if (any (`elem` zs) [2,5] && all (`notElem` zs) [3,4] && elem 7 zs)
            || all (`elem` zs) [5,10] then ("sus4"++) else (""++)
          ,if all (`elem` zs) [2,5] then ("sus2/4"++) else (""++)
          ,if notElem 5 zs && elem 2 zs && all (`notElem` zs) [3,4]
            && notElem 7 zs then ("sus2"++) else (""++)
          ,if notElem 2 zs && elem 5 zs && all (`notElem` zs) [3,4]
            && notElem 7 zs then ("sus4"++) else (""++)
          ,if all (`elem` zs) [2,3] || all (`elem` zs) [2,4]
            then ("add9"++) else (""++)
          ,if all (`elem` zs) [5,3] || all (`elem` zs) [5,4]
            then ("add11"++) else (""++)
          ,if elem 1 zs then ("b9"++) else (""++)
          ,if all (`elem` zs) [3,4] then ("#9"++) else (""++)
          ,if elem 6 zs && notElem 5 zs && any (`elem` zs) [7,8]
            then ("#11"++) else (""++)
          ,if ((elem 6 zs && notElem 7 zs) || (elem 6 zs && notElem 8 zs))
            && notElem 3 zs && all (`notElem` zs) [7,8]
            then ("b5"++) else (""++)
          ,if ((elem 8 zs && notElem 7 zs) || all (`elem` zs) [8,9])
            && notElem 4 zs then ("#5"++) else (""++)
          ,if all (`notElem` zs) [2,3,4,5] then ("no3"++) else (""++)
          ,if all (`notElem` zs) [6,7,8] then ("no5"++) else (""++)
          ,if all (`elem` zs) [3,6] then ("dim"++) else (""++)
          ,if all (`elem` zs) [4,8] then ("aug"++) else (""++)]
       in foldr (.) id chain

-- |shortcut version of toTriad with flat partially applied
flatTriad :: (Integral a, Num a) => [a] -> Chord
flatTriad = toTriad flat

-- |shortcut version of toTriad with sharp partially applied
sharpTriad :: (Integral a, Num a) => [a] -> Chord
sharpTriad = toTriad sharp

-- |mapping from Chord object to a string representation for maximum readability
showTriad :: (PitchClass -> NoteName) -> Chord -> String
showTriad f (Chord ((a,b),c))
    | "sus4" `List.isInfixOf` b && not (any (`List.isInfixOf` b) ["_1stInv", "_2ndInv"]) =
      (show . f $ pitchClass a) ++ " " ++ b
    | all (`List.isInfixOf` b) ["_1stInv", "maj"] =
      (show . f $ pitchClass a) ++ " " ++ (takeWhile Char.isAlphaNum b)
      ++ "/" ++ (show $ f (a <-> 4))
    | all (`List.isInfixOf` b) ["_1stInv", "min"] =
      (show . f $ pitchClass a) ++ " " ++ (takeWhile Char.isAlphaNum b)
      ++ "/" ++ (show $ f (a <-> 3))
    | all (`List.isInfixOf` b) ["_1stInv", "sus4"] =
      (show $ f (a <-> 5)) ++ " sus2"
    | all (`List.isInfixOf` b) ["_1stInv", "dim"] =
      (show . f $ pitchClass a) ++ " " ++ (takeWhile Char.isAlphaNum b) ++
      "/" ++ (show $ f (a <-> 3))
    | "_2ndInv" `List.isInfixOf` b && any (`List.isInfixOf` b) ["maj", "min"] =
      (show . f $ pitchClass a) ++ " " ++
      (takeWhile Char.isAlphaNum b) ++ "/" ++ (show $ f (a <+> 7))
    | "_2ndInv" `List.isInfixOf` b && "sus4" `List.isInfixOf` b =
      (show . f $ pitchClass a) ++ " " ++
      (takeWhile Char.isAlphaNum b) ++ "/" ++ (show $ f (a <+> 7))
    | "_2ndInv" `List.isInfixOf` b && "dim" `List.isInfixOf` b =
      (show . f $ pitchClass a) ++ " " ++
      (takeWhile Char.isAlphaNum b) ++ "/" ++ (show $ f (a <+> 6))
    | otherwise = (show . f $ pitchClass a) ++ " " ++ b

-- |shortcut version of showTriad with sharp partially applied
showFlatTriad :: Chord -> String
showFlatTriad = showTriad flat

-- |shortcut version of showTriad with sharp partially applied
showSharpTriad :: Chord -> String
showSharpTriad = showTriad sharp

-- |representation of a transition from old to new functionality by an interval
data Transition =
  Transition ((Functionality, Functionality), (Movement, [Integer]))
    deriving (Eq, Ord)

-- |show hides underlying Cadence data and presents data in a human readable way
instance Show Transition where
  show (Transition ((prv, new), (dist, ps))) =
    show dist ++ " (" ++ prv ++ " -> " ++ new ++ ")"

-- |concrete representation of movement by a musical interval
data Movement = Asc PitchClass | Desc PitchClass | Unison | Tritone | Empty
  deriving (Ord, Eq)

-- |displays musical interval in a human readable way
instance Show Movement where
  show :: Movement -> String
  show (Asc n)  = "asc " ++ show (i n)
  show (Desc n) = "desc " ++ show (i n)
  show Unison   = "pedal"
  show Tritone  = "tritone"
  show Empty    = "empty"

-- |reads show instance of movement
instance Read Movement where
  readsPrec :: Int -> ReadS Movement
  readsPrec _ s
    | s == "pedal"   = [(Unison, "")]
    | s == "asc 1"   = [(Asc (P 1), "")]
    | s == "asc 2"   = [(Asc (P 2), "")]
    | s == "asc 3"   = [(Asc (P 3), "")]
    | s == "asc 4"   = [(Asc (P 4), "")]
    | s == "asc 5"   = [(Asc (P 5), "")]
    | s == "tritone" = [(Tritone, "")]
    | s == "desc 5"  = [(Desc (P 5), "")]
    | s == "desc 4"  = [(Desc (P 4), "")]
    | s == "desc 3"  = [(Desc (P 3), "")]
    | s == "desc 2"  = [(Desc (P 2), "")]
    | s == "desc 1"  = [(Desc (P 1), "")]
    | otherwise      = [(Empty, "")]

-- |mapping from two numeric 'pitchclass' values into a Movement
toMovement :: (Integral a, Num a) => a -> a -> Movement
toMovement from to
  | x < y = Asc  x
  | y < x = Desc y
  | x == 0 && y == 0 = Unison
  | otherwise   = Tritone
  where
    x = last $ zeroForm [from, to]
    y = last $ zeroForm [to, from]

-- |mapping from Movement data type to PitchClass
fromMovement :: Movement -> PitchClass
fromMovement (Asc n)   = pc n
fromMovement (Desc n)  = 12 - (pc n)
fromMovement (Unison)  = P 0
fromMovement (Tritone) = P 6

-- |mapping from Movement data type to int
fromMovement' :: (Num a, Integral a) => Movement -> a
fromMovement' (Asc n)   = i n
fromMovement' (Desc n)  = 0 - (i n)
fromMovement' (Unison)  = 0
fromMovement' (Tritone) = (-6)

-- |helper function to extract a Movement value from a Cadence
movementFromCadence :: Cadence -> PitchClass
movementFromCadence (Cadence (_,(mvmt,_))) = fromMovement mvmt

-- |helper function to extract a Movement value from a Cadence as an integer
movementFromCadence' :: (Num a, Integral a) => Cadence -> a
movementFromCadence' (Cadence (_,(mvmt,_))) = fromMovement' mvmt

-- |mapping from tupled pair of Chords to a representation of transition between
toTransition :: (Chord, Chord) -> Transition
toTransition ((Chord ((_, prv), from@(x:_))), (Chord ((_, new), to@(y:_)))) =
  Transition ((prv, new), (toMovement x y, i <$> zeroForm to))

-- |representation of a Cadence as a transition to a stucture by an interval
data Cadence = Cadence (Functionality, (Movement, [PitchClass]))
  deriving (Eq, Ord)

-- |customised Show instance for readability
instance Show Cadence where
  show :: Cadence -> String
  show (Cadence (functionality, (dist, ps))) =
    "( " ++ show dist ++ " -> " ++ functionality ++ " )"

-- |mapping from two Chord data structures to a Cadence
toCadence :: (Chord, Chord) -> Cadence
toCadence ((Chord ((_, _), from@(x:_))), (Chord ((_, new), to@(y:_)))) =
  Cadence (new, (toMovement x y, zeroForm to))

type CadenceState = (Cadence, PitchClass)

-- |print CadenceState in a user readable way
showCadenceState :: CadenceState -> (PitchClass -> NoteName) -> String
showCadenceState (c, p) fs = show $ fromCadence fs p c

-- |interaction friendly interface to initialise a CadenceState
initCadenceState :: (Integral a, Num a) => a -> String -> [a] -> CadenceState
initCadenceState movement note quality =
  let approach = toMovement 0 movement
      from     = toTriad flat [0]
      to       = toTriad flat $ (+ fromMovement' approach) <$> zeroForm quality
      root = readNoteName note
   in (toCadence (from, to), pitchClass $ root)

-- |mapping from possible Cadence and Pitchclass into next Chord with transposition
fromCadence :: (PitchClass -> NoteName) -> PitchClass -> Cadence -> Chord
fromCadence f root c@(Cadence (_,(_,tones))) =
  (toTriad f) $ i . (+ movementFromCadence c) . (+ root) <$> tones

-- |mapping from possible Cadence and int into next Chord with transposition
fromCadence' :: (Num a, Integral a) => a -> Cadence -> [a]
fromCadence' root c@(Cadence (_,(_,tones))) =
  (+ movementFromCadence' c) . (+ root) . i <$> tones

-- |mapping from serialised format to Cadence
-- deconstructCadence :: Cadence -> (String, String)
deconstructCadence :: Cadence -> (Movement, [PitchClass])
deconstructCadence (Cadence (_, (m, c))) = (m, c)

-- |mapping from serialised string format to Cadence
constructCadence :: (String, String) -> Cadence
constructCadence (m,c) =
  let functionality = toFunctionality (read c)
      movement = read m
      chord = read c
   in Cadence (functionality, (movement, chord))

-- |mapping from prev Cadence and Pitchclass into current Chord with transposition
transposeCadence :: (PitchClass -> NoteName) -> PitchClass -> Cadence -> Chord
transposeCadence f root (Cadence (_,(_,tones))) =
  (toTriad f) $ i . (+ root) <$> tones

-- |mapping from Chord the root note of that chord
rootNote :: Chord -> PitchClass
rootNote (Chord (_,(x:_))) = pc x

---------------------------
-- #### ADDITIONS #### ----
---------------------------

-- |mapping from integer list to tuple of root and chord name
toChord :: (Integral a, Num a) => (PitchClass -> NoteName) -> [a] -> Chord
toChord f xs@(fund:tones)
    | otherwise     = Chord ((f . pc $ head xs, nameFunc zeroForm xs ""),
                      (`mod` 12) . fromIntegral <$> chord)
  where
    chord           = (+fund) <$> (i' . zeroForm $ fund : (List.reverse $ List.sort tones))
    nameFunc f xs   = --
      let
        zs          = i <$> f xs
        chain       =
          [(""++)
          ,if all (`elem` zs) [0,4,7] && all (`notElem` zs) [1,2,3,5,6,8,9,10,11] 
            then ("maj"++) else (""++)
          ,if elem 3 zs && all (`notElem` zs) [4,10] then ("m"++) else (""++)
          ,if all (`elem` zs) [3,10] && notElem 4 zs then ("m7"++) else (""++)
          ,if elem 9 zs then ("6"++) else (""++)
          ,if elem 10 zs && (notElem 3 zs || all (`elem` zs) [3,4]) then ("7"++) else (""++)
          -- ,if (elem 10 zs && elem 3 zs) then ("7"++) else (""++)
          ,if elem 11 zs then ("maj7"++) else (""++)
          ,if all (`elem` zs) [2,5] && all (`notElem` zs) [3,4] then ("sus2/4"++) else (""++)
          ,if notElem 5 zs && elem 2 zs && all (`notElem` zs) [3,4]
            then ("sus2"++) else (""++)
          ,if notElem 2 zs && elem 5 zs && all (`notElem` zs) [3,4]
            then ("sus4"++) else (""++)
          ,if ((elem 6 zs && notElem 7 zs) && notElem 5 zs) 
            || (elem 5 zs && elem 6 zs) then ("b5"++) else (""++)
          ,if ((elem 8 zs && notElem 7 zs) || all (`elem` zs) [8,9]) then ("#5"++) else (""++)
          ,if all (`elem` zs) [2,3,5] || all (`elem` zs) [2,4,5]
            then ("add9/11"++) else (""++)
          ,if notElem 5 zs && (all (`elem` zs) [2,3] || all (`elem` zs) [2,4])
            then ("add9"++) else (""++)
          ,if notElem 2 zs && (all (`elem` zs) [5,3] || all (`elem` zs) [5,4])
            then ("add11"++) else (""++)
          ,if (elem 6 zs && notElem 5 zs) && (elem 7 zs && notElem 8 zs) then ("#11"++) else (""++)
          ,if elem 1 zs then ("b9"++) else (""++)
          ,if all (`elem` zs) [3,4] then ("#9"++) else (""++)
          ,if all (`elem` zs) [7,8] then ("b13"++) else (""++)
          ,if all (`notElem` zs) [2,3,4,5] then ("no3"++) else (""++)
          ,if all (`notElem` zs) [6,7,8] then ("no5"++) else (""++)
          -- ,if all (`elem` zs) [4,8] && not (any (`elem` zs) [1,3,5,7,9,11])
          --   then ("aug"++) else (""++)
          -- ,if all (`elem` zs) [3,6] then ("dim"++) else (""++)
          ]
       in foldr (.) id chain

-- |shortcut version of toTriad with flat partially applied
flatChord :: (Integral a, Num a) => [a] -> Chord
flatChord = toChord flat

-- |sortcut version of toTriad with sharp partially applied
sharpChord :: (Integral a, Num a) => [a] -> Chord
sharpChord = toChord sharp

fromChord :: (Integral a, Num a) => Chord -> [a]
fromChord (Chord (_,xs)) = fromIntegral . toInteger <$> xs

-- |mapping from integer list to tuple of mode and chord name
toMode :: (Integral a, Num a) => (PitchClass -> NoteName) -> [a] -> Chord
toMode f xs@(fund:tones)
    | otherwise     = Chord ((f . pc $ head xs, nameFunc zeroForm xs ""),
                      (`mod` 12) . fromIntegral <$> chord)
  where
    chord           = (+fund) <$> (i' . zeroForm $ fund : (List.reverse $ List.sort tones))
    nameFunc f xs   = --
      let
        zs          = i <$> f xs
        chain       =
          [(""++)
          -- Major Modes
          ,if all (`elem` zs) [0,2,4,5,7,9,11] && all (`notElem` zs) [1,3,6,8,10] then ("Ionian"++) else (""++)
          ,if all (`elem` zs) [0,2,3,5,7,9,10] && all (`notElem` zs) [1,4,6,8,11] then ("Dorian"++) else (""++)
          ,if all (`elem` zs) [0,1,3,5,7,8,10] && all (`notElem` zs) [2,4,6,9,11] then ("Phrygian"++) else (""++)
          ,if all (`elem` zs) [0,2,4,6,7,9,11] && all (`notElem` zs) [1,3,5,8,10] then ("Lydian"++) else (""++)
          ,if all (`elem` zs) [0,2,4,5,7,9,10] && all (`notElem` zs) [1,3,6,8,11] then ("Mixolydian"++) else (""++)
          ,if all (`elem` zs) [0,2,3,5,7,8,10] && all (`notElem` zs) [1,4,6,9,11] then ("Aeolian"++) else (""++)
          ,if all (`elem` zs) [0,1,3,5,6,8,10] && all (`notElem` zs) [2,4,7,9,11] then ("Locrian"++) else (""++)
          -- Melodic Minor Modes
          ,if all (`elem` zs) [0,2,3,5,7,9,11] && all (`notElem` zs) [1,4,6,8,10] then ("Melodic_Minor"++) else (""++)
          ,if all (`elem` zs) [0,1,3,5,7,9,10] && all (`notElem` zs) [2,4,6,8,11] then ("Dorian_b2"++) else (""++)
          ,if all (`elem` zs) [0,2,4,6,8,9,11] && all (`notElem` zs) [1,3,5,7,10] then ("Lydian_#5"++) else (""++)
          ,if all (`elem` zs) [0,2,4,6,7,9,10] && all (`notElem` zs) [1,3,5,8,11] then ("Lydian_Dominant"++) else (""++)
          ,if all (`elem` zs) [0,2,4,5,7,8,10] && all (`notElem` zs) [1,3,6,9,11] then ("Mixolydian_b6"++) else (""++)
          ,if all (`elem` zs) [0,2,3,5,6,8,10] && all (`notElem` zs) [1,4,7,9,11] then ("Locrian_nat.2"++) else (""++)
          ,if all (`elem` zs) [0,1,3,4,6,8,10] && all (`notElem` zs) [2,5,7,9,11] then ("Altered_Dominant"++) else (""++)
          -- Harmonic Minor Modes
          ,if all (`elem` zs) [0,2,3,5,7,8,11] && all (`notElem` zs) [1,4,6,9,10] then ("Harmonic_Minor"++) else (""++)
          ,if all (`elem` zs) [0,1,3,5,6,9,10] && all (`notElem` zs) [2,4,7,8,11] then ("Locrian_nat.6"++) else (""++)
          ,if all (`elem` zs) [0,2,4,5,8,9,11] && all (`notElem` zs) [1,3,6,7,10] then ("Ionian_#5"++) else (""++)
          ,if all (`elem` zs) [0,2,3,6,7,9,10] && all (`notElem` zs) [1,4,5,8,11] then ("Dorian_#4"++) else (""++)
          ,if all (`elem` zs) [0,1,4,5,7,8,10] && all (`notElem` zs) [2,3,6,9,11] then ("Phrygian_nat.3"++) else (""++)
          ,if all (`elem` zs) [0,3,4,6,7,9,11] && all (`notElem` zs) [1,2,5,8,10] then ("Lydian_#2"++) else (""++)
          ,if all (`elem` zs) [0,1,3,4,6,8,9] && all (`notElem` zs) [2,5,7,10,11] then ("Altered_bb7"++) else (""++)
          -- Harmonic Major Modes
          ,if all (`elem` zs) [0,2,4,5,7,8,11] && all (`notElem` zs) [1,3,6,9,10] then ("Harmonic_Major"++) else (""++)
          ,if all (`elem` zs) [0,2,3,5,6,9,10] && all (`notElem` zs) [1,4,7,8,11] then ("Dorian_b5"++) else (""++)
          ,if all (`elem` zs) [0,1,3,4,7,8,10] && all (`notElem` zs) [2,5,6,9,11] then ("Phrygian_b4"++) else (""++)
          ,if all (`elem` zs) [0,2,3,6,7,9,11] && all (`notElem` zs) [1,4,5,8,10] then ("Lydian_b3"++) else (""++)
          ,if all (`elem` zs) [0,1,4,5,7,9,10] && all (`notElem` zs) [2,3,6,8,11] then ("Mixolydian_b2"++) else (""++)
          ,if all (`elem` zs) [0,3,4,6,8,9,11] && all (`notElem` zs) [1,2,5,7,10] then ("Lydian_Augmented_#2"++) else (""++)
          ,if all (`elem` zs) [0,1,3,5,6,8,9] && all (`notElem` zs) [2,4,7,10,11] then ("Locrian_bb7"++) else (""++)
          ]
       in foldr (.) id chain

basePenta :: Integral a => [a] -> [String]
basePenta pcs  = 
  unique ((\x -> 
  if any ((snd x) `List.isInfixOf`) majorPentaChr
    then ((show (flat $ pc (head (snd x)))) ++ "_major," ++
      show ((flat . pc) <$> ((+(head (snd x))) <$> (i' . zeroForm $ snd x))))
    else if any ((snd x) `List.isInfixOf`) okinaPentaChr
      then ((show (flat $ pc (head (snd x)))) ++ "_okina," ++
        show ((flat . pc) <$> ((+(head (snd x))) <$> (i' . zeroForm $ snd x))))
      else if any ((snd x) `List.isInfixOf`) iwatoPentaChr
        then ((show (flat $ pc (head (snd x)))) ++ "_iwato," ++
          show ((flat . pc) <$> ((+(head (snd x))) <$> (i' . zeroForm $ snd x))))
        else if any ((snd x) `List.isInfixOf`) kumoiPentaChr
          then ((show (flat $ pc (head (snd x)))) ++ "_kumoi," ++
            show ((flat . pc) <$> ((+(head (snd x))) <$> (i' . zeroForm $ snd x))))
          else ("n/a," ++
            show ((flat . pc) <$> ((+(head (snd x))) <$> (i' . zeroForm $ snd x))))
  ) <$> filtered)
  where
    ps = fromIntegral <$> pcs
    filtered = fst <$> filter (\(_,x) -> x==True) results
    results  = [ ((sortPcSet ps, ys), (`isContainedIn` ys) xs) | 
                  xs <- choose 4 (sortPcSet ps), 
                  ys <- majorPentaChr ++ okinaPentaChr ++ iwatoPentaChr ++ kumoiPentaChr ]


isContainedIn :: (Eq a) => [a] -> [a] -> Bool
isContainedIn ps0 ps1 = all (`elem` ps1) ps0
  -- w [ ((sortPcSet ps, ys), (`List.isInfixOf` ys) xs) | xs <- choose 4 (sortPcSet ps), ys <- majorPentaChr ++ okinaPentaChr ++ iwatoPentaChr ]

sortPcSet :: (Num a, Integral a) => [a] -> [a]
sortPcSet pcs = head ps : (List.sort $ tail ps)
  where ps = i . pc <$> pcs

majorPentaChr :: Integral a => [[a]]
majorPentaChr = (\xs -> head xs : (List.sort $ tail xs)) <$> sets
  where sets = i' . pcSet <$> ((sequence ((+) <$> [0,2,4,7,9])) <$> [0..11])

okinaPentaChr :: Integral a => [[a]]
okinaPentaChr = (\xs -> head xs : (List.sort $ tail xs)) <$> sets
  where sets = i' . pcSet <$> ((sequence ((+) <$> [0,4,5,7,11])) <$> [0..11])

iwatoPentaChr :: Integral a => [[a]]
iwatoPentaChr = (\xs -> head xs : (List.sort $ tail xs)) <$> sets
  where sets = i' . pcSet <$> ((sequence ((+) <$> [0,1,5,6,10])) <$> [0..11])

kumoiPentaChr :: Integral a => [[a]]
kumoiPentaChr = (\xs -> head xs : (List.sort $ tail xs)) <$> sets
  where sets = i' . pcSet <$> ((sequence ((+) <$> [0,2,3,7,9])) <$> [0..11])

toFunctionality :: [PitchClass] -> Functionality
toFunctionality ps = (\(x:xs) -> if (length xs > 1)
                                 then (head xs) ++ "_" ++ (last xs)
                                 else (head xs)) $
                                 (splitOn "_") . show $ toTriad flat ps
--toFunctionality ps = last $ (splitOn "_") (show $ toTriad flat ps)
-- wrote this when I was really tired and it can be a lot better

toFunctionality' :: (Integral a, Num a) => [a] -> Functionality
toFunctionality' = toFunctionality . pcSet

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

progRoots' :: (Num a, Integral a) => a -> [Cadence] -> [a]
progRoots' _ [] =  []
progRoots' p (x:xs) = p : progRoots' (p + (movementFromCadence') x) xs

-- |generalised version of toTriad, applied directly to integers
toEnhTriad :: (Integral a, Num a) => [a] -> Chord
toEnhTriad set@(x:xs)
  | (pc x) `elem` (pcSet [0, 5, 10, 3, 8, 1])
    && (not $ "F#" `List.isInfixOf` ss && "Inv" `List.isInfixOf` ss)
    && (not $ "C#" `List.isInfixOf` ss && "Inv" `List.isInfixOf` ss)
        = toTriad flat set
  | (pc x) `elem` (pcSet [7, 2, 9, 4, 11, 6]) && True
    && (not $ "Bb" `List.isInfixOf` sf && "Inv" `List.isInfixOf` sf)
    && (not $ "Eb" `List.isInfixOf` sf && "Inv" `List.isInfixOf` sf)
      = toTriad sharp set
  | otherwise = toTriad flat set
  where
    ss = show $ toTriad sharp set
    sf = show $ toTriad flat set

