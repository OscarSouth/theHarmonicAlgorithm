{-# LANGUAGE FlexibleContexts #-}

module MusicData where

import           Utility

import           Data.Function (on)
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
import           GHC.Base      (modInt, quotInt, remInt)
import           GHC.Real      ((%))

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
  let zs = (subtract x) <$> x:xs
   in unique $ pc <$> zs

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
simpleInversions :: [a] -> [[a]]
simpleInversions xs =
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
    pitches = pcSet xs
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
intervalVector         :: (Integral a, Num a) => [a] -> [Integer]
intervalVector xs       = toInteger . vectCounts <$> [1..6]
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
    -- | length c > 3 = show a ++ " " ++ b
    | otherwise    = show a ++ "_" ++ b

-- |mapping from integer list to tuple of root and chord name
toTriad :: (Integral a, Num a) => (PitchClass -> NoteName) -> [a] -> Chord
toTriad f xs@(fund:tones)
  | length triad > 3 = toTriad f $ mostConsonant
    $ possibleTriads (f . pc $ fund) tones
  | any (`elem` [[P 0, P 3, P 7], [P 0, P 2, P 7],[0, 3, 6]]) [primeForm xs] =
    Chord ((fst $ inv, (nameFunc normalForm xs "") ++ (snd $ inv)),
    (`mod` 12) . fromIntegral <$> triad)
  | otherwise =
    Chord ((f . pc $ head xs, nameFunc zeroForm xs ""),
    (`mod` 12) . fromIntegral <$> triad)
  where
    triad = (+fund) <$> (i' . zeroForm $ fund : (List.reverse $ List.sort tones))
    invs = inversions triad
    inv
      | head invs == [P 0, P 4, P 7] || head invs == [P 0, P 3, P 7]
                                      = (f . pc $ triad!!0, "")
      | head invs == [P 0, P 5, P 9] || head invs == [P 0, P 5, P 8]
                                      = (f . pc $ triad!!1, "_2ndInv")
      | head invs == [P 0, P 3, P 8] || head invs == [P 0, P 4, P 9]
                                      = (f . pc $ triad!!2, "_1stInv")
      | head invs == [P 0, P 5, P 7]  = (f . pc $ triad!!0, "")
      | head invs == [P 0, P 5, P 10] = (f . pc $ triad!!1, "_2ndInv")
      | head invs == [P 0, P 2, P 7]  = (f . pc $ triad!!2, "_1stInv")
      | head invs == [P 0, P 3, P 6]  = (f . pc $ triad!!0, "")
      | head invs == [P 0, P 6, P 9]  = (f . pc $ triad!!1, "_2ndInv")
      | head invs == [P 0, P 3, P 9]  = (f . pc $ triad!!2, "_1stInv")
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
data Movement = Asc PitchClass | Desc PitchClass | Unison | Tritone
  deriving (Ord, Eq)

-- |displays musical interval in a human readable way
instance Show Movement where
  show (Asc n)   = "asc " ++ show (i n)
  show (Desc n)  = "desc " ++ show (i n)
  show (Unison)  = "pedal"
  show (Tritone) = "tritone"

-- |mapping from two numeric 'pitchclass' values into a Movement
toMovement        :: (Integral a, Num a) => a -> a -> Movement
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

-- |helper function to extract a Movement value from a Cadence
movementFromCadence :: Cadence -> PitchClass
movementFromCadence (Cadence (_,(mvmt,_))) = fromMovement mvmt

-- |mapping from tupled pair of Chords to a representation of transition between
toTransition :: (Chord, Chord) -> Transition
toTransition ((Chord ((_, prv), from@(x:_))), (Chord ((_, new), to@(y:_)))) =
  Transition ((prv, new), (toMovement x y, i <$> zeroForm to))

-- |representation of a Cadence as a transition to a stucture by an interval
data Cadence = Cadence (Functionality, (Movement, [PitchClass]))
  deriving (Eq, Ord)

-- |customised Show instance for readability
instance Show Cadence where
  show (Cadence (functionality, (dist, ps))) =
    "( " ++ show dist ++ " -> " ++ functionality ++ " )"

-- |mapping from two Chord data structures to a Cadence
toCadence :: (Chord, Chord) -> Cadence
toCadence ((Chord ((_, _), from@(x:_))), (Chord ((_, new), to@(y:_)))) =
  Cadence (new, (toMovement x y, zeroForm to))

-- |mapping from possible Cadence and Pitchclass into next Chord with transposition
fromCadence :: (PitchClass -> NoteName) -> PitchClass -> Cadence -> Chord
fromCadence f root c@(Cadence (_,(_,tones))) =
  (toTriad f) $ i . (+ movementFromCadence c) . (+ root) <$> tones

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
    -- | (any (True==) $ (xs?>) <$> vocabulary) == False = -- fix with recursion
    --                   Chord ((C, "N/A"), [])
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
          ,if all (`elem` zs) [3,10] && notElem 4 zs then ("-"++) else (""++)
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

pentaChords :: (Integral a, Num a) => ([a], [a]) -> [[a]]
pentaChords (up, rs) = 
  let upper = replicate (length rs) up
   in zipWith (:) rs upper

fromChord :: (Integral a, Num a) => Chord -> [a]
fromChord (Chord (_,xs)) = fromIntegral . toInteger <$> xs

tetra0 = ([0,2,4,7], [0,5,10,8,6,11,9])
tetra1 = ([0,2,5,7], [0,10,3,8,11,4,9])
tetra2 = ([0,3,5,8], [0,10,1,6,11,9,2,7])
tetra3 = ([0,3,5,7], [0,10,8,1,11,9,2])

penta0 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords tetra0)
penta1 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords tetra1)
penta2 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords tetra2)
penta3 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords tetra3)

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
      -- ++ harmonicMajor

vocabulary' :: (Integral a, Num a) => [[a]]
vocabulary' = fromChord <$> []
              ++ concat penta0
              ++ concat penta1
              ++ concat penta2
              ++ concat penta3

(<?) :: (Integral a, Num a) => [a] -> [a] -> Bool
(<?) k p
  | k == [] || p == [] = False
  | otherwise          = List.intersect p k == p

(?>) :: (Integral a, Num a) => [a] -> [a] -> Bool
(?>) p k = k <? p

oktet0 = ([0,4,5,7],[0,10,8,1,11,9,2])
oktet1 = ([0,1,3,7],[0,5,10,8,6,4,9])
oktet2 = ([0,2,6,7],[0,10,3,11,4,9])
oktet3 = ([0,4,5,9],[0,10,8,1,11,2,7])
oktet4 = ([0,1,5,6],[0,10,3,8,9])

okpen0 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords oktet0)
okpen1 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords oktet1)
okpen2 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords oktet2)
okpen3 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords oktet3)
okpen4 = (fmap flatChord) <$> (simpleInversions) <$> (pentaChords oktet4)

okinawan :: (Integral a, Num a) => [[a]]
okinawan = fromChord <$> []
           ++ concat okpen0
           ++ concat okpen1
           ++ concat okpen2
           ++ concat okpen3
           ++ concat okpen4

penta :: Num a => Int -> Int -> Int -> [a]
penta tt rt iv
  | tt == 0 = fromIntegral <$> simpleInversions (pentaChords tetra0!!rt)!!iv
  | tt == 1 = fromIntegral <$> simpleInversions (pentaChords tetra1!!rt)!!iv
  | tt == 2 = fromIntegral <$> simpleInversions (pentaChords tetra2!!rt)!!iv
  | tt == 3 = fromIntegral <$> simpleInversions (pentaChords tetra3!!rt)!!iv
  | otherwise = []

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length xs /= length set
  where set = Set.fromList xs 

triadSets tt rt = 
  let ps = penta tt rt 0
      results = overtoneSets 3 ps ps
   in unique $ filter (not . hasDuplicates) results

triadSets' ps = 
  let results = overtoneSets 3 ps ps
   in unique $ filter (not . hasDuplicates) results

consonance = List.sortBy (compare `on` (\x ->  fst . dissonanceLevel $ x))

allPenta' :: [[Integer]]
allPenta' = 
  let one   = simpleInversions <$> pentaChords tetra0
      two   = simpleInversions <$> pentaChords tetra1
      three = simpleInversions <$> pentaChords tetra2
      four  = simpleInversions <$> pentaChords tetra3
   in  (concat (one ++ two ++ three ++ four))

allPenta  :: [[Integer]]
allPenta   =
  let f xs = (sequence ((+) <$> xs)) <$> [0..11]
   in filter (\xs -> length xs >= 5) (unique $ i' . pcSet <$> concat (f <$> allPenta'))

chr               :: Int -> Int -> [(((Integer, [Integer]), [Integer]), [Char],
                                     ((Integer, [Integer]), [Integer]))]
chr n k            = 
  let combinations = ([ (xs, ys) | xs <- unique (i' . normalForm <$> allPenta), ys <- allPenta ]) 
      filt         = filter (\(xs,ys) -> (length (pcSet (xs ++ ys)) == 10-n)) combinations
      sorted       =  List.sortBy (compare `on` (\(xs,ys) -> (fst $ dissonanceLevel xs) + 
                                                             (fst $ dissonanceLevel ys))) filt
   in (\(xs,ys) -> 
     ((dissonanceLevel xs, intervalVector xs), " --> ", 
      (dissonanceLevel ys, intervalVector ys))
     ) <$> (take k $ sorted)

chr' n p k         = 
  let combinations = ([ (xs, ys) | xs <- allPenta, ys <- allPenta ]) 
      filt         = filter (\(xs,ys) -> (length (pcSet (xs ++ ys)) == 10-n) && 
                              ((i' . pcSet $ p) == xs)) combinations
      sorted       = List.sortBy (compare `on` (\(_,ys) -> fst $ dissonanceLevel ys)) filt
   in (\(xs,ys) -> 
     ((dissonanceLevel xs, intervalVector xs), " --> ", 
      (dissonanceLevel ys, intervalVector ys))
     ) <$> (take k $ sorted)

-- NAMING FUNCTION FOR PENTATONICS 

