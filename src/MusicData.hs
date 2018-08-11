module MusicData where

import           Utility

import           Data.Function (on)
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
import           GHC.Base      (modInt, quotInt, remInt)
import           GHC.Real      ((%))

import qualified Data.Char     as Char (isAlphaNum)
import qualified Data.List     as List (concat, isInfixOf, reverse, sort,
                                        sortBy, sort)
import qualified Data.Set      as Set (fromList, toList)

-- |set of pitch classes
newtype PitchClass = P Int deriving (Ord, Eq, Show, Read)

instance Bounded PitchClass where
  minBound = P 0
  maxBound = P 11

instance Num PitchClass where
  (+) (P n1) (P n2) = P (n1 + n2) `mod` P 12
  (-) (P n1) (P n2) = P (n1 - n2) `mod` P 12
  (*) (P n1) (P n2) = P (n1 * n2) `mod` P 12
  negate            = id
  fromInteger n     = P $ fromInteger n `mod` 12
  abs               = id
  signum a          = 1

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

instance Real PitchClass where
  toRational n = toInteger n % 1

instance Enum PitchClass where
  succ n
    | n == maxBound = minBound
    | otherwise     = n + 1
  pred n
    | n == minBound = maxBound
    | otherwise     = n - 1
  toEnum n          = P $ n `mod` 12
  fromEnum n        = case n of {P v -> v}

class Ord a => MusicData a where
  pitchClass :: a -> PitchClass -- mappings into pitch classes
  sharp :: a -> NoteName -- mappings into sharp note names
  flat :: a -> NoteName -- mappings into flat note names
  (<+>) :: a -> Integer -> PitchClass
  (<->) :: a -> Integer -> PitchClass
  i :: Num b => a -> b
  (+|) :: a -> Integer -> NoteName
  (-|) :: a -> Integer -> NoteName
  (+#) :: a -> Integer -> NoteName
  (-#) :: a -> Integer -> NoteName
  i      = fromIntegral . toInteger . pitchClass
  (+|) a = flat . (<+>) a
  (-|) a = flat . (<->) a
  (+#) a = sharp . (<+>) a
  (-#) a = sharp . (<->) a

-- |set of note names
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
pcSet xs = Set.toList . Set.fromList $ pc <$> xs

-- |'prime' version for work with MusicData typeclass
pcSet'   :: MusicData a => [a] -> [PitchClass]
pcSet' xs = pcSet $ i <$> xs

-- |transform list of integers into 'zero' form of
zeroForm       :: (Integral a, Num a) => [a] -> [PitchClass]
zeroForm (x:xs) =
  let zs = (subtract $ x) <$> x:xs
   in Set.toList . Set.fromList $ pc <$> zs

-- |'prime' version for work with MusicData typeclass
zeroForm'   :: MusicData a => [a] -> [PitchClass]
zeroForm' xs = zeroForm $ i <$> xs

-- |'double prime' version for work purely with integral numbers **does not trim
zeroForm''       :: (Num a, Integral a) => [a] -> [a]
zeroForm'' (x:xs) = List.sort $ [(zeroTrans x) i | i <- (x:xs)]
  where zeroTrans x y | x <= y = y-x
                      | x > y  = y+12-x

-- |creates a 'non-deterministic' list of inversions (use !! to select)
-- #### make this more concise & able to operate on pitch sets of any size
inversions        :: (Integral a, Num a) => [a] -> [[PitchClass]]
inversions []      = [[]]
inversions xs
  | length xs == 1 = zeroForm xs : []
inversions xs
  | length xs == 2 =
    let inv        = zeroForm xs
        inv'       = zeroForm $ i <$> last inv : init inv
     in inv : inv' : []
  | length xs == 3 =
    let inv        = zeroForm xs
        inv'       = zeroForm $ i <$> last inv : init inv
        inv''      = zeroForm $ i <$> last inv' : init inv'
     in inv : inv'' : inv' : []
  | length xs == 4 =
    let inv        = zeroForm xs
        inv'       = zeroForm $ i <$> last inv : init inv
        inv''      = zeroForm $ i <$> last inv' : init inv'
        invp''     = zeroForm $ i <$> last inv'' : init inv''
     in inv : invp'' : inv'' : inv' : []
  | otherwise      = [[]] -- not a permanant feature

-- |'prime' version for work with MusicData typeclass
inversions' :: MusicData a => [a] -> [[PitchClass]]
inversions' xs = inversions $ i <$> xs

-- |mapping from integer pitchclass set to the normal (most compact) form
-- #### make generalisable for set of more than 4 pitches
normalForm   :: (Integral a, Num a) => [a] -> [PitchClass]
normalForm xs = -- #### partial function
  let invs xs = fmap i <$> inversions xs
      lst xs  = filter (\x ->
        last x == (minimum $ last <$> invs xs)) $ invs xs
      sfl xs  = filter (\x ->
        (last . init) x == (minimum $ last . init <$> lst xs)) $ lst xs
      tfl xs  = filter (\x ->
        (last . init . init) x ==
        (minimum $ last . init . init <$> lst xs)) $ sfl xs
    in fromInteger <$> (head $ tfl xs)

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

-- |mapping from interval vector to degree of dissonance
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
  show (Chord ((a,b),c)) = show a ++ "_" ++ b

-- |mapping from integer list to tuple of root and chord name
toTriad :: (Integral a, Num a) => (PitchClass -> NoteName) -> [a] -> Chord
toTriad f xs@(fund:tones)
  | length triad > 3 = toTriad f $ mostConsonant
    $ possibleTriads (f . pc $ fund) tones
  | primeForm xs == [P 0, P 3, P 7] =
    Chord ((fst $ inv, (nameFunc normalForm xs "") ++ (snd $ inv)),
    (`mod` 12) . fromIntegral <$> triad)
  | otherwise =
    Chord ((f . pc $ head xs, nameFunc zeroForm xs ""),
    (`mod` 12) . fromIntegral <$> triad)
  where
    triad = (+fund) <$> (i' . zeroForm $ fund
      : (List.reverse $ List.sort tones))
    invs = inversions triad
    inv
      | head invs == [P 0, P 4, P 7] || head invs == [P 0, P 3, P 7]
        = (f . pc $ triad!!0, "")
      | head invs == [P 0, P 5, P 9] || head invs == [P 0, P 5, P 8]
        = (f . pc $ triad!!1, "_2ndInv")
      | head invs == [P 0, P 3, P 8] || head invs == [P 0, P 4, P 9]
        = (f . pc $ triad!!2, "_1stInv")
    nameFunc f xs =
      let
        zs = i <$> f xs
        chain =
          [if (elem 4 zs && all (`notElem` zs) [3,10,11]) && notElem 8 zs
            then ("maj"++) else (""++)
          ,if (elem 3 zs && notElem 4 zs) && notElem 6 zs
            then ("min"++) else (""++)
          ,if elem 9 zs then ("6"++) else (""++)
          ,if elem 10 zs then ("7"++) else (""++)
          ,if elem 11 zs then ("maj7"++) else (""++)
          ,if all (`elem` zs) [7,8] then ("b13"++) else (""++)
          ,if elem 2 zs && all (`notElem` zs) [3,4] then ("sus2"++) else (""++)
          ,if elem 5 zs && all (`notElem` zs) [3,4] then ("sus4"++) else (""++)
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
    | all (`List.isInfixOf` b) ["_1stInv", "maj"] =
      (show . f $ pitchClass a) ++ " " ++ (takeWhile Char.isAlphaNum b)
      ++ "/" ++ (show $ f (a <-> 4))
    | all (`List.isInfixOf` b) ["_1stInv", "min"] =
      (show . f $ pitchClass a) ++ " " ++ (takeWhile Char.isAlphaNum b)
      ++ "/" ++ (show $ f (a <-> 3))
    | "_2ndInv" `List.isInfixOf` b = (show . f $ pitchClass a) ++ " " ++
      (takeWhile Char.isAlphaNum b) ++ "/" ++ (show $ f (a <+> 7))
    | otherwise                    = (show . f $ pitchClass a) ++ " " ++ b

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
    show dist ++ " -> (" ++ prv ++ " -> " ++ new ++ ")"

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
    y   = last $ zeroForm [to, from]

-- |mapping from tupled pair of Chords to a representation of transition between
toTransition :: (Chord, Chord) -> Transition
toTransition ((Chord ((_, prv), from@(x:_))), (Chord ((_, new), to@(y:_)))) =
  Transition ((prv, new), (toMovement x y, i <$> zeroForm to))

data Cadence = Cadence (Functionality, (Movement, [Integer]))
  deriving (Eq, Ord)

instance Show Cadence where
  show (Cadence (functionality, (dist, ps))) =
    "( " ++ show dist ++ " -> " ++ functionality ++ " )"

toCadence :: (Chord, Chord) -> Cadence
toCadence ((Chord ((_, _), from@(x:_))), (Chord ((_, new), to@(y:_)))) =
  Cadence (new, (toMovement x y, i <$> zeroForm to))
