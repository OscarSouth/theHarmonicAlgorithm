module MusicData where

import GHC.Real ( (%) )
import GHC.Base ( quotInt, remInt, modInt )

newtype PitchClass = P Int deriving (Ord, Eq, Show)

instance Bounded PitchClass where
  minBound = P 0
  maxBound = P 11

instance Num PitchClass where
  (+) (P n1) (P n2) = P (n1 + n2) `mod` P 12
  (-) (P n1) (P n2) = P (n1 - n2) `mod` P 12
  (*) (P n1) (P n2) = P (n1 * n2) `mod` P 12
  negate = id
  fromInteger n = P $ fromInteger n
  abs = id
  signum a = 1

instance Integral PitchClass where
  toInteger = toInteger . fromEnum
  a `mod` b   
    | b == 0                     = error "divide by zero"
    | b == (-1)                  = 0
    | otherwise                  = P $ a' `modInt` b'
      where toInt = fromIntegral . toInteger
            (a', b') = (toInt a, toInt b)
  a `quotRem` b
    | b == 0 = error "divide by zero"
    | b == (-1) && a == minBound = (error "divide by zero", P 0)
    | otherwise = (P $ a' `quotInt` b', P $ a' `remInt` b' )
      where toInt = fromIntegral . toInteger
            (a', b') = (toInt a, toInt b)

instance Real PitchClass where
  toRational n = toInteger n % 1

instance Enum PitchClass where
  succ n
    | n == maxBound = minBound
    | otherwise = n + 1
  pred n
    | n == minBound = maxBound
    | otherwise = n - 1
  toEnum n = P $ n `mod` 12
  fromEnum n = case n of {P v -> v}

