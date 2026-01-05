{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Harmonic.Core.Pitch
-- Description : Foundational pitch-class algebra as ℤ₁₂ cyclic group
-- 
-- This module defines 'PitchClass' as a proper cyclic group with modular
-- arithmetic. All pitch operations automatically wrap within the octave.
-- 
-- The design follows the Creative Systems Framework mapping:
--   R (Rules) = The type system itself constrains valid pitch operations
--   T (Traversal) = Transpose operation as group action
--   E (Evaluation) = Interval measurement for dissonance/voice-leading
--
-- Pitch classes are elements of ℤ₁₂, the integers modulo 12:
--   * Addition: transposition
--   * Subtraction: interval measurement
--   * Identity: unison (0)
--   * Inverse: octave complement

module Harmonic.Core.Pitch
  ( -- * Core Types
    PitchClass(..)  -- Export P constructor for pattern matching
  , mkPitchClass
  
    -- * Enharmonic Note Names
  , NoteName(..)
  , pitchClass
  , sharp
  , flat
  , enharmFromNoteName
  
    -- * Transposition (Group Action)
  , transpose
  , interval
  , invert
  
    -- * Utilities
  , allPitchClasses
  , pcSet
  , zeroForm
  ) where

import GHC.Generics (Generic)
import Data.List (nub, sort)
import GHC.Base (modInt, quotInt, remInt)
import GHC.Real ((%))

-------------------------------------------------------------------------------
-- PitchClass as ℤ₁₂
-------------------------------------------------------------------------------

-- |PitchClass represents an element of ℤ₁₂, the cyclic group of integers mod 12.
-- The 'Num' instance ensures all arithmetic automatically wraps to [0..11].
newtype PitchClass = P { unPitchClass :: Int }
  deriving (Ord, Eq, Generic)

-- |Show instance matches legacy format for DB compatibility: "P 0" not "P0"
instance Show PitchClass where
  show (P n) = "P " ++ show n

-- |Read instance handles legacy DB format "P 0" (with space after P)
instance Read PitchClass where
  readsPrec _ ('P':' ':rest) = [(mkPitchClass (read num), remaining)]
    where (num, remaining) = span (`elem` ("0123456789-" :: String)) rest
  readsPrec _ ('P':rest) = [(mkPitchClass (read num), remaining)]
    where (num, remaining) = span (`elem` ("0123456789-" :: String)) rest
  readsPrec _ _ = []

-- |Smart constructor ensuring values stay in [0..11]
mkPitchClass :: Int -> PitchClass
mkPitchClass n = P (n `mod` 12)
{-# INLINE mkPitchClass #-}

-- |Bounded instance: pitch classes range from 0 to 11
instance Bounded PitchClass where
  minBound = P 0
  maxBound = P 11

-- |Num instance implements ℤ₁₂ group operations.
-- All operations automatically wrap modulo 12.
instance Num PitchClass where
  (+) (P n1) (P n2) = P ((n1 + n2) `mod` 12)
  (-) (P n1) (P n2) = P ((n1 - n2) `mod` 12)
  (*) (P n1) (P n2) = P ((n1 * n2) `mod` 12)
  negate (P n)      = P ((12 - n) `mod` 12)
  fromInteger n     = P (fromInteger n `mod` 12)
  abs               = id  -- Always positive in ℤ₁₂
  signum _          = 1   -- All non-zero elements are "positive"

-- |Integral instance for compatibility with legacy code
instance Integral PitchClass where
  toInteger (P n) = toInteger n
  quotRem (P a) (P b)
    | b == 0    = error "PitchClass: divide by zero"
    | otherwise = (P (a `quotInt` b), P (a `remInt` b))
  
instance Real PitchClass where
  toRational (P n) = toInteger n % 1

-- |Enum instance provides cyclic enumeration
instance Enum PitchClass where
  succ (P n)
    | n == 11   = P 0
    | otherwise = P (n + 1)
  pred (P n)
    | n == 0    = P 11
    | otherwise = P (n - 1)
  toEnum n      = P (n `mod` 12)
  fromEnum (P n) = n

-------------------------------------------------------------------------------
-- Enharmonic Note Names (Ported from Legacy MusicData.hs)
-------------------------------------------------------------------------------

-- |All standard enharmonic note names within an octave
data NoteName 
  = C | C' | Db | D | D' | Eb | E | F | F' | Gb
  | G | G' | Ab | A | A' | Bb | B
  deriving (Eq, Ord, Read, Enum, Bounded)

-- |Custom Show instance: renders sharp notation C' as C#, D' as D#, etc.
-- (matches legacy behavior and musical convention)
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

-- |Extract pitch class from a note name
pitchClass :: NoteName -> PitchClass
pitchClass C  = P 0
pitchClass C' = P 1
pitchClass Db = P 1
pitchClass D  = P 2
pitchClass D' = P 3
pitchClass Eb = P 3
pitchClass E  = P 4
pitchClass F  = P 5
pitchClass F' = P 6
pitchClass Gb = P 6
pitchClass G  = P 7
pitchClass G' = P 8
pitchClass Ab = P 8
pitchClass A  = P 9
pitchClass A' = P 10
pitchClass Bb = P 10
pitchClass B  = P 11

-- |Map pitch class to sharp enharmonic spelling
sharp :: PitchClass -> NoteName
sharp (P 0)  = C
sharp (P 1)  = C'
sharp (P 2)  = D
sharp (P 3)  = D'
sharp (P 4)  = E
sharp (P 5)  = F
sharp (P 6)  = F'
sharp (P 7)  = G
sharp (P 8)  = G'
sharp (P 9)  = A
sharp (P 10) = A'
sharp (P 11) = B
sharp _      = error "PitchClass out of range"

-- |Map pitch class to flat enharmonic spelling
flat :: PitchClass -> NoteName
flat (P 0)  = C
flat (P 1)  = Db
flat (P 2)  = D
flat (P 3)  = Eb
flat (P 4)  = E
flat (P 5)  = F
flat (P 6)  = Gb
flat (P 7)  = G
flat (P 8)  = Ab
flat (P 9)  = A
flat (P 10) = Bb
flat (P 11) = B
flat _      = error "PitchClass out of range"

-- |Helper function for mapping NoteName to enharmonic function.
-- This determines whether to use sharp or flat spelling based on 
-- the root note of a chord (for consistent enharmonic spelling).
-- Ported from legacy MusicData.hs
enharmFromNoteName :: NoteName -> (PitchClass -> NoteName)
enharmFromNoteName n = case n of
  C  -> flat
  C' -> sharp
  Db -> flat
  D  -> sharp
  D' -> sharp
  Eb -> flat
  E  -> sharp
  F  -> flat
  F' -> sharp
  Gb -> flat
  G  -> sharp
  G' -> sharp
  Ab -> flat
  A  -> sharp
  A' -> sharp
  Bb -> flat
  B  -> sharp

-------------------------------------------------------------------------------
-- Transposition (Group Actions on ℤ₁₂)
-------------------------------------------------------------------------------

-- |Transpose a pitch class by an interval (group addition)
transpose :: Int -> PitchClass -> PitchClass
transpose n (P x) = P ((x + n) `mod` 12)
{-# INLINE transpose #-}

-- |Calculate the interval between two pitch classes (group subtraction)
-- Returns the ascending interval from first to second.
interval :: PitchClass -> PitchClass -> PitchClass
interval (P from) (P to) = P ((to - from) `mod` 12)
{-# INLINE interval #-}

-- |Invert a pitch class (octave complement)
-- The inverse of n is (12 - n) mod 12
invert :: PitchClass -> PitchClass
invert (P n) = P ((12 - n) `mod` 12)
{-# INLINE invert #-}

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- |All 12 pitch classes
allPitchClasses :: [PitchClass]
allPitchClasses = P <$> [0..11]

-- |Convert a list of integers to a set of unique pitch classes
pcSet :: [Int] -> [PitchClass]
pcSet = sort . nub . map mkPitchClass

-- |Normalize a pitch set to start from zero (transposition-invariant form)
-- Ported from legacy MusicData.hs
zeroForm :: [Int] -> [PitchClass]
zeroForm [] = []
zeroForm xs = 
  let sorted = sort $ map (`mod` 12) xs
      minVal = head sorted
  in map (\x -> mkPitchClass (x - minVal)) sorted
