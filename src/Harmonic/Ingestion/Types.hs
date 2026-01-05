module Harmonic.Ingestion.Types where

-- |Represents a single YCACL slice with its pitch collection and detected fundamental.
data ChordSlice = ChordSlice
  { slicePitches     :: [Int]
  , sliceFundamental :: Int
  } deriving (Show, Eq)
