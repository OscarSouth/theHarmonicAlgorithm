#!/usr/bin/env stack
{- stack script --resolver lts-22.28 --package theHarmonicAlgorithm -}

{-# LANGUAGE OverloadedStrings #-}

import Harmonic.Lib
import qualified Harmonic.Core.Pitch as P
import qualified Harmonic.Core.Harmony as H

main :: IO ()
main = do
  putStrLn "Testing D major key filter (bug report scenario)..."
  putStrLn "================================================\n"

  -- Create the exact test case from the bug report
  let start = initCadenceState 5 "E" [0,3,7] FlatSpelling
  putStrLn $ "Starting state: E minor"

  -- Create D major context (2# = D major)
  let ctx = hContext "*" "2#" "*"
  putStrLn "Context: overtones=*, key=2# (D major), roots=*"
  putStrLn "D major scale: D E F# G A B C# = pitch classes [2,4,6,7,9,11,1]\n"

  -- Generate progression
  next <- gen start 4 "*" 0.1 ctx

  putStrLn "Generated progression:"
  print next
  putStrLn ""

  -- Extract all pitch classes from the progression
  let pitches = progChords next
  let extractPitchClasses chord =
        map (\i -> (fromIntegral i :: Int) `mod` 12) (H.chordIntervals chord)
  let allPitchClasses = concatMap extractPitchClasses pitches

  -- D major scale pitch classes
  let dmajor = [1,2,4,6,7,9,11] :: [Int]
  let invalidPitches = filter (`notElem` dmajor) allPitchClasses

  putStrLn "All pitch classes in generated chords:"
  print allPitchClasses
  putStrLn ""

  putStrLn "Pitch classes NOT in D major (should be []):"
  print invalidPitches
  putStrLn ""

  -- Check specifically for the G# (8) that was in the bug report
  let hasGsharp = 8 `elem` allPitchClasses
  let hasEb = 3 `elem` allPitchClasses
  let hasBb = 10 `elem` allPitchClasses

  putStrLn "Forbidden pitch classes check:"
  putStrLn $ "  Contains G#/Ab (8): " ++ show hasGsharp ++ if hasGsharp then " ❌ FAIL" else " ✓ PASS"
  putStrLn $ "  Contains D#/Eb (3): " ++ show hasEb ++ if hasEb then " ❌ FAIL" else " ✓ PASS"
  putStrLn $ "  Contains A#/Bb (10): " ++ show hasBb ++ if hasBb then " ❌ FAIL" else " ✓ PASS"
  putStrLn ""

  if null invalidPitches
    then putStrLn "✅ SUCCESS: All chords respect D major key filter!"
    else putStrLn "❌ FAILURE: Some chords contain pitches outside D major!"
