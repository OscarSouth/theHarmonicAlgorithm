{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Core.FilterSpec
-- Description : Tests for the Filter parsing and matching module
--
-- Validates the legacy filter notation from the original README:
--
-- == Overtones/Pitch Set Filter
-- * @"E A D G"@ → overtones of bass tuning
-- * @"E'"@ → single pitch E (no overtones)
-- * @"G E' A' A#'"@ → G overtones + individual pitches E, A, A#
-- * @"*"@ → wildcard (all pitches)
--
-- == Key Filter (and Roots Filter — same unified rules)
-- * Note name: @"C"@ = single pitch C, @"Bb"@ = single pitch Bb (PC 10), @"b"@ = B note (PC 11)
-- * Numbered key sig: @"0#"@ = C major, @"1#"@ = G major, @"2b"@ = Bb major, @"4b"@
-- * @"*"@ → wildcard
--
-- == Root Notes Filter
-- * @"E F# G"@ → specific pitches (note names)
-- * @"1b"@, @"2#"@ → key signature (roots from that key)
-- * @"*"@ → wildcard

module Harmonic.Rules.Constraints.FilterSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)
import qualified Data.IntSet as IntSet

import Harmonic.Rules.Constraints.Filter

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Wildcard handling" $ do
    
    it "\"*\" is wildcard" $ do
      isWildcard "*" `shouldBe` True
    
    it "\"all\" is wildcard" $ do
      isWildcard "all" `shouldBe` True
    
    it "\"chr\" (chromatic) is wildcard" $ do
      isWildcard "chr" `shouldBe` True
    
    it "wildcards are case-insensitive" $ do
      isWildcard "ALL" `shouldBe` True
      isWildcard "CHR" `shouldBe` True
    
    it "non-wildcards return False" $ do
      isWildcard "C" `shouldBe` False
      isWildcard "1#" `shouldBe` False
      isWildcard "" `shouldBe` False

  describe "Overtone parsing (parseTuning)" $ do
    
    describe "Single note overtones" $ do
      it "C generates first 4 overtones [0,4,7,10] = C,E,G,Bb" $ do
        sort (parseTuning "C") `shouldBe` [0, 4, 7, 10]
      
      it "G generates [2,5,7,11] = G,B,D,F" $ do
        sort (parseTuning "G") `shouldBe` [2, 5, 7, 11]
      
      it "E generates [2,4,8,11] = E,G#,B,D" $ do
        sort (parseTuning "E") `shouldBe` [2, 4, 8, 11]
    
    describe "Bass tuning example (E A D G)" $ do
      it "E A D G combines overtones of all four strings" $ do
        let result = parseTuning "E A D G"
        -- E overtones: 4, 8, 11 (E, G#, B)
        -- A overtones: 9, 1, 4 (A, C#, E)
        -- D overtones: 2, 6, 9 (D, F#, A)
        -- G overtones: 7, 11, 2 (G, B, D)
        4 `elem` result `shouldBe` True   -- E
        9 `elem` result `shouldBe` True   -- A
        2 `elem` result `shouldBe` True   -- D
        7 `elem` result `shouldBe` True   -- G
    
    describe "Wildcard" $ do
      it "\"*\" returns all 12 pitch classes" $ do
        sort (parseTuning "*") `shouldBe` [0..11]
      
      it "\"all\" returns all 12 pitch classes" $ do
        sort (parseTuning "all") `shouldBe` [0..11]
    
    describe "Case insensitivity" $ do
      it "\"c\" and \"C\" give same result" $ do
        parseTuning "c" `shouldBe` parseTuning "C"
      
      it "\"e a d g\" and \"E A D G\" give same result" $ do
        parseTuning "e a d g" `shouldBe` parseTuning "E A D G"

  describe "Prime notation (single pitch, no overtones)" $ do
    
    it "C' gives just pitch class 0" $ do
      parseTuning' 3 "C'" `shouldBe` [0]
    
    it "E' gives just pitch class 4" $ do
      parseTuning' 3 "E'" `shouldBe` [4]
    
    it "A#' gives just pitch class 10" $ do
      parseTuning' 3 "A#'" `shouldBe` [10]
    
    it "Combined: G E' A' A#' (from README example)" $ do
      let result = sort $ parseTuning' 3 "G E' A' A#'"
      -- G overtones: 7, 11, 2
      -- E' = 4 (single pitch)
      -- A' = 9 (single pitch)
      -- A#' = 10 (single pitch)
      2 `elem` result `shouldBe` True   -- from G
      4 `elem` result `shouldBe` True   -- E'
      7 `elem` result `shouldBe` True   -- G root
      9 `elem` result `shouldBe` True   -- A'
      10 `elem` result `shouldBe` True  -- A#'
      11 `elem` result `shouldBe` True  -- from G

  describe "parseTuningNamed (overtone annotation support)" $ do
    it "parses EADG bass tuning with names" $
      parseTuningNamed "E A D G" `shouldBe` [("E",4),("A",9),("D",2),("G",7)]

    it "parses EAeGB cittern tuning preserving case" $
      parseTuningNamed "E A e G B" `shouldBe` [("E",4),("A",9),("e",4),("G",7),("B",11)]

    it "returns empty for wildcard" $
      parseTuningNamed "*" `shouldBe` []

    it "is case-preserving for string names" $ do
      let result = parseTuningNamed "e A"
      map fst result `shouldBe` ["e","A"]

  describe "Key signature parsing (parseKey)" $ do
    
    describe "Numbered sharp key signatures" $ do
      it "\"1#\" = G major (1 sharp)" $ do
        sort (parseKey "1#") `shouldBe` [0, 2, 4, 6, 7, 9, 11]
      
      it "\"2#\" = D major (2 sharps)" $ do
        sort (parseKey "2#") `shouldBe` [1, 2, 4, 6, 7, 9, 11]
    
    describe "Numbered flat key signatures" $ do
      it "\"1b\" = F major (1 flat)" $ do
        sort (parseKey "1b") `shouldBe` [0, 2, 4, 5, 7, 9, 10]
      
      it "\"2b\" = Bb major (2 flats)" $ do
        sort (parseKey "2b") `shouldBe` [0, 2, 3, 5, 7, 9, 10]
      
      it "\"0#\" = C major (no accidentals)" $ do
        sort (parseKey "0#") `shouldBe` [0, 2, 4, 5, 7, 9, 11]
    
    describe "Note names in key context (single pitch)" $ do
      it "\"b\" = B note (PC 11), not F major" $ do
        parseKey "b" `shouldBe` [11]
      
      it "\"bb\" = Bb note (PC 10), not 2-flat key signature" $ do
        parseKey "bb" `shouldBe` [10]
      
      it "\"C\" = single pitch C (PC 0), not C major scale" $ do
        parseKey "C" `shouldBe` [0]
      
      it "\"G\" = single pitch G (PC 7), not G major scale" $ do
        parseKey "G" `shouldBe` [7]
      
      it "\"F\" = single pitch F (PC 5), not F major scale" $ do
        parseKey "F" `shouldBe` [5]
    
    describe "Wildcard" $ do
      it "\"*\" returns all 12 pitch classes" $ do
        sort (parseKey "*") `shouldBe` [0..11]

  describe "Root/Fundamentals parsing (parseFunds)" $ do
    
    describe "Note name roots" $ do
      it "\"E F# G\" gives [4, 6, 7]" $ do
        sort (parseFunds "E F# G") `shouldBe` [4, 6, 7]
      
      it "\"C\" gives [0]" $ do
        -- Note: "C" alone might be parsed as C major key
        -- For single note, use just the note name
        parseFunds "C" `shouldBe` [0]
      
      it "\"bb\" = Bb note (PC 10), not 2-flat key signature" $ do
        parseFunds "bb" `shouldBe` [10]
      
      it "\"Bb\" = Bb note (PC 10), case-insensitive" $ do
        parseFunds "Bb" `shouldBe` [10]
      
      it "\"D A G Bb F\" parses to individual pitches only (regression: C not included)" $ do
        sort (parseFunds "D A G Bb F") `shouldBe` [2, 5, 7, 9, 10]
    
    describe "Key signature roots" $ do
      it "\"1b\" gives F major scale degrees as roots" $ do
        let result = sort $ parseFunds "1b"
        result `shouldBe` [0, 2, 4, 5, 7, 9, 10]  -- F major scale
    
    describe "Wildcard" $ do
      it "\"*\" returns all 12 pitch classes" $ do
        sort (parseFunds "*") `shouldBe` [0..11]

  describe "Matching predicates" $ do
    
    describe "matchesPitchSet" $ do
      it "wildcard matches any pitch set" $ do
        matchesPitchSet "*" [0, 4, 7] `shouldBe` True
        matchesPitchSet "*" [1, 2, 3, 4, 5] `shouldBe` True
      
      it "C overtones contain C major triad [0,4,7]" $ do
        matchesPitchSet "C" [0, 4, 7] `shouldBe` True
      
      it "C overtones do not contain [1, 5, 8]" $ do
        matchesPitchSet "C" [1, 5, 8] `shouldBe` False
    
    describe "matchesKey" $ do
      it "wildcard matches any pitch set" $ do
        matchesKey "*" [0, 4, 7] `shouldBe` True
      
      it "G major (1#) contains G major triad" $ do
        matchesKey "1#" [7, 11, 2] `shouldBe` True
      
      it "G major (1#) does not contain F (5)" $ do
        matchesKey "1#" [5] `shouldBe` False
    
    describe "matchesRoots" $ do
      it "wildcard matches any root" $ do
        matchesRoots "*" 0 `shouldBe` True
        matchesRoots "*" 7 `shouldBe` True
      
      it "\"E F# G\" matches E (4)" $ do
        matchesRoots "E F# G" 4 `shouldBe` True
      
      it "\"E F# G\" matches F# (6)" $ do
        matchesRoots "E F# G" 6 `shouldBe` True
      
      it "\"E F# G\" does not match C (0)" $ do
        matchesRoots "E F# G" 0 `shouldBe` False

  describe "Filter functions" $ do
    
    describe "filterPitchSet" $ do
      it "wildcard preserves all pitches" $ do
        filterPitchSet "*" [0, 4, 7] `shouldBe` [0, 4, 7]
      
      it "C filter keeps only C overtone pitches" $ do
        sort (filterPitchSet "C" [0, 1, 2, 3, 4, 5, 6, 7]) 
          `shouldBe` [0, 4, 7]  -- C, E, G
    
    describe "filterByKey" $ do
      it "wildcard preserves all pitches" $ do
        filterByKey "*" [0, 1, 2, 3] `shouldBe` [0, 1, 2, 3]
      
      it "G major (1#) removes F" $ do
        sort (filterByKey "1#" [0, 2, 4, 5, 7, 9, 11])
          `shouldBe` [0, 2, 4, 7, 9, 11]  -- removes F (5)
    
    describe "filterRoots" $ do
      it "wildcard preserves all roots" $ do
        filterRoots "*" [0, 4, 7] `shouldBe` [0, 4, 7]
      
      it "\"E G\" keeps only E and G" $ do
        sort (filterRoots "E G" [0, 2, 4, 5, 7, 9])
          `shouldBe` [4, 7]

  describe "Real-world examples from live/*.tidal" $ do
    
    it "harmonicContext \"*\" \"1#\" \"1#\" - G major tonality and roots" $ do
      -- Tonality "1#" = G major scale
      let keyPcs = sort $ parseKey "1#"
      -- G major = G A B C D E F# = [0, 2, 4, 6, 7, 9, 11]
      keyPcs `shouldBe` [0, 2, 4, 6, 7, 9, 11]
    
    it "harmonicContext \"*\" \"*\" \"2#\" - D major roots only" $ do
      let rootPcs = sort $ parseFunds "2#"
      -- D major = D E F# G A B C# = [2, 4, 6, 7, 9, 11, 1]
      rootPcs `shouldBe` [1, 2, 4, 6, 7, 9, 11]
    
    it "harmonicContext \"D A D F A Ab\" \"*\" \"*\" - specific pitches" $ do
      let pcs = sort $ parseOvertones "D A D F A Ab"
      -- D overtones, A overtones, F overtones, Ab overtones
      2 `elem` pcs `shouldBe` True   -- D
      5 `elem` pcs `shouldBe` True   -- F
      8 `elem` pcs `shouldBe` True   -- Ab
      9 `elem` pcs `shouldBe` True   -- A

  describe "Bass direction parsing" $ do

    describe "parseBassDirection" $ do
      it "detects 'fall' token" $
        parseBassDirection "* fall" `shouldBe` Just (Fall 1)

      it "detects 'rise' token" $
        parseBassDirection "0# rise" `shouldBe` Just (Rise 1)

      it "returns Nothing when no direction token" $
        parseBassDirection "C E G" `shouldBe` Nothing

      it "returns Nothing for wildcard alone" $
        parseBassDirection "*" `shouldBe` Nothing

      it "is case-insensitive" $
        parseBassDirection "* FALL" `shouldBe` Just (Fall 1)

      it "works with multiple pitch tokens" $
        parseBassDirection "C E G fall" `shouldBe` Just (Fall 1)

      it "parses rise2" $
        parseBassDirection "* rise2" `shouldBe` Just (Rise 2)

      it "parses fall3" $
        parseBassDirection "0# fall3" `shouldBe` Just (Fall 3)

      it "parses rise6" $
        parseBassDirection "* rise6" `shouldBe` Just (Rise 6)

      it "rejects rise7 (out of range)" $
        parseBassDirection "* rise7" `shouldBe` Nothing

      it "rejects rise0 (out of range)" $
        parseBassDirection "* rise0" `shouldBe` Nothing

      it "rejects rise10 (multi-digit)" $
        parseBassDirection "* rise10" `shouldBe` Nothing

    describe "stripDirectionToken" $ do
      it "strips 'fall'" $
        stripDirectionToken "* fall" `shouldBe` "*"

      it "strips 'rise'" $
        stripDirectionToken "0# rise" `shouldBe` "0#"

      it "preserves string without direction" $
        stripDirectionToken "C E G" `shouldBe` "C E G"

      it "strips from multi-token string" $
        stripDirectionToken "C E G fall" `shouldBe` "C E G"

      it "handles case insensitivity" $
        stripDirectionToken "* FALL" `shouldBe` "*"

      it "strips 'rise3'" $
        stripDirectionToken "* rise3" `shouldBe` "*"

      it "strips 'fall2' from multi-token" $
        stripDirectionToken "C E G fall2" `shouldBe` "C E G"

  describe "Bass direction helpers (closestAbove/closestBelow)" $ do
    let cMajor = IntSet.fromList [0, 2, 4, 5, 7, 9, 11]

    describe "closestAbove" $ do
      it "G -> A in C major" $
        closestAbove 7 cMajor `shouldBe` 9

      it "B -> C (wraps around) in C major" $
        closestAbove 11 cMajor `shouldBe` 0

      it "C -> D in C major" $
        closestAbove 0 cMajor `shouldBe` 2

      it "E -> F in C major" $
        closestAbove 4 cMajor `shouldBe` 5

      it "single-element set returns self (pedal)" $
        closestAbove 5 (IntSet.singleton 5) `shouldBe` 5

    describe "closestBelow" $ do
      it "G -> F in C major" $
        closestBelow 7 cMajor `shouldBe` 5

      it "C -> B (wraps around) in C major" $
        closestBelow 0 cMajor `shouldBe` 11

      it "D -> C in C major" $
        closestBelow 2 cMajor `shouldBe` 0

      it "E -> D in C major" $
        closestBelow 4 cMajor `shouldBe` 2

      it "single-element set returns self (pedal)" $
        closestBelow 5 (IntSet.singleton 5) `shouldBe` 5

    describe "closestAbove/Below with sparse set" $ do
      let sparse = IntSet.fromList [0, 4, 7]  -- C, E, G

      it "closestAbove C -> E" $
        closestAbove 0 sparse `shouldBe` 4

      it "closestAbove E -> G" $
        closestAbove 4 sparse `shouldBe` 7

      it "closestAbove G -> C (wraps)" $
        closestAbove 7 sparse `shouldBe` 0

      it "closestBelow C -> G (wraps)" $
        closestBelow 0 sparse `shouldBe` 7

      it "closestBelow E -> C" $
        closestBelow 4 sparse `shouldBe` 0

      it "closestBelow G -> E" $
        closestBelow 7 sparse `shouldBe` 4

  describe "nthAbove/nthBelow (step sizes)" $ do
    let cMajor = IntSet.fromList [0, 2, 4, 5, 7, 9, 11]  -- C D E F G A B
        sparse = IntSet.fromList [0, 4, 7]  -- C E G
        twoNote = IntSet.fromList [0, 2]  -- C D

    describe "nthAbove in C major" $ do
      it "step 1: C -> D" $
        nthAbove 1 0 cMajor `shouldBe` 2
      it "step 2: C -> E" $
        nthAbove 2 0 cMajor `shouldBe` 4
      it "step 3: C -> F" $
        nthAbove 3 0 cMajor `shouldBe` 5
      it "step 4: C -> G" $
        nthAbove 4 0 cMajor `shouldBe` 7
      it "step 5: C -> A" $
        nthAbove 5 0 cMajor `shouldBe` 9
      it "step 6: C -> B" $
        nthAbove 6 0 cMajor `shouldBe` 11

    describe "nthBelow in C major" $ do
      it "step 1: G -> F" $
        nthBelow 1 7 cMajor `shouldBe` 5
      it "step 2: G -> E" $
        nthBelow 2 7 cMajor `shouldBe` 4
      it "step 3: G -> D" $
        nthBelow 3 7 cMajor `shouldBe` 2

    describe "wrapping when N exceeds set size" $ do
      it "nthAbove any N in 2-note set always lands on the other note" $
        -- others = {2}, length 1. (n-1) mod 1 = 0 for all n.
        nthAbove 3 0 twoNote `shouldBe` 2
      it "nthBelow any N in 2-note set always lands on the other note" $
        nthBelow 3 2 twoNote `shouldBe` 0

    describe "nthAbove/Below in sparse set" $ do
      it "nthAbove 2: C -> G (skip E)" $
        nthAbove 2 0 sparse `shouldBe` 7
      it "nthAbove 3: C -> wraps to E (3 mod 2 = index 0 = E)" $
        -- others from 0 = {4, 7}, sorted by dist above = [4, 7]
        -- (3-1) mod 2 = 0 → sorted !! 0 = 4
        nthAbove 3 0 sparse `shouldBe` 4
      it "nthBelow 2: G -> C (skip E)" $
        nthBelow 2 7 sparse `shouldBe` 0

    describe "pedal (single element)" $ do
      it "nthAbove 3 returns self" $
        nthAbove 3 5 (IntSet.singleton 5) `shouldBe` 5
      it "nthBelow 3 returns self" $
        nthBelow 3 5 (IntSet.singleton 5) `shouldBe` 5
