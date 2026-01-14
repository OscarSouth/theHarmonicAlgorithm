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
-- == Key Filter
-- * @"#"@, @"##"@, @"###"@ → sharps
-- * @"b"@, @"bb"@, @"bbb"@ → flats
-- * @"1#"@, @"2b"@, @"4b"@ → numbered key signatures
-- * @"C"@, @"G"@, @"F#"@, @"Bb"@ → named keys
-- * @"*"@ → wildcard
--
-- == Root Notes Filter
-- * @"E F# G"@ → specific pitches
-- * @"1b"@, @"#"@ → key signature (roots from that key)
-- * @"*"@ → wildcard

module Harmonic.Rules.Constraints.FilterSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)

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

  describe "Key signature parsing (parseKey)" $ do
    
    describe "Sharp key signatures" $ do
      it "\"#\" = G major (1 sharp) contains F#, not F" $ do
        let result = sort $ parseKey "#"
        -- G major: G A B C D E F# = [7, 9, 11, 0, 2, 4, 6]
        result `shouldBe` [0, 2, 4, 6, 7, 9, 11]
      
      it "\"##\" = D major (2 sharps) contains F# and C#" $ do
        let result = sort $ parseKey "##"
        -- D major: D E F# G A B C# = [2, 4, 6, 7, 9, 11, 1]
        result `shouldBe` [1, 2, 4, 6, 7, 9, 11]
      
      it "\"1#\" = G major" $ do
        parseKey "1#" `shouldBe` parseKey "#"
      
      it "\"2#\" = D major" $ do
        parseKey "2#" `shouldBe` parseKey "##"
    
    describe "Flat key signatures" $ do
      it "\"b\" = F major (1 flat)" $ do
        let result = sort $ parseKey "b"
        result `shouldBe` [0, 2, 4, 5, 7, 9, 10]  -- F major scale
      
      it "\"bb\" = Bb major (2 flats)" $ do
        let result = sort $ parseKey "bb"
        result `shouldBe` [0, 2, 3, 5, 7, 9, 10]  -- Bb major scale
      
      it "\"1b\" = F major" $ do
        parseKey "1b" `shouldBe` parseKey "b"
      
      it "\"2b\" = Bb major" $ do
        parseKey "2b" `shouldBe` parseKey "bb"
    
    describe "Named keys" $ do
      it "\"C\" = C major" $ do
        let result = sort $ parseKey "C"
        result `shouldBe` [0, 2, 4, 5, 7, 9, 11]  -- C major scale
      
      it "\"G\" = G major" $ do
        parseKey "G" `shouldBe` parseKey "#"
      
      it "\"F\" = F major" $ do
        parseKey "F" `shouldBe` parseKey "b"
    
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
      
      it "\"bb\" is parsed as key signature (2 flats), not Bb note" $ do
        -- "bb" = 2 flats = Bb major key
        -- For the single note Bb, legacy uses different notation
        let result = sort $ parseFunds "bb"
        -- Bb major: Bb C D Eb F G A = [10, 0, 2, 3, 5, 7, 9]
        result `shouldBe` [0, 2, 3, 5, 7, 9, 10]
    
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
      
      it "G major (#) contains G major triad" $ do
        matchesKey "#" [7, 11, 2] `shouldBe` True
      
      it "G major (#) does not contain F (5)" $ do
        matchesKey "#" [5] `shouldBe` False
    
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
      
      it "G major (#) removes F" $ do
        sort (filterByKey "#" [0, 2, 4, 5, 7, 9, 11])
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
    
    it "harmonicContext \"*\" \"*\" \"##\" - D major roots only" $ do
      let rootPcs = sort $ parseFunds "##"
      -- D major = D E F# G A B C# = [2, 4, 6, 7, 9, 11, 1]
      rootPcs `shouldBe` [1, 2, 4, 6, 7, 9, 11]
    
    it "harmonicContext \"D A D F A Ab\" \"*\" \"*\" - specific pitches" $ do
      let pcs = sort $ parseOvertones "D A D F A Ab"
      -- D overtones, A overtones, F overtones, Ab overtones
      2 `elem` pcs `shouldBe` True   -- D
      5 `elem` pcs `shouldBe` True   -- F
      8 `elem` pcs `shouldBe` True   -- Ab
      9 `elem` pcs `shouldBe` True   -- A
