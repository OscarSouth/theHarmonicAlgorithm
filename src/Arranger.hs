{-# LANGUAGE BangPatterns #-}

module Arranger where

import           MusicData
import           Utility


-- |instantiate a Progression from a list of Chords and Cadences and a single enharmonic function
initProgression :: EnharmonicFunction -> ([Chord], [Cadence]) -> Progression
initProgression enharm (chords, cadences) =
  let nCadences = length cadences
      enharms = replicate nCadences enharm
   in Progression (chords, cadences, enharms)

initProgression' :: ([Chord], [Cadence], [EnharmonicFunction]) -> Progression
initProgression' (chords, cadences, enharms) = Progression (chords, cadences, enharms)

-- |instantiate a Progression from an enharm functions and list of lists of ints
-- cadences are generated from the chords
-- the initial cadence is the first chord to itself
prog :: EnharmonicFunction -> [[Integer]] -> Progression
prog enharm chords =
  let chordList = toChord enharm <$> chords
      cadences = head (fromChords chordList) : fromChords chordList
   in initProgression enharm (chordList, cadences)

-- |convert a Progression into a list of lists of ints
fromProgression :: Progression -> [[Integer]]
fromProgression (Progression (chords,_, _)) =
  let chordPitches = map (\(Chord (_, pitches)) -> pitches) chords
  in chordPitches  

triadProg :: EnharmonicFunction -> [[Integer]] -> Progression
triadProg enharm chords =
  let triadList = toTriad enharm <$> chords
      cadences = head (fromChords triadList) : fromChords triadList
   in initProgression enharm (triadList, cadences)

-- function which will take a list of chords and produce a list of cadences which match the progression
-- |produce a corresponding list of Cadences from a list of Chords
fromChords :: [Chord] -> [Cadence]
fromChords chords =
  let chordPairs = zip chords (tail chords)
      cadences = fmap (\(x, y) -> toCadence (x, y)) chordPairs
  in cadences

-- |take a single CadenceState and convert it into a Progression
toProgression :: CadenceState -> Progression
toProgression (CadenceState (c, root)) =
  Progression ([fromCadenceState (CadenceState (c, root))], [c], [enharmFromNoteName root])

-- |excerpt a range of CadenceStates from a Progression as a new progression
excerptProgression :: Int -> Int -> Progression -> Progression
excerptProgression s e (Progression (chords, cadences, enharm)) =
  let (start, end) = (s-1, e-1)
      sliceChords = take (end - start + 1) $ drop start chords
      sliceEnharm = take (end - start + 1) $ drop start enharm
      newCadences = fromChords sliceChords
      sliceCadences = (cadences !! start) : newCadences
  in initProgression' (sliceChords, sliceCadences, sliceEnharm)

-- |performance version of excerptProgression
excerpt :: Int -> Int -> Progression -> Progression
excerpt = excerptProgression

-- |extract a single CadenceState from a specified index of a Progression
extractCadenceState :: Int -> Progression -> CadenceState
extractCadenceState n (Progression (chords, cadences, enharm)) =
  let chord = chords !! (n-1)
      cadence = cadences !! (n-1)
      enharm' = enharm !! (n-1)
   in CadenceState (cadence, sharp . pc . bassNoteFromChord $ chord)

-- |performance version of extractCadenceState
extract :: Int -> Progression -> CadenceState
extract = extractCadenceState

-- insert replaces a specified cadence state with a given new cadenceState
-- |overwrite a specified cadence state with a given new cadence state
insertProgression' :: CadenceState -> Int -> Progression -> Progression
insertProgression' (CadenceState (c, root)) n (Progression (chords, cadences, enharm)) =
  let (startChords, _:endChords) = splitAt (n-1) chords
      (startCadences, _:endCadences) = splitAt (n-1) cadences
      (startEnharm, _:endEnharm) = splitAt (n-1) enharm
      newChords = startChords ++ [fromCadenceState (CadenceState (c, root))] ++ endChords
      newCadences = startCadences ++ [c] ++ endCadences
      newEnharms = startEnharm ++ [enharmFromNoteName root] ++ endEnharm
      sliceChords = startChords ++ [fromCadenceState (CadenceState (c, root))] ++ endChords
      newCadences' = head newCadences : fromChords sliceChords
  in Progression (newChords, newCadences', newEnharms)

-- |performance version of insertProgression
insert' :: CadenceState -> Int -> Progression -> Progression
insert' = insertProgression'

-- insert replaces a specified cadence state with a given new list of ints
-- |overwrite a specified cadence state with a given new cadence state
insertProgression :: [Int] -> Int -> Progression -> Progression
insertProgression ps n (Progression (chords, cadences, enharm)) =
  let (startChords, _:endChords) = splitAt (n-1) chords
      (startCadences, _:endCadences) = splitAt (n-1) cadences
      (startEnharm, _:endEnharm) = splitAt (n-1) enharm
      -- Convert the integer list to a Chord using the enharmonic function at position n-1
      currentEnharm = enharm !! (n-1)
      newChord = toTriad currentEnharm (map fromIntegral ps)
      newChords = startChords ++ [newChord] ++ endChords
      -- Create a new cadence for the new chord
      sliceChords = startChords ++ [newChord] ++ endChords
      -- Generate new cadences list with the initial cadence preserved
      newCadences' = head cadences : fromChords sliceChords
      -- Keep the same enharmonic functions
      newEnharms = enharm
  in Progression (newChords, newCadences', newEnharms)
-- |performance version of insertProgression
insert :: [Int] -> Int -> Progression -> Progression
insert = insertProgression

-- clone replaces a specified cadence state with a specified cadence state from within the same progression
-- |overwrite a specified cadence state with a different specified cadence state from within the same progression
cloneProgression :: Int -> Int -> Progression -> Progression
cloneProgression m n progression@(Progression (chords, cadences, enharm)) =
  let CadenceState (c, root) = extractCadenceState m progression
      (startChords, _:endChords) = splitAt (n-1) chords
      (startCadences, _:endCadences) = splitAt (n-1) cadences
      (startEnharm, _:endEnharm) = splitAt (n-1) enharm
      newChords = startChords ++ [fromCadenceState (CadenceState (c, root))] ++ endChords
      newCadences = startCadences ++ [c] ++ endCadences
      newEnharms = startEnharm ++ [enharmFromNoteName root] ++ endEnharm
      sliceChords = startChords ++ [fromCadenceState (CadenceState (c, root))] ++ endChords
      newCadences' = head newCadences : fromChords sliceChords
  in Progression (newChords, newCadences', newEnharms)

-- |performance version of cloneProgression
clone = cloneProgression

-- switch swaps the location of 2 cadence states in a progression to each others positions
-- |switch the location of 2 cadence states with each other in a Progression
switchProgression :: Int -> Int -> Progression -> Progression
switchProgression m n (Progression (chords, cadences, enharm)) =
  let (startChordsM, chordM:endChordsM) = splitAt (m-1) chords
      (startChordsN, chordN:endChordsN) = splitAt (n-1) chords
      (startCadencesM, cadenceM:endCadencesM) = splitAt (m-1) cadences
      (startCadencesN, cadenceN:endCadencesN) = splitAt (n-1) cadences
      (startEnharmM, enharmM:endEnharmM) = splitAt (m-1) enharm
      (startEnharmN, enharmN:endEnharmN) = splitAt (n-1) enharm
      newChords = startChordsM ++ [chordN] ++ endChordsM
      newChords' = take (n-1) newChords ++ [chordM] ++ drop n newChords
      newCadences = startCadencesM ++ [cadenceN] ++ endCadencesM
      newCadences' = take (n-1) newCadences ++ [cadenceM] ++ drop n newCadences
      newEnharm = startEnharmM ++ [enharmN] ++ endEnharmM
      newEnharm' = take (n-1) newEnharm ++ [enharmM] ++ drop n newEnharm
   in Progression (newChords', newCadences', newEnharm')

-- |performance version of switchProgression
switch = switchProgression

-- fuse combines two progressions together up to a total length of 8 bars
-- |take a list of multiple Progressions and fuse into single Progression
fuseProgression :: [Progression] -> Progression
fuseProgression ps = Progression (chords, cadences, enharms)
  where
    chords         = concat $ (\(Progression (t,_,_)) -> t) <$> ps
    initCadence    = head . head $ (\(Progression (_,t,_)) -> t) <$> ps
    cadences       = initCadence : fromChords (concat $ (\(Progression (t,_,_)) -> t) <$> ps)
    enharms        = concat $ (\(Progression (_,_,t)) -> t) <$> ps

-- |performance version of fuseProgression
fuse = fuseProgression

-- rotate shifts the 'phase' of the progression round by the amount specified
-- |rotate a Progression by a specified amount
rotateProgression :: Int -> Progression -> Progression
rotateProgression n (Progression (chords, cadences, enharm)) =
  let n' = (length chords) - n `mod` (length chords)
      (startChords, endChords) = splitAt n' chords
      (startCadences, endCadences) = splitAt n' cadences
      (startEnharm, endEnharm) = splitAt n' enharm
      rotatedChords = endChords ++ startChords
      rotatedEnharm = endEnharm ++ startEnharm
      newCadences = fromChords rotatedChords
      rotatedCadences = (endCadences ++ startCadences) !! 0 : newCadences
   in Progression (rotatedChords, rotatedCadences, rotatedEnharm)

-- |performance version of rotateProgression
rotate = rotateProgression

-- reverseProgression takes a progression and reverses it
-- |reverseProgression a Progression
reverseProgression :: Progression -> Progression
reverseProgression (Progression (chords, cadences, enharm)) =
  let chords' = reverse chords
      enharm' = reverse enharm
      newCadences = fromChords chords'
      cadences' = head (reverse cadences) : newCadences
   in Progression (chords', cadences', enharm')

-- |print out the contents of a Progression
showProgression :: Progression -> String
showProgression (Progression (chords, cadences, enharm)) =
  let showChords = concat $ map ("  " ++) (show <$> chords)
      showCadences = concat $ map ("  " ++) (show <$> cadences)
   in "|| Chords:" ++ showChords ++ " | Cadences:" ++ showCadences ++ " ||"


-- |transpose a Progression by a specified number of semitones
transposeProgression :: Int -> Progression -> Progression
transposeProgression semitones (Progression (chords, cadences, enharmFuncs)) =
  let
    transposeChord enharm (Chord ((root, func), pitches)) =
      let transposedRootPc = pitchClass root + fromIntegral semitones
          newRootName = enharm transposedRootPc
          transposedPitches = fmap (+(fromIntegral semitones)) pitches
      in Chord ((newRootName, func), transposedPitches)
    transposedChords = zipWith transposeChord enharmFuncs chords
    newCadences = cadences
    transposedEnharms = fmap (enharmFromNoteName . sharp . pc . bassNoteFromChord) transposedChords
  in initProgression' (transposedChords, newCadences, transposedEnharms)

-- |performance version of transposeProgression
transpose :: Int -> Progression -> Progression
transpose = transposeProgression

-- ----------|
-- review    v

-- | Extract the last CadenceState in a Progression.
lastCadence :: Progression -> CadenceState
lastCadence (Progression (chords, cadences, _)) =
  let
    -- Get the last chord and cadence from their respective lists.
    lastChord   = last chords
    lastCadence = last cadences
  in
    -- Construct the CadenceState using the same logic as extractCadenceState.
    -- This uses the 'sharp' enharmonic function, just like the provided example.
    CadenceState (lastCadence, sharp . pc . bassNoteFromChord $ lastChord)


-- | Create a new Progression where each chord contains pitches from neighboring chords
-- | within the specified number of steps in both directions.
overlapProgression :: Int -> Progression -> Progression
overlapProgression range (Progression (chords, cadences, enharms)) =
  let
    -- Create a cyclic version of the chord list for wrapping
    cyclic = cycle chords
    -- Drop elements to align with the original list
    offsetChords n = take (length chords) $ drop (length chords - (n `mod` length chords)) cyclic
    
    -- Get all relevant chord fragments for each position
    overlappedChords = 
      [extractOverlappingPitches $ take (2*range + 1) $ drop (i - range) 
      (chords ++ take range (offsetChords range)) | i <- [0..length chords - 1]]
    
    -- Extract pitch information from each chord
    extractOverlappingPitches chordGroup = 
      let
        -- Extract the root and function information from the center chord
        centerChord = chordGroup !! min range (length chordGroup `div` 2)
        Chord ((root, func), _) = centerChord
        -- Collect all pitches from the chord group
        allPitches = concatMap (\(Chord (_, pitches)) -> pitches) chordGroup
        -- Remove duplicates from the pitch collection
        uniquePitches = Utility.unique allPitches
      in
        -- Create a new chord with the same root/functionality but all overlapping pitches
        Chord ((root, func), uniquePitches)
    
  in
    -- Use the same cadences and enharmonic functions, but with overlapped chords
    Progression (overlappedChords, cadences, enharms)

-- |performance version of overlapProgression
overlap :: Int -> Progression -> Progression
overlap = overlapProgression

-- | Create a new Progression where each chord contains pitches from forward neighboring chords
-- | within the specified number of steps (only forward looking)
overlapProgressionForward :: Int -> Progression -> Progression
overlapProgressionForward range (Progression (chords, cadences, enharms)) =
  let
    -- Create a cyclic version of the chord list for wrapping at the end
    cyclic = cycle chords
    
    -- Get all relevant chord fragments for each position (forward only)
    overlappedChords = 
      [extractOverlappingPitches i $ take (range + 1) $ drop i 
      (chords ++ take range cyclic) | i <- [0..length chords - 1]]
    
    -- Extract pitch information from each chord, using only the current and forward chords
    extractOverlappingPitches pos chordGroup = 
      let
        -- Extract the root and function from the current chord (first in group)
        Chord ((root, func), _) = head chordGroup
        -- Collect all pitches from the chord group
        allPitches = concatMap (\(Chord (_, pitches)) -> pitches) chordGroup
        -- Remove duplicates from the pitch collection
        uniquePitches = Utility.unique allPitches
      in
        -- Create a new chord with the same root/functionality but all overlapping pitches
        Chord ((root, func), uniquePitches)
    
  in
    -- Use the same cadences and enharmonic functions, but with overlapped chords
    Progression (overlappedChords, cadences, enharms)

-- |performance version of overlapProgressionForward
overlapF :: Int -> Progression -> Progression
overlapF = overlapProgressionForward

-- | Create a new Progression where each chord contains pitches from backward neighboring chords
-- | within the specified number of steps (only backward looking)
overlapProgressionBackward :: Int -> Progression -> Progression
overlapProgressionBackward range (Progression (chords, cadences, enharms)) =
  let
    -- Create a cyclic version of the chord list for wrapping at the beginning
    cyclic = cycle chords
    -- Get offset for wrapping at the beginning
    offsetChords = take (length chords) $ drop (length chords - range) cyclic
    
    -- Get all relevant chord fragments for each position (backward only)
    overlappedChords = 
      [extractOverlappingPitches $ take (range + 1) $ drop (max 0 (i - range)) 
      (offsetChords ++ chords) | i <- [0..length chords - 1]]
    
    -- Extract pitch information from each chord, using the current and backward chords
    extractOverlappingPitches chordGroup = 
      let
        -- Extract the root and function from the last chord in the group (the current chord)
        Chord ((root, func), _) = last chordGroup
        -- Collect all pitches from the chord group
        allPitches = concatMap (\(Chord (_, pitches)) -> pitches) chordGroup
        -- Remove duplicates from the pitch collection
        uniquePitches = Utility.unique allPitches
      in
        -- Create a new chord with the same root/functionality but all overlapping pitches
        Chord ((root, func), uniquePitches)
    
  in
    -- Use the same cadences and enharmonic functions, but with overlapped chords
    Progression (overlappedChords, cadences, enharms)

-- |performance version of overlapProgressionBackward
overlapB :: Int -> Progression -> Progression
overlapB = overlapProgressionBackward

-- | Expands a Progression by repeating each cadence state N times
-- | This creates a new progression where each chord and its harmonic context
-- | is repeated the specified number of times
expandProgression :: Int -> Progression -> Progression -- CHANGE ARGUMENT ORDER
expandProgression n (Progression (chords, cadences, enharms))
  | n <= 0    = Progression ([], [], []) -- Return empty progression for invalid input
  | n == 1    = Progression (chords, cadences, enharms) -- No change needed
  | otherwise =
    let
      -- Repeat each chord n times
      expandedChords = concatMap (replicate n) chords
      
      -- Generate new cadences for the expanded chord sequence
      -- We use fromChords which calculates cadences between adjacent chords
      expandedCadences = head cadences : fromChords expandedChords
      
      -- Repeat each enharmonic function n times
      expandedEnharms = concatMap (replicate n) enharms
    in
      Progression (expandedChords, expandedCadences, expandedEnharms)

-- |performance version of expandProgression
expand :: Int -> Progression -> Progression
expand = expandProgression

-- | Create a Progression where every N+1 bars alternate between original and overlapped chords
-- | First bar is original, followed by N bars of overlapped chords, then repeat
overlapPassingProgression :: Int -> Progression -> Progression
overlapPassingProgression passingBars prog@(Progression (chords, cadences, enharms)) 
  | passingBars <= 0 = prog -- Return original if invalid input
  | otherwise =
    let
      -- Create a bidirectionally overlapped version of the progression using overlap 1
      Progression (overlappedChords, _, _) = overlapProgression 1 prog
      
      -- Calculate the new sequence length based on cycle pattern (1 original + N overlapped)
      cycleLength = 1 + passingBars
      numCycles = length chords `div` cycleLength
      remainder = length chords `mod` cycleLength
      
      -- Create the new chord sequence by applying the alternating pattern
      newChords = concat 
        [ let start = i * cycleLength
              resetChord = chords !! start
              passingPositions = [start + j | j <- [1..min passingBars (length chords - start - 1)]]
              passingChords = map (overlappedChords !!) passingPositions
          in resetChord : passingChords
        | i <- [0..numCycles-1]
        ] 
        ++ (if remainder > 0 
              then let start = numCycles * cycleLength
                       resetChord = chords !! start
                       passingPositions = [start + j | j <- [1..remainder-1]]
                       passingChords = map (overlappedChords !!) passingPositions
                   in resetChord : passingChords
              else [])
      
      -- Generate cadences for the new chord sequence
      newCadences = head cadences : fromChords newChords
    in
      Progression (newChords, newCadences, enharms)

-- | Create a Progression where every N+1 bars alternate between original and forward-overlapped chords
-- | First bar is original, followed by N bars of forward-overlapped chords, then repeat
overlapPassingForwardProgression :: Int -> Progression -> Progression
overlapPassingForwardProgression passingBars prog@(Progression (chords, cadences, enharms)) 
  | passingBars <= 0 = prog -- Return original if invalid input
  | otherwise =
    let
      -- Create a forward overlapped version of the progression using overlap 1
      Progression (overlappedChords, _, _) = overlapProgressionForward 1 prog
      
      -- Calculate the new sequence length based on cycle pattern (1 original + N overlapped)
      cycleLength = 1 + passingBars
      numCycles = length chords `div` cycleLength
      remainder = length chords `mod` cycleLength
      
      -- Create the new chord sequence by applying the alternating pattern
      newChords = concat 
        [ let start = i * cycleLength
              resetChord = chords !! start
              passingPositions = [start + j | j <- [1..min passingBars (length chords - start - 1)]]
              passingChords = map (overlappedChords !!) passingPositions
          in resetChord : passingChords
        | i <- [0..numCycles-1]
        ] 
        ++ (if remainder > 0 
              then let start = numCycles * cycleLength
                       resetChord = chords !! start
                       passingPositions = [start + j | j <- [1..remainder-1]]
                       passingChords = map (overlappedChords !!) passingPositions
                   in resetChord : passingChords
              else [])
      
      -- Generate cadences for the new chord sequence
      newCadences = head cadences : fromChords newChords
    in
      Progression (newChords, newCadences, enharms)

-- | Create a Progression where every N+1 bars alternate between original and backward-overlapped chords
-- | First bar is original, followed by N bars of backward-overlapped chords, then repeat
overlapPassingBackwardProgression :: Int -> Progression -> Progression
overlapPassingBackwardProgression passingBars prog@(Progression (chords, cadences, enharms)) 
  | passingBars <= 0 = prog -- Return original if invalid input
  | otherwise =
    let
      -- Create a backward overlapped version of the progression using overlap 1
      Progression (overlappedChords, _, _) = overlapProgressionBackward 1 prog
      
      -- Calculate the new sequence length based on cycle pattern (1 original + N overlapped)
      cycleLength = 1 + passingBars
      numCycles = length chords `div` cycleLength
      remainder = length chords `mod` cycleLength
      
      -- Create the new chord sequence by applying the alternating pattern
      newChords = concat 
        [ let start = i * cycleLength
              resetChord = chords !! start
              passingPositions = [start + j | j <- [1..min passingBars (length chords - start - 1)]]
              passingChords = map (overlappedChords !!) passingPositions
          in resetChord : passingChords
        | i <- [0..numCycles-1]
        ] 
        ++ (if remainder > 0 
              then let start = numCycles * cycleLength
                       resetChord = chords !! start
                       passingPositions = [start + j | j <- [1..remainder-1]]
                       passingChords = map (overlappedChords !!) passingPositions
                   in resetChord : passingChords
              else [])
      
      -- Generate cadences for the new chord sequence
      newCadences = head cadences : fromChords newChords
    in
      Progression (newChords, newCadences, enharms)

-- |performance version of overlapPassingProgression
overlapPassing :: Int -> Progression -> Progression
overlapPassing = overlapPassingProgression

-- |performance version of overlapPassingForwardProgression
overlapPassingF :: Int -> Progression -> Progression
overlapPassingF = overlapPassingForwardProgression

-- |performance version of overlapPassingBackwardProgression
overlapPassingB :: Int -> Progression -> Progression
overlapPassingB = overlapPassingBackwardProgression

-- --------- |
-- song mode v

-- -------------------------- |
-- negative harmony functions v


-- ----------------------------- |
-- vertical structure functions  v

-- -- |directly extracts the chords from a Progression without any transformation
-- -- ie. || E sus4 | E min/G | A 6sus2no5 | A 7sus2no5 || -> [[4,9,11],[7,11,4],[9,11,6],[9,11,7]]
-- extractChords :: Progression -> [[Integer]]
-- extractChords (Progression (chords,_,_)) = 
--   let extractChord (Chord ((_, _), pitches)) = pitches
--    in extractChord <$> chords

-- -- |transforms extracted chords with minimal transformation to make them suitable for performance
-- -- ie. [[4,9,11],[7,11,4],[9,11,6],[9,11,7]] -> [[-8,9,11],[-5,11,4],[-3,11,6],[-3,11,7]]
-- -- #1 root pitches (index 0) transposed down 1 octave so that they are always below upper structures
-- literalHarmony :: Progression -> [[Integer]]
-- literalHarmony prog = 
--   let extractedChords = extractChords prog
--    in (\(x:xs) -> (x - 12):xs) <$> extractedChords
