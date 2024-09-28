module Arranger where

import MusicData

-- |instantiate a Progression from a list of Chords and Cadences and as single enharmonic function
initProgression :: EnharmonicFunction -> ([Chord], [Cadence]) -> Progression
initProgression enharm (chords, cadences) =
  let nCadences = length cadences
      enharms = replicate nCadences enharm
   in Progression (chords, cadences, enharms)

initProgression' :: ([Chord], [Cadence], [EnharmonicFunction]) -> Progression
initProgression' (chords, cadences, enharms) = Progression (chords, cadences, enharms)

-- |extract a slice of a cadence between 2 bars (inclusive)
sliceProgression :: Progression -> Int -> Int -> Progression
sliceProgression (Progression (chords, cadences, enharm)) s e =
  let (start, end) = (s-1, e-1)
      sliceCadences = take (end - start + 1) $ drop start cadences
      sliceChords = take (end - start + 1) $ drop start chords
      sliceEnharm = take (end - start + 1) $ drop start enharm
  in initProgression' (sliceChords, sliceCadences, sliceEnharm)

-- |take a single CadenceStates and convert it into a Progression
toProgression :: CadenceState -> Progression
toProgression (CadenceState (c, root)) =
  Progression ([fromCadenceState (CadenceState (c, root))], [c], [enharmFromNoteName root])

-- insert replaces a specified cadence state with a given new cadence state
-- |overwrite a specified cadence state with a given new cadence state
insertProgression :: Int -> CadenceState -> Progression -> Progression
insertProgression n (CadenceState (c, root)) (Progression (chords, cadences, enharm)) =
  let (startChords, _:endChords) = splitAt (n-1) chords
      (startCadences, _:endCadences) = splitAt (n-1) cadences
      (startEnharm, _:endEnharm) = splitAt (n-1) enharm
   in Progression (startChords ++ [fromCadenceState (CadenceState (c, root))] ++ endChords,
                   startCadences ++ [c] ++ endCadences,
                   startEnharm ++ [enharmFromNoteName root] ++ endEnharm)

-- |performance version of insertProgression
insert = insertProgression

-- clone replaces a specified cadence state with a specified cadence state from within the same progression
-- |overwrite a specified cadence state with a different specified cadence state from within the same progression
cloneProgression :: Int -> Int -> Progression -> Progression
cloneProgression m n (Progression (chords, cadences, enharm)) =
  let (startChords, _:endChords) = splitAt (n-1) chords
      (startCadences, _:endCadences) = splitAt (n-1) cadences
      (startEnharm, _:endEnharm) = splitAt (n-1) enharm
   in Progression (startChords ++ [chords !! (m-1)] ++ endChords,
                   startCadences ++ [cadences !! (m-1)] ++ endCadences,
                   startEnharm ++ [enharm !! (m-1)] ++ endEnharm)

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

-- extract discards the beginning and end of a sequence outside a range
-- |extract a range of CadenceStates from a Progression as a new progression
extractProgression :: Int -> Int -> Progression -> Progression
extractProgression s e (Progression (chords, cadences, enharm)) =
  let (startChords, endChords) = splitAt (e-1) chords
      (startCadences, endCadences) = splitAt (e-1) cadences
      (startEnharm, endEnharm) = splitAt (e-1) enharm
   in Progression (take (e - s + 1) $ drop (s-1) chords,
                   take (e - s + 1) $ drop (s-1) cadences,
                   take (e - s + 1) $ drop (s-1) enharm)

-- |performance version of extractProgression
extract = extractProgression

-- fuse combines two progressions together up to a total length of 8 bars
-- |take a list of multiple Progressions and fuse into single Progression
fuseProgression :: [Progression] -> Progression
fuseProgression ps = Progression (chords, cadences, enharms)
  where
    chords     = concat $ (\(Progression (t,_,_)) -> t) <$> ps
    cadences   = concat $ (\(Progression (_,t,_)) -> t) <$> ps
    enharms    = concat $ (\(Progression (_,_,t)) -> t) <$> ps

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
   in Progression (endChords ++ startChords,
                   endCadences ++ startCadences,
                   endEnharm ++ startEnharm)

-- |performance version of rotateProgression
rotate = rotateProgression

-- reverseProgression takes a progression and reverses it
-- |reverseProgression a Progression
reverseProgression :: Progression -> Progression
reverseProgression (Progression (chords, cadences, enharm)) =
  let chords' = reverse chords
      cadences' = reverse cadences
      enharm' = reverse enharm
   in Progression (chords', cadences', enharm')

-- --------- |
-- song mode v




-- -------------------------- |
-- negative harmony functions v