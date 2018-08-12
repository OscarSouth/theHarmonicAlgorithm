module Lib (
------------
-- |MusicData
MusicData,
PitchClass (P),
NoteName,
Chord (Chord),
Cadence,
i,
pitchClass,
mostConsonant,
possibleTriads'',
toTriad,
flatTriad,
sharpTriad,
flat,
sharp,
showTriad,
toCadence,
pc,
fromCadence,
movementFromCadence,
dissonanceLevel,
-----------
-- |Markov
MarkovMap,
markovMap,
bigrams,
------------
-- |Overtone
-- theHarmonicAlgorithm,
theHarmonicAlgorithm',
parseOvertones,
parseTuning,
parseKey,
parseFunds,
------------
-- |Chorale
------------
-- |Utility
unique
  ) where

import           Chorale
import           Markov
import           MusicData
import           Overtone
import           Utility

