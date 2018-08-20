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
dissonanceLevel,
toCadence,
pc,
fromCadence,
movementFromCadence,
transposeCadence,
rootNote,
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
-- |Utility
unique
  ) where

import           Markov -- contains markov chain numerical processing machinery
import           MusicData -- defines MusicData and many pitchclass analysis functions
import           Overtone -- mainly parsing functions for generating lists of MusicData
import           Utility -- various 'misc' helper functions