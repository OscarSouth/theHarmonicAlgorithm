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
chordList',
parseOvertones,
parseTuning,
parseKey,
parseFunds,
------------
-- |Utility
unique,
-- |GraphDB
-- testFunction
  ) where

import           Markov -- contains markov chain numerical processing machinery
import           MusicData -- defines MusicData and many pitchclass analysis functions
import           Overtone -- mainly parsing functions for generating lists of MusicData
import           Utility -- various 'misc' helper functions
import           GraphDB -- functions to populate and access markov graph database (Neo4j)
import           Perform -- functions related to performance with TidalCycles