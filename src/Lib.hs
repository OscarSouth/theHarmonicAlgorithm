module Lib (
------------
-- |MusicData
MusicData,
PitchClass (P),
NoteName,
EnharmonicFunction,
Chord (Chord),
Cadence,
CadenceState (CadenceState),
Progression (Progression),
i,
pitchClass,
readNoteName,
mostConsonant,
possibleTriads'',
toTriad,
flatTriad,
sharpTriad,
flatChord,
sharpChord,
flat,
sharp,
showTriad,
dissonanceLevel,
toCadence,
initCadenceState,
cadenceStateEnharm,
fromCadenceState,
getCadenceState,
initProgression,
initProgression',
sliceProgression,
orderVoicing,
harmony,
smoothBass,
normaliseRegister,
chordScale,
--modeNames,
pc,
pcSet,
simpleInversions,
zeroForm,
compactForm,
intervalVector,
fromCadence,
fromCadence',
movementFromCadence,
toMovement,
fromMovement,
fromMovement',
movementFromCadence',
transposeCadence,
rootNote,
rootNote',
toMode,
basePenta,
sortPcSet,
fromChord,
rootNoteFromChord,
bassNoteFromChord,
progRoots',
toEnhTriad,
--progression,
----------
-- experimental stuff
-- Movement,
-- Functionality,
constructCadence,
deconstructCadence,
-----------
-- |Analysis
prog3ecbc, pentaPatterns, -- temp
fullSet3title,
pentatonicSet1title,
pentatonicSet2title,
pentatonicSet3title,
diatonicSet12title,
diatonicSet23title,
diatonicSet31title,
generateScale,
triadSets,
chordSets,
vocab'',
allModes,
majorPentaChr,
okinaPentaChr,
iwatoPentaChr,
(?>),(<?),
-----------
-- |Markov
MarkovMap,
markovMap,
bigrams,
------------
-- |Overtone
chordList',
parseOvertones,
parseNotes,
parseTuning,
parseKey,
parseFunds,
harmonicContext,
------------
-- |Utility
unique,
zip12
-- uniqueAnalysis,
-- |GraphDB
-- testFunc,
-- testData
  ) where

import           Markov -- contains markov chain numerical processing machinery
import           MusicData -- defines MusicData and many pitchclass analysis functions
import           Analysis -- ad hoc analysis functionality for composing
import           Overtone -- mainly parsing functions for generating lists of MusicData
import           Utility -- various 'misc' helper functions
import           GraphDB -- functions to populate and access markov graph database (Neo4j)
import           Perform -- functions related to performance with TidalCycles