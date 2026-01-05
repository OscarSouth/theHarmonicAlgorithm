{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Harmonic.Config
import           Harmonic.Database.Graph
import           Harmonic.Ingestion.CSV
import           Harmonic.Ingestion.Transform
import           Harmonic.Ingestion.Types
import qualified Harmonic.Core.Harmony as H
import qualified Harmonic.Core.Pitch as P

import qualified Database.Bolt as Bolt
import           Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isAlphaNum)
import           Data.List (foldl')

type ComposerPieces = Map Text (Map Text [ChordSlice])

-- Local type alias for H.Cadence-based edges (replacing Markov.Edge which uses MD.Cadence)
type HEdge = (H.Cadence, H.Cadence)

composerInclude :: [Text]
composerInclude =
  [ "bach" -- Bach
  , "debussy" -- Debussy
  , "stravinsky" -- Stravinsky
  , "abel" -- Abel
  , "accolay" -- Accolay
  , "adam" -- Adam
  , "agnesi" -- Agnesi
  , "aguado" -- Aguado
  , "ahlefeldt" -- Ahlefeldt
  , "albeniz" -- Albeniz
  , "albinoni" -- Albinoni
  , "albrechtsberger" -- Albrechtsberger
  , "alexandrov" -- Alexandrov
  , "alkan" -- Alkan
  , "ambroise" -- Ambroise
  , "andersen" -- Andersen
  , "annaamalia" -- Anna Amalia
  , "anonymous" -- Anonymous
  , "archer" -- Archer
  , "arensky" -- Arensky
  , "arne" -- Arne
  , "arnold" -- Arnold
  , "arriaga" -- Arriaga
  , "arrieta" -- Arrieta
  , "ascher" -- Ascher
  , "astorga" -- Astorga
  , "attwood" -- Attwood
  , "auber" -- Auber
  , "audran" -- Audran
  , "bachman" -- Bachman
  , "badarzewska" -- Badarzewska
  , "balakirev" -- Balakirev
  , "balfe" -- Balfe
  , "banister" -- Banister
  , "barbieri" -- Barbieri
  , "bargiel" -- Bargiel
  , "bargy" -- Bargy
  , "barnard" -- Barnard
  , "barnett" -- Barnett
  , "baron" -- Baron
  , "barret" -- Barret
  , "barrios" -- Barrios
  , "bartok" -- Bartok
  , "bayreuth" -- Bayreuth
  , "bazin" -- Bazin
  , "bazzini" -- Bazzini
  , "beethoven" -- Beethoven
  , "bellini" -- Bellini
  , "bellinzani" -- Bellinzani
  , "bellman" -- Bellman
  , "bembo" -- Bembo
  , "benda" -- Benda
  , "benoit" -- Benoit
  , "berens" -- Berens
  , "berlioz" -- Berlioz
  , "berthelemy" -- Berthelemy
  , "berthomieu" -- Berthomieu
  , "berwald" -- Berwald
  , "best" -- Best
  , "bevilacqua" -- Bevilacqua
  , "billema" -- Billema
  , "billi" -- Billi
  , "billings" -- Billings
  , "bizet" -- Bizet
  , "blavet" -- Blavet
  , "bloch" -- Bloch
  , "blodek" -- Blodek
  , "blow" -- Blow
  , "boccherini" -- Boccherini
  , "bochsa" -- Bochsa
  , "boehm" -- Boehm
  , "boellmann" -- Boellmann
  , "boely" -- Boely
  , "bohm" -- Bohm
  , "boieldieu" -- Boieldieu
  , "boismortier" -- Boismortier
  , "bondivenezia" -- Bon Di Venezia
  , "bonis" -- Bonis
  , "borodin" -- Borodin
  , "bortnyansky" -- Bortnyansky
  , "bouffil" -- Bouffil
  , "boulanger" -- Boulanger
  , "bourgaultducoudray" -- Bourgault-Ducoudray
  , "boyce" -- Boyce
  , "braga" -- Braga
  , "braham" -- Braham
  , "brahms" -- Brahms
  , "brant" -- Brant
  , "braunschweig" -- Braunschweig
  , "brenton" -- Brenton
  , "brescianello" -- Brescianello
  , "broca" -- Broca
  , "bruch" -- Bruch
  , "bruckner" -- Bruckner
  , "bruhns" -- Bruhns
  , "burgmuller" -- Burgmuller
  , "busoni" -- Busoni
  , "buxtehude" -- Buxtehude
  , "byrd" -- Byrd
  , "cabanilles" -- Cabanilles
  , "caldara" -- Caldara
  , "campion" -- Campion
  , "campra" -- Campra
  , "cano" -- Cano
  , "cantallos" -- Cantallos
  , "carcassi" -- Carcassi
  , "cardillo" -- Cardillo
  , "carey" -- Carey
  , "carissimi" -- Carissimi
  , "carr" -- Carr
  , "carreno" -- Carreno
  , "carulli" -- Carulli
  , "casadesus" -- Casadesus
  , "casanovas" -- Casanovas
  , "casciolini" -- Casciolini
  , "casella" -- Casella
  , "castello" -- Castello
  , "catel" -- Catel
  , "cernohorsky" -- Cernohorsky
  , "cervantes" -- Cervantes
  , "chabrier" -- Chabrier
  , "chambonnieres" -- Chambonnieres
  , "chaminade" -- Chaminade
  , "chapi" -- Chapi
  , "charpentier" -- Charpentier
  , "cherubini" -- Cherubini
  , "chopin" -- Chopin
  , "chuecayrobles" -- Chueca y Robles
  , "chdeville" -- Ch_deville
  , "ciardi" -- Ciardi
  , "cilea" -- Cilea
  , "cimarosa" -- Cimarosa
  , "clarke" -- Clarke
  , "clementi" -- Clementi
  , "clerambault" -- Clerambault
  , "coleridgetaylor" -- Coleridge-Taylor
  , "colkin" -- Colkin
  , "cons" -- Cons
  , "cooke" -- Cooke
  , "corbetta" -- Corbetta
  , "corelli" -- Corelli
  , "coste" -- Coste
  , "couperin" -- Couperin
  , "cpebach" -- CPEBach
  , "cramer" -- Cramer
  , "croft" -- Croft
  , "crusell" -- Crusell
  , "cui" -- Cui
  , "czerny" -- Czerny
  , "czibulka" -- Czibulka
  , "dacosta" -- Da Costa
  , "dandrieu" -- Dandrieu
  , "daquin" -- Daquin
  , "daragona" -- DAragona
  , "dargomizhsky" -- Dargomizhsky
  , "dauprat" -- Dauprat
  , "dawes" -- Dawes
  , "dediesbach" -- De Diesbach
  , "defalla" -- De Falla
  , "dekoven" -- De Koven
  , "demacque" -- De Macque
  , "devilbac" -- De Vilbac
  , "debali" -- Debali
  , "delgadopalacios" -- Delgado Palacios
  , "delibes" -- Delibes
  , "delioux" -- Delioux
  , "delius" -- Delius
  , "demersseman" -- Demersseman
  , "dentella" -- Dentella
  , "denza" -- Denza
  , "devienne" -- Devienne
  , "diabelli" -- Diabelli
  , "dittersdorf" -- Dittersdorf
  , "donizetti" -- Donizetti
  , "doppler" -- Doppler
  , "dornell" -- Dornell
  , "draeseke" -- Draeseke
  , "dreyshock" -- Dreyshock
  , "dukas" -- Dukas
  , "duparc" -- Duparc
  , "dupre" -- Dupre
  , "durand" -- Durand
  , "durango" -- Durango
  , "durante" -- Durante
  , "dusek" -- Dusek
  , "duval" -- Duval
  , "duvernoy" -- Duvernoy
  , "dvorak" -- Dvorak
  , "eberlin" -- Eberlin
  , "eberling" -- Eberling
  , "eccles" -- Eccles
  , "eichner" -- Eichner
  , "elgar" -- Elgar
  , "enescu" -- Enescu
  , "erkel" -- Erkel
  , "ewald" -- Ewald
  , "exaudet" -- Exaudet
  , "fasch" -- Fasch
  , "faure" -- Faure
  , "ferrer" -- Ferrer
  , "fibich" -- Fibich
  , "field" -- Field
  , "finger" -- Finger
  , "fiocco" -- Fiocco
  , "fiorillo" -- Fiorillo
  , "fischer" -- Fischer
  , "fisher" -- Fisher
  , "flies" -- Flies
  , "fomin" -- Fomin
  , "franck" -- Franck
  , "frederickiithegreat" -- Frederick II the Great
  , "freixanet" -- Freixanet
  , "friese" -- Friese
  , "froberger" -- Froberger
  , "fucik" -- Fucik
  , "fux" -- Fux
  , "gade" -- Gade
  , "galliard" -- Galliard
  , "galuppi" -- Galuppi
  , "ganne" -- Ganne
  , "gaultier" -- Gaultier
  , "gelinek" -- Gelinek
  , "geminiani" -- Geminiani
  , "geoffroy" -- Geoffroy
  , "german" -- German
  , "gershwin" -- Gershwin
  , "giardini" -- Giardini
  , "gibbons" -- Gibbons
  , "gibel" -- Gibel
  , "gigout" -- Gigout
  , "gimenez" -- Gimenez
  , "giordani" -- Giordani
  , "giuliani" -- Giuliani
  , "glazunov" -- Glazunov
  , "gliere" -- Gliere
  , "glinka" -- Glinka
  , "gluck" -- Gluck
  , "godard" -- Godard
  , "godowsky" -- Godowsky
  , "haberbier" -- Haberbier
  , "haeffner" -- Haeffner
  , "hahn" -- Hahn
  , "halevy" -- Halevy
  , "halvorsen" -- Halvorsen
  , "handel" -- Handel
  , "hanon" -- Hanon
  , "haslinger" -- Haslinger
  , "hasse" -- Hasse
  , "haydn" -- Haydn
  , "hbach" -- HBach
  , "heller" -- Heller
  , "heller" -- heller
  , "henselt" -- Henselt
  , "herbert" -- Herbert
  , "herold" -- Herold
  , "herz" -- Herz
  , "hewitt" -- Hewitt
  , "hindemith" -- Hindemith
  , "hoffmeister" -- Hoffmeister
  , "hol" -- Hol
  , "holcombe" -- Holcombe
  , "holst" -- Holst
  , "hough" -- Hough
  , "huber" -- Huber
  , "hummel" -- Hummel
  , "humperdinck" -- Humperdinck
  , "hunten" -- Hunten
  , "ilynsky" -- Ilynsky
  , "ives" -- Ives
  , "jhofmann" -- J. Hofmann
  , "jmhaydn" -- J.M.Haydn
  , "jackson" -- Jackson
  , "jacquetdelaguerre" -- Jacquet de la Guerre
  , "jadassohn" -- Jadassohn
  , "jcbach" -- JCBach
  , "jensen" -- Jensen
  , "joncieres" -- Joncieres
  , "joplin" -- Joplin
  , "joseffy" -- Joseffy
  , "jullien" -- Jullien
  , "juon" -- Juon
  , "kalkbrenner" -- Kalkbrenner
  , "kelerbela" -- KelerBela
  , "kerll" -- Kerll
  , "kessler" -- Kessler
  , "ketelbey" -- Ketelbey
  , "ketterer" -- Ketterer
  , "khachaturian" -- Khachaturian
  , "kiel" -- Kiel
  , "kircher" -- Kircher
  , "kirkpatrick" -- Kirkpatrick
  , "klendel" -- Klendel
  , "kodaly" -- Kodaly
  , "komarowski" -- Komarowski
  , "kopylov" -- Kopylov
  , "koschat" -- Koschat
  , "kowalski" -- Kowalski
  , "krafft" -- Krafft
  , "kraus" -- Kraus
  , "kreisler" -- Kreisler
  , "kreutzer" -- Kreutzer
  , "krieger" -- Krieger
  , "krommer" -- Krommer
  , "krov" -- Krov
  , "kuhlau" -- Kuhlau
  , "kuhnau" -- Kuhnau
  , "kunzen" -- Kunzen
  , "lhofmann" -- L. Hofmann
  , "labitzky" -- Labitzky
  , "lachner" -- Lachner
  , "lacombe" -- Lacombe
  , "lajarte" -- Lajarte
  , "lalo" -- Lalo
  , "lassen" -- Lassen
  , "lauffensteiner" -- Lauffensteiner
  , "lawes" -- Lawes
  , "leclair" -- Leclair
  , "lecuona" -- Lecuona
  , "lefebure" -- Lefebure
  , "lefebvre" -- Lefebvre
  , "legnani" -- Legnani
  , "lehar" -- Lehar
  , "lemire" -- Lemire
  , "lemmens" -- Lemmens
  , "leo" -- Leo
  , "leonarda" -- Leonarda
  , "leschetizky" -- Leschetizky
  , "levy" -- Levy
  , "leybach" -- Leybach
  , "liguori" -- Liguori
  , "lindberg" -- Lindberg
  , "linley" -- Linley
  , "liobet" -- Liobet
  , "liszt" -- Liszt
  , "lobo" -- Lobo
  , "loeillet" -- Loeillet
  , "lori" -- Lori
  , "losy" -- Losy
  , "lotti" -- Lotti
  , "loud" -- Loud
  , "luigini" -- Luigini
  , "lully" -- Lully
  , "lyadov" -- Lyadov
  , "lyssenko" -- Lyssenko
  , "ma" -- Ma
  , "maccunn" -- Maccunn
  , "macdowell" -- Macdowell
  , "magnani" -- Magnani
  , "mahler" -- Mahler
  , "malats" -- Malats
  , "mancinelli" -- Mancinelli
  , "manfredini" -- Manfredini
  , "marais" -- Marais
  , "marcello" -- Marcello
  , "marchand" -- Marchand
  , "marpurg" -- Marpurg
  , "martin" -- Martin
  , "martini" -- Martini
  , "mascagni" -- Mascagni
  , "masse" -- Masse
  , "massenet" -- Massenet
  , "mattheson" -- Mattheson
  , "mcgranahan" -- McGranahan
  , "mehul" -- Mehul
  , "meirelez" -- Meirelez
  , "mencken" -- Mencken
  , "mendelssohn" -- Mendelssohn
  , "mercadante" -- Mercadante
  , "mertz" -- Mertz
  , "meyerbeer" -- Meyerbeer
  , "michaelis" -- Michaelis
  , "milhaud" -- Milhaud
  , "millocker" -- Millocker
  , "molina" -- Molina
  , "molique" -- Molique
  , "moncayo" -- Moncayo
  , "monckton" -- Monckton
  , "moniuszko" -- Moniuszko
  , "monn" -- Monn
  , "monti" -- Monti
  , "moretti" -- Moretti
  , "moscheles" -- Moscheles
  , "mosonyi" -- Mosonyi
  , "moszkowski" -- Moszkowski
  , "moulinie" -- Moulinie
  , "mouret" -- Mouret
  , "mozart" -- Mozart
  , "mussorgsky" -- Mussorgsky
  , "myaskovsky" -- Myaskovsky
  , "naderman" -- Naderman
  , "nascimbeni" -- Nascimbeni
  , "nazareth" -- Nazareth
  , "nebrablasco" -- Nebra Blasco
  , "nedbal" -- Nedbal
  , "nicolai" -- Nicolai
  , "nicolini" -- Nicolini
  , "nielsen" -- Nielsen
  , "novacek" -- Novacek
  , "nunesgarcia" -- Nunes Garcia
  , "ocarolan" -- Ocarolan
  , "offenbach" -- Offenbach
  , "oliveira" -- Oliveira
  , "onslow" -- Onslow
  , "pachelbel" -- Pachelbel
  , "pacini" -- Pacini
  , "paderewski" -- Paderewski
  , "paganini" -- Paganini
  , "paisiello" -- Paisiello
  , "paradis" -- Paradis
  , "paradisi" -- Paradisi
  , "parker" -- Parker
  , "parry" -- Parry
  , "pasquini" -- Pasquini
  , "pergolesi" -- Pergolesi
  , "perosi" -- Perosi
  , "persichetti" -- Persichetti
  , "pescetti" -- Pescetti
  , "peter" -- Peter
  , "petersonberger" -- Peterson-Berger
  , "pieczonka" -- Pieczonka
  , "pierpont" -- Pierpont
  , "pixis" -- Pixis
  , "platti" -- Platti
  , "pleyel" -- Pleyel
  , "ponce" -- Ponce
  , "ponchielli" -- Ponchielli
  , "poole" -- Poole
  , "popp" -- Popp
  , "poulenc" -- Poulenc
  , "prokofiev" -- Prokofiev
  , "prudent" -- Prudent
  , "pryor" -- Pryor
  , "puccini" -- Puccini
  , "purcell" -- Purcell
  , "quantz" -- Quantz
  , "quidant" -- Quidant
  , "quilter" -- Quilter
  , "rachmaninov" -- Rachmaninov
  , "raff" -- Raff
  , "rameau" -- Rameau
  , "rance" -- Rance
  , "ravel" -- Ravel
  , "ravina" -- Ravina
  , "redmer" -- Redmer
  , "reger" -- Reger
  , "reicha" -- Reicha
  , "reinecke" -- Reinecke
  , "respighi" -- Respighi
  , "reubke" -- Reubke
  , "rheinberger" -- Rheinberger
  , "rieding" -- Rieding
  , "rietz" -- Rietz
  , "rimskykorsakov" -- Rimsky-Korsakov
  , "rinaldi" -- Rinaldi
  , "riveking" -- Rive-King
  , "rodriges" -- Rodriges
  , "rodriguez" -- Rodriguez
  , "roman" -- Roman
  , "romberg" -- Romberg
  , "roncalli" -- Roncalli
  , "rosas" -- Rosas
  , "rossini" -- Rossini
  , "rubinstein" -- Rubinstein
  , "saintluc" -- Saint-Luc
  , "saintsaens" -- Saint-Saens
  , "salieri" -- Salieri
  , "sambucetti" -- Sambucetti
  , "sammartini" -- Sammartini
  , "sanz" -- Sanz
  , "sarasate" -- Sarasate
  , "satie" -- Satie
  , "scarlatti" -- Scarlatti
  , "scharwenka" -- Scharwenka
  , "scheidler" -- Scheidler
  , "schenker" -- Schenker
  , "schmelzer" -- Schmelzer
  , "scholtz" -- Scholtz
  , "schreiner" -- Schreiner
  , "schubert" -- Schubert
  , "schubertfrancios" -- SchubertFrancios
  , "schulhoff" -- Schulhoff
  , "schulz" -- Schulz
  , "schumann" -- Schumann
  , "scott" -- Scott
  , "scriabin" -- Scriabin
  , "seixas" -- Seixas
  , "serrano" -- Serrano
  , "sgambati" -- Sgambati
  , "shand" -- Shand
  , "shostakovich" -- Shostakovich
  , "sibelius" -- Sibelius
  , "sibencanin" -- Sibencanin
  , "sinding" -- Sinding
  , "smart" -- Smart
  , "smetana" -- Smetana
  , "smith" -- Smith
  , "sokolov" -- Sokolov
  , "soler" -- Soler
  , "somis" -- Somis
  , "sor" -- Sor
  , "sousa" -- Sousa
  , "souza" -- Souza
  , "spindler" -- Spindler
  , "spohr" -- Spohr
  , "stainer" -- Stainer
  , "stamitz" -- Stamitz
  , "stanely" -- Stanely
  , "stanford" -- Stanford
  , "steemson" -- Steemson
  , "steffani" -- Steffani
  , "steiner" -- Steiner
  , "strauss" -- Strauss
  , "straussi" -- Strauss I
  , "straussii" -- Strauss II
  , "strozzi" -- Strozzi
  , "sullivan" -- Sullivan
  , "suppe" -- Suppe
  , "taffanel" -- Taffanel
  , "taki" -- Taki
  , "talexy" -- Talexy
  , "tallard" -- Tallard
  , "tarrega" -- Tarrega
  , "tartini" -- Tartini
  , "tchaikovsky" -- Tchaikovsky
  , "tchesnokov" -- Tchesnokov
  , "telemann" -- Telemann
  , "thalberg" -- Thalberg
  , "thomas" -- Thomas
  , "torelli" -- Torelli
  , "translateur" -- Translateur
  , "tulou" -- Tulou
  , "turk" -- Turk
  , "umlauf" -- Umlauf
  , "vanhal" -- Vanhal
  , "vaughanwilliams" -- Vaughan Williams
  , "veracini" -- Veracini
  , "verdi" -- Verdi
  , "vierne" -- Vierne
  , "villalobos" -- Villa-Lobos
  , "vinci" -- Vinci
  , "viraldini" -- Viraldini
  , "visee" -- Visee
  , "vitali" -- Vitali
  , "vivaldi" -- Vivaldi
  , "vonbeliczay" -- Von Beliczay
  , "wagenseil" -- Wagenseil
  , "wagner" -- Wagner
  , "walckiers" -- Walckiers
  , "waldteufel" -- Waldteufel
  , "walker" -- Walker
  , "walthew" -- Walthew
  , "walton" -- Walton
  , "weber" -- Weber
  , "weckmann" -- Weckmann
  , "weiss" -- Weiss
  , "wesley" -- Wesley
  , "wfbach" -- WFBach
  , "whitaker" -- Whitaker
  , "widor" -- Widor
  , "wieniawski" -- Wieniawski
  , "wildman" -- Wildman
  , "willis" -- Willis
  , "witt" -- Witt
  , "wolf" -- Wolf
  , "wolff" -- Wolff
  , "woodcock" -- WoodCock
  , "xavier" -- Xavier
  , "yeatman" -- Yeatman
  , "yost" -- Yost
  , "yradier" -- Yradier
  , "zach" -- Zach
  , "zenetti" -- Zenetti
  , "zilcher" -- Zilcher
  , "zimmermann" -- Zimmermann
  , "zipoli" -- Zipoli
  ]

composerExclude :: [Text]
composerExclude = []

main :: IO ()
main = do
  putStrLn "theHarmonicAlgorithm: Populating Neo4j graph from YCACL artifact"
  dataset <- loadYCACLData ycaclArtifactPath
  let normalized      = normalizeComposers dataset
      activeComposers = filterComposers normalized
  putStrLn $ "Active composers: " ++ show (Map.size activeComposers)
  putStrLn "Raw YCACL coverage by composer:"
  logRawComposerStats activeComposers

  putStrLn "Deriving cadence sets per composer:"
  -- Map every composer's catalog into the duplicated cadence stream. Each
  -- `ChordSlice` sequence already carries exporter fundamentals.
  let composerCadences = Map.map (buildCadencesPerPiece . Map.elems) activeComposers
  logCadenceStats composerCadences

  putStrLn "Computing Markov transitions per composer:"
  let composerTransitions = Map.map transitionProbabilitiesH composerCadences
  logTransitionStats composerTransitions

  let composerKeys = Map.keys activeComposers
      edges        = mergeComposerTransitions composerTransitions composerKeys

  pipe <- connectNeo4j
  -- Truncate the cadence subgraph each run so composer-specific MERGEs remain
  -- deterministic; APOC handles the heavy lifting in 5k batches.
  putStrLn "Clearing existing cadences from Neo4j (batched via APOC)..."
  Bolt.run pipe truncateCadenceGraph
  Bolt.run pipe initGraph
  putStrLn $ "Writing " ++ show (length edges) ++ " transitions into Neo4j..."
  Bolt.run pipe (writeCadenceEdges edges)
  putStrLn $ "Wrote " ++ show (length edges) ++ " transitions"

normalizeComposers :: YCACLData -> ComposerPieces
normalizeComposers dataset = foldl' insertComposer Map.empty (Map.toList dataset)
  where
    insertComposer acc (composer, pieces) =
      let key = slug composer
       in Map.insertWith (Map.unionWith (++)) key pieces acc

filterComposers :: ComposerPieces -> ComposerPieces
filterComposers dataset = Map.filterWithKey keep dataset
  where
    includes = if null composerInclude then const True else (`elem` composerInclude)
    keep key _ = includes key && key `notElem` composerExclude

-------------------------------------------------------------------------------
-- Local Markov functions using H.Cadence (replacing Harmonic.Analysis.Markov)
-- These will be migrated to Markov.hs in a later step
-------------------------------------------------------------------------------

type HTransitionCounts = Map HEdge Double
type HTotals = Map H.Cadence Double

transitionCountsH :: [H.Cadence] -> HTransitionCounts
transitionCountsH cadences =
  foldl' insertEdge Map.empty (zip cadences (drop 1 cadences))
  where
    insertEdge acc edge = Map.insertWith (+) edge 1 acc

transitionProbabilitiesH :: [H.Cadence] -> Map HEdge Double
transitionProbabilitiesH cadences =
  let counts = transitionCountsH cadences
      totals = buildTotals counts
   in Map.mapWithKey (normalise totals) counts
  where
    buildTotals :: HTransitionCounts -> HTotals
    buildTotals = foldl' accumulate Map.empty . Map.toList
      where
        accumulate acc ((from,_), weight) = Map.insertWith (+) from weight acc

    normalise :: HTotals -> HEdge -> Double -> Double
    normalise totals (from, _) weight =
      case Map.lookup from totals of
        Just total | total > 0 -> weight / total
        _                     -> 0

-------------------------------------------------------------------------------
-- Composer Transition Merging
-------------------------------------------------------------------------------

mergeComposerTransitions
  :: Map Text (Map HEdge Double)
  -> [Text]
  -> [(H.Cadence, H.Cadence, ComposerWeights)]
mergeComposerTransitions transitionMaps selected =
  let merged       = Map.foldlWithKey' accumulate Map.empty transitionMaps
      completeKeys = if null selected then Map.keys transitionMaps else selected
   in [ (from, to, ensureAll completeKeys weights)
      | ((from, to), weights) <- Map.toList merged
      , sum (Map.elems weights) > 0
      ]
  where
    accumulate acc composer edgeMap =
      foldl' (insertWeight composer) acc (Map.toList edgeMap)

    insertWeight composer acc (edge, weight) =
      Map.insertWith (Map.unionWith (+)) edge (Map.singleton composer weight) acc

    ensureAll composers weights = foldl' ensure weights composers
    ensure mp name = Map.insertWith (const id) name (0 :: Double) mp

-------------------------------------------------------------------------------
-- Logging Utilities
-------------------------------------------------------------------------------

logRawComposerStats :: ComposerPieces -> IO ()
logRawComposerStats composers =
  forM_ (Map.toList composers) $ \(name, pieces) ->
    let pieceCount = Map.size pieces
        chordCount = sum (map length (Map.elems pieces))
     in putStrLn $ "  - " ++ T.unpack name ++ ": " ++ show pieceCount ++ " pieces / " ++ show chordCount ++ " chord events"

logCadenceStats :: Map Text [H.Cadence] -> IO ()
logCadenceStats cadencesMap =
  forM_ (Map.toList cadencesMap) $ \(name, cadences) ->
    putStrLn $ "    cadences -> " ++ T.unpack name ++ ": " ++ show (length cadences)

logTransitionStats :: Map Text (Map HEdge Double) -> IO ()
logTransitionStats transitionMap =
  forM_ (Map.toList transitionMap) $ \(name, transitions) ->
    putStrLn $ "    transitions -> " ++ T.unpack name ++ ": " ++ show (Map.size transitions)

slug :: Text -> Text
slug = sanitize . T.toLower
  where
    sanitize = T.map replaceChar . T.filter validChar
    validChar c = isAlphaNum c || c == ' '
    replaceChar c
      | isAlphaNum c = c
      | otherwise    = '_'
