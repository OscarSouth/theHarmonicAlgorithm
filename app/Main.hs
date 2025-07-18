{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib

import           Control.Monad.Reader
import           System.IO

import qualified Language.R.Instance  as R
import qualified Language.R.Literal   as R
import qualified Language.R.QQ        as QQ

import qualified Data.Char            as Char (isAlphaNum, toLower)
import           Data.Function        (on)
import qualified Data.List            as List (sort, sortBy, zip4, zip5, zip7, isInfixOf, (\\))
import           Data.Map             (Map)
import qualified Data.Map             as Map (toList, fromList, lookup, keys, assocs)
import           Data.Maybe           (fromMaybe)
import           Text.Read            (readMaybe)
import           Data.List.Split      (chunksOf)

---- |copilot suggestions
--import Control.Parallel.Strategies (parMap, rseq)
--import qualified Data.Set as Set

-- GraphDB stuff --
import qualified Database.Bolt as Bolt

import Data.Default
import qualified Data.Text as Text
import Control.Monad
import Control.Monad.Except

import qualified Sound.Tidal.Context as Tidal


-- |entrypoint of the program
-- ask user if they want to reload the graph,
-- if "y" reload the graph and then runs the main program,
-- if anything other than "y" directly run the main program
main = do
  -- Prompt the user to reload the graph
  putStrLn "reload graph? (y/n, default n)"
  putStr ">> "
  hFlush stdout
  choice <- getLine
  case choice of
    "y" -> do
      -- If the user chooses to reload the graph
      putStrLn "reloading"
      reloadGraph
      putStrLn "complete"
--      runMain
--    _ -> runMain
  where
    -- |'reloadGraph' function builds the graph used as a persistance layer
    -- ot starts an embedded R instance, initializes R, computes and binds the trained model,
    -- connects to the Bolt database, runs the 'initGraph' function, writes nodes to the database,
    -- and connects nodes in the database.
    reloadGraph = do
      R.withEmbeddedR R.defaultConfig $ do
        initR -- load R libraries & settings, initialise R log, print info to stout
        putStrLn "binding model"
        model <- choraleData -- compute and bind trained model
        putStrLn "cleaning R environment"
        cleanEnvironmentR -- remove unneeded data from R environment
        putStrLn "initialising graph"
        pipe <- Bolt.connect $ def { Bolt.version = 3 }
        Bolt.run pipe initGraph
        putStrLn "generating nodes"
        forM_ (Map.keys model) (\keys -> Bolt.run pipe $ cadenceToNode keys)
        putStrLn "generating relationships"
        forM_ (Map.assocs model) (\assocs -> Bolt.run pipe $ connectNodes assocs)
        return ()
    -- |'runMain' function initialises the main program.
    -- ot starts an embedded R instance, initializes R, retrieves the serialized model from the graph and binds it,
    -- prints the main title, welcomes the user, and enters the ReaderT (Model) monad with the trained model.
    runMain = do
      R.withEmbeddedR R.defaultConfig $ do
        initR -- load R libraries & settings, initialise R log, print info to stout
        model <- modelFromGraph -- retrieve serialised model from graph and bind
        header -- print main title
        putStrLn "Welcome to The Harmonic Algorithm!\n"
        runReaderT loadLoop model -- enter ReaderT (Model) monad with trained model
        return ()


-- |initialise and prepare the graph database for use
initGraph :: Bolt.BoltActionT IO ()
initGraph = do
  liftIO $ putStrLn "preparing graph"
  Bolt.query "MATCH (n) DETACH DELETE n"
  liftIO $ putStrLn "dropped nodes and relationships"
  Bolt.query "CALL apoc.schema.assert({}, {})"
  liftIO $ putStrLn "dropped constraints"
  Bolt.query "CREATE CONSTRAINT ON (n:Cadence) ASSERT n.show IS UNIQUE"
  liftIO $ putStrLn "created constraints"
  return ()


-- |create a node representing a cadence in the graph database
cadenceToNode :: Cadence -> Bolt.BoltActionT IO ()
cadenceToNode cadence = do
  liftIO $ putStrLn ("writing node " ++ show cadence)
  let (m,c)      = deconstructCadence cadence
      dissonance = fst $ dissonanceLevel (i <$> c)
  Bolt.query $ Text.pack ("CREATE (n:Cadence {show: '"++ show cadence ++"',\
                                    \movement: '"++ show m ++"' ,\
                                    \chord: '"++ show c ++"' ,\
                                    \dissonance: '"++ show dissonance ++"' })")
  return ()


-- |construct links between Cadence nodes in the graph database
connectNodes :: (Cadence, [(Cadence, Double)]) -> Bolt.BoltActionT IO ()
connectNodes (from, rels) = do
  let relationships = [ x | x <- rels, snd x > 0]
--  let relationships = rels -- write a function to programmatically determine likely cadences
  liftIO $ putStrLn ("writing relationships for " ++ show from)
  forM_ relationships (\(to, conf) -> Bolt.query $
              Text.pack ("MATCH (from: Cadence{show: '"++ show from ++"'}) \
                         \MATCH (to: Cadence{show: '"++ show to ++"'}) \
                         \CREATE (from)-[r: NEXT {confidence: "++ show conf ++" }]->(to) "))
  return ()


-- |retrieve all the Cadence nodes from the graph database
getNodesFromGraph :: Bolt.BoltActionT IO [Cadence]
getNodesFromGraph = do
  records <- Bolt.query "MATCH (n:Cadence) RETURN n.movement, n.chord"
  m <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n.movement")
  c <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n.chord")
  let cadencesFrom = constructCadence <$> zip m c
  return cadencesFrom


-- |retrieve all the relationships between cadences from the graph database
getRelsFromGraph :: Cadence -> Bolt.BoltActionT IO [(Cadence, Double)]
getRelsFromGraph cadence = do
  records <- Bolt.query $ Text.pack ("MATCH (from:Cadence{show:'"++ show cadence ++"'})-[r]->(n) \
                                \RETURN r.confidence, n.movement, n.chord \
                                \ORDER BY r.confidence DESC")
  p <- forM records $ (\record -> record `Bolt.at` "r.confidence")
  m <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n.movement")
  c <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n.chord")
  let cadencesFrom = constructCadence <$> zip m c
  let cadencesProportions = zip cadencesFrom p
  return cadencesProportions


-- |retrieve static model from graph
modelFromGraph :: IO MarkovMap
modelFromGraph = do
  uciRef -- print dataset source reference
  pipe <- Bolt.connect $ def { Bolt.version = 3 }
  cadences <- Bolt.run pipe getNodesFromGraph
  cadenceRels <- forM cadences $ \cadence -> Bolt.run pipe $ getRelsFromGraph cadence
  let fillerSets = (\filters -> zip (cadences List.\\ filters) (repeat 0 :: [Double])) <$> (fmap fst <$> cadenceRels)
  let markovValues = ((\(ml, fill) -> ml ++ fill) <$> (zip cadenceRels fillerSets))
  let model = Map.fromList $ zip cadences markovValues :: MarkovMap
--  forM_ model print
  return model


---- |script directing process of loading & transforming data then training model
--choraleData :: IO MarkovMap
--choraleData = do
--  uciRef -- print dataset source reference
----  bachData -- execute R script to preprocess data
--  pitchData -- execute R script to load extended dataset
--  chFunds <- bachFundamental -- retrieve and bind R column of fundamental notes
--  x1 <- fromRMatrix 1 -- retrieve and bind columns from R matrix
--  x2 <- fromRMatrix 2 -- |
--  x3 <- fromRMatrix 3 -- |
--  x4 <- fromRMatrix 4 -- |
--  x5 <- fromRMatrix 5 -- V
--  let model = markovMap $ -- call Markov module to train model
--        fmap toCadence <$> -- map bigram sets into Cadence data types
--        bigrams $ -- combine chords into sequential bigrams
--        flatTriad . -- convert to 'Chord' data type
--        mostConsonant . possibleTriads'' <$> -- derive most suitable triad
--        filter (\(_, ys) -> length ys >= 3) ( -- remove sets of less than 3
--        zip chFunds $ -- zip with fundamentals R column
--        fmap round . unique <$> -- remove duplicate elems . convert to Integer
--        [[a,b,c,d,e] | (a,b,c,d,e) <- List.zip5 x1 x2 x3 x4 x5]
--        ) -- ^ convert R matrix columns to a list of lists
--  return model


-- |script directing process of loading & transforming data then training model
choraleData :: IO MarkovMap
choraleData = do
--  uciRef -- print dataset source reference
  pitchData -- execute R script to load extended dataset
  x1 <- fromRMatrix 1 -- retrieve and bind columns from R matrix __
  x2 <- fromRMatrix 2 --                                           |
  x3 <- fromRMatrix 3 --                                           |
  x4 <- fromRMatrix 4 --                                           |
  x5 <- fromRMatrix 5 --                                           |
  x6 <- fromRMatrix 6 --                                           |
  x7 <- fromRMatrix 7 --                                           |
--  x8 <- fromRMatrix 8 --                                           |
--  x9 <- fromRMatrix 9 --                                           |
--  x10 <- fromRMatrix 10 --                                         |
--  x11 <- fromRMatrix 11 --                                         |
--  x12 <- fromRMatrix 12 --                                         v
  let model = markovMap $ -- call Markov module to train model
        fmap toCadence <$> -- map bigram sets into Cadence data types
        bigrams $ -- combine chords into sequential bigrams
        flatTriad . -- convert to 'Chord' data type
        mostConsonant . possibleTriads'' <$> -- derive most suitable triad
        filter (\(_, ys) -> length ys >= 3) ( -- remove sets of less than 3
        zip (round <$> x1) $ -- zip with fundamentals
        fmap round . unique <$> -- remove duplicate elems . convert to Integer
--        [[a,b,c,d,e,f,g,h,i,j,k,l] | (a,b,c,d,e,f,g,h,i,j,k,l) <- zip12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]
        [[a,b,c,d,e,f,g] | (a,b,c,d,e,f,g) <- List.zip7 x1 x2 x3 x4 x5 x6 x7]
        ) -- ^ convert R matrix columns to a list of lists
  return model


-- |type synonyms for readability
type Model a = ReaderT MarkovMap IO a -- representation of trained model
type Enharmonic = String -- string reference for representation of enharmonic (♭♯) preference
--type EnharmonicFunction = (PitchClass -> NoteName) -- enharmonic (♭♯) preference function
type Root = PitchClass -- representation of current root note
type Filters = (EnharmonicFunction -> [Chord]) -- partially applied filter results


-- |entry to 'interactive' environment for working with the trained model
loadLoop :: Model ()
loadLoop = do
  liftIO $ putStrLn "Do you want to begin with ♭ (flat) or ♯ (sharp) notation?\n"
  enharmonic <- flatSharp
  liftIO $ putStrLn "\nSelect starting root note:\n"
  root <- initFundamental enharmonic
  liftIO $ putStrLn "\nSelect starting functionality:\n"
  chord <- chooseFunctionality enharmonic root
  let cadence = toCadence (chord, chord)
  filters <- harmonicFilters
  markovLoop enharmonic root cadence filters 14
  return ()


-- |returns a String to be used as a lookup for choosing 'enharmonic' function
flatSharp :: Model String
flatSharp = do
  let enh  = ["Flat ♭", "Sharp ♯"]
      opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] enh
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO getLine
  if notElem num $ fmap show [1..(length opts)]
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      flatSharp
      else do
        let index      = read num - 1 :: Int
            enharmonic = takeWhile Char.isAlphaNum $ Char.toLower <$> enh!!index
        return enharmonic


-- |returns a PitchClass to designate starting root note
initFundamental :: String -> Model PitchClass
initFundamental k = do
  let pcs  = show . enharmMap k <$> [P 0 .. P 11]
      opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] pcs
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO getLine
  if notElem num $ fmap show [1..(length opts)]
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      initFundamental k
      else do
        let index = read num - 1 :: Int
        return (pc index)


-- |returns a Chord to designate starting functionality over root
chooseFunctionality :: String -> PitchClass -> Model Chord
chooseFunctionality k r = do
  let opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] initFcList
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO getLine
  if notElem num $ fmap show [1..(length opts)]
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      chooseFunctionality k r
      else do
        let index = read num - 1 :: Int
            chord = toTriad (enharmMap k) $ -- make correctly enharmonic Chord
                    (+ i r) <$> -- transpose structure to meet fundamental note
                    fromMaybe [0,4,7] ( -- extract Map lookup from Maybe
                    Map.lookup (initFcList!!index) initFcMap
                    ) -- ^ extract choice from Map
        return chord


-- |interactive dialogue for selecting tuning/key/roots filters
harmonicFilters :: Model Filters
harmonicFilters = do
  liftIO $ putStrLn "\nEnter tuning (with strings separated by spaces) or * for chromatic:"
  liftIO prompt
  getTuning <- liftIO getLine
  let tuning = parseNotes getTuning
  -- let tuning = parseTuning getTuning -- FOR USE WITH POLYPHONIC OVERTONE INSTRUMENTS
  liftIO $ putStrLn "\nEnter upper structure key signature (eg. bbb, ##, 2b, 0#) or * :"
  liftIO prompt
  getKey <- liftIO getLine
  let overtones = filter (\x -> x `elem` parseKey getKey) tuning
  liftIO $ putStrLn "\nEnter desired 'next' root notes, a key signature, or * :"
  liftIO prompt
  getFunds <- liftIO getLine
  let roots = parseFunds getFunds
      filters = chordList' 3 roots overtones
  return filters


-- |function to retrieve and filter down possibilities based on current state
recommendations :: Enharmonic -> Root -> Cadence -> Filters -> Int -> Model [Cadence]
recommendations fs root prev filters n = do
  model <- ask
  let enharm = enharmMap fs -- extract enharmonic 'key' into function
      hAlgo = (\xs -> [ toCadence (transposeCadence enharm root prev, nxt)
              | nxt <- xs ]) $ -- ^ Convert list of Chords into Cadences from last state
              List.sortBy (compare `on` (\(Chord (_,x)) -> -- sort by dissonance level
              fst . dissonanceLevel $ x)) $ filters enharm -- get values from Filters
      bach  = filter (\(x,_) -> x `elem` hAlgo) $ -- remove elements not in Filters
              List.sortBy (compare `on` (\(_,x) -> 1-x)) $ -- sort by markov probability
              fromMaybe [(prev, 1.0)] $ -- extract Cadence list from maybe
              Map.lookup prev model -- extract current markov state from model
      nexts = take n $ (fst <$> bach) ++ -- append to markov list and take n
              filter (\x -> x `notElem` fmap fst bach) hAlgo
              -- ^ keep elements of Filters list not in markov list
  return nexts


-- |interactive loop in which most of the user interaction takes place
markovLoop :: Enharmonic -> Root -> Cadence -> Filters -> Int -> Model ()
markovLoop fs root prev filters n = do
  nexts <- recommendations fs root prev filters n
  let enharm = enharmMap fs
      choose = ["[       Modify filter       ]",
                "[      Random sequence      ]",
                if n == 14 then "[         Show more         ]"
                else "[         Show less         ]",
                if fs == "sharp" then "[  Switch to flat notation  ]"
                else "[ Switch to sharp notation  ]",
                "[ Select new starting chord ]",
                "[           Quit            ]"]
      menu   = (showTriad enharm . fromCadence enharm root <$>
                nexts) ++ choose
      opts    = zipWith (\n p ->
                (if n < 10 then show n ++ " " else show n) ++ " - " ++ p)
                [1..] menu
  liftIO $ putStrLn $ "\nThe current chord is " ++
           showTriad enharm (transposeCadence enharm root prev) ++
           " -- Select next chord or choose another option:\n"
  num <- liftIO $ mapM_ putStrLn opts >> prompt >> getLine
  let index = read num - 1 :: Int
  if notElem num $ fmap show [1..length opts]
    then do
    liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
    markovLoop fs root prev filters n
    else if index == length nexts
      then do
      filters' <- harmonicFilters
      markovLoop fs root prev filters' n
      else if index == 1 + length nexts
        then if length opts == length choose
          then do
            liftIO $ putStrLn
              "\nCannot generate from empty set of possibilities.\n\
               \Adjust filters or choose another option:"
            markovLoop fs root prev filters n
            else getSeqParams fs root prev filters n
          else if index == 2 + length nexts
            then do
            liftIO $ putStrLn ""
            markovLoop fs root prev filters $ if n == 14 then 29 else 14
            else if index == 3 + length nexts
              then do
              liftIO $ putStrLn ""
              markovLoop (if fs == "flat" then "sharp"
                          else "flat") root prev filters n
              else if index == 4 + length nexts
                then do
                liftIO $ putStrLn ""
                loadLoop
                else if index == 5 + length nexts
                  then do
                  liftIO $ putStrLn exitText
                  return ()
                  else do
                  let next = nexts!!index
                      root' = root + movementFromCadence next
                  liftIO $ putStrLn ""
                  markovLoop fs root' next filters n
  return ()


getSeqParams :: Enharmonic -> Root -> Cadence -> Filters -> Int -> Model ()
getSeqParams fs root prev filters n = do
  liftIO $ putStrLn "\nEnter desired length of sequence (default 4, max 16):"
  len <- do
    liftIO prompt
    getLen <- liftIO getLine
    let readLen = fromMaybe 4 (readMaybe getLen :: Maybe Double)
    if readLen >= 16 then return 16
    else if readLen <= 0 then return 4
    else return readLen
  liftIO $ putStrLn "\nChoose entropy level as a number between 1 and 10 (default 2):"
  entropy <- do
    liftIO prompt
    getEntropy <- liftIO getLine
    let readEntropy = fromMaybe 2 (readMaybe getEntropy :: Maybe Double)
    if readEntropy >= 10 then return 1 else return (readEntropy/10)
  randomSeq fs root prev filters n (len, entropy)
  return ()


-- |interactive loop for generating random sequences
randomSeq :: Enharmonic -> Root -> Cadence -> Filters -> Int -> (Double, Double)
          -> Model ()
randomSeq fs root prev filters n seqParams = do
  let (len, entropy) = seqParams
  rns <- liftIO $ gammaGen len entropy
  cadences <- cadenceSeq fs root prev filters rns
  let chordLen  = length $ chords fs :: Int
      chords fs = showTriad (enharmMap fs) <$> fst cadences
      lines     = (++"|   ") . concat . (`replicate`" ") .
                  ((14-) . length) <$> chords fs
      fours fs  = concat $ zipWith (++)
                  ["\n1   ||   ", "\n5    |   ", "\n9    |   ", "\n13   |   "]
                  (init . init . init <$> (fmap concat <$> chunksOf 4 $
                  zipWith (++) (chords fs) lines))
  liftIO $ putStr "\nPress enter to accept, \
           \specify a bar number, or choose another option: \n"
        >> hFlush stdout
  liftIO $ putStr (fours fs) >> putStrLn "|\n"
  let menu = ["[      Reject sequence      ]",
              "[    Regenerate sequence    ]",
              if fs == "sharp" then "[  Show with flat notation  ]"
              else "[ Show with sharp notation  ]"]
      opts = zipWith (\n p ->
                  (if n < 10 then show n ++ " " else show n) ++ " - " ++ p)
                  [1+length (chords fs)..]
      actions ls = do
        num <- liftIO $ mapM_ putStrLn ls >> prompt >> getLine
        let index | num == "" = -1
                  | otherwise = read num - 1 :: Int
        if index == (-1)
          then markovLoop fs (rootNote' $ last $ fst cadences)
                             (last $ snd cadences) filters n
          else if notElem num $
                  fmap show [1..chordLen + length ls]
            then do
              liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
                    >> putStr (fours fs) >> putStrLn "|\n"
              actions $ opts menu
              else if index == chordLen
                then markovLoop fs root prev filters n
                else if index == 1 + chordLen
                  then randomSeq fs root prev filters n seqParams
                  else if index == 2 + chordLen
                    then do
                    liftIO $ putStrLn "\nUnrecognised input, please retry:"
                          >> putStr (fours
                             (if fs == "flat" then "sharp" else "flat"))
                          >> putStrLn "|\n"
                    actions $ opts ["[      Reject sequence      ]",
                                    "[    Regenerate sequence    ]"]
                    else do
                    let root' = rootNote' $ fst cadences!!index
                        next = snd cadences!!index
                    markovLoop fs root' next filters n
  actions $ opts menu
  return ()


-- |function to retrieve and return random sequence
cadenceSeq :: Enharmonic -> Root -> Cadence -> Filters -> [Integer]
              -> Model ([Chord], [Cadence])
cadenceSeq _ _ c _ [] = return ([], [])
cadenceSeq fs root prev filters rns@(x:xs) = do
  let enharm = enharmMap fs
      x' = fromIntegral x
  nexts <- recommendations fs root prev filters (length rns)
  let next = if x' >= length rns then last nexts else nexts!!x'
      triad = transposeCadence enharm root prev
      root' = root + movementFromCadence next
  nexts <- cadenceSeq fs root' next filters xs
  return (triad : fst nexts, prev : snd nexts)


-- |mapping from string to 'enharmonic' function
enharmMap :: MusicData a => String -> (a -> NoteName)
enharmMap key =
  let funcMap = Map.fromList [("flat", flat), ("sharp", sharp)]
   in fromMaybe flat $ Map.lookup key funcMap


-- |mapping from string to Integral pitchclass set representation
initFcMap :: (Integral a, Num a) => Map String [a]
initFcMap = Map.fromList
  [("maj",[0,4,7]),
  ("min",[0,3,7]),
  ("maj 1stInv",[0,3,8]),
  ("min 1stInv",[0,4,9]),
  ("maj 2ndInv",[0,5,9]),
  ("min 2ndInv",[0,5,8]),
  ("dim",[0,3,6]),
  ("aug",[0,4,8]),
  ("sus2",[0,2,7]),
  ("sus4",[0,5,7]),
  ("7sus4no5",[0,5,10]),
  ("min7no5",[0,3,10]),
  ("7no3",[0,7,10]),
  ("7no5",[0,4,10])]


-- |list of options for starting functionality
initFcList :: [String]
initFcList =
  ["maj",
  "maj 1stInv",
  "maj 2ndInv",
  "min",
  "min 1stInv",
  "min 2ndInv",
  "dim",
  "aug",
  "sus2",
  "sus4",
  "7sus4no5",
  "min7no5",
  "7no3",
  "7no5"]


-- |Initialise R + session log, load libraries/set options & log session info
initR :: IO ()
initR = do
  putStrLn "\nInitialising R Interpreter..\n"
  loadPackages
  putStrLn ""
  -- initLogR
  -- putStrLn "session will be logged:"
  -- rDir >>= putStr
  -- putStrLn "/output/sessionlog.txt\n"
  return ()


-- |initialise R session log
initLogR :: IO ()
initLogR  = R.runRegion $ do
  [QQ.r| sink("output/sessionlog.txt")
      cat("=======================================\n")
      cat("The Harmonic Algorithm\nSession: ")
      print(Sys.time())
      cat("=======================================\n")
      cat("\n")
      print(sessionInfo())
    |]
  return ()


-- |Retrieve R working directory
rDir :: IO String
rDir =
  let rData () = R.fromSomeSEXP <$> [QQ.r| getwd() |]
   in R.runRegion $ rData ()


-- |print out main title
header :: IO ()
header  = do
  putStrLn "\n___________________________________________________________________________\n"
  putStrLn "\n\n\n"
  putStrLn "  .___________________________________________."
  putStrLn "  |__/___\\_.___The______________________._____|"
  putStrLn "  |__\\___|_.______Harmonic_____________/|_____|"
  putStrLn "  |_____/______________Algorithm______/_|_____|"
  putStrLn "  |____/_____________________________|__|_____|"
  putStrLn "                                     |-()-"
  putStrLn "                                 by  |"
  putStrLn "                                   -()-scar South, 2018 "
  putStrLn "\n\n\n"
  return ()


-- |print out reference to dataset source
uciRef :: IO ()
uciRef  = do
    putStrLn "Loading Bach Chorale Dataset from UCI Machine Learning Repository...\n"
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "| Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository |"
    putStrLn "| [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,   |"
    putStrLn "| School of Information and Computer Science.                              |"
    putStrLn "+--------------------------------------------------------------------------+\n"
    return ()


-- |print prompt for user input
prompt :: IO ()
prompt = do
  putStr "\n>> "
  hFlush stdout
  return ()


-- |load R packaged and options
loadPackages :: IO ()
loadPackages = R.runRegion $ do
  [QQ.r| options(warn = -1)
      library("tidyverse")
    |]
  return ()


-- |R script to ingest and process raw data to be passed to Haskell
bachData :: IO ()
bachData = R.runRegion $ do
  [QQ.r| bach <- read_csv("/home/oscarsouth/.stack/global-project/data/jsbach_chorals_harmony.data",
                 col_names = c(
                   "seq", "event",
                   "0", "1", "2", "3", "4", "5",
                   "6", "7", "8", "9", "10", "11",
                   "fund", "acc", "label"
                 ), cols(
                   seq = col_character(),
                   event = col_integer(),
                   `0` = col_character(),
                   `1` = col_character(),
                   `2` = col_character(),
                   `3` = col_character(),
                   `4` = col_character(),
                   `5` = col_character(),
                   `6` = col_character(),
                   `7` = col_character(),
                   `8` = col_character(),
                   `9` = col_character(),
                   `10` = col_character(),
                   `11` = col_character(),
                   fund = col_character(),
                   acc = col_integer(),
                   label = col_character()
                 )
               )

    bach <-
      bach %>%
        select(seq, event, fund, acc, label) %>%
        add_column(pitch = bach %>%
                     select(`0`:`11`) %>%
                     t() %>%
                     as.data.frame() %>%
                     unname() %>%
                     map(function(x) str_which(x, "YES")-1)
                  )

    pitchMatrix <<-
      reduce(bach$pitch,
             rbind,
               matrix(,0,bach$pitch %>%
                         map(length) %>%
                         rapply(c) %>%
                         max()
                     )
             ) %>%
      unname()

    fund <<- bach$fund

    |]
  return ()

-- |R script to ingest and process raw data to be passed to Haskell
pitchData :: IO ()
pitchData = R.runRegion $ do
  [QQ.r|

    load("/home/os/.stack/global-project/data/pitchMatrix.R")

    pitchMatrix <<- pitchMatrix

    |]
  return ()

-- |R script to remove unneeded data from memory
cleanEnvironmentR :: IO ()
cleanEnvironmentR = R.runRegion $ do
  [QQ.r|

    if (exists("pitchMatrix", envir = .GlobalEnv)) {
      rm(pitchMatrix, envir = .GlobalEnv)
    }

    gc()

    |]
  return ()


-- |helper function to extract R matrix column from R and deliver to Haskell
fromRMatrix  :: Double -> IO [Double]
fromRMatrix x =
  let rData x = R.fromSomeSEXP <$> [QQ.r| pitchMatrix[,x_hs] |]
   in R.runRegion $ rData x


---- |helper function to extract vector of fundamental notes from R into Haskell
--bachFundamental  :: IO [Double]
--bachFundamental =
--  let rData () = R.fromSomeSEXP <$> [QQ.r| pitchMatrix[,1] |]
--   in R.runRegion $ rData ()


-- appendLogR :: IO ()
-- appendLogR  = runRegion $ do
--   [r| sink("output/output.txt", append=TRUE)
--       cat("Some more stuff here...\n")
--     |]
--   return ()

-- plotR :: IO ()
-- plotR  = runRegion $ do
--   [r| p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
--       ggsave(filename="output/plots/plot.png", plot=p,
--               width=4, height=4, scale=2)
--     |]
--   return ()


-- |string to be printed on app exit
exitText :: String
exitText =
  "\n\n\n\
  \Thanks for using The Harmonic Algorithm!\n\
  \\n\
  \The Harmonic Algorithm, written in Haskell and R, generates musical\n\
  \domain specific data inside user defined constraints then filters it\n\
  \down and deterministically ranks it using a tailored Markov Chain\n\
  \model trained on ingested musical data. This presents a unique tool\n\
  \in the hands of the composer or performer which can be used as a\n\
  \writing aid, analysis device or even in live performance.\n\
  \\n\
  \The Harmonic Algorithm is currently in active development.\n\
  \Keep checking back and don't hesitate to get in touch via the\n\
  \repository's 'Issues' section:\n\
  \https://github.com/OscarSouth/theHarmonicAlgorithm/issues\n\
  \\n\
  \Or the contact form for my main performance project: \n\
  \https://UDAGANuniverse.com/contact\n\
  \\n\
  \Oscar\n\
  \"


-- |wrapper for the 'rgamma' R function
gammaDist :: Double -> Double -> IO [Double]
gammaDist n x =
  let rData () = R.fromSomeSEXP <$> [QQ.r| rgamma(n_hs, x_hs) |]
   in R.runRegion $ rData ()


-- |function to deliver 'rgamma' data in integer form with a tailored scale
gammaGen :: Double -> Double -> IO [Integer]
gammaGen n x = do
  let entropy | x >=1 = 8 | x <= 0 = 0 | otherwise = (7+x)*x
  rand <- gammaDist n entropy
  return (floor <$> rand)


-- |function that returns required data (tupled) for generating random sequences
randomGen :: (Num a, Integral a) =>
             Double -> Double -> [a] -> IO (PitchClass, Cadence, [Integer])
randomGen n x c = do
  let gamma = gammaGen (n+2) x
  rns <- gamma
  let motion = 5*(head rns - rns!!1) `mod` 12
      c' = (+motion) . fromIntegral <$> c
      start = toCadence (toTriad flat c', toTriad flat c)
      xs = drop 2 rns
      root = pc $ head c
  return (root, start, xs)

---------------------
-- for live coding

-- |retrieve a sequence of 4 Cadences from the graph database
retrieveSeq4 :: Bolt.BoltActionT IO [Cadence]
retrieveSeq4 = do
  records <- Bolt.query $ Text.pack " \
  \ match (n:Cadence) \
  \ with apoc.coll.randomItem(COLLECT(n)) AS n_0 \
  \ call { \
  \ with n_0 \
  \ call { \
  \ with n_0 \
  \ match (n_0)-[r]->(n_1) \
  \ return r, n_1 \
  \   order by (r.confidence*rand()) desc \
  \   limit 3 \
  \ union all \
  \ with n_0 \
  \ match (n_0)-[r]->(n_1) \
  \ return r, n_1 \
  \   order by r.confidence desc \
  \   limit 1 \
  \ } \
  \ return n_1 \
  \   order by rand() \
  \   limit 1 \
  \ } \
  \ call { \
  \ with n_1 \
  \ call { \
  \ with n_1 \
  \ match (n_1)-[r]->(n_2) \
  \ return r, n_2 \
  \   order by (r.confidence*rand()) desc \
  \   limit 3 \
  \ union all \
  \ with n_1 \
  \ match (n_1)-[r]->(n_2) \
  \ return r, n_2 \
  \   order by r.confidence desc \
  \   limit 1 \
  \ } \
  \ return n_2 \
  \   order by rand() \
  \   limit 1 \
  \ } \
  \ call { \
  \ with n_2 \
  \ call { \
  \ with n_2 \
  \ match (n_2)-[r]->(n_3)-->(n0) \
  \ return r, n_3 \
  \   order by (r.confidence*rand()) desc \
  \   limit 3 \
  \ union all \
  \ with n_2 \
  \ match (n_2)-[r]->(n_3)-[r_last]->(n0) \
  \ return r, n_3 \
  \   order by r.confidence desc, r_last.confidence desc \
  \   limit 1 \
  \ } \
  \ return n_3 \
  \   order by rand() \
  \   limit 1 \
  \ } \
  \ return \
  \   n_0.chord, n_0.movement, \
  \   n_1.chord, n_1.movement, \
  \   n_2.chord, n_2.movement, \
  \   n_3.chord, n_3.movement; \
  \ "
  m0 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_0.movement")
  c0 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_0.chord")
  m1 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_1.movement")
  c1 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_1.chord")
  m2 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_2.movement")
  c2 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_2.chord")
  m3 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_3.movement")
  c3 <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "n_3.chord")
  let cadence0 = head (constructCadence <$> zip m0 c0) :: Cadence
  let cadence1 = head (constructCadence <$> zip m1 c1) :: Cadence
  let cadence2 = head (constructCadence <$> zip m2 c2) :: Cadence
  let cadence3 = head (constructCadence <$> zip m3 c3) :: Cadence
  return $ cadence0 : cadence1 : cadence2 : cadence3 : []


-- |generate a sequence of Cadences
prog4 :: (Show a, Num a, Integral a) => IO [[a]]
prog4 = do
  pipe <- Bolt.connect $ def { Bolt.version = 3 }
  cadences <- Bolt.run pipe retrieveSeq4
  let roots = take 4 $ progRoots' 0 $ cycle cadences
  let chords = (\(p,c) -> fromCadence' p c) <$> zip roots (cycle cadences)
  let labels = show . toEnhTriad <$> chords
  let promptLines = (++"|   ") . concat . (`replicate`" ") . ((15-) . length) <$> labels
  let patLines = (++",   ") . concat . (`replicate`" ") . ((15-) . length) <$> (show <$> chords)
  let printPat = zip ["1    |   ", "5    |   ", "9    |   ", "13   |   "]
                     (init . init . init <$> (fmap concat <$> chunksOf 4 $ zipWith (++) (show <$> chords) patLines))
  let prompts = zip ["1    |   ", "5    |   ", "9    |   ", "13   |   "]
                    (init . init . init <$> (fmap concat <$> chunksOf 4 $ zipWith (++) labels promptLines))
  forM_ ["generated by 'The Harmonic Algorithm' -> https://github.com/OscarSouth/theHarmonicAlgorithm"] print
  forM_ prompts $ print . ("|   "++) . concat
  forM_ printPat $ print . ("[   "++) . (\ s -> init s ++ "]") . concat
  return chords


-- |converts a string representation of a musical pattern into a Tidal ControlPattern
progStringToPat :: String -> Tidal.ControlPattern
progStringToPat s =
  let pat = Tidal.parseBP_E s
    in Tidal.note pat

-- |wrapper for 'progStringToPat' that retrieves a progression and returns a ControlPattern
progToPatIO :: IO Tidal.ControlPattern
progToPatIO = do
  r <- prog4
  let progString = show r
  let ctrlPat = progStringToPat progString
  return ctrlPat

-- |interface for retrieving next choices from graph database
getNextsFromGraph :: Cadence -> IO [(Cadence, Double)]
getNextsFromGraph cadence = do
  pipe <- Bolt.connect def { Bolt.version = 3 }
  r <- Bolt.run pipe (retrieveNext cadence)
  return r
  where
    retrieveNext cadence = do
      let cql = " \
      \ MATCH (n:Cadence{show:'"++show cadence++"'}) \
      \ WITH n \
      \ MATCH (n)-[r]->(to) \
      \ RETURN to.movement, to.chord, toString(r.confidence) AS confidence \
      \ ;"
      records <- Bolt.query $ Text.pack cql
      m <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "to.movement")
      c <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "to.chord")
      p <- forM records $ \record -> Text.unpack <$> (record `Bolt.at` "confidence")
      let cadences = constructCadence <$> zip m c
      let cadenceMap = zip cadences (read <$> p :: [Double])
      return cadenceMap


-- |takes current state, context and filters and returns a list of potential next Cadences
getCadenceOptions :: CadenceState -> Filters -> EnharmonicFunction -> IO [Cadence]
getCadenceOptions (CadenceState (prev, root)) filters enharm = do
  let  hAlgo = (\xs -> [ toCadence (transposeCadence enharm (pitchClass root) prev, nxt)
              | nxt <- xs ]) $ -- ^ Convert list of Chords into Cadences from last state
              List.sortBy (compare `on` (\(Chord (_,x)) -> -- sort by dissonance level
              fst . dissonanceLevel $ x)) $ filters enharm -- get values from Filters
  bachFromGraph <- liftIO $ getNextsFromGraph prev -- extract current markov state from graph
  let bach  = filter (\(x,_) -> x `elem` hAlgo) $ -- remove elements not in Filters
              List.sortBy (compare `on` (\(_,x) -> 1-x)) bachFromGraph
      nexts = take 30 $ (fst <$> bach) ++ -- append to markov list and take n
              filter (\x -> x `notElem` fmap fst bach) hAlgo
              -- ^ keep elements of Filters list not in markov list
  return nexts


-- |performance interface for generating ahead a single step of harmony state
nextCadence :: EnharmonicFunction -> CadenceState -> Filters -> Double -> IO CadenceState
nextCadence enharm state context entropy = do
  let fromRoot = rootNote' $ fromCadenceState state
  rnd <- gammaGen 1 entropy
  let index = fromIntegral $ min 30 $ head rnd
  nextOptions <- getCadenceOptions state context enharm
  nextCadenceStates <- forM nextOptions $ \next -> do
    let movement = fromMovement $ fst (deconstructCadence next)
    let nextRoot = enharm $ fromRoot + movement
    return (CadenceState (next, nextRoot))
  let next = if index < length nextCadenceStates
             then nextCadenceStates !! index
             else last nextCadenceStates
  return next


-- |function to retrieve and return random sequence
cadenceSeq' :: Integral a => EnharmonicFunction -> CadenceState -> Filters -> [a]
            -> IO ([Chord], [Cadence])
cadenceSeq' _ _ _ [] = return ([], [])
cadenceSeq' enharm state@(CadenceState (prev, root)) context rns@(x:xs) = do
  let x' = fromIntegral x
  let rootPC = pitchClass root
  nexts <- getCadenceOptions state context enharm
  let next = if x' >= length nexts then last nexts else nexts!!x'
      triad = transposeCadence enharm rootPC prev
      root' = rootPC + movementFromCadence next
  let nextState = CadenceState (next, enharm root')
  nextsRecurse <- cadenceSeq' enharm nextState context xs
  return (triad : fst nextsRecurse, prev : snd nextsRecurse)


-- |performance interface for generating a sequence of harmony states
chainCadence :: EnharmonicFunction -> CadenceState -> Filters -> Double -> Double
             -> IO Progression
chainCadence enharm state context len entropy = do
  liftIO $ putStr "composing .."
  rns               <- gammaGen (max 2 (min 8 len)) entropy
  cadenceSequence   <- cadenceSeq' enharm state context rns
  let
    chords          = showTriad enharm <$> fst cadenceSequence
    chordLen        = length $ chords :: Int
    chordLines      = (++"|   ") . concat . (`replicate`" ") .
                      ((14-) . length) <$> chords
    fours enharm    = concat $ zipWith (++)
                      ["\n   1   ||   ", "\n   5    |   ", "\n   9    |   ", "\n   13   |   "]
                      (init . init . init <$> (fmap concat <$> chunksOf 4 $
                      zipWith (++) chords chordLines))
  liftIO $ putStrLn "generated by 'The Harmonic Algorithm' -> https://github.com/OscarSouth/theHarmonicAlgorithm"
  liftIO $ putStr (fours enharm) >> putStrLn "|\n"
  return $ initProgression enharm cadenceSequence

-- transposeProgression
-- |function to transpose a progression by a given interval
