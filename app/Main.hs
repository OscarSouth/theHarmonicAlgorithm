{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Lib

import           Control.Concurrent   (threadDelay)
import           Control.Monad.Reader
import           System.IO

import qualified Foreign.R            as R
import           Language.R.Instance
import qualified Language.R.Literal   as R
import           Language.R.QQ

import           Data.Function        (on)
import qualified Data.List            as List (sortBy, zip5)
import qualified Data.Map             as Map (keys, lookup, fromList)
import           Data.Map             (Map)
import           Data.Maybe           (fromMaybe)
import qualified Data.Char            as Char (toLower, isAlphaNum)

main = withEmbeddedR defaultConfig $ do
  initR -- load R libraries & settings, initialise R log, print info to stout
  model <- choraleData -- bind trained model
  header
  putStrLn "Welcome to The Harmonic Algorithm!\n"
  threadDelay 300000
  runReaderT loadLoop model -- enter ReaderT Monad with trained model
  return ()

type Tuning = [Integer]
type Key = [Integer]
type Root = PitchClass
type Fundamentals = [PitchClass]
type Enharmonic = String

-- |'interactive' environment for working with the trained model
loadLoop :: ReaderT MarkovMap IO ()
loadLoop = do
  liftIO $ putStrLn "Do you want to begin with ♭ (flat) or ♯ (sharp) notation?\n"
  enharmonic <- flatSharp
  liftIO $ putStrLn "\nSelect starting root note:\n"
  liftIO $ threadDelay 300000
  root <- initFundamental enharmonic
  liftIO $ putStrLn "\nSelect starting functionality:\n"
  liftIO $ threadDelay 300000
  chord <- chooseFunctionality enharmonic root
  let cadence = toCadence (chord, chord)
  liftIO $ putStrLn "\nEnter starting tuning (with strings separated by spaces) or * for chromatic:"
  liftIO $ threadDelay 300000
  liftIO prompt
  getTuning <- liftIO getLine
  let tuning = parseTuning getTuning
  liftIO $ putStrLn "\nEnter starting key (for example E, Bm, ##, 3b) or * for chromatic:"
  liftIO $ threadDelay 400000
  liftIO prompt
  getKey <- liftIO getLine
  let key = parseKey getKey
  liftIO $ putStrLn "\nEnter desired 'next' root notes (one or many separated by spaces) or * for chromatic:"
  liftIO $ threadDelay 400000
  liftIO prompt
  getFunds <- liftIO getLine
  let funds = parseFunds getFunds
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Loading.."
  liftIO $ putStrLn ""
  markovLoop enharmonic root cadence tuning key funds
  return ()
  -- let context = theHarmonicAlgorithm (enharmMap enharmonic) 3 [P 0 .. P 11] str 

-- |recursive loop in which most of the user interaction takes place
markovLoop :: Enharmonic -> 
              Root -> 
              Cadence -> 
              Tuning -> 
              Key ->
              Fundamentals ->
              ReaderT MarkovMap IO ()
markovLoop fs root prev tuning key funds = do
  model <- ask
  let nexts = take 16 $ List.sortBy (compare `on` (\(_, x) -> 1-x)) $ 
              fromMaybe [(prev, 1.0)] $ 
              Map.lookup prev model
      menu = ((showTriad enharm) . (fromCadence enharm root) . fst <$> nexts) ++ 
             ["Filter possibilities",
              "Switch to " ++ enharmOpp ++ " notation", 
              "Select new starting Chord",
              "Quit"]
      opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] menu
      enharm = enharmMap fs
      enharmOpp = if fs == "flat" then "sharp" else "flat"
  liftIO $ putStrLn ("The current chord is " ++ 
           (showTriad enharm $ fromCadence enharm (P 0) prev))
  liftIO $ putStrLn ""
  liftIO $ threadDelay 300000
  liftIO $ putStrLn "Possibilities and options: "
  liftIO $ putStrLn ""
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO $ getLine
  let index = ((read num) - 1) :: Int
  if notElem num $ fmap show [1..length opts]
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      liftIO $ threadDelay 300000
      markovLoop fs root prev tuning key funds
      else if index == length nexts
        then do
        return ()
        else if index == 1 + length nexts
          then do
          liftIO $ putStrLn ""
          markovLoop enharmOpp root prev tuning key funds
          else if index == 2 + length nexts
            then do
            liftIO $ putStrLn ""
            loadLoop
            else if index == 3 + length nexts
              then do
              liftIO $ putStrLn exitText
              return ()
              else do
              let next = fst $ nexts!!index  
                  root' = root + movementFromCadence next
              liftIO $ putStrLn ""
              liftIO $ threadDelay 300000
              markovLoop fs root' next tuning key funds
  -- liftIO $ print from
  -- markovLoop fs' root' next tuning' key' funds' 
  return ()

-- |returns a String to be used as a lookup for choosing 'enharmonic' function
flatSharp :: ReaderT MarkovMap IO String
flatSharp = do 
  let enh  = ["Flat ♭", "Sharp ♯"]
      opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] enh
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO $ getLine
  if notElem num $ fmap show [1..(length opts)]
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      liftIO $ threadDelay 300000
      flatSharp
      else do
        let index      = ((read num) - 1) :: Int
            enharmonic = takeWhile Char.isAlphaNum $ Char.toLower <$> enh!!index
        return enharmonic


-- |returns a PitchClass to designate starting root note
initFundamental :: String -> ReaderT MarkovMap IO PitchClass
initFundamental k = do
  let pcs  = show . (enharmMap k) <$> [P 0 .. P 11]
      opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] pcs
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO $ getLine
  if notElem num $ fmap show [1..(length opts)]     
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      liftIO $ threadDelay 300000
      initFundamental k
      else do
        let index = ((read num) - 1) :: Int
        return (pc index)

-- |returns a Chord to designate starting functionality over root
chooseFunctionality :: String -> PitchClass -> ReaderT MarkovMap IO Chord
chooseFunctionality k r = do
  let opts = zipWith (\n p -> show n ++ " - " ++ p) [1..] initFcList
  liftIO $ mapM_ putStrLn opts
  liftIO prompt
  num <- liftIO $ getLine
  if notElem num $ fmap show [1..(length opts)]     
    then do
      liftIO $ putStrLn "\nUnrecognised input, please retry:\n"
      liftIO $ threadDelay 300000
      chooseFunctionality k r
      else do
        let index = ((read num) - 1) :: Int
            chord = toTriad (enharmMap k) $ -- make correctly enharmonic Chord
                    (+ i r) <$> -- transpose structure to meet fundamental note
                    (fromMaybe [0,4,7] $ -- extract Map lookup from Maybe
                    Map.lookup (initFcList!!index) initFcMap
                    ) -- ^ extract choice from Map
        return chord

-- |mapping from string to 'enharmonic' function
enharmMap :: MusicData a => String -> (a -> NoteName)
enharmMap key = 
  let funcMap = (Map.fromList [("flat", flat), ("sharp", sharp)])
   in fromMaybe (flat) $ Map.lookup key funcMap

-- |mapping from string to Integral pitchclass set representation
initFcMap :: (Integral a, Num a) => Map String [a]
initFcMap = Map.fromList $
  [("maj",[0,4,7]),
  ("maj_1stInv",[0,3,8]),
  ("min",[0,3,7]),
  ("min_1stInv",[0,4,9]),
  ("maj_2ndInv",[0,5,9]),
  ("sus2",[0,2,7]),
  ("min6no5",[0,3,9]),
  ("sus4",[0,5,7]),
  ("7sus4no5",[0,5,10]),
  ("min_2ndInv",[0,5,8]),
  ("min7no5",[0,3,10]),
  ("6sus2no5",[0,2,9]),
  ("dim",[0,3,6]),
  ("6b5no3",[0,6,9]),
  ("7no3",[0,7,10]),
  ("7no5",[0,4,10]),
  ("sus2sus4no5",[0,2,5]),
  ("minadd11no5",[0,3,5])]

-- |list of options for starting functionality
initFcList :: [String] 
initFcList = 
  ["maj",
  "maj_1stInv",
  "min",
  "min_1stInv",
  "maj_2ndInv",
  "sus2",
  "min6no5",
  "sus4",
  "7sus4no5",
  "min_2ndInv",
  "min7no5",
  "6sus2no5",
  "dim",
  "6b5no3",
  "7no3",
  "7no5",
  "sus2sus4no5",
  "minadd11no5"]

-- |Initialise R + session log, load libraries/set options & log session info
initR :: IO ()
initR = do
  putStrLn "\nInitialising R Interpreter..\n"
  loadPackages
  putStrLn ""
  initLogR
  putStrLn "session will be logged:"
  rDir >>= putStr
  putStrLn "/output/sessionlog.txt\n"
  threadDelay 100000
  return ()

choraleData :: IO MarkovMap
choraleData = do
  uciRef -- print dataset source reference
  bachData -- execute R script to preprocess data
  chFunds <- bachFundamental -- retrieve and bind R column of fundamental notes
  x1 <- fromBachMatrix 1 -- retrieve and bind columns from R matrix
  x2 <- fromBachMatrix 2 -- |
  x3 <- fromBachMatrix 3 -- |
  x4 <- fromBachMatrix 4 -- |
  x5 <- fromBachMatrix 5 -- V
  let model = markovMap $ -- train model on
        fmap toCadence <$> -- map bigram sets into Cadence data types
        bigrams $ -- combine chords into sequential bigrams
        flatTriad <$> -- convert to 'Chord' data type
        mostConsonant . possibleTriads'' <$> -- derive most suitable triad over fundamental
        filter (\(_, ys) -> length ys >= 3) ( -- remove sets of less than 3
        zip chFunds $ -- zip with fundamentals R column
        (fmap round) . unique <$> -- remove duplicate elems . convert to Integer
        [[a,b,c,d,e] | (a,b,c,d,e) <- List.zip5 x1 x2 x3 x4 x5]
        ) -- ^ convert R matrix columns to a list of lists
  return model

-- |Retrieve R working directory
rDir :: IO String
rDir =
  let rData () = R.fromSomeSEXP <$> [r| getwd() |]
   in runRegion $ rData ()

header :: IO ()
header  = do
  putStrLn "\n___________________________________________________________________________\n"
  threadDelay 300000
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "  .___________________________________________."
  putStrLn "  |__/___\\_.___The____________________________|"
  putStrLn "  |__\\___|_.______Harmonic_____________/|_____|"
  putStrLn "  |_____/______________Algorithm______/_|_____|"
  putStrLn "  |____/_____________________________|__|_____|"
  putStrLn "                                     |-()-"
  putStrLn "                                 by  |"
  putStrLn "                                   -()-scar South, 2018 "
  putStrLn ""
  putStrLn ""
  return ()

uciRef :: IO ()
uciRef  = do
    threadDelay 500000
    putStrLn "Loading Bach Chorale Dataset from UCI Machine Learning Repository...\n"
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "| Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository |"
    putStrLn "| [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,   |"
    putStrLn "| School of Information and Computer Science.                              |"
    putStrLn "+--------------------------------------------------------------------------+\n"
    threadDelay 100000
    putStrLn "."
    threadDelay 100000
    putStrLn ".."
    threadDelay 100000
    putStrLn "...\n"
    threadDelay 100000
    putStr "Loading"
    hFlush stdout
    threadDelay 100000
    putStr "."
    hFlush stdout
    threadDelay 100000
    putStr "."
    hFlush stdout
    threadDelay 100000
    putStr ".\n\n"
    hFlush stdout
    return ()

initLogR :: IO ()
initLogR  = runRegion $ do
  [r| sink("output/sessionlog.txt")
      cat("=======================================\n")
      cat("The Harmonic Algorithm\nSession: ")
      print(Sys.time())
      cat("=======================================\n")
      cat("\n")
      print(sessionInfo())
    |]
  return ()


prompt :: IO ()
prompt = do
  putStr "\n>> "
  hFlush stdout
  return ()

loadPackages = runRegion $ do
  [r| options(warn = -1)
      library("tidyverse") 
    |]
  return ()

bachData = runRegion $ do
  [r| bach <- read_csv("data/jsbach_chorals_harmony.data",
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

bachMatrix <<-
  reduce(bach$pitch,
         rbind,
           matrix(,0,bach$pitch %>%
                     map(length) %>%
                     rapply(c) %>%
                     max()
                 )
         ) %>%
  unname()

bachFund <<- bach$fund

    |]
  return ()

fromBachMatrix  :: Double -> IO [Double]
fromBachMatrix x =
  let rData x = R.fromSomeSEXP <$> [r| bachMatrix[,x_hs] |]
   in runRegion $ rData x

bachFundamental  :: IO [String]
bachFundamental =
  let rData () = R.fromSomeSEXP <$> [r| bachFund |]
   in runRegion $ rData ()


-- appendLogR :: IO ()
-- appendLogR  = runRegion $ do
--   [r| sink("output/output.txt", append=TRUE)
--       cat("Some more stuff here...\n")
--     |]
--   return ()

-- plotR :: IO ()
-- plotR  = runRegion $ do
--   [r| library(ggplot2)
--       p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
--       ggsave(filename="output/plots/plot.png", plot=p,
--               width=4, height=4, scale=2)
--     |]
--   return ()

exitText :: String
exitText =
  "\n\
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
