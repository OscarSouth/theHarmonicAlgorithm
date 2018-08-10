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

import qualified Data.List            as List (zip5)
import           Data.Map             (Map)
import qualified Data.Map             as Map

-- boot R
-- load data into reader
-- activate state loop

main = withEmbeddedR defaultConfig $ do
  initR
  model <- choraleData
  header
  runReaderT testReader model
  putStrLn "Finished!"
  return ()

-- printReaderContent :: ReaderT String IO ()
-- printReaderContent = do
    -- content <- ask
    -- input <- liftIO getLine
    -- liftIO $ putStrLn ("The Reader Content: " ++ content ++ input)

testReader :: ReaderT MarkovMap IO ()
testReader = do
  model <- ask
  liftIO . print $ (take 10 $ Map.keys model)
  return ()

-- |Initialise R + session log, load libraries & log session info
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
  uciRef
  bachData
  chFunds <- bachFundamental
  x1 <- fromBachMatrix 1
  x2 <- fromBachMatrix 2
  x3 <- fromBachMatrix 3
  x4 <- fromBachMatrix 4
  x5 <- fromBachMatrix 5
  let model = markovMap $
        fmap toCadence <$> bigrams $ flatTriad <$>
        mostConsonant . possibleTriads'' <$>
        filter (\(_, ys) -> length ys >= 3) (zip chFunds $
        (fmap round) . unique <$> [[a,b,c,d,e] |
        (a,b,c,d,e) <- List.zip5 x1 x2 x3 x4 x5])
  return model



-- |Retrieve R working directory
rDir :: IO String
rDir =
  let rData () = R.fromSomeSEXP <$> [r| getwd() |]
   in runRegion $ rData ()

-- initScript = do
--   putStrLn "\n___________________________________________________________________________\n"
--   -- putStrLn "Welcome to The Harmonic Algorithm.\n" --, what's your name?"
--   -- name <- getLine
--   -- putStrLn (">> Hey " ++ name)
--   -- threadDelay 100000
--   -- putStrLn (">> Let's go")
--   -- threadDelay 500000
--   threadDelay 300000
--   -- putStrLn "Begin..\n"
--   return ()

-- loadData = do
--   putStrLn ">> Load demo dataset?"
--   putStrLn ">> y/n"
--   load <- getLine
--   if load == "n"
--   then putStrLn ">> No Data Loaded"
--     else if load == "y"
--     then do
--     uciRef
--     -- |data should be loaded here
--     threadDelay 1000000
--     putStrLn ">> Dataset loaded"
--       else do
--       putStrLn ">> Unknown Input"
--       threadDelay 500000
--       putStrLn ">> Load demo dataset?"
--       putStrLn ">> y/n"
--       loadData

-- loadChoraleData = do
--   uciRef
--   bachData
--   choraleFundamental <- bachFundamental
--   x1 <- fromBachMatrix 1
--   x2 <- fromBachMatrix 2
--   x3 <- fromBachMatrix 3
--   x4 <- fromBachMatrix 4
--   x5 <- fromBachMatrix 5
--   let rawChorale = unique <$>
--         [[a,b,c,d,e] | (a,b,c,d,e) <- List.zip5 x1 x2 x3 x4 x5]
--   -- let filterConvert xs = filter (\x -> length x >= 3) $ (fmap round) <$> xs
--   -- fill out names and convert to 'cadence' object
--   -- feed into markov machinery and generate markov map and transition matrix
--   -- implement state monad to maintain state of markov chain
--   return ()

header :: IO ()
header  = do
  putStrLn "\n___________________________________________________________________________\n"
  threadDelay 300000
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
    putStrLn "Loading Bach Chorale Dataset from UCI Machine Learning Repository..."
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "| Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository |"
    putStrLn "| [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,   |"
    putStrLn "| School of Information and Computer Science.                              |"
    putStrLn "+--------------------------------------------------------------------------+"
    -- threadDelay 100000
    -- putStrLn "."
    -- threadDelay 100000
    -- putStrLn ".."
    -- threadDelay 100000
    -- putStrLn "...\n"
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

-- markovState = do
--   putStrLn "Select starting chord:\n"
--   let ps = lines $ showCadences $ take 3 chords -- highest three probabilities depending on state go here
--       enumPs = zipWith (\n p -> show n ++ " - " ++ p) [1..] ps
--   mapM_ putStrLn enumPs
--   prompt
--   num <- getLine
--   if notElem num $ fmap show [1..3] -- requires generalisation
--     then do
--       putStrLn "\nUnrecognised input."
--       threadDelay 300000
--       markovState
--       else do
--         let index = ((read num) - 1) :: Int
--         putStr "\nInitial state: "
--         putStrLn $ ps!!index

prompt :: IO ()
prompt = do
  putStr "\n♭♯ >> "
  hFlush stdout
  return ()

loadPackages = runRegion $ do
  [r| options(warn = -1) ; library("tidyverse") |]
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

