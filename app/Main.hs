{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

import System.IO
import qualified Foreign.R as R
import qualified Language.R.Literal as R 
import Language.R.Instance
import Language.R.QQ

import Control.Concurrent ( threadDelay )

import Data.Map ( Map )
import Data.Int
import qualified Data.Map as Map
import qualified Data.List as List ( zip5  )

main = do
  putStrLn "Initialising R Interpreter.."
  initR
  initScript
  header
  -- markovState
  return ()

-- |Initialise R + session log, load libraries & log session info
initR :: IO ()
initR = withEmbeddedR defaultConfig $ do
  putStrLn ""
  loadPackages
  putStrLn ""
  initLogR
  putStrLn "session will be logged:"
  rDir >>= putStr
  putStrLn "/output/sessionlog.txt\n"
  loadData
  return ()

-- |Retrieve R working directory
rDir :: IO String
rDir = 
  let rData () = R.fromSomeSEXP <$> [r| getwd() |]
   in runRegion $ rData ()

-- rGetDir :: R s String
-- rGetDir  = do
--   R.fromSomeSEXP <$> [r| getwd() |]

initScript = do  
  putStrLn "\n___________________________________________________________________________\n"
  -- putStrLn "Welcome to The Harmonic Algorithm.\n" --, what's your name?"  
  -- name <- getLine
  -- putStrLn (">> Hey " ++ name)
  -- threadDelay 100000
  -- putStrLn (">> Let's go")
  -- threadDelay 500000
  threadDelay 300000
  -- putStrLn "Begin..\n"
  return ()
  
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

loadData = do
  uciRef
  bachData
  choraleFundamental <- bachFundamental
  x1 <- fromBachMatrix 1
  x2 <- fromBachMatrix 2
  x3 <- fromBachMatrix 3
  x4 <- fromBachMatrix 4
  x5 <- fromBachMatrix 5
  let rawChorale = unique <$> 
        [[a,b,c,d,e] | (a,b,c,d,e) <- List.zip5 x1 x2 x3 x4 x5]
  let choraleData = mostConsonant <$> 
        possibleTriads' choraleFundamental (fmap round <$> rawChorale)
  -- fill out names and convert to 'cadence' object
  -- feed into markov machinery and generate markov map and transition matrix
  -- implement state monad to maintain state of markov chain
  return ()

header :: IO ()
header  = do  
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
  putStr "\n>> "
  hFlush stdout

loadPackages = runRegion $ do
  [r| library("tidyverse") |]
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


-- testR = runRegion $ do
--   let func x = return (x + 1) :: R s Double
--   [r| print(func_hs(1)) |]
--   return ()

-- testR' = runRegion $ do
--   x <- [r| 1 + 1 |]
--   [r| y <<- 1 |]
--   [r| print(x_hs + y) |]
--   return ()





-- rTest' :: (a -> R s [Double]) -> a -> [b]
-- rTest' n = runRegion $ rData' n

-- -- rTest'' :: (R s [Double]) -> a -> [b]
-- rTest'' f n = runRegion <$> f <*> n

-- |
-- execR :: (a -> R s [Double]) -> a -> [b]
-- execR f x =

rData'  :: () -> IO [Double]
rData' () = do
  runRegion $ R.fromSomeSEXP <$> [r| c(1,2,3) |]



-- rTest


-- rDef  :: Double -> R s [Double]
-- rDef n = do
--   R.fromSomeSEXP <$> [r| list(c(1,2,3),c(4,5,6),c(7,8,9)) |]

-- rRet  :: Double -> IO [Double]
-- rRet n = runRegion $ rDef n

-- rPrint x = runRegion $ do
--   [r| print(x_hs) |]
--   return ()

-- foo = runRegion $ do
--   let x = [rsafe| c(1,2,3) |]-- [rsafe| list(c(1,2,3),c(4,5,6),c(7,8,9)) |]
--       -- y = 
--   return x







-- rList  :: Double -> Double -> IO [Double]
rList x n = 
  let rData x n = R.fromSomeSEXP <$> [r| list(rep(x_hs, n_hs),rep(x_hs, n_hs)) |]
   in runRegion $ rData x n

-- repTest' = do
--   x <- rep 3 3
--   let y = fmap (+3) x
--   return y


-- repTestSafe = runRegion $ do
--    let x = [rsafe| rep(3, 3) |]
--    return x

-- repTestSafe' = runRegion $ do
--    let x = [rsafe| rep(3, 3) |]
--        y = fmap (+3) x
--    return y

-- rep'  :: Double -> Double -> IO [Double]
-- rep' x n = 
--   let rData n = R.fromSomeSEXP <$> [r| list(rep(x_hs, n_hs),rep(x_hs, n_hs),rep(x_hs, n_hs) |]
--    in runRegion $ rData n

-- bachTest :: R s [String]
-- bachTest = R.dynSEXP <$> [r| bach$label |]

bachTest  :: IO [Int32]
bachTest = 
  let rData () = R.dynSEXP <$> [r| bach$pitch |]
   in runRegion $ rData ()

-- bachIO :: IO [Int32] 
-- bachIO = do
--   x <- bachTest
--   y <- fmap (take 3) x
--   return y



