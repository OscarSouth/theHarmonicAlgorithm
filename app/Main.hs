{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

import qualified Foreign.R as R
import qualified Language.R.Literal as R
import Language.R.Instance
import Language.R.QQ
import Control.Concurrent (threadDelay)

main = do
  header
  initScript
  return ()














  -- testQuery -- P "this was made in haskell!"
  -- executeR
  -- appendLogR
  -- return ()

-- neoDB :: BoltCfg
-- neoDB  = def {user = "neo4j", password = "0980"}

-- testQuery :: IO ()
-- testQuery  = do
--   pipe <- connect $ neoDB
--   run pipe $ query "CREATE (n:test{wow:'this was made in haskell!'})"
--   records <- run pipe $ query "MATCH (n) RETURN n"
--   run pipe $ query "MATCH (n) DETACH DELETE n"
--   close pipe
--   let a = head records
--   toNode a >>= print

-- testQueryP    :: [Char] -> IO ()
-- testQueryP val = do
--   pipe <- connect $ neoDB
--   run pipe $ query "CREATE (n:test{wow:"testing"})" 
--   records <- run pipe $ queryP "MATCH (n:{testing}) RETURN n" (fromList [("text", T val)])
--   run pipe $ query "MATCH (n) DETACH DELETE n"
--   close pipe
--   let a = head records
--   toNode a >>= print

-- toNode       :: Monad m => Record -> m Node
-- toNode record = record `at` "n" >>= exact

-- plotR :: IO ()
-- plotR  = R.runRegion $ do
--   [r| library(ggplot2)
--       p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
--       ggsave(filename="output/plots/plot.png", plot=p, 
--               width=4, height=4, scale=2)
--     |]
--   return ()

-- initLogR :: IO ()
-- initLogR  = R.runRegion $ do
--   [r| data(iris)
--       fit <- lm(Petal.Width ~ Petal.Length, data=iris)
--       sink("output/output.txt")
--       cat("=============================\n")
--       cat("test\n")
--       cat("=============================\n")
--       print(head(iris))
--       print(summary(fit))
--     |]
--   return ()

-- appendLogR :: IO ()
-- appendLogR  = R.runRegion $ do
--   [r| sink("output/output.txt", append=TRUE)
--       cat("Some more stuff here...\n")
--     |]
--   return ()

-- executeR :: IO ()
-- executeR  = R.withEmbeddedR defaultConfig $ do
--   plotR
--   initLogR
--   appendLogR
--   return ()

-- |Initialise R + session log, load libraries & log session info



-- loadBachData = R.runRegion $ do
--   [r| library("tidyverse")
--       bach <- read_csv("data/jsbach_chorals_harmony.data", 
--                       col_names = c(
--                         "seq", "event",
--                         "0", "1", "2", "3", "4", "5", 
--                         "6", "7", "8", "9", "10", "11",
--                         "fund", "acc", "label"
--                       ), cols(
--                         seq = col_character(),
--                         event = col_integer(),
--                         `0` = col_character(),
--                         `1` = col_character(),
--                         `2` = col_character(),
--                         `3` = col_character(),
--                         `4` = col_character(),
--                         `5` = col_character(),
--                         `6` = col_character(),
--                         `7` = col_character(),
--                         `8` = col_character(),
--                         `9` = col_character(),
--                         `10` = col_character(),
--                         `11` = col_character(),
--                         fund = col_character(),
--                         acc = col_integer(),
--                         label = col_character()
--                       )
--                     )
--       bach <-
--         bach %>% 
--           select(seq, event, fund, acc) %>%
--           add_column(pitch = bach %>% 
--                       select(`0`:`11`) %>% 
--                       t() %>% 
--                       as.data.frame() %>%
--                       unname() %>%
--                       map(function(x) str_which(x, "YES")-1)
--                     ) 
--     |]
--   return ()

-- testR = R.runRegion $ do
--   let f x = return (x + 1) :: R s Double
--   [r| print(f_hs(1)) |]
--   return ()

-- testR' = R.runRegion $ do
--   x <- [r| 1 + 1 |]
--   [r| print(1 + x_hs) |]
--   return ()

-- testR'' = R.runRegion $ do
--   x <- [r| 123 |]
--   return ()

initScript = do  
  putStrLn ">> Welcome to The Harmonic Algorithm, what's your name?"  
  name <- getLine  
  putStrLn (">> Hey " ++ name)
  threadDelay 100000
  putStrLn (">> Let's go")
  threadDelay 500000
  loadData
  threadDelay 300000
  putStrLn ">> Begin.."
  char <- getLine
  return ()
  
loadData = do
  putStrLn ">> Load demo dataset?"
  putStrLn ">> y/n"
  load <- getLine
  if load == "n"
  then putStrLn ">> No Data Loaded"
    else if load == "y" 
    then do
    uciRef
    threadDelay 1000000
    putStrLn ">> Dataset loaded"
      else do
      putStrLn ">> Unknown Input"
      threadDelay 500000
      putStrLn ">> Load demo dataset?"
      putStrLn ">> y/n"
      loadData

-- |Pass data from Haskell to R then print
-- hDataR = do 
--   print [0,3,7]
--   return ()

-- |Pass data to R from Haskell then print
rDataH = runRegion $ do
  x <- [r| c(0,3,7) |]
  return ()

-- getNormals :: Double -> R.R s [Double]
-- getNormals n = do
--   R.dynSEXP <$> [r| rnorm(n_hs) |]
-- rTest = R.runRegion $ do
--   get_normals 4

-- get_normals :: Double -> R s [Double]
-- get_normals n = do
--   R.dynSEXP <$> [r| rnorm(n_hs) |]

rTest n = runRegion $ rData n

rData  :: Double -> R s [Double]
rData n = do
  R.fromSomeSEXP <$> [r| rnorm(n_hs) |]

-- |Pass function from Haskell to R

-- |Pass function from R to Haskell

header :: IO ()
header  = do  
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
  return ()

uciRef :: IO ()
uciRef  = do
    putStrLn ">> Loading Bach Chorale Dataset from UCI Machine Learning Repository..."
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "| Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository |"
    putStrLn "| [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,   |"
    putStrLn "| School of Information and Computer Science.                              |"
    putStrLn "+--------------------------------------------------------------------------+"
    threadDelay 100000
    putStrLn "."
    threadDelay 100000
    putStrLn ".."
    threadDelay 100000
    putStrLn "..."
    return ()