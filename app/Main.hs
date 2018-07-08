-- {-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

-- import qualified Foreign.R as R
-- import qualified Language.R.Literal as R
-- import Language.R.Instance
-- import Language.R.QQ
-- import Control.Concurrent (threadDelay)

-- main = do
--   initR
--   header
--   initScript
--   return ()

-- -- |Initialise R + session log, load libraries & log session info
-- initR = withEmbeddedR defaultConfig $ do
--   initLogR
--   -- putStrLn $ runRegion $ rGetDir
--   return ()

-- -- |Retrieve R working directory
-- rGetDir :: R s String
-- rGetDir  = do
--   R.fromSomeSEXP <$> [r| getwd() |]

-- -- rReturnDir = runRegion $ rGetDir

-- initScript = do  
--   putStrLn ">> Welcome to The Harmonic Algorithm, what's your name?"  
--   name <- getLine
--   putStrLn (">> Hey " ++ name)
--   threadDelay 100000
--   putStrLn (">> Let's go")
--   threadDelay 500000
--   loadData
--   threadDelay 300000
--   putStrLn ">> Begin.."
--   char <- getLine
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

-- header :: IO ()
-- header  = do  
--   putStrLn ""
--   putStrLn "  .___________________________________________."
--   putStrLn "  |__/___\\_.___The____________________________|"
--   putStrLn "  |__\\___|_.______Harmonic_____________/|_____|"
--   putStrLn "  |_____/______________Algorithm______/_|_____|"
--   putStrLn "  |____/_____________________________|__|_____|"
--   putStrLn "                                     |-()-"
--   putStrLn "                                 by  |"
--   putStrLn "                                   -()-scar South, 2018 "
--   putStrLn ""
--   return ()

-- uciRef :: IO ()
-- uciRef  = do
--     putStrLn ">> Loading Bach Chorale Dataset from UCI Machine Learning Repository..."
--     putStrLn "+--------------------------------------------------------------------------+"
--     putStrLn "| Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository |"
--     putStrLn "| [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,   |"
--     putStrLn "| School of Information and Computer Science.                              |"
--     putStrLn "+--------------------------------------------------------------------------+"
--     threadDelay 100000
--     putStrLn "."
--     threadDelay 100000
--     putStrLn ".."
--     threadDelay 100000
--     putStrLn "..."
--     return ()

-- initLogR :: IO ()
-- initLogR  = runRegion $ do
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



  -- [r| library("tidyverse") |]

-- loadBachData = runRegion $ do
--   [r| bach <- read_csv("data/jsbach_chorals_harmony.data", 
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

-- testR = runRegion $ do
--   let f x = return (x + 1) :: R s Double
--   [r| print(f_hs(1)) |]
--   return ()

-- testR' = runRegion $ do
--   x <- [r| 1 + 1 |]
--   [r| print(1 + x_hs) |]
--   return ()

-- testR'' = runRegion $ do
--   x <- [r| 123 |]
--   return ()







-- rRet'  :: (Double -> R s [Double]) -> Double -> IO [Double]
-- rRet' f n = runRegion $ f n




-- |Pass data to R from Haskell

-- |Pass function from R to Haskell

-- |Pass data from Haskell to R

-- |Pass function from Haskell to R


-- rData'  :: Double -> R s [Double]
-- rData' n = let func = R.fromSomeSEXP <$> [r| rnorm(n_hs) |]
--              in runRegion $ func n


-- rTest' :: (a -> R s [Double]) -> a -> [b]
-- rTest' n = runRegion $ rData' n

-- -- rTest'' :: (R s [Double]) -> a -> [b]
-- rTest'' f n = runRegion <$> f <*> n

-- |
-- execR :: (a -> R s [Double]) -> a -> [b]
-- execR f x =

-- rData'  :: Double -> [Double]
-- rData' n = do
--   runRegion $ R.fromSomeSEXP <$> [r| rnorm(n_hs) |]





-- rDef  :: Double -> R s [Double]
-- rDef n = do
--   R.fromSomeSEXP <$> [r| rnorm(n_hs) |]

-- rRet  :: Double -> IO [Double]
-- rRet n = runRegion $ rDef n


--------------------------------------------------------------------------------
-- MARKOV CHAIN


-- module Main where

-- import qualified Control.Monad.Random as X
-- import qualified Data.List as L
-- import qualified Data.Map as M

-- type TransitionMap = M.Map (String,String) Int
-- type MarkovChain   = M.Map String [(String,Int)]

-- addTransition    :: TransitionMap -> (String,String) -> TransitionMap
-- addTransition m k = M.insertWith (+) k 1 m

-- fromTransitionMap  :: TransitionMap -> MarkovChain
-- fromTransitionMap m =
--   M.fromList [(k, frequencies k) | k <- ks]
--   where ks = L.nub $ map fst $ M.keys m
--         frequencies a = map reduce $ filter (outboundFor a) $ M.toList m
--         outboundFor a k = fst (fst k) == a
--         reduce e = (snd (fst e), snd e)

-- -- outboundFor a k = fst (fst k) == a

-- -- reduce e = (snd (fst e), snd e)

-- -- generateSequence :: (X.MonadRandom m) => MarkovChain -> String -> m String
-- -- generateSequence m s
-- --   | not (null s) && last s == '.' = return s
-- --   | otherwise = do
-- --                 s' <- X.fromList $ m M.! s
-- --                 ss <- generateSequence m s'
-- --                 return $ if null s then ss else s ++ " " ++ ss

-- -- |tupled 
-- fromSample   :: [String] -> MarkovChain
-- fromSample ss = fromTransitionMap $ bigramMap ss

-- -- |Map object of bigrams with counts of keys
-- bigramMap   :: [String] -> TransitionMap
-- bigramMap ss = foldl addTransition M.empty $ bigrams ss

-- -- |tupled bigrams of sampleset
-- bigrams   :: [String] -> [(String,String)]
-- bigrams ss = concatMap pairs ss

-- -- |tupled bigrams of a single sample
-- pairs   :: String -> [(String,String)]
-- pairs s  =
--   let ws = words s 
--     in zipWith (,) ("":ws) ws

-- sample :: [String]
-- sample  = [ "I am a monster."
--           , "I am a rock star."]
--          --  , "I want to go to Hawaii."
--          --  , "I want to eat a hamburger."
--          --  , "I have a really big headache."
--          --  , "Haskell is a fun language."
--          --  , "Go eat a big hamburger."
--          --  , "Markov chains are fun to use."
--          --  ]

-- main = do
--   s <- generateSequence (fromSample sample) ""
--   print s

main = do 
  return ()

--------------------------------------------------------------------------------


  
--------------------------------------------------------------------------------





-- main = do
--   x <- X.evalRandIO X.getRandom :: IO Float
--   putStrLn $ "Your random number is " ++ show x