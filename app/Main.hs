{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import qualified Data.Vector.SEXP as R
import Data.Default (Default (..))
import Data.List
import Database.Bolt

main :: IO ()
main  = do
  -- testQuery -- P "this was made in haskell!"
  executeR
  appendLogR
  return ()

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

plotR :: IO ()
plotR  = R.runRegion $ do
  [r| library(ggplot2)
      p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
      ggsave(filename="output/plots/plot.png", plot=p, 
              width=4, height=4, scale=2)
    |]
  return ()

initLogR :: IO ()
initLogR  = R.runRegion $ do
  [r| data(iris)
      fit <- lm(Petal.Width ~ Petal.Length, data=iris)
      sink("output/output.txt")
      cat("=============================\n")
      cat("test\n")
      cat("=============================\n")
      print(head(iris))
      print(summary(fit))
    |]
  return ()

appendLogR :: IO ()
appendLogR  = R.runRegion $ do
  [r| sink("output/output.txt", append=TRUE)
      cat("Some more stuff here...\n")
    |]
  return ()

executeR :: IO ()
executeR  = R.withEmbeddedR defaultConfig $ do
  plotR
  initLogR
  appendLogR
  return ()

-- |Initialise R + session log, load libraries & log session info

