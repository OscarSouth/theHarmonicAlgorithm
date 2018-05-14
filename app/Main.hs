{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default (Default (..))
import Data.List
import Database.Bolt

main :: IO ()
main = testFunction

neo :: BoltCfg
neo = def {user = "neo4j", password = "0980"}

testFunction :: IO ()
testFunction = do
  pipe <- connect $ neo
  run pipe $ query "CREATE (n:test{wow:'this was made in haskell!'})"
  records <- run pipe $ query "MATCH (n) RETURN n"
  run pipe $ query "MATCH (n) DETACH DELETE n"
  close pipe
  let a = head records
  toNode a >>= print

testFunctionP :: IO ()
testFunctionP = do
  pipe <- connect $ neo
  run pipe $ query "CREATE (n:test{wow:'this was made in haskell!'})"
  records <- run pipe $ query "MATCH (n) RETURN n"
  run pipe $ query "MATCH (n) DETACH DELETE n"
  close pipe
  let a = head records
  toNode a >>= print

toNode :: Monad m => Record -> m Node
toNode record = record `at` "n" >>= exact