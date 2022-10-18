{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Exception             (bracket)
import           Control.Monad.State           (execState, modify)
import           Data.Aeson                    (encode)
import qualified Data.ByteString.Lazy.Char8    as B (putStrLn)
import           Data.Default                  (def)
import           Data.Text                     (Text)
import           Database.Bolt                 (BoltActionT, BoltCfg (..),
                                                Value (..), close, connect, run)
import           Database.Bolt.Extras          (NodeLike (..),
                                                URelationLike (..))
import qualified Database.Bolt.Extras.Graph as Graph
import           Database.Bolt.Extras.Template (makeNodeLike, makeURelationLike)
import           GHC.Generics                  (Generic)

-- | Configuration for connection to local database.
boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "0980"
              }

-- | Helper to run queries in Neo4j DB.

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

data ExampleNode = ExampleNode { exampleFieldT :: Text
                               , exampleFieldI :: Int
                               }
  deriving (Show, Generic)

data EXAMPLE_RELATION = EXAMPLE_RELATION
  deriving (Show, Generic)

makeNodeLike ''ExampleNode
makeURelationLike ''EXAMPLE_RELATION

exNodeAVar :: Text
exNodeAVar = "nodeA"

exNodeBVar :: Text
exNodeBVar = "nodeB"

-- | Builds query:
-- CREATE (nodeA:ExampleNode { exampleFieldT: "A" , exampleFieldI: 1}) WITH nodeA
-- MERGE  (nodeB:ExampleNode { exampleFieldT: "B" , exampleFieldI: 2}) WITH nodeB
-- MERGE  (nodeA)-[nodeA0nodeB:EXAMPLE_RELATION]-(nodeB) WITH nodeA, nodeB, nodeA0nodeB
-- RETURN ID(nodeA), ID(nodeB), ID(nodeA0nodeB)
--
examplePutGraph :: Graph.GraphPutRequest
examplePutGraph = flip execState Graph.emptyGraph $ do
    modify $ Graph.addNode exNodeAVar (Graph.CreateN . toNode $ exNodeA)
    modify $ Graph.addNode exNodeBVar (Graph.MergeN  . toNode $ exNodeB)

    modify $ Graph.addRelation exNodeAVar exNodeBVar (Graph.MergeR . toURelation $ exRel)
  where
    exNodeA = ExampleNode "A" 1
    exNodeB = ExampleNode "B" 2
    exRel   = EXAMPLE_RELATION

-- | Builds query:
-- MATCH (nodeA)-[:EXAMPLE_RELATION]-(nodeB),
-- (nodeA:ExampleNode { exampleField: "A" }),
-- (nodeB:ExampleNode { exampleField: "B" })
-- RETURN { id: ID(nodeA), labels: labels(nodeA), props: properties(nodeA) }

exampleGetGraphB :: Graph.GraphGetRequest
exampleGetGraphB = flip execState Graph.emptyGraph $ do
    modify $ Graph.addNode exNodeAVar exNodeA
    modify $ Graph.addNode exNodeBVar exNodeB

    modify $ Graph.addRelation exNodeAVar exNodeBVar exRel
  where
    exNodeA = Graph.defaultNodeReturn       Graph.# Graph.withLabelQ ''ExampleNode
                                            Graph.# Graph.withProp   ("exampleFieldT", T "A")
                                            Graph.# Graph.withReturn Graph.allProps

    exNodeB = Graph.defaultNodeNotReturn    Graph.# Graph.withLabelQ ''ExampleNode
                                            Graph.# Graph.withProp   ("exampleFieldT", T "B")

    exRel   = Graph.defaultRelNotReturn     Graph.# Graph.withLabelQ ''EXAMPLE_RELATION

-- | Builds query:
-- MATCH (nodeA:ExampleNode { exampleField: "A" })
-- RETURN { id: ID(nodeA), labels: labels(nodeA), props: nodeA {.exampleFieldI} }

exampleGetGraphA :: Graph.GraphGetRequest
exampleGetGraphA = flip execState Graph.emptyGraph $
    modify $ Graph.addNode exNodeAVar exNodeA
  where
    exNodeA = Graph.defaultNodeReturn   Graph.# Graph.withLabelQ ''ExampleNode
                                        Graph.# Graph.withProp   ("exampleFieldT", T "A")
                                        Graph.# Graph.withReturn ["exampleFieldI"]

-- | Put 'examplePutGraph' to the database.
--
putGraph :: IO ()
putGraph = do
    putGraphR <- runQueryDB $ Graph.makeRequest @Graph.PutRequest [] examplePutGraph
    putStrLn "Uploaded graph: "
    print putGraphR

-- Get 'exampleGetGraphB' and parse it to Haskell object.
--
getGraphB :: IO ()
getGraphB = do
    getGraphsR                  <- runQueryDB $ Graph.makeRequest @Graph.GetRequest [] exampleGetGraphB
    let nodesA :: [ExampleNode] = Graph.extractNode exNodeAVar <$> getGraphsR
    putStrLn "Downloaded graph and converted to Haskell object: "
    print nodesA

-- Get 'exampleGetGraphA' and parse it to JSON.
--
getGraphA :: IO ()
getGraphA = do
    getGraphsR <- runQueryDB $ Graph.makeRequest @Graph.GetRequest [] exampleGetGraphA
    let nodesA = Graph.extractNodeAeson exNodeAVar <$> getGraphsR
    putStrLn "Downloaded graph and converted to JSON: "
    B.putStrLn . encode $ nodesA

main :: IO ()
main = putGraph >> getGraphB >> getGraphA

-- main = return ()