{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Circuits
  (
  ) where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


newtype NodeId = NodeId Int deriving (Eq, Ord, Show)
newtype WireId = WireId Int deriving (Eq, Ord, Show)


data Port = InPort NodeId Int | OutPort NodeId Int
  deriving (Eq, Ord, Show)
  

data Edge = Edge { from :: Port, to :: Port }
  deriving (Eq, Show)


data Node where
  Node :: KnownSymbol r => Proxy r -> Node

instance Show Node where
  show (Node (pr :: Proxy r)) = symbolVal pr

instance Eq Node where
  Node (pa :: Proxy a) == Node (pb :: Proxy b) =
    symbolVal pa == symbolVal pb


data Graph = Graph 
  { nextNode :: Int
  , nextWire :: Int
  , nodes    :: Map NodeId Node
  , edges    :: [Edge]
  } deriving (Eq, Show)

empty :: Graph
empty = Graph 0 0 M.empty []

addNode :: Node -> Graph -> (NodeId, Graph)
addNode n g =
  let i  = nextNode g
      k  = NodeId i
      g' = g { nextNode = i + 1, nodes = M.insert k n (nodes g) }
  in (k, g')

addEdge :: Port -> Port -> Graph -> Graph
addEdge p q g = g { edges = Edge p q : edges g }


data Compiled = Compiled
  { cGraph   :: Graph
  , cInputs  :: [Port]
  , cOutputs :: [Port]
  } deriving (Eq, Show)
