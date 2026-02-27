{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Circuits
  ( Graph(..), Node(..), Edge(..), Port(..), NodeId(..), EdgeId(..)
  , addNode, addEdge
  , empty
  ) where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


newtype NodeId = NodeId Int deriving (Eq, Ord, Show)
newtype EdgeId = EdgeId Int deriving (Eq, Ord, Show)


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
  , nextEdge :: Int
  , nodes    :: Map NodeId Node
  , edges    :: Map EdgeId Edge
  } deriving (Eq, Show)

empty :: Graph
empty = Graph 0 0 M.empty M.empty

addNode :: Node -> Graph -> (NodeId, Graph)
addNode n g =
  let i  = nextNode g
      k  = NodeId i
      g' = g { nextNode = i + 1, nodes = M.insert k n (nodes g) }
  in (k, g')

addEdge :: Port -> Port -> Graph -> Graph
addEdge p q g = 
  let i  = nextEdge g
      k  = EdgeId i
      g' = g { nextEdge = i + 1, edges = M.insert k (Edge p q) (edges g) }
  in g'
