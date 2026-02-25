{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Circuits
  (
  ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Data (Proxy)
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
  }

empty :: Graph
empty = Graph 0 0 M.empty []
