{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Compile
  ( compile
  ) where

import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy(..))
import Control.Monad.State.Strict
import SyntaxV4
import qualified Circuits as G


type CompileM = State G.Graph

addNode :: G.Node -> CompileM G.NodeId
addNode n = state $ G.addNode n

addWire :: G.Port -> G.Port -> CompileM ()
addWire p q = modify' $ G.addEdge p q

wireZip :: [G.Port] -> [G.Port] -> CompileM ()
wireZip ps qs = sequence_ [ addWire p q | (p, q) <- zip ps qs ]

juncNode :: G.Node
juncNode = G.Node (Proxy @"JUNC")


compileWith
  :: forall a b.
     Mor a b
  -> [G.Port]
  -> CompileM [G.Port]

compileWith mor ins =
  case mor of
    Id -> pure ins

    Comp f g -> do
      mid <- compileWith f ins
      compileWith g mid

    Tens (f :: Mor a1 b1) (g :: Mor a2 b2) -> do
      outs1 <- compileWith f ins1
      outs2 <- compileWith g ins2
      pure (outs1 ++ outs2)
      where
        l = objLen (Proxy @a1)
        (ins1, ins2) = splitAt l ins

    Prim (pr :: Proxy (r :: Symbol)) -> do
      nid <- addNode (G.Node pr)

      let nIn  = objLen (Proxy @a)
          nOut = objLen (Proxy @b)
          pin  = [ G.InPort  nid i | i <- [0 .. nIn  - 1]]
          pout = [ G.OutPort nid j | j <- [0 .. nOut - 1]]

      wireZip ins pin
      pure pout

    Swap ->
      case ins of
        (p:q:rest) -> pure (q:p:rest)
        _          -> pure ins

    Split -> do
      nid <- addNode juncNode
      case ins of
        [p] -> do
          addWire p (G.InPort nid 0)
          pure [G.OutPort nid 0, G.OutPort nid 1]
        _   -> error "Split: expected exactly 1 input port"

    Merge -> do
      nid <- addNode juncNode
      case ins of
        [p,q] -> do
          addWire p (G.InPort nid 0)
          addWire q (G.InPort nid 1)
          pure [G.OutPort nid 0]
        _     -> error "Merge: expected exactly 2 input ports"


compile :: Mor I I -> G.Graph
compile m = execState (compileWith m []) G.empty
      
