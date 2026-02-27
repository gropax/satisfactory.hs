{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Compile
  ( compile
  ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))
import Control.Monad.State.Strict
import SyntaxV4
import qualified Circuits as G


type CompileM = State G.Graph

addNode :: G.Node -> CompileM G.NodeId
addNode n = state $ G.addNode n

addWire :: G.Port -> G.Port -> CompileM ()
addWire p q = modify' $ G.addEdge p q

-- Strict version of zip
wireZip :: [G.Port] -> [G.Port] -> CompileM ()
wireZip ps qs =
  case compare (length ps) (length qs) of
    EQ -> sequence_ [ addWire p q | (p, q) <- zip ps qs ]
    LT -> error "not enough inputs"
    GT -> error "too many inputs"

juncNode :: G.Node
juncNode = G.Junction


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

    Prim   (pr :: Proxy (r :: Symbol)) -> compileNode G.Recipe (Proxy @a) (Proxy @b) pr ins
    Source (pr :: Proxy (r :: Symbol)) -> compileNode G.Source (Proxy @a) (Proxy @b) pr ins
    Target (pr :: Proxy (r :: Symbol)) -> compileNode G.Target (Proxy @a) (Proxy @b) pr ins

  where
    compileNode
      :: forall x y (r :: Symbol).
         (KnownObj x, KnownObj y, KnownSymbol r)
      => (String -> G.Node)
      -> Proxy x
      -> Proxy y
      -> Proxy r
      -> [G.Port]
      -> CompileM [G.Port]

    compileNode mkNode _ _ pr inputs = do
      nid <- addNode $ mkNode $ symbolVal pr

      let nIn  = objLen (Proxy @x)
          nOut = objLen (Proxy @y)
          pin  = [ G.InPort  nid i | i <- [0 .. nIn  - 1]]
          pout = [ G.OutPort nid j | j <- [0 .. nOut - 1]]

      wireZip inputs pin
      pure pout


compile :: Mor I I -> G.Graph
compile m = execState (compileWith m []) G.empty
      
