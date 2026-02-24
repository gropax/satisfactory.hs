{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Syntax
  ( Obj(..)
  , Mor(..)
  ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Kind (Type)
import Data.Proxy
import Symbols.Items
import Symbols.Recipes


data Obj
  = Itm Symbol
  | I
  | O
  | Obj :⊗ Obj
  | Obj :⊕ Obj

infixr 7 :⊗
infixr 6 :⊕

--instance Show Obj where
--  show I = "1"
--  show O = "0"
--  show (Itm s) = symbolVal (Proxy s)


data Mor :: Obj -> Obj -> Type where
  -- Category structure
  Id   :: Mor a a
  Comp :: Mor a b -> Mor b c -> Mor a c

  -- Monoidal bifunctors
  Tens :: Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)
  Plus :: Mor a b -> Mor c d -> Mor (a :⊕ c) (b :⊕ d)

  -- Trace on ⊗
  --Tr   :: Mor (a :⊗ x) (b :⊗ x) -> Mor a b

  -- Isomorphisms of ⊗
  TAssoc  :: Mor ((a :⊗ b) :⊗ c) (a :⊗ (b :⊗ c))
  TAssoc' :: Mor (a :⊗ (b :⊗ c)) ((a :⊗ b) :⊗ c)
  TUnitL  :: Mor (I :⊗ a) a
  TUnitL' :: Mor a (I :⊗ a)
  TUnitR  :: Mor (a :⊗ I) a
  TUnitR' :: Mor a (a :⊗ I)
  TSwap   :: Mor (a :⊗ b) (b :⊗ a)

  -- Isomorphisms of ⊕
  PAssoc  :: Mor ((a :⊕ b) :⊕ c) (a :⊕ (b :⊕ c))
  PAssoc' :: Mor (a :⊕ (b :⊕ c)) ((a :⊕ b) :⊕ c)
  PUnitL  :: Mor (O :⊕ a) a
  PUnitL' :: Mor a (O :⊕ a)
  PUnitR  :: Mor (a :⊕ O) a
  PUnitR' :: Mor a (a :⊕ O)
  PSwap   :: Mor (a :⊕ b) (b :⊕ a)

  -- Distributivity
  DistL  :: Mor (a :⊗ (b :⊕ c)) ((a :⊗ b) :⊕ (a :⊗ c))
  DistL' :: Mor ((a :⊗ b) :⊕ (a :⊗ c)) (a :⊗ (b :⊕ c))
  DistR  :: Mor ((a :⊕ b) :⊗ c) ((a :⊗ c) :⊕ (b :⊗ c))
  DistR' :: Mor ((a :⊗ c) :⊕ (b :⊗ c)) ((a :⊕ b) :⊗ c)

  -- Absorption
  AnnihL  :: Mor (O :⊗ a) O
  AnnihL' :: Mor O (O :⊗ a)
  AnnihR  :: Mor (a :⊗ O) O
  AnnihR' :: Mor O (a :⊗ O)

  -- Primitive morphisms
  Prim :: KnownSymbol r => Proxy r -> Mor a b



infixl 1 ∘
(∘) :: Mor b c -> Mor a b -> Mor a c
f ∘ g = Comp g f

infixl 2 ∙
(∙) :: Mor a b -> Mor b c -> Mor a c
(∙) = Comp

infixl 7 ⊗
(⊗) :: Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)
(⊗) = Tens

infixl 6 ⊕
(⊕) :: Mor a b -> Mor c d -> Mor (a :⊕ c) (b :⊕ d)
(⊕) = Plus



ironMiner :: Mor I (Itm IronOre)
ironMiner = Prim (Proxy @IronOreRecipe)

ironSmelter :: Mor (Itm IronOre) (Itm IronIngot)
ironSmelter = Prim (Proxy @IronIngotRecipe)


test = ironMiner ∙ ironSmelter
