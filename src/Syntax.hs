{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax
  ( Obj(..)
  , Mor(..)
  ) where

import GHC.TypeLits
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


infixl 7 :⊗
infixl 6 :⊕


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
  Prim :: forall (r :: Symbol) a b. Proxy r -> Mor a b


σ :: Mor (a :⊗ b) (b :⊗ a)
σ = TSwap

σ231 :: Mor ((a :⊗ b) :⊗ c) ((b :⊗ c) :⊗ a)
σ231 = (TSwap ⊗ Id) ∙ TAssoc ∙ (Id ⊗ TSwap) ∙ TAssoc'

σ231' :: Mor (a :⊗ (b :⊗ c)) (b :⊗ (c :⊗ a))
σ231' = TAssoc' ∙ (TSwap ⊗ Id) ∙ TAssoc ∙ (Id ⊗ TSwap)


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



store :: forall (r :: Symbol) a. Mor I a
store = Prim (Proxy @r)

coalMiner :: Mor I (Itm Coal)
coalMiner = Prim (Proxy @CoalRecipe)

ironMiner :: Mor I (Itm IronOre)
ironMiner = Prim (Proxy @IronOreRecipe)

ironSmelter :: Mor (Itm IronOre) (Itm IronIngot)
ironSmelter = Prim (Proxy @IronIngotRecipe)

steelFoundry :: Mor (Itm IronOre :⊗ Itm Coal) (Itm SteelIngot)
steelFoundry = Prim (Proxy @SteelIngotRecipe)


ironIngotFactory = ironMiner ∙ ironSmelter
steelIngotFactory   = (ironMiner ⊗ coalMiner) ∙ steelFoundry
steelIngotFactory'  = (ironMiner ⊗ store @Coal) ∙ steelFoundry
steelIngotFactory'' = (coalMiner ⊗ ironMiner) ∙ σ ∙ steelFoundry


