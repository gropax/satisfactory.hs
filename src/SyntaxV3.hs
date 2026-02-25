{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SyntaxV3
  ( (∘), (∙), (⊗), σ
  , id1, id2, id3
  , σ132, σ213, σ231, σ312, σ321
  , source, target
  , ironMiner, coalMiner
  , ironSmelter
  , steelFoundry
  ) where

import GHC.TypeLits
import Data.Kind (Type)
import Data.Proxy
import Symbols.Items
import Symbols.Recipes


-- ===== Syntaxe v3 =====
-- Hypothèses :
-- * On ne travaille qu'au niveau des machines : on ignore les connecteurs, et donc l'opération de somme monoïdale.
-- * On ignore les boucles (pas d'opérateur de trace)
-- * On strictifie les tyes produits en les représentant par des listes, le produit devenant la concaténation.
-- * On représente les symétries comme produits d'identités et de la permutation élémentaire
--
-- Observations :
-- * Il y a deux approches à l'implémentation des permutations. Celle-ci est plus simple que dans la v2, sans
-- doute sera-t-elle suffisante.
--

data Atom = Atom Symbol
type Obj = [Atom]
type I = '[] :: Obj

type family (:⊗) (xs :: Obj) (ys :: Obj) :: Obj where
  '[] :⊗ ys = ys
  (x ': xs) :⊗ ys = x ': (xs :⊗ ys)

type Itm (s :: Symbol) = '[ 'Atom s ]


data Mor :: Obj -> Obj -> Type where
  -- Category structure
  Id   :: Mor a a
  Comp :: Mor a b -> Mor b c -> Mor a c

  -- Primitive morphisms
  Prim :: forall (r :: Symbol) a b. Proxy r -> Mor a b

  -- Monoidal bifunctor
  Tens :: Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)

  -- Isomorphisms of ⊗
  TSwap   :: Mor (x ': y ': xs) (y ': x ': xs)


infixl 1 ∘
(∘) :: Mor b c -> Mor a b -> Mor a c
f ∘ g = Comp g f

infixl 2 ∙
(∙) :: Mor a b -> Mor b c -> Mor a c
(∙) = Comp

infixl 7 ⊗
(⊗) :: Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)
(⊗) = Tens


id1 :: Mor '[a] '[a]
id1 = Id

id2 :: Mor '[a,b] '[a,b]
id2 = Id

id3 :: Mor '[a,b,c] '[a,b,c]
id3 = Id


σ :: Mor '[a,b] '[b,a]
σ = TSwap

σ132 :: Mor '[a,b,c] '[a,c,b]
σ132 = id1 ⊗ σ

σ213 :: Mor '[a,b,c] '[b,a,c]
σ213 = σ ⊗ id1

σ231 :: Mor '[a,b,c] '[b,c,a]
σ231 = σ213 ∙ σ132

σ312 :: Mor '[a,b,c] '[c,a,b]
σ312 = σ132 ∙ σ213

σ321 :: Mor '[a,b,c] '[c,b,a]
σ321 = σ132 ∙ σ213 ∙ σ132



source :: forall (r :: Symbol). Mor I (Itm r)
source = Prim (Proxy @r)

target :: forall (r :: Symbol). Mor (Itm r) I
target = Prim (Proxy @r)

coalMiner :: Mor I (Itm Coal)
coalMiner = Prim (Proxy @CoalRecipe)

ironMiner :: Mor I (Itm IronOre)
ironMiner = Prim (Proxy @IronOreRecipe)

ironSmelter :: Mor (Itm IronOre) (Itm IronIngot)
ironSmelter = Prim (Proxy @IronIngotRecipe)

steelFoundry :: Mor (Itm IronOre :⊗ Itm Coal) (Itm SteelIngot)
steelFoundry = Prim (Proxy @SteelIngotRecipe)


test  = ironMiner ⊗ source @Coal
test' = source @Coal ⊗ ironMiner 

ironIngotFactory = ironMiner ∙ ironSmelter
steelIngotFactory   = (ironMiner ⊗ coalMiner) ∙ steelFoundry
steelIngotFactory'  = (ironMiner ⊗ source @Coal) ∙ steelFoundry

steelIngotTarget = (coalMiner ⊗ ironMiner) ∙ σ ∙ steelFoundry ∙ target @SteelIngot


