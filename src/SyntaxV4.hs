{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SyntaxV4
  ( Obj, Atom(..), Mor(..), I, KnownObj(..), Itm
  , (∘), (∙), (⊗)
  , id1, id2, id3
  , σ
  , σ132, σ213, σ231, σ312, σ321
  , split2, split3, split4
  , merge2, merge3, merge4
  , source, target
  , ironMiner, coalMiner
  , ironSmelter
  , steelFoundry
  ) where

import GHC.TypeLits
import Data.Kind (Type)
import Data.Proxy
import Symbols.Items
import qualified Symbols.Recipes as R


-- ===== Syntaxe v4 =====
-- Hypothèses :
-- * On ne travaille qu'au niveau des machines : on ignore les connecteurs, et donc l'opération de somme monoïdale.
-- * On ignore les boucles (pas d'opérateur de trace)
-- * On strictifie les tyes produits en les représentant par des listes, le produit devenant la concaténation.
-- * On représente les symétries comme produits d'identités et de la permutation élémentaire
-- * On représente les morphismes "split" et "merge" comme morphismes structurels
--
-- Observations :
-- * Il y a deux approches à l'implémentation des permutations. Celle-ci est plus simple que dans la v2, sans
-- doute sera-t-elle suffisante.
--

data Atom = Atom Symbol
type Obj = [Atom]

type I = '[] :: Obj
type Itm (s :: Symbol) = '[ 'Atom s ]


type family (:⊗) (xs :: Obj) (ys :: Obj) :: Obj where
  I :⊗ ys = ys
  (x ': xs) :⊗ ys = x ': (xs :⊗ ys)


class KnownObj (xs :: Obj) where
  objLen :: Proxy xs -> Int

instance KnownObj '[] where
  objLen _ = 0

instance forall x xs. KnownObj xs => KnownObj (x ': xs) where
  objLen _ = 1 + objLen (Proxy @xs)



data Mor :: Obj -> Obj -> Type where
  -- Category structure
  Id   :: Mor a a
  Comp :: Mor a b -> Mor b c -> Mor a c

  -- Primitive morphisms
  Prim :: forall r a b. (KnownSymbol r, KnownObj a, KnownObj b)
       => Proxy r -> Mor a b

  -- Monoidal bifunctor
  Tens :: forall a b c d. (KnownObj a, KnownObj c)
       => Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)

  -- Isomorphisms of ⊗
  Swap  :: Mor (x ': y ': xs) (y ': x ': xs)

  -- Split and merge morphisms
  Split :: Mor a (a :⊗ a)
  Merge :: Mor (a :⊗ a) a

  Source :: KnownSymbol r => Proxy r -> Mor I (Itm r)
  Target :: KnownSymbol r => Proxy r -> Mor (Itm r) I



infixl 1 ∘
(∘) :: Mor b c -> Mor a b -> Mor a c
f ∘ g = Comp g f

infixl 2 ∙
(∙) :: Mor a b -> Mor b c -> Mor a c
(∙) = Comp

infixl 7 ⊗
(⊗) :: forall a b c d. (KnownObj a, KnownObj c)
    => Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)
(⊗) = Tens


id1 :: Mor '[a] '[a]
id1 = Id

id2 :: Mor '[a,b] '[a,b]
id2 = Id

id3 :: Mor '[a,b,c] '[a,b,c]
id3 = Id


σ :: Mor '[a,b] '[b,a]
σ = Swap

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


split2 :: Mor '[a] '[a,a]
split2 = Split

split3 :: Mor '[a] '[a,a,a]
split3 = split2 ∙ (split2 ⊗ id1)

split4 :: Mor '[a] '[a,a,a,a]
split4 = split3 ∙ (split2 ⊗ id2)

merge2 :: Mor '[a,a] '[a]
merge2 = Merge

merge3 :: Mor '[a,a,a] '[a]
merge3 = (merge2 ⊗ id1) ∙ merge2  

merge4 :: Mor '[a,a,a,a] '[a]
merge4 = (merge2 ⊗ id2) ∙ merge3


source :: forall r. KnownSymbol r => Mor I (Itm r)
source = Source (Proxy @r)

target :: forall r. KnownSymbol r => Mor (Itm r) I
target = Target (Proxy @r)


coalMiner :: Mor I (Itm Coal)
coalMiner = Prim (Proxy @R.Coal)

ironMiner :: Mor I (Itm IronOre)
ironMiner = Prim (Proxy @R.IronOre)

ironSmelter :: Mor (Itm IronOre) (Itm IronIngot)
ironSmelter = Prim (Proxy @R.IronIngot)

steelFoundry :: Mor (Itm IronOre :⊗ Itm Coal) (Itm SteelIngot)
steelFoundry = Prim (Proxy @R.SteelIngot)


test :: Mor I (Itm IronOre :⊗ Itm Coal)
test  = ironMiner ⊗ source @Coal

test' = source @Coal ⊗ ironMiner 
test'' = ironMiner ∙ split2 ∙ (ironSmelter ⊗ ironSmelter) ∙ merge2 ∙ target @IronIngot

ironIngotFactory = ironMiner ∙ ironSmelter
steelIngotFactory   = (ironMiner ⊗ coalMiner) ∙ steelFoundry
steelIngotFactory'  = (ironMiner ⊗ source @Coal) ∙ steelFoundry

steelIngotTarget = (coalMiner ⊗ ironMiner) ∙ σ ∙ steelFoundry ∙ target @SteelIngot


