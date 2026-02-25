{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Syntax2
  ( Mor(..)
  ) where

import GHC.TypeLits
import Data.Kind (Type)
import Data.Proxy
import Symbols.Items
import Symbols.Recipes


-- ===== Syntaxe 1 =====
-- Hypothèses :
-- * On ne travaille qu'au niveau des machines : on ignore les connecteurs, et donc l'opération de somme monoïdale.
-- * On ignore les boucles (pas d'opérateur de trace)
-- * On strictifie les tyes produits en les représentant par des listes, le produit devenant la concaténation.
-- * On représente les symétries comme composition de permutations adjacentes (σ0, σ1…)
--
-- Observations :
-- * 
-- 
-- Questions :
-- * 
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



data Prefix :: Obj -> Type where
  PNil  :: Prefix '[]
  PCons :: Proxy x -> Prefix xs -> Prefix (x ': xs)

liftR :: Prefix p -> Mor xs ys -> Mor (p :⊗ xs) (p :⊗ ys)
liftR  PNil                    f = f
liftR (PCons (_ :: Proxy x) p) f = Tens (Id :: Mor '[x] '[x]) (liftR p f)


p0 :: Prefix '[]
p0 = PNil

p1 :: forall a. Prefix '[a]
p1 = PCons (Proxy @a) PNil

p2 :: forall a b. Prefix '[a,b]
p2 = PCons (Proxy @a) (PCons (Proxy @b) PNil)


swapAt :: forall p x y xs. Prefix p -> Mor (p :⊗ (x ': y ': xs)) (p :⊗ (y ': x ': xs))
swapAt p = liftR p (TSwap @x @y @xs)


σ :: Mor (x ': y ': xs) (y ': x ': xs)
σ = TSwap

σ1 :: Mor (x ': y ': xs) (y ': x ': xs)
σ1 = TSwap

σ2 :: Mor (a ': x ': y ': xs) (a ': y ': x ': xs)
σ2 = swapAt p1

σ3 :: Mor (a ': b ': x ': y ': xs) (a ': b ': y ': x ': xs)
σ3 = swapAt p2


σ123 :: Mor '[a,b,c] '[a,b,c]
σ123 = Id

σ132 :: Mor '[a,b,c] '[a,c,b]
σ132 = σ2

σ213 :: Mor '[a,b,c] '[b,a,c]
σ213 = σ1

σ231 :: Mor '[a,b,c] '[b,c,a]
σ231 = σ1 ∙ σ2

σ312 :: Mor '[a,b,c] '[c,a,b]
σ312 = σ2 ∙ σ1

σ321 :: Mor '[a,b,c] '[c,b,a]
σ321 = σ1 ∙ σ2 ∙ σ1



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


