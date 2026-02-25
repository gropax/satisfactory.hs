{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax1
  ( Obj(..)
  , Mor(..)
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
-- * On représente les types comme arbres de compositions binaires (catégorie non stricte)
--    + On ne détermine pas de forme canonique pour l'associativité : on définit tous les morphismes de réassociation,
--    on écrit systématiquement tout le parenthésage.
--
-- Observations :
-- * Ceci étant la première implémentation naïve, les morphismes associateurs et uniteurs ne semblent pas pertinents
-- du point de vue de la sémantique.
-- * La sémantique est en réalité une catégorie **stricte** : on pourrait se débarrasser des morphismes associateurs
-- et uniteurs en représentant les produits comme concaténation de listes.
-- * La sémantique de ⊗ n'est pas commutative : on conserve les morphismes de symétries σ…
-- * Une implémentation avec liste de Types doit permettre d'implémenter toutes les symétries sans recourir aux associateurs.
-- 
-- Questions :
-- * Vaut-il mieux partir sur une véritable liste de Type ? Ou bien sur un produit binaire canonicalisé (associatif à gauche) ?


data Obj
  = Itm Symbol
  | I
  | Obj :⊗ Obj


infixl 7 :⊗


data Mor :: Obj -> Obj -> Type where
  -- Category structure
  Id   :: Mor a a
  Comp :: Mor a b -> Mor b c -> Mor a c

  -- Primitive morphisms
  Prim :: forall (r :: Symbol) a b. Proxy r -> Mor a b

  -- Monoidal bifunctor
  Tens :: Mor a b -> Mor c d -> Mor (a :⊗ c) (b :⊗ d)

  -- Isomorphisms of ⊗
  TAssoc  :: Mor ((a :⊗ b) :⊗ c) (a :⊗ (b :⊗ c))
  TAssoc' :: Mor (a :⊗ (b :⊗ c)) ((a :⊗ b) :⊗ c)
  TUnitL  :: Mor (I :⊗ a) a
  TUnitL' :: Mor a (I :⊗ a)
  TUnitR  :: Mor (a :⊗ I) a
  TUnitR' :: Mor a (a :⊗ I)
  TSwap   :: Mor (a :⊗ b) (b :⊗ a)


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


