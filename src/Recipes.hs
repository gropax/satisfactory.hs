{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Recipes
  ( Recipe(..)
  , RecipeId(..), mkRecipeId
  , RecipeRegistry, lookupRecipe
  , allRecipes
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as M
import Symbols.Recipes
import qualified Items as I


newtype RecipeId = RecipeId String
  deriving (Eq, Ord, Show, Generic)

mkRecipeId :: forall (s :: Symbol). KnownSymbol s => RecipeId
mkRecipeId = RecipeId (symbolVal (Proxy @s))


data Part = Part I.Item Int Float deriving (Eq, Ord, Show)

data Recipe = Recipe
  { recipeId      :: RecipeId
  , recipeName    :: String
  , recipeInputs  :: [Part] 
  , recipeOutputs :: [Part] 
  , prodTime      :: Int
  } deriving (Eq, Ord, Show)


type RecipeRegistry = M.Map RecipeId Recipe

recipeRegistry :: RecipeRegistry
recipeRegistry = M.fromList [ (recipeId i, i) | i <- allRecipes ]

lookupRecipe :: RecipeId -> Maybe Recipe
lookupRecipe = flip M.lookup $ recipeRegistry


allRecipes :: [Recipe]
allRecipes =
  [ ironIngot
  ]


ironIngot :: Recipe
ironIngot = Recipe
  { recipeId = mkRecipeId @IronIngot
  , recipeName = "Iron Ingot"
  , recipeInputs  = [Part I.ironOre 1 30]
  , recipeOutputs = [Part I.ironIngot 1 30]
  , prodTime = 2
  }
