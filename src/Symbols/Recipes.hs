{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Symbols.Recipes
  ( CoalRecipe
  , IronOreRecipe
  , IronIngotRecipe
  , SteelIngotRecipe
  ) where


type CoalRecipe   = "Coal"
type IronOreRecipe   = "IronOre"
type IronIngotRecipe = "IronIngot"
type SteelIngotRecipe = "SteelIngot"
