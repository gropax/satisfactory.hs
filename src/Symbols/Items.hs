{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Symbols.Items
  ( Coal
  , IronOre
  , IronIngot
  , SteelIngot
  ) where


type Coal = "Coal"
type IronIngot = "IronIngot"
type IronOre   = "IronOre"
type SteelIngot = "SteelIngot"
