{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Items
  ( ItemId(..)
  , Item(..)
  , ironOre
  , ironIngot
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Proxy
import Symbols.Items


newtype ItemId = ItemId String
  deriving (Eq, Ord, Generic)

instance Show ItemId where
  show (ItemId i) = i

itemId :: forall (s :: Symbol). KnownSymbol s => ItemId
itemId = ItemId (symbolVal (Proxy @s))


data Item = Item
  { iId    :: ItemId
  , iName  :: String
  , stackSize :: Int
  }

instance Show Item where
  show = show . iId


ironOre :: Item
ironOre = Item
  { iId = itemId @IronOre
  , iName = "Iron Ore"
  , stackSize = 100
  }

ironIngot :: Item
ironIngot = Item
  { iId = itemId @IronIngot
  , iName = "Iron Ingot"
  , stackSize = 100
  }
