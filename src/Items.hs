{-# LANGUAGE DeriveGeneric #-}

module Items
  ( ItemId(..)
  , Item(..)
  , ironOre
  , ironIngot
  ) where

import GHC.Generics (Generic)


newtype ItemId = ItemId String
  deriving (Eq, Ord, Generic)

instance Show ItemId where
  show (ItemId i) = i


data Item = Item
  { itemId    :: ItemId
  , itemName  :: String
  , stackSize :: Int
  }

instance Show Item where
  show = show . itemId


ironOre :: Item
ironOre = Item
  { itemId = ItemId "iron-ore"
  , itemName = "Iron Ore"
  , stackSize = 100
  }

ironIngot :: Item
ironIngot = Item
  { itemId = ItemId "iron-ingot"
  , itemName = "Iron Ingot"
  , stackSize = 100
  }
