module InventarioTipos
    ( Item(..)                      
    , Inventario
    , AcaoLog(..)
    , StatusLog(..)
    , LogEntry(..)
    , emptyInventario
    , exemploItem
    , exemploLog
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)


data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)