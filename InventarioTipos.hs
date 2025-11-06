-- Módulo: InventarioTipos.hs
-- Descrição: Define os tipos de dados do sistema de inventário
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

-- Importações
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)


-- Tipo: Item
-- Descrição: Representa um item no inventário
data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)


-- Tipo: Inventario
-- Descrição: Representa o inventário como um mapa de itens
-- Implementação: Map usando itemID como chave e Item como valor
type Inventario = Map String Item

-- Função auxiliar para criar um inventário vazio
emptyInventario :: Inventario
emptyInventario = Map.empty


-- Tipo: AcaoLog (ADT)
-- Descrição: Tipo algébrico para representar as diferentes ações do sistema
data AcaoLog
    = Add
    | Remove
    | Update
    | QueryFail
    deriving (Show, Read, Eq)

-- Tipo: StatusLog (ADT)
-- Descrição: Tipo algébrico para representar o resultado de uma operação
data StatusLog
    = Sucesso
    | Falha String
    deriving (Show, Read, Eq)


-- Tipo: LogEntry
-- Descrição: Representa uma entrada de log no sistema
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq)



