-- Módulo: TestesTipos
-- Descrição: Testes de validação para os tipos de dados do sistema
-- Objetivo: Demonstrar que read . show é a identidade para os tipos definidos

-- Importações
import InventarioTipos
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map as Map


-- Testes de Serialização
main :: IO ()
main = do
    putStrLn "------------------------------------------"
    putStrLn "Testes de Serialização para Tipos de Dados"
    putStrLn "------------------------------------------"
    putStrLn ""

    -- Obter a data e hora atual
    agora <- getCurrentTime

    putStrLn "Teste 1: Serialização de Item"
    testeItem agora
    putStrLn ""

    putStrLn "Teste 2: Serialização de AcaoLog"
    testeAcaoLog
    putStrLn ""

    putStrLn "Teste 3: Serialização de StatusLog"
    testeStatusLog
    putStrLn ""

    putStrLn "Teste 4: Serialização de LogEntry"
    testeLogEntry agora
    putStrLn ""

    putStrLn "Teste 5: Serialização de Inventario"
    testeInventario
    putStrLn ""

    putStrLn "---------------------------"
    putStrLn "Todos os testes concluídos."
    putStrLn "---------------------------"


