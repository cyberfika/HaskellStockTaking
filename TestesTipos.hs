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


-- Funções de Teste Individuais

testeItem :: UTCTime -> IO ()
testeItem _ = do
    let item1 = Item "001" "Teclado Mecânico" 15 "Periféricos"
    let item2 = Item "002" "Mouse Gamer" 25 "Periféricos"
    let item3 = Item "003" "Monitor 24\"" 10 "Monitores"

    putStrLn "Item original:"
    print item1
    
    let serializado = show item1
    putStrLn "Item serializado (String):"
    putStrLn serializado
    
    let desserializado = read serializado :: Item
    putStrLn "Item desserializado:"
    print desserializado
    
    if item1 == desserializado
        then putStrLn "SUCESSO: Serialização de Item está correta."
        else putStrLn "FALHA: Problema na serialização de Item!"
    
    -- Testar múltiplos itens
    putStrLn "\nTestando múltiplos itens:"
    let itens = [item1, item2, item3]
    let serializadoLista = show itens
    let desserializadoLista = read serializadoLista :: [Item]
    
    if itens == desserializadoLista
        then putStrLn "SUCESSO: Serialização de lista de Items está correta."
        else putStrLn "FALHA: Problema na serialização de lista de Items!"