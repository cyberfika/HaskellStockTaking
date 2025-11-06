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

    putStrLn "Teste 5: Serialização de Inventário"
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

testeAcaoLog :: IO ()
testeAcaoLog = do
    let acoes = [Add, Remove, Update, QueryFail]
    
    putStrLn "Testando todas as ações:"
    mapM_ testarAcao acoes
    
    where
        testarAcao :: AcaoLog -> IO ()
        testarAcao acao = do
            let serializado = show acao
            let desserializado = read serializado :: AcaoLog
            if acao == desserializado
                then putStrLn $ "SUCESSO: " ++ show acao
                else putStrLn $ "FALHA: " ++ show acao

testeStatusLog :: IO ()
testeStatusLog = do
    let status1 = Sucesso
    let status2 = Falha "Estoque insuficiente"
    let status3 = Falha "Item nao encontrado"
    
    let statusList = [status1, status2, status3]
    
    putStrLn "Testando todos os status:"
    mapM_ testarStatus statusList
    
    where
        testarStatus :: StatusLog -> IO ()
        testarStatus status = do
            let serializado = show status
            let desserializado = read serializado :: StatusLog
            if status == desserializado
                then putStrLn $ "SUCESSO: " ++ show status
                else putStrLn $ "FALHA: " ++ show status

testeLogEntry :: UTCTime -> IO ()
testeLogEntry agora = do
    let log1 = LogEntry agora Add "Adicionado item 001" Sucesso
    let log2 = LogEntry agora Remove "Removido 5 unidades do item 002" Sucesso
    let log3 = LogEntry agora Update "Tentativa de remover estoque insuficiente" 
                        (Falha "Quantidade insuficiente")
    
    putStrLn "LogEntry original:"
    print log1
    
    let serializado = show log1
    putStrLn "\nLogEntry serializado:"
    putStrLn serializado
    
    let desserializado = read serializado :: LogEntry
    putStrLn "\nLogEntry desserializado:"
    print desserializado
    
    if log1 == desserializado
        then putStrLn "\nSUCESSO: Serialização de LogEntry está correta."
        else putStrLn "\nFALHA: Problema na serialização de LogEntry!"

    -- Testar lista de logs
    let logs = [log1, log2, log3]
    let serializadoLista = show logs
    let desserializadoLista = read serializadoLista :: [LogEntry]
    
    if logs == desserializadoLista
        then putStrLn "SUCESSO: Serialização de lista de LogEntry está correta."
        else putStrLn "FALHA: Problema na serialização de lista de LogEntry!"

testeInventario :: IO ()
testeInventario = do
    let item1 = Item "001" "Teclado" 15 "Periféricos"
    let item2 = Item "002" "Mouse" 25 "Periféricos"
    let item3 = Item "003" "Monitor" 10 "Monitores"
    
    let inv = Map.fromList [("001", item1), ("002", item2), ("003", item3)]
    
    putStrLn "Inventário original:"
    print inv
    
    let serializado = show inv
    putStrLn "\nInventário serializado:"
    putStrLn serializado
    
    let desserializado = read serializado :: Inventario
    putStrLn "\nInventário desserializado:"
    print desserializado
    
    if inv == desserializado
        then putStrLn "\nSUCESSO: Serialização de Inventário está correta."
        else putStrLn "\nFALHA: Problema na serialização de Inventário!"

-- Testes para casos extremos
testeCasosExtremos :: IO ()
testeCasosExtremos = do
    putStrLn "\n--- Testes de Casos Extremos ---"
    
    -- Item com strings vazias
    let itemVazio = Item "" "" 0 ""
    testarSerializacao "Item vazio" itemVazio
    
    -- Item com caracteres especiais
    let itemEspecial = Item "ID-001" "Item com \"aspas\" e 'apóstrofos'" 100 "Cat/Especial"
    testarSerializacao "Item com caracteres especiais" itemEspecial
    
    -- StatusLog com mensagem longa
    let statusLongo = Falha "Esta é uma mensagem de erro muito longa que \
                            \contém múltiplas linhas e caracteres especiais"
    testarSerializacao "Status com mensagem longa" statusLongo
    
    where
        testarSerializacao :: (Show a, Read a, Eq a) => String -> a -> IO ()
        testarSerializacao descricao valor = do
            let serializado = show valor
            let desserializado = read serializado
            if valor == desserializado
                then putStrLn $ "SUCESSO: " ++ descricao
                else putStrLn $ "FALHA: " ++ descricao
