-- Módulo: TestesLogicaNegocio
-- Descrição: Testes para validar a lógica de negócio pura
-- Objetivo: Garantir que todas as funções puras funcionam corretamente

module Main where

-- Importações
import InventarioTipos
import LogicaNegocio
import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)


-- Função Principal de Testes
main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "Testes de Logica de Negocio Pura"
    putStrLn "=========================================="
    putStrLn ""

    agora <- getCurrentTime

    putStrLn "Teste 1: addItem (Sucesso)"
    testeAddItemSucesso agora
    putStrLn ""

    putStrLn "Teste 2: addItem (ID Duplicado)"
    testeAddItemDuplicado agora
    putStrLn ""

    putStrLn "Teste 3: addItem (Quantidade Negativa)"
    testeAddItemQuantidadeNegativa agora
    putStrLn ""

    putStrLn "Teste 4: removeItem (Sucesso)"
    testeRemoveItemSucesso agora
    putStrLn ""

    putStrLn "Teste 5: removeItem (Item Nao Encontrado)"
    testeRemoveItemNaoEncontrado agora
    putStrLn ""

    putStrLn "Teste 6: removeItem (Estoque Insuficiente)"
    testeRemoveItemEstoqueInsuficiente agora
    putStrLn ""

    putStrLn "Teste 7: removeItem (Quantidade Invalida - Zero)"
    testeRemoveItemQuantidadeZero agora
    putStrLn ""

    putStrLn "Teste 8: updateQty (Sucesso)"
    testeUpdateQtySucesso agora
    putStrLn ""

    putStrLn "Teste 9: updateQty (Item Nao Encontrado)"
    testeUpdateQtyNaoEncontrado agora
    putStrLn ""

    putStrLn "Teste 10: updateQty (Quantidade Negativa)"
    testeUpdateQtyQuantidadeNegativa agora
    putStrLn ""

    putStrLn "Teste 11: Multiplas Operacoes Sequenciais"
    testeMultiplasOperacoes agora
    putStrLn ""

    putStrLn "Teste 12: Validacao de LogEntry (Sucesso)"
    testeLogEntrySucesso agora
    putStrLn ""

    putStrLn "Teste 13: Validacao de LogEntry (Falha)"
    testeLogEntryFalha agora
    putStrLn ""

    putStrLn "=========================================="
    putStrLn "Todos os testes foram executados!"
    putStrLn "=========================================="


-- Funções de Teste Individuais
-- =============================

-- Teste 1: addItem com sucesso
testeAddItemSucesso :: UTCTime -> IO ()
testeAddItemSucesso tempo = do
    let inv = emptyInventario
    let item = Item "001" "Teclado Mecanico" 10 "Perifericos"

    case addItem tempo item inv of
        Left erro ->
            putStrLn $ "FALHA: " ++ erro
        Right (invNovo, logEntry) -> do
            putStrLn $ "SUCESSO: " ++ detalhes logEntry
            putStrLn $ "  Status: " ++ show (status logEntry)
            putStrLn $ "  Acao: " ++ show (acao logEntry)
            putStrLn $ "  Tamanho do inventario: " ++ show (Map.size invNovo)
            case Map.lookup "001" invNovo of
                Just itemAdicionado ->
                    putStrLn $ "  Item verificado: " ++ nome itemAdicionado ++
                               " (Qtd: " ++ show (quantidade itemAdicionado) ++ ")"
                Nothing ->
                    putStrLn "  FALHA: Item nao encontrado no inventario!"


-- Teste 2: addItem com ID duplicado
testeAddItemDuplicado :: UTCTime -> IO ()
testeAddItemDuplicado tempo = do
    let item1 = Item "001" "Teclado" 10 "Perifericos"
    let item2 = Item "001" "Mouse" 5 "Perifericos"
    let inv = Map.fromList [("001", item1)]

    case addItem tempo item2 inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado ID duplicado. Log: " ++
                       detalhes logEntry


-- Teste 3: addItem com quantidade negativa
testeAddItemQuantidadeNegativa :: UTCTime -> IO ()
testeAddItemQuantidadeNegativa tempo = do
    let inv = emptyInventario
    let item = Item "001" "Teclado" (-5) "Perifericos"

    case addItem tempo item inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado quantidade negativa. Log: " ++
                       detalhes logEntry


-- Teste 4: removeItem com sucesso
testeRemoveItemSucesso :: UTCTime -> IO ()
testeRemoveItemSucesso tempo = do
    let item = Item "001" "Teclado" 10 "Perifericos"
    let inv = Map.fromList [("001", item)]

    case removeItem tempo "001" 3 inv of
        Left erro ->
            putStrLn $ "FALHA: " ++ erro
        Right (invNovo, logEntry) -> do
            putStrLn $ "SUCESSO: " ++ detalhes logEntry
            putStrLn $ "  Status: " ++ show (status logEntry)
            case Map.lookup "001" invNovo of
                Just itemAtualizado ->
                    putStrLn $ "  Quantidade restante: " ++ show (quantidade itemAtualizado)
                Nothing ->
                    putStrLn "  FALHA: Item nao encontrado!"


-- Teste 5: removeItem com item não encontrado
testeRemoveItemNaoEncontrado :: UTCTime -> IO ()
testeRemoveItemNaoEncontrado tempo = do
    let inv = emptyInventario

    case removeItem tempo "999" 5 inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado item inexistente. Log: " ++
                       detalhes logEntry


-- Teste 6: removeItem com estoque insuficiente
testeRemoveItemEstoqueInsuficiente :: UTCTime -> IO ()
testeRemoveItemEstoqueInsuficiente tempo = do
    let item = Item "001" "Teclado" 5 "Perifericos"
    let inv = Map.fromList [("001", item)]

    case removeItem tempo "001" 10 inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado estoque insuficiente. Log: " ++
                       detalhes logEntry


-- Teste 7: removeItem com quantidade zero
testeRemoveItemQuantidadeZero :: UTCTime -> IO ()
testeRemoveItemQuantidadeZero tempo = do
    let item = Item "001" "Teclado" 10 "Perifericos"
    let inv = Map.fromList [("001", item)]

    case removeItem tempo "001" 0 inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado quantidade invalida. Log: " ++
                       detalhes logEntry


-- Teste 8: updateQty com sucesso
testeUpdateQtySucesso :: UTCTime -> IO ()
testeUpdateQtySucesso tempo = do
    let item = Item "001" "Teclado" 10 "Perifericos"
    let inv = Map.fromList [("001", item)]

    case updateQty tempo "001" 25 inv of
        Left erro ->
            putStrLn $ "FALHA: " ++ erro
        Right (invNovo, logEntry) -> do
            putStrLn $ "SUCESSO: " ++ detalhes logEntry
            putStrLn $ "  Status: " ++ show (status logEntry)
            case Map.lookup "001" invNovo of
                Just itemAtualizado ->
                    putStrLn $ "  Nova quantidade: " ++ show (quantidade itemAtualizado)
                Nothing ->
                    putStrLn "  FALHA: Item nao encontrado!"


-- Teste 9: updateQty com item não encontrado
testeUpdateQtyNaoEncontrado :: UTCTime -> IO ()
testeUpdateQtyNaoEncontrado tempo = do
    let inv = emptyInventario

    case updateQty tempo "999" 10 inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado item inexistente. Log: " ++
                       detalhes logEntry


-- Teste 10: updateQty com quantidade negativa
testeUpdateQtyQuantidadeNegativa :: UTCTime -> IO ()
testeUpdateQtyQuantidadeNegativa tempo = do
    let item = Item "001" "Teclado" 10 "Perifericos"
    let inv = Map.fromList [("001", item)]

    case updateQty tempo "001" (-5) inv of
        Left erro ->
            putStrLn $ "SUCESSO: Erro detectado corretamente - " ++ erro
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter detectado quantidade negativa. Log: " ++
                       detalhes logEntry


-- Teste 11: Múltiplas operações sequenciais
testeMultiplasOperacoes :: UTCTime -> IO ()
testeMultiplasOperacoes tempo = do
    putStrLn "Executando sequencia de operacoes:"

    -- 1. Inventário vazio
    let inv0 = emptyInventario
    putStrLn "  1. Inventario inicial vazio"

    -- 2. Adicionar primeiro item
    case addItem tempo (Item "001" "Teclado" 20 "Perifericos") inv0 of
        Left erro -> putStrLn $ "  FALHA ao adicionar item 001: " ++ erro
        Right (inv1, log1) -> do
            putStrLn $ "  2. " ++ detalhes log1

            -- 3. Adicionar segundo item
            case addItem tempo (Item "002" "Mouse" 30 "Perifericos") inv1 of
                Left erro -> putStrLn $ "  FALHA ao adicionar item 002: " ++ erro
                Right (inv2, log2) -> do
                    putStrLn $ "  3. " ++ detalhes log2

                    -- 4. Remover quantidade do primeiro item
                    case removeItem tempo "001" 5 inv2 of
                        Left erro -> putStrLn $ "  FALHA ao remover: " ++ erro
                        Right (inv3, log3) -> do
                            putStrLn $ "  4. " ++ detalhes log3

                            -- 5. Atualizar quantidade do segundo item
                            case updateQty tempo "002" 50 inv3 of
                                Left erro -> putStrLn $ "  FALHA ao atualizar: " ++ erro
                                Right (inv4, log4) -> do
                                    putStrLn $ "  5. " ++ detalhes log4

                                    -- 6. Verificar estado final
                                    putStrLn "\n  Estado final do inventario:"
                                    putStrLn $ "    Numero de itens: " ++ show (Map.size inv4)

                                    case Map.lookup "001" inv4 of
                                        Just item ->
                                            putStrLn $ "    Item 001: " ++
                                                       show (quantidade item) ++ " unidades"
                                        Nothing -> putStrLn "    Item 001 nao encontrado"

                                    case Map.lookup "002" inv4 of
                                        Just item ->
                                            putStrLn $ "    Item 002: " ++
                                                       show (quantidade item) ++ " unidades"
                                        Nothing -> putStrLn "    Item 002 nao encontrado"

                                    putStrLn "\n  SUCESSO: Todas as operacoes executadas!"


-- Teste 12: Validação de LogEntry em caso de sucesso
testeLogEntrySucesso :: UTCTime -> IO ()
testeLogEntrySucesso tempo = do
    let inv = emptyInventario
    let item = Item "100" "Monitor" 5 "Monitores"

    case addItem tempo item inv of
        Left erro ->
            putStrLn $ "FALHA: " ++ erro
        Right (_, logEntry) -> do
            if status logEntry == Sucesso && acao logEntry == Add
            then do
                putStrLn "SUCESSO: LogEntry criado corretamente"
                putStrLn $ "  Timestamp: " ++ show (timestamp logEntry)
                putStrLn $ "  Acao: " ++ show (acao logEntry)
                putStrLn $ "  Status: " ++ show (status logEntry)
                putStrLn $ "  Detalhes: " ++ detalhes logEntry
            else
                putStrLn $ "FALHA: LogEntry incorreto - " ++ show logEntry


-- Teste 13: Validação de LogEntry em caso de falha
testeLogEntryFalha :: UTCTime -> IO ()
testeLogEntryFalha tempo = do
    let item = Item "001" "Teclado" 5 "Perifericos"
    let inv = Map.fromList [("001", item)]

    -- Tentar remover mais do que existe (deve gerar LogEntry de falha)
    case removeItem tempo "001" 10 inv of
        Left erro -> do
            putStrLn $ "SUCESSO: Falha detectada corretamente"
            putStrLn $ "  Mensagem de erro: " ++ erro
            putStrLn "  (LogEntry de falha seria criado no contexto real)"
        Right (_, logEntry) ->
            putStrLn $ "FALHA: Deveria ter retornado Left. Log: " ++ show logEntry
