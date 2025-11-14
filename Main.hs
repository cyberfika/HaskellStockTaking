-- ============================================================================
-- Arquivo: Main.hs
-- Descrição: Arquivo principal com TODAS as operações de I/O
-- Responsável: Aluno 3 (Jafte Carneiro Fagundes da Silva)
-- Objetivo: Gerenciar TODA a interação com arquivos, usuário e sistema
-- ============================================================================
-- IMPORTANTE: Este arquivo contém APENAS funções IMPURAS (com IO)
-- Funções auxiliares puras estão em IOPersistencia.hs (parser)
-- Lógica de negócio pura está em LogicaNegocio.hs (Aluno 2)
-- ============================================================================

-- Importa os tipos de dados definidos pelo Aluno 1
import InventarioTipos

-- Importa as funções puras de lógica de negócio do Aluno 2
import LogicaNegocio

-- Importa as funções PURAS de parsing (Aluno 3)
import IOPersistencia (Comando(..), parseComando, arquivoInventario, arquivoAuditoria)

-- Importa o módulo Map para manipular o inventário
import qualified Data.Map as Map

import Analise (formatarRelatorioCompleto, historicoPorItem, formatarLog)

-- Importa funções de tempo
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.LocalTime (getCurrentTimeZone)

-- Importa funções de I/O
import System.IO (hFlush, stdout)

-- Importa tratamento de exceções
import Control.Exception (catch, IOException)


-- ============================================================================
-- FUNÇÕES DE INICIALIZAÇÃO (IMPURAS - fazem I/O)
-- ============================================================================

carregarInventario :: IO Inventario
carregarInventario = do
    conteudo <- catch (readFile arquivoInventario)
                      handleArquivoNaoExiste
    if null conteudo
        then do
            putStrLn "[INFO] Iniciando com inventario vazio."
            return emptyInventario
        else do
            putStrLn "[INFO] Inventario carregado com sucesso!"
            return (read conteudo :: Inventario)
  where
    handleArquivoNaoExiste :: IOException -> IO String
    handleArquivoNaoExiste _ = do
        putStrLn "[INFO] Arquivo Inventario.dat nao encontrado."
        return ""


carregarLogs :: IO [LogEntry]
carregarLogs = do
    conteudo <- catch (readFile arquivoAuditoria)
                      handleArquivoNaoExiste
    if null conteudo
        then do
            putStrLn "[INFO] Iniciando com log de auditoria vazio."
            return []
        else do
            putStrLn "[INFO] Log de auditoria carregado com sucesso!"
            let linhas = lines conteudo
            let parseLinha l = case reads l of
                                 [(log, "")] -> Just log
                                 _           -> Nothing
            let logsMaybe = map parseLinha linhas
            let logs = [log | Just log <- logsMaybe]
            let falhas = length linhas - length logs
            if falhas > 0
                then putStrLn $ "[AVISO] Ignoradas " ++ show falhas ++ " linhas mal formatadas no Auditoria.log."
                else return ()
            return logs
  where
    handleArquivoNaoExiste :: IOException -> IO String
    handleArquivoNaoExiste _ = do
        putStrLn "[INFO] Arquivo Auditoria.log nao encontrado."
        return ""


-- ============================================================================
-- FUNÇÕES DE PERSISTÊNCIA (IMPURAS - escrevem em disco)
-- ============================================================================

salvarInventario :: Inventario -> IO ()
salvarInventario inventario = do
    writeFile arquivoInventario (show inventario)
    putStrLn "[PERSISTENCIA] Inventario salvo em Inventario.dat"


adicionarLogAuditoria :: LogEntry -> IO ()
adicionarLogAuditoria logEntry = do
    appendFile arquivoAuditoria (show logEntry ++ "\n")
    putStrLn "[AUDITORIA] Log registrado em Auditoria.log"


-- ============================================================================
-- FUNÇÕES AUXILIARES DE EXIBIÇÃO (IMPURAS - escrevem na tela)
-- ============================================================================

imprimirItem :: Item -> IO ()
imprimirItem item = do
    putStrLn $ "  ID: " ++ itemID item
    putStrLn $ "  Nome: " ++ nome item
    putStrLn $ "  Quantidade: " ++ show (quantidade item)
    putStrLn $ "  Categoria: " ++ categoria item
    putStrLn "  ---"


mostrarAjuda :: IO ()
mostrarAjuda = do
    putStrLn "\n========== COMANDOS DISPONIVEIS =========="
    putStrLn "add <ID> <nome> <quantidade> <categoria>"
    putStrLn "  - Adiciona um novo item ao inventario"
    putStrLn "  - Exemplo: add 001 Teclado 10 Perifericos"
    putStrLn ""
    putStrLn "remove <ID> <quantidade>"
    putStrLn "  - Remove uma quantidade de um item"
    putStrLn "  - Exemplo: remove 001 5"
    putStrLn ""
    putStrLn "update <ID> <nova_quantidade>"
    putStrLn "  - Atualiza a quantidade de um item"
    putStrLn "  - Exemplo: update 001 20"
    putStrLn ""
    putStrLn "list"
    putStrLn "  - Lista todos os itens do inventario"
    putStrLn ""
    putStrLn "report"
    putStrLn "  - Gera relatorio de logs"
    putStrLn ""
    putStrLn "help"
    putStrLn "  - Mostra esta mensagem de ajuda"
    putStrLn ""
    putStrLn "exit"
    putStrLn "  - Encerra o programa"
    putStrLn "==========================================\n"


-- ============================================================================
-- PROCESSAMENTO DE COMANDOS (IMPURA - coordena I/O e lógica pura)
-- ============================================================================

processarComando :: Comando -> Inventario -> IO Inventario
processarComando cmd inventario = do
    tempo <- getCurrentTime
    
    case cmd of        

        CmdAdd idItem nomeItem qtd cat -> do
            let novoItem = Item
                    { itemID = idItem
                    , nome = nomeItem
                    , quantidade = qtd
                    , categoria = cat
                    }
            
            case addItem tempo novoItem inventario of
                Left erro -> do
                    putStrLn $ "\n[ERRO] " ++ erro
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Add
                            , detalhes = "Falha ao adicionar item ID: " ++ idItem
                            , status = Falha erro
                            }
                    adicionarLogAuditoria logEntry
                    return inventario
                
                Right (novoInventario, logEntry) -> do
                    putStrLn $ "\n[SUCESSO] Item adicionado: " ++ nomeItem ++ " (ID: " ++ idItem ++ ")"
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    return novoInventario

        CmdRemove idItem qtd -> do
            case removeItem tempo idItem qtd inventario of
                Left erro -> do
                    putStrLn $ "\n[ERRO] " ++ erro
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Remove
                            , detalhes = "Falha ao remover do item ID: " ++ idItem
                            , status = Falha erro
                            }
                    adicionarLogAuditoria logEntry
                    return inventario
                
                Right (novoInventario, logEntry) -> do
                    putStrLn $ "\n[SUCESSO] Removidas " ++ show qtd ++ " unidade(s) do item ID: " ++ idItem
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    return novoInventario

        CmdUpdate idItem novaQtd -> do
            case updateQty tempo idItem novaQtd inventario of
                Left erro -> do
                    putStrLn $ "\n[ERRO] " ++ erro
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Update
                            , detalhes = "Falha ao atualizar item ID: " ++ idItem
                            , status = Falha erro
                            }
                    adicionarLogAuditoria logEntry
                    return inventario
                
                Right (novoInventario, logEntry) -> do
                    putStrLn $ "\n[SUCESSO] Quantidade do item ID: " ++ idItem ++ " atualizada para " ++ show novaQtd
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    return novoInventario

        CmdList -> do
            putStrLn "\n========== INVENTARIO ATUAL =========="
            
            if Map.null inventario
                then putStrLn "Inventario vazio."
                else mapM_ imprimirItem (Map.elems inventario)
            
            putStrLn "======================================\n"
            return inventario

        CmdReport -> do
            putStrLn "\n[INFO] Carregando logs para gerar relatorio..."
            logs <- carregarLogs
            
            -- Obtém o timezone atual para formatação correta das datas
            tz <- getCurrentTimeZone
            
            -- Gera o relatório usando o módulo Analise.hs
            if null logs
            then putStrLn "\n[INFO] Nenhum log encontrado para gerar relatorio."
            else do
                -- Gera o relatório principal
                putStrLn $ formatarRelatorioCompleto tz logs
            
            putStrLn "\nDigite um ID de item para ver seu historico (ou aperte o ENTER para pular):"
            putStr "> "
            hFlush stdout
            idInput <- getLine
            
            if null idInput
            then return inventario
            else do
                let historico = historicoPorItem logs idInput
                putStrLn $ "\n--- Historico para Item ID: " ++ idInput ++ " ---"
                if null historico
                then putStrLn "Nenhum registro encontrado para este item."
                else 
                    -- Imprime cada log formatado
                    mapM_ (putStrLn . ("  " ++) . formatarLog tz) historico
                
                putStrLn "---------------------------------------------"
                return inventario

        CmdHelp -> do
            mostrarAjuda
            return inventario

        CmdExit -> do
            putStrLn "\n[INFO] Encerrando o sistema..."
            return inventario

        CmdInvalido msg -> do
            putStrLn $ "\n[ERRO] " ++ msg
            putStrLn "Digite 'help' para ver os comandos disponiveis.\n"
            return inventario


-- ============================================================================
-- LOOP PRINCIPAL (IMPURA - lê entrada do usuário)
-- ============================================================================

loopPrincipal :: Inventario -> IO ()
loopPrincipal inventario = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    let cmd = parseComando input
    
    if cmd == CmdExit
        then do
            putStrLn "\n[INFO] Sistema encerrado com sucesso!"
            putStrLn "Ate logo!\n"
        else do
            novoInventario <- processarComando cmd inventario
            loopPrincipal novoInventario


-- ============================================================================
-- FUNÇÃO MAIN - PONTO DE ENTRADA
-- ============================================================================

main :: IO ()
main = do
    putStrLn "\n=========================================="
    putStrLn "   SISTEMA DE GERENCIAMENTO DE INVENTARIO"
    putStrLn "=========================================="
    putStrLn ""
    
    putStrLn "[INICIALIZACAO] Carregando dados do sistema..."
    inventario <- carregarInventario
    logs <- carregarLogs
    
    putStrLn $ "[INFO] Inventario contem " ++ show (Map.size inventario) ++ " item(ns)"
    putStrLn $ "[INFO] Log contem " ++ show (length logs) ++ " entrada(s)"
    putStrLn ""
    putStrLn "Digite 'help' para ver os comandos disponiveis."
    
    loopPrincipal inventario