-- ============================================================================
-- Módulo: IOPersistencia.hs
-- Descrição: Implementa a camada de I/O e persistência do sistema de inventário
-- Responsável: Aluno 3 (Jafte Carneiro Fagundes da Silva)
-- Objetivo: Gerenciar interação com usuário, leitura/escrita de arquivos e loop principal
-- ============================================================================
-- ATENÇÃO: Este arquivo contém APENAS operações de I/O (entrada/saída)
-- A lógica de negócio pura está em LogicaNegocio.hs (Aluno 2)
-- ============================================================================

module IOPersistencia where

-- ============================================================================
-- IMPORTAÇÕES - Bibliotecas e módulos necessários
-- ============================================================================

-- Importa os tipos de dados definidos pelo Aluno 1
-- (Item, Inventario, LogEntry, AcaoLog, StatusLog, etc.)
import InventarioTipos

-- Importa as funções puras de lógica de negócio do Aluno 2
-- (addItem, removeItem, updateQty, ResultadoOperacao)
import LogicaNegocio

-- Importa o módulo Map (usado para o tipo Inventario)
-- 'qualified' significa que usamos Map.insert, Map.lookup, etc.
import qualified Data.Map as Map

-- Importa funções de tempo para registrar quando operações ocorrem
-- getCurrentTime: pega o horário atual
-- UTCTime: tipo que representa data e hora
import Data.Time (getCurrentTime, UTCTime)

-- Importa funções de I/O do sistema
-- hFlush: força o buffer a ser escrito imediatamente
-- stdout: representa a saída padrão (terminal)
import System.IO (hFlush, stdout)

-- Importa funções para tratamento de exceções
-- catch: captura erros de I/O
-- IOException: tipo de exceção de entrada/saída
import Control.Exception (catch, IOException)


-- ============================================================================
-- CONSTANTES - Nomes dos arquivos usados pelo sistema
-- ============================================================================

-- Nome do arquivo que armazena o inventário
-- FilePath é o tipo usado para caminhos de arquivos
-- Este arquivo será SOBRESCRITO a cada operação bem-sucedida
arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

-- Nome do arquivo que armazena o log de auditoria
-- Este arquivo funciona em modo APPEND-ONLY (só adiciona, nunca apaga)
arquivoAuditoria :: FilePath
arquivoAuditoria = "Auditoria.log"


-- ============================================================================
-- FUNÇÕES DE INICIALIZAÇÃO
-- Estas funções são executadas quando o programa inicia
-- ============================================================================

-- Função: carregarInventario
-- Descrição: Tenta ler o inventário do arquivo Inventario.dat
-- Tipo: IO Inventario significa que esta função faz I/O e retorna um Inventario
-- IMPORTANTE: Esta função NÃO quebra o programa se o arquivo não existir
carregarInventario :: IO Inventario
carregarInventario = do
    -- 'do' inicia um bloco de ações sequenciais em IO
    
    -- Tenta ler o arquivo. Se der erro, chama handleArquivoNaoExiste
    -- catch: captura exceções de I/O
    -- readFile: lê todo o conteúdo do arquivo como String
    conteudo <- catch (readFile arquivoInventario)
                      handleArquivoNaoExiste
    
    -- Verifica se o conteúdo está vazio
    -- null: retorna True se a string é vazia
    if null conteudo
        then do
            -- Se arquivo vazio ou não existe:
            -- putStrLn: imprime uma linha no terminal
            putStrLn "[INFO] Iniciando com inventario vazio."
            -- return: retorna um valor dentro do contexto IO
            -- emptyInventario: Map vazio (definido em InventarioTipos)
            return emptyInventario
        else do
            -- Se arquivo existe e tem conteúdo:
            putStrLn "[INFO] Inventario carregado com sucesso!"
            -- read: converte String de volta para o tipo Inventario
            -- :: Inventario: anotação de tipo explícita
            return (read conteudo :: Inventario)
  where
    -- 'where' define funções auxiliares locais
    
    -- Função auxiliar que trata quando arquivo não existe
    -- _ : ignora o argumento (a exceção em si)
    -- IO String: retorna uma String dentro de IO
    handleArquivoNaoExiste :: IOException -> IO String
    handleArquivoNaoExiste _ = do
        putStrLn "[INFO] Arquivo Inventario.dat nao encontrado."
        -- Retorna string vazia para indicar "arquivo não existe"
        return ""


-- Função: carregarLogs
-- Descrição: Tenta ler os logs de auditoria do arquivo Auditoria.log
-- Tipo: IO [LogEntry] significa que retorna uma lista de LogEntry em IO
-- IMPORTANTE: Também não quebra se arquivo não existir
carregarLogs :: IO [LogEntry]
carregarLogs = do
    -- Mesma lógica de carregarInventario, mas para o arquivo de log
    -- Tenta ler, se falhar chama o handler
    conteudo <- catch (readFile arquivoAuditoria)
                      handleArquivoNaoExiste
    
    -- Se arquivo vazio ou não existe
    if null conteudo
        then do
            putStrLn "[INFO] Iniciando com log de auditoria vazio."
            -- Retorna lista vazia []
            return []
        else do
            putStrLn "[INFO] Log de auditoria carregado com sucesso!"
            -- read converte String para [LogEntry]
            -- :: [LogEntry]: anotação de tipo explícita
            return (read conteudo :: [LogEntry])
  where
    -- Função auxiliar idêntica à de carregarInventario
    handleArquivoNaoExiste :: IOException -> IO String
    handleArquivoNaoExiste _ = do
        putStrLn "[INFO] Arquivo Auditoria.log nao encontrado."
        return ""


-- ============================================================================
-- FUNÇÕES DE PERSISTÊNCIA
-- Estas funções salvam dados em disco
-- ============================================================================

-- Função: salvarInventario
-- Descrição: Salva o inventário atual no arquivo Inventario.dat
-- IMPORTANTE: SOBRESCREVE o arquivo completamente (writeFile)
-- Parâmetros:
--   inventario :: Inventario - o inventário a ser salvo
-- Retorno: IO () - executa I/O mas não retorna valor útil
salvarInventario :: Inventario -> IO ()
salvarInventario inventario = do
    -- writeFile: SOBRESCREVE o arquivo com o novo conteúdo
    -- show: converte o Inventario para String (serialização)
    -- show inventario: transforma o Map em texto que pode ser lido depois
    writeFile arquivoInventario (show inventario)
    
    -- Mensagem de confirmação
    putStrLn "[PERSISTENCIA] Inventario salvo em Inventario.dat"


-- Função: adicionarLogAuditoria
-- Descrição: Adiciona UMA entrada de log ao final do arquivo Auditoria.log
-- IMPORTANTE: NÃO sobrescreve, apenas ADICIONA ao final (appendFile)
-- Este é o modo "append-only" requerido pela especificação
-- Parâmetros:
--   logEntry :: LogEntry - a entrada de log a ser adicionada
-- Retorno: IO ()
adicionarLogAuditoria :: LogEntry -> IO ()
adicionarLogAuditoria logEntry = do
    -- appendFile: ADICIONA ao final do arquivo sem apagar o conteúdo anterior
    -- show logEntry: converte LogEntry para String
    -- ++ "\n": adiciona uma quebra de linha no final
    appendFile arquivoAuditoria (show logEntry ++ "\n")
    
    -- Mensagem de confirmação
    putStrLn "[AUDITORIA] Log registrado em Auditoria.log"


-- Função auxiliar: salvarLogCompleto
-- Descrição: Salva a lista COMPLETA de logs (usado raramente)
-- DIFERENTE de adicionarLogAuditoria que adiciona UM log
-- Esta função SOBRESCREVE todo o arquivo
-- Parâmetros:
--   logs :: [LogEntry] - lista completa de logs
salvarLogCompleto :: [LogEntry] -> IO ()
salvarLogCompleto logs = do
    -- writeFile: sobrescreve o arquivo
    -- show logs: converte a lista inteira para String
    writeFile arquivoAuditoria (show logs)
    putStrLn "[AUDITORIA] Log completo salvo em Auditoria.log"


-- ============================================================================
-- PARSER DE COMANDOS
-- Esta seção interpreta o que o usuário digita
-- ============================================================================

-- Tipo: Comando
-- Descrição: Representa TODOS os comandos que o usuário pode digitar
-- Este é um ADT (Algebraic Data Type) - tipo algébrico de dados
data Comando
    -- Comando para adicionar item: add <ID> <nome> <qtd> <categoria>
    -- String String Int String: itemID, nome, quantidade, categoria
    = CmdAdd String String Int String
    
    -- Comando para remover: remove <ID> <quantidade>
    -- String Int: itemID, quantidadeRemover
    | CmdRemove String Int
    
    -- Comando para atualizar: update <ID> <nova_quantidade>
    -- String Int: itemID, novaQuantidade
    | CmdUpdate String Int
    
    -- Comando para listar: list
    -- Sem parâmetros, por isso não tem tipos depois
    | CmdList
    
    -- Comando para gerar relatório: report
    -- Sem parâmetros
    | CmdReport
    
    -- Comando para ajuda: help
    -- Sem parâmetros
    | CmdHelp
    
    -- Comando para sair: exit
    -- Sem parâmetros
    | CmdExit
    
    -- Comando inválido: guarda mensagem de erro
    -- String: mensagem explicando o erro
    | CmdInvalido String
    
    -- deriving: cria automaticamente implementações de type classes
    -- Show: permite converter Comando para String (para debug)
    -- Eq: permite comparar Comandos com == e /=
    --     (necessário para "if cmd == CmdExit")
    deriving (Show, Eq)


-- Função: parseComando
-- Descrição: Transforma o texto digitado pelo usuário em um Comando
-- Esta é uma das funções MAIS IMPORTANTES - ela interpreta entrada do usuário
-- Parâmetros:
--   input :: String - o que o usuário digitou (ex: "add 001 Mouse 10 Perifericos")
-- Retorno:
--   Comando - um dos construtores do tipo Comando
parseComando :: String -> Comando
parseComando input =
    -- let: define variáveis locais
    -- words: separa String em lista de palavras
    -- Ex: "add 001 Mouse" vira ["add", "001", "Mouse"]
    let tokens = words input
    
    -- in: usa as variáveis definidas acima
    -- case: escolhe ação baseada no padrão (pattern matching)
    in case tokens of
        -- Padrão: ("add" : idItem : nomeItem : qtd : cat : _)
        -- : é o construtor de lista (cons)
        -- _ significa "ignora o resto da lista"
        -- Exemplo: ["add", "001", "Mouse", "10", "Perifericos", ...]
        ("add":idItem:nomeItem:qtd:cat:_) ->
            -- reads: tenta converter String em Int
            -- reads "10" :: [(Int, String)] dá [(10, "")]
            -- Se falhar, dá lista vazia []
            case reads qtd of
                -- [(quantidade, "")]: conversão bem-sucedida
                -- "" significa que consumiu toda a string
                [(quantidade, "")] -> 
                    -- CmdAdd: cria comando de adicionar
                    -- unwords: junta lista de palavras de volta em String
                    -- (cat:drop 4 tokens): pega categoria e resto
                    -- drop 4: remove os 4 primeiros elementos (add, id, nome, qtd)
                    CmdAdd idItem nomeItem quantidade (unwords (cat:drop 4 tokens))
                
                -- Qualquer outro caso (_): conversão falhou
                _ -> CmdInvalido "Quantidade invalida para comando 'add'"
        
        -- Padrão para remove: ["remove", "001", "5", ...]
        ("remove":idItem:qtd:_) ->
            -- Mesma lógica de conversão de quantidade
            case reads qtd of
                [(quantidade, "")] -> CmdRemove idItem quantidade
                _ -> CmdInvalido "Quantidade invalida para comando 'remove'"
        
        -- Padrão para update: ["update", "001", "20", ...]
        ("update":idItem:qtd:_) ->
            case reads qtd of
                [(quantidade, "")] -> CmdUpdate idItem quantidade
                _ -> CmdInvalido "Quantidade invalida para comando 'update'"
        
        -- Padrão exato: ["list"] - SEM outros elementos
        ["list"] -> CmdList
        
        -- Padrão exato: ["report"]
        ["report"] -> CmdReport
        
        -- Padrão exato: ["help"]
        ["help"] -> CmdHelp
        
        -- Padrão exato: ["exit"]
        ["exit"] -> CmdExit
        
        -- Padrão: [] - lista vazia (usuário só apertou Enter)
        [] -> CmdInvalido "Comando vazio"
        
        -- Padrão _: qualquer outra coisa
        -- unwords tokens: junta as palavras de volta
        _ -> CmdInvalido ("Comando desconhecido: " ++ unwords tokens)


-- ============================================================================
-- PROCESSAMENTO DE COMANDOS
-- Esta seção executa os comandos interpretados pelo parser
-- ============================================================================

-- Função: processarComando
-- Descrição: Esta é a função CENTRAL que executa cada comando
-- Ela faz a PONTE entre o mundo puro (LogicaNegocio) e o mundo impuro (IO)
-- 
-- Fluxo:
--   1. Recebe comando do usuário
--   2. Pega tempo atual
--   3. Chama função PURA do Aluno 2
--   4. Processa resultado (Either)
--   5. Salva arquivos se sucesso
--   6. Retorna novo estado
--
-- Parâmetros:
--   cmd :: Comando - o comando a ser executado
--   inventario :: Inventario - estado ATUAL do inventário
-- Retorno:
--   IO Inventario - novo estado do inventário (dentro de IO)
processarComando :: Comando -> Inventario -> IO Inventario
processarComando cmd inventario = do
    -- getCurrentTime: pega o horário atual do sistema
    -- tempo :: UTCTime
    -- <- : extrai valor de uma ação IO
    tempo <- getCurrentTime
    
    -- case: escolhe ação baseada no comando
    -- Cada 'case' abaixo processa um tipo diferente de comando
    case cmd of
        -- ====================================================================
        -- COMANDO: ADD (Adicionar item)
        -- ====================================================================
        CmdAdd idItem nomeItem qtd cat -> do
            -- Primeiro: criar o item a ser adicionado
            -- let: define variável local
            -- Item { ... }: construtor de registro do tipo Item
            let novoItem = Item
                    { itemID = idItem        -- Ex: "001"
                    , nome = nomeItem        -- Ex: "Mouse"
                    , quantidade = qtd       -- Ex: 10
                    , categoria = cat        -- Ex: "Perifericos"
                    }
            
            -- Segundo: chamar função PURA do Aluno 2
            -- addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
            -- addItem retorna Either:
            --   - Left String: se deu erro (ex: ID duplicado)
            --   - Right (Inventario, LogEntry): se deu certo
            case addItem tempo novoItem inventario of
                -- CASO 1: Deu ERRO (Left)
                Left erro -> do
                    -- \n: quebra de linha
                    -- ++: concatena strings
                    -- Mostra mensagem de erro pro usuário
                    putStrLn $ "\n[ERRO] " ++ erro
                    
                    -- IMPORTANTE: Mesmo com erro, registramos no log!
                    -- Criamos uma LogEntry de falha
                    let logEntry = LogEntry
                            { timestamp = tempo           -- quando ocorreu
                            , acao = Add                  -- qual operação
                            , detalhes = "Falha ao adicionar item ID: " ++ idItem
                            , status = Falha erro         -- Falha com mensagem
                            }
                    
                    -- Adiciona essa falha ao log
                    adicionarLogAuditoria logEntry
                    
                    -- Retorna inventário SEM MUDANÇAS (porque deu erro)
                    return inventario
                
                -- CASO 2: Deu CERTO (Right)
                -- novoInventario: o inventário atualizado
                -- logEntry: a entrada de log criada pela função pura
                Right (novoInventario, logEntry) -> do
                    -- Mostra mensagem de sucesso
                    putStrLn $ "\n[SUCESSO] Item adicionado: " ++ nomeItem ++ " (ID: " ++ idItem ++ ")"
                    
                    -- SALVA o novo inventário no arquivo (writeFile)
                    salvarInventario novoInventario
                    
                    -- ADICIONA a entrada de log ao arquivo (appendFile)
                    adicionarLogAuditoria logEntry
                    
                    -- Retorna o NOVO inventário (com o item adicionado)
                    return novoInventario

        -- ====================================================================
        -- COMANDO: REMOVE (Remover quantidade de um item)
        -- ====================================================================
        CmdRemove idItem qtd -> do
            -- Chama função PURA removeItem do Aluno 2
            -- removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
            -- Parâmetros: tempo, itemID, quantidadeRemover, inventarioAtual
            case removeItem tempo idItem qtd inventario of
                -- ERRO: Ex: item não existe ou estoque insuficiente
                Left erro -> do
                    putStrLn $ "\n[ERRO] " ++ erro
                    
                    -- Cria log de falha
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Remove              -- acao é do tipo AcaoLog
                            , detalhes = "Falha ao remover do item ID: " ++ idItem
                            , status = Falha erro
                            }
                    
                    -- Registra falha no log
                    adicionarLogAuditoria logEntry
                    
                    -- Retorna inventário inalterado
                    return inventario
                
                -- SUCESSO: item foi removido
                Right (novoInventario, logEntry) -> do
                    -- show qtd: converte Int para String
                    putStrLn $ "\n[SUCESSO] Removidas " ++ show qtd ++ " unidade(s) do item ID: " ++ idItem
                    
                    -- Salva novo estado
                    salvarInventario novoInventario
                    
                    -- Registra sucesso no log
                    adicionarLogAuditoria logEntry
                    
                    -- Retorna inventário atualizado
                    return novoInventario

        -- ====================================================================
        -- COMANDO: UPDATE (Atualizar quantidade)
        -- ====================================================================
        CmdUpdate idItem novaQtd -> do
            -- Chama função PURA updateQty do Aluno 2
            -- updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
            case updateQty tempo idItem novaQtd inventario of
                -- ERRO: Ex: item não existe, quantidade negativa
                Left erro -> do
                    putStrLn $ "\n[ERRO] " ++ erro
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Update              -- acao do tipo AcaoLog
                            , detalhes = "Falha ao atualizar item ID: " ++ idItem
                            , status = Falha erro
                            }
                    adicionarLogAuditoria logEntry
                    return inventario
                
                -- SUCESSO: quantidade atualizada
                Right (novoInventario, logEntry) -> do
                    putStrLn $ "\n[SUCESSO] Quantidade do item ID: " ++ idItem ++ " atualizada para " ++ show novaQtd
                    salvarInventario novoInventario
                    adicionarLogAuditoria logEntry
                    return novoInventario

        -- ====================================================================
        -- COMANDO: LIST (Listar todos os itens)
        -- ====================================================================
        CmdList -> do
            putStrLn "\n========== INVENTARIO ATUAL =========="
            
            -- Map.null: verifica se o Map está vazio
            if Map.null inventario
                then putStrLn "Inventario vazio."
                else 
                    -- Map.elems: pega todos os valores (Items) do Map
                    -- mapM_: aplica uma ação IO para cada elemento da lista
                    -- imprimirItem: função que imprime um Item (definida abaixo)
                    mapM_ imprimirItem (Map.elems inventario)
            
            putStrLn "======================================\n"
            
            -- NÃO modifica o inventário, então retorna o mesmo
            return inventario

        -- ====================================================================
        -- COMANDO: REPORT (Gerar relatório - para Aluno 4)
        -- ====================================================================
        CmdReport -> do
            putStrLn "\n[INFO] Carregando logs para gerar relatorio..."
            
            -- Carrega todos os logs do arquivo
            logs <- carregarLogs
            
            putStrLn "\n========== RELATORIO DE LOGS =========="
            -- length: retorna tamanho da lista
            putStrLn $ "Total de logs: " ++ show (length logs)
            putStrLn "======================================\n"
            
            -- NOTA: Aqui o Aluno 4 vai adicionar as funções de análise
            putStrLn "[NOTA] Relatorios detalhados serao implementados pelo Aluno 4"
            
            -- Não modifica inventário
            return inventario

        -- ====================================================================
        -- COMANDO: HELP (Mostrar ajuda)
        -- ====================================================================
        CmdHelp -> do
            -- Chama função que mostra menu de ajuda
            mostrarAjuda
            
            -- Não modifica inventário
            return inventario

        -- ====================================================================
        -- COMANDO: EXIT (Sair do programa)
        -- ====================================================================
        CmdExit -> do
            putStrLn "\n[INFO] Encerrando o sistema..."
            
            -- Retorna inventário
            -- O loop principal vai detectar CmdExit e parar
            return inventario

        -- ====================================================================
        -- COMANDO: INVÁLIDO (Comando não reconhecido)
        -- ====================================================================
        -- msg: mensagem de erro que veio do parser
        CmdInvalido msg -> do
            putStrLn $ "\n[ERRO] " ++ msg
            putStrLn "Digite 'help' para ver os comandos disponiveis.\n"
            
            -- Não modifica inventário
            return inventario


-- ============================================================================
-- FUNÇÕES AUXILIARES DE EXIBIÇÃO
-- ============================================================================

-- Função: imprimirItem
-- Descrição: Imprime as informações de UM item de forma formatada
-- Parâmetros:
--   item :: Item - o item a ser impresso
-- Retorno: IO () - apenas imprime, não retorna valor útil
imprimirItem :: Item -> IO ()
imprimirItem item = do
    -- Acessa os campos do registro Item usando funções de acesso
    -- itemID item: pega o campo itemID do item
    putStrLn $ "  ID: " ++ itemID item
    putStrLn $ "  Nome: " ++ nome item
    -- show: converte Int para String
    putStrLn $ "  Quantidade: " ++ show (quantidade item)
    putStrLn $ "  Categoria: " ++ categoria item
    putStrLn "  ---"  -- Separador entre itens


-- Função: mostrarAjuda
-- Descrição: Exibe o menu de ajuda com todos os comandos disponíveis
-- Retorno: IO ()
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
    putStrLn "  - Gera relatorio de logs (sera implementado pelo Aluno 4)"
    putStrLn ""
    putStrLn "help"
    putStrLn "  - Mostra esta mensagem de ajuda"
    putStrLn ""
    putStrLn "exit"
    putStrLn "  - Encerra o programa"
    putStrLn "==========================================\n"


-- ============================================================================
-- LOOP PRINCIPAL
-- Esta é a função que mantém o programa rodando
-- ============================================================================

-- Função: loopPrincipal
-- Descrição: Loop infinito que lê comandos do usuário e os executa
-- Este loop só termina quando o usuário digita "exit"
-- 
-- Como funciona:
--   1. Mostra prompt ">"
--   2. Lê entrada do usuário
--   3. Interpreta comando (parser)
--   4. Se for "exit", encerra
--   5. Se não, processa comando
--   6. Volta para o passo 1 (recursão)
--
-- Parâmetros:
--   inventario :: Inventario - estado ATUAL do inventário
-- Retorno: IO () - não retorna nada útil, apenas executa ações
loopPrincipal :: Inventario -> IO ()
loopPrincipal inventario = do
    -- putStr: imprime SEM quebra de linha no final
    -- Mostra o prompt ">" esperando entrada
    putStr "\n> "
    
    -- hFlush stdout: força o buffer a ser escrito IMEDIATAMENTE
    -- Sem isso, o ">" pode não aparecer até o usuário digitar algo
    -- stdout: é a saída padrão (o terminal)
    hFlush stdout
    
    -- getLine: lê uma linha inteira digitada pelo usuário
    -- <- : extrai a String da ação IO
    -- input :: String
    input <- getLine
    
    -- Chama o parser para interpretar o que foi digitado
    -- parseComando :: String -> Comando
    let cmd = parseComando input
    
    -- Verifica se o comando é CmdExit
    -- == : operador de igualdade
    if cmd == CmdExit
        then do
            -- Se for exit:
            putStrLn "\n[INFO] Sistema encerrado com sucesso!"
            putStrLn "Ate logo!\n"
            -- NÃO chama loopPrincipal novamente, então o programa encerra
        else do
            -- Se NÃO for exit:
            
            -- Processa o comando e obtém o NOVO estado do inventário
            -- processarComando :: Comando -> Inventario -> IO Inventario
            -- <- : extrai o Inventario da ação IO
            novoInventario <- processarComando cmd inventario
            
            -- RECURSÃO: Chama loopPrincipal novamente com o NOVO inventário
            -- Isso cria o "loop" - a função chama a si mesma
            -- Em Haskell, recursão é a forma idiomática de fazer loops
            loopPrincipal novoInventario
            -- Quando esta função retornar, o loop terá terminado


-- ============================================================================
-- FUNÇÃO MAIN - PONTO DE ENTRADA DO PROGRAMA
-- Esta é a PRIMEIRA função executada quando o programa inicia
-- ============================================================================

-- Função: main
-- Descrição: Ponto de entrada principal do programa
-- Esta função é chamada automaticamente quando você executa o programa
-- 
-- Sequência de execução:
--   1. Mostra banner de boas-vindas
--   2. Carrega Inventario.dat e Auditoria.log do disco
--   3. Mostra informações sobre dados carregados
--   4. Inicia o loop principal
--
-- Retorno: IO () - executa ações de I/O, não retorna valor útil
main :: IO ()
main = do
    -- Banner de boas-vindas
    putStrLn "\n=========================================="
    putStrLn "   SISTEMA DE GERENCIAMENTO DE INVENTARIO"
    putStrLn "=========================================="
    putStrLn ""
    
    -- Fase de inicialização
    putStrLn "[INICIALIZACAO] Carregando dados do sistema..."
    
    -- Carrega o inventário do disco (ou cria vazio se não existir)
    -- carregarInventario :: IO Inventario
    -- <- : extrai o Inventario da ação IO
    -- inventario :: Inventario
    inventario <- carregarInventario
    
    -- Carrega os logs do disco (ou cria lista vazia se não existir)
    -- carregarLogs :: IO [LogEntry]
    -- logs :: [LogEntry]
    logs <- carregarLogs
    
    -- Mostra estatísticas dos dados carregados
    
    -- Map.size: retorna quantos itens tem no Map
    -- show: converte Int para String
    -- $: operador de aplicação de função (evita parênteses)
    putStrLn $ "[INFO] Inventario contem " ++ show (Map.size inventario) ++ " item(ns)"
    
    -- length: retorna tamanho da lista
    putStrLn $ "[INFO] Log contem " ++ show (length logs) ++ " entrada(s)"
    putStrLn ""
    
    -- Dica para o usuário
    putStrLn "Digite 'help' para ver os comandos disponiveis."
    
    -- Inicia o loop principal com o inventário carregado
    -- loopPrincipal :: Inventario -> IO ()
    -- A partir daqui, o programa fica em loop até o usuário digitar "exit"
    loopPrincipal inventario
    
    -- Quando loopPrincipal retornar, o programa encerra


-- ============================================================================
-- FIM DO MÓDULO IOPersistencia.hs
-- ============================================================================
-- RESUMO DO QUE ESTE MÓDULO FAZ:
--
-- 1. INICIALIZAÇÃO:
--    - carregarInventario: lê Inventario.dat (com tratamento de erro)
--    - carregarLogs: lê Auditoria.log (com tratamento de erro)
--
-- 2. PERSISTÊNCIA:
--    - salvarInventario: SOBRESCREVE Inventario.dat
--    - adicionarLogAuditoria: ADICIONA ao final de Auditoria.log
--
-- 3. PARSER:
--    - parseComando: converte texto em Comando
--    - Suporta: add, remove, update, list, report, help, exit
--
-- 4. PROCESSAMENTO:
--    - processarComando: executa comandos
--    - Chama funções PURAS do Aluno 2
--    - Trata Either (sucesso/falha)
--    - Salva arquivos quando sucesso
--    - Registra logs sempre (sucesso E falha)
--
-- 5. LOOP:
--    - loopPrincipal: mantém programa rodando
--    - Lê comandos do usuário
--    - Usa recursão para criar o loop
--
-- 6. MAIN:
--    - Ponto de entrada
--    - Carrega dados iniciais
--    - Inicia loop
--
-- SEPARAÇÃO PURE/IMPURE:
--    - TUDO neste arquivo é IMPURO (IO)
--    - Funções PURAS estão em LogicaNegocio.hs (Aluno 2)
--    - Esta é a PONTE entre usuário e lógica
-- ============================================================================
