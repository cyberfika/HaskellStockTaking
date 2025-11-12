-- ============================================================================
-- Módulo: IOPersistencia.hs
-- Descrição: Funções PURAS auxiliares para parsing e tipos de comandos
-- Responsável: Aluno 3 (Jafte Carneiro Fagundes da Silva)
-- Objetivo: Parser de comandos e definições auxiliares (SEM I/O)
-- ============================================================================
-- IMPORTANTE: Este módulo contém APENAS funções PURAS
-- Todas as operações de I/O estão em Main.hs
-- ============================================================================

module IOPersistencia 
    ( Comando(..)         -- Exporta tipo Comando e seus construtores
    , parseComando        -- Exporta função de parsing
    , arquivoInventario   -- Exporta constante
    , arquivoAuditoria    -- Exporta constante
    ) where


-- ============================================================================
-- CONSTANTES - Nomes dos arquivos usados pelo sistema
-- ============================================================================
-- Estas são constantes PURAS (não fazem I/O)

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
-- TIPOS DE DADOS AUXILIARES
-- ============================================================================

-- Tipo: Comando
-- Descrição: Representa TODOS os comandos que o usuário pode digitar
-- Este é um ADT (Algebraic Data Type) - tipo algébrico de dados
-- PURO: apenas define tipos, não faz I/O
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


-- ============================================================================
-- PARSER DE COMANDOS (FUNÇÃO PURA)
-- ============================================================================

-- Função: parseComando
-- Descrição: Transforma o texto digitado pelo usuário em um Comando
-- Esta é uma FUNÇÃO PURA - não faz I/O, apenas transforma String em Comando
-- 
-- Parâmetros:
--   input :: String - o que o usuário digitou (ex: "add 001 Mouse 10 Perifericos")
-- Retorno:
--   Comando - um dos construtores do tipo Comando
--
-- IMPORTANTE: Esta função é PURA porque:
--   - Sempre retorna o mesmo Comando para a mesma String
--   - Não faz leitura/escrita de arquivos
--   - Não interage com o usuário
--   - Apenas transforma dados
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
-- FIM DO MÓDULO IOPersistencia.hs (FUNÇÕES PURAS)
-- ============================================================================
-- RESUMO:
-- Este módulo contém APENAS código PURO:
--   1. Constantes (arquivoInventario, arquivoAuditoria)
--   2. Tipo de dados (Comando)
--   3. Função de parsing (parseComando)
--
-- NENHUMA função aqui faz I/O!
-- Toda a interação com arquivos e usuário está em Main.hs
-- ============================================================================