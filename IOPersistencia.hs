-- ============================================================================
-- Módulo: IOPersistencia.hs
-- Descrição: Funções PURAS para parsing de comandos
-- Responsável: Aluno 3 (Jafte Carneiro Fagundes da Silva)
-- ============================================================================

module IOPersistencia 
    ( Comando(..)
    , parseComando
    , arquivoInventario
    , arquivoAuditoria
    ) where


-- ============================================================================
-- CONSTANTES
-- ============================================================================

-- Nome do arquivo de inventário (sobrescrito a cada operação)
arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

-- Nome do arquivo de auditoria (modo append-only)
arquivoAuditoria :: FilePath
arquivoAuditoria = "Auditoria.log"


-- ============================================================================
-- TIPOS DE DADOS
-- ============================================================================

-- ADT representando todos os comandos possíveis do sistema
data Comando
    = CmdAdd String String Int String    -- add <ID> <nome> <qtd> <categoria>
    | CmdRemove String Int                -- remove <ID> <qtd>
    | CmdUpdate String Int                -- update <ID> <nova_qtd>
    | CmdList                             -- list
    | CmdReport                           -- report
    | CmdHelp                             -- help
    | CmdExit                             -- exit
    | CmdInvalido String                  -- comando inválido
    deriving (Show, Eq)


-- ============================================================================
-- PARSER DE COMANDOS (FUNÇÃO PURA)
-- ============================================================================

-- Transforma a entrada do usuário em um Comando
-- Função pura: mesma entrada sempre produz mesma saída
parseComando :: String -> Comando
parseComando input =
    let tokens = words input
    in case tokens of
        ("add":idItem:nomeItem:qtd:cat:_) ->
            case reads qtd of
                [(quantidade, "")] -> 
                    CmdAdd idItem nomeItem quantidade (unwords (cat:drop 4 tokens))
                _ -> CmdInvalido "Quantidade invalida para comando 'add'"
        
        ("remove":idItem:qtd:_) ->
            case reads qtd of
                [(quantidade, "")] -> CmdRemove idItem quantidade
                _ -> CmdInvalido "Quantidade invalida para comando 'remove'"
        
        ("update":idItem:qtd:_) ->
            case reads qtd of
                [(quantidade, "")] -> CmdUpdate idItem quantidade
                _ -> CmdInvalido "Quantidade invalida para comando 'update'"
        
        ["list"] -> CmdList
        ["report"] -> CmdReport
        ["help"] -> CmdHelp
        ["exit"] -> CmdExit
        [] -> CmdInvalido "Comando vazio"
        _ -> CmdInvalido ("Comando desconhecido: " ++ unwords tokens)