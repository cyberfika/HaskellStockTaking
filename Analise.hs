-- Módulo: Analise.hs
-- Descrição: Implementa a camada de análise e geração de relatórios
-- Responsável: Aluno 4 (Ângelo Piovezan Jorgeto)
-- Objetivo: Funções puras de análise sobre a lista de LogEntry

module Analise
    ( logsDeErro
    , historicoPorItem
    , itemMaisMovimentado
    , formatarRelatorioCompleto -- Função de formatação para IO
    , formatarLog -- Helper
    ) where

-- Importações
import InventarioTipos
import Data.List (group, sort, sortBy, filter, find, tails, isPrefixOf, drop, isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down(..))
import Data.Time (TimeZone, utcToZonedTime, formatTime)
import Data.Time.Format (defaultTimeLocale)




-- ============================================================================
-- FUNÇÕES DE ANÁLISE PURAS
-- ============================================================================

-- Função: logsDeErro
-- Descrição: Filtra a lista de logs, retornando apenas aqueles com status 'Falha'
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (ehFalha . status)
  where
    ehFalha :: StatusLog -> Bool
    ehFalha (Falha _) = True
    ehFalha Sucesso   = False

-- Função: historicoPorItem
-- Descrição: Filtra logs que mencionam um ID de item específico
-- Procura por logs que contenham "ID: <idAlvo>" ou "(ID: <idAlvo>)"
historicoPorItem :: [LogEntry] -> String -> [LogEntry]
historicoPorItem logs idAlvo =
    let -- Cria os marcadores de busca com base nos padrões de LogicaNegocio.hs
        marcador1 = "ID: " ++ idAlvo
        marcador2 = "(ID: " ++ idAlvo
    in filter (\log -> (marcador1 `isInfixOf` detalhes log) || (marcador2 `isInfixOf` detalhes log)) logs


-- Função: itemMaisMovimentado
-- Descrição: Encontra o itemID com mais logs de SUCESSO
-- "Movimentado" é definido como o número de transações bem-sucedidas (Add, Remove, Update)
itemMaisMovimentado :: [LogEntry] -> String
itemMaisMovimentado logs =
    -- Extrai todos os IDs de logs de SUCESSO
    let ids = mapMaybe extractSuccessItemID logs
    in case ids of
        -- Se não houver IDs retorna a mensagem
        [] -> "Nenhuma movimentacao de sucesso registrada."
        
        -- Se houver IDs
        _  -> 
           -- Agrupa e ordena
           let groupedIDs = group (sort ids)
               sortedGroups = sortBy (comparing (Down . length)) groupedIDs
           in 
           -- Usa pattern matching seguro
           case sortedGroups of
               ((topID:_):_) -> 
                   let count = length (filter (== topID) ids) -- Conta o total
                   in topID ++ " (com " ++ show count ++ " movimentacoes)"
               _ -> "Erro interno de analise de logs."

-- Helper para extrair ItemID de logs de SUCESSO
-- Procura pelo padrão "(ID: <ID>)" que é consistente em LogicaNegocio.hs para sucessos
extractSuccessItemID :: LogEntry -> Maybe String
extractSuccessItemID log
    | status log /= Sucesso = Nothing -- Processa apenas logs de sucesso
    | otherwise =
        let details = detalhes log
            marker = "(ID: "
        in case find (isPrefixOf marker) (tails details) of
               -- Encontrou "(ID: ", pega o que vem depois até o ')'
               Just match -> Just $ takeWhile (/= ')') (drop (length marker) match)
               Nothing    -> Nothing

-- ============================================================================
-- FUNÇÕES DE FORMATAÇÃO DE RELATÓRIO
-- ============================================================================

-- Função: formatarLog
-- Descrição: Converte um LogEntry em uma String formatada para exibição
formatarLog :: TimeZone -> LogEntry -> String
formatarLog tz log =
    let 
        -- Converte o UTC para ZonedTime (horário local)
        zonedTime = utcToZonedTime tz (timestamp log)
        -- Formata a data/hora
        formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" zonedTime
    in
        "[" ++ formattedTime ++ "] " ++
        "[" ++ show (acao log) ++ "] " ++
        "[" ++ formatarStatus (status log) ++ "] " ++
        detalhes log
  where
    formatarStatus Sucesso     = "SUCESSO"
    formatarStatus (Falha msg) = "FALHA: " ++ msg

-- Função: formatarRelatorioCompleto
-- Descrição: Cria o texto completo do relatório para ser impresso pelo módulo de IO
formatarRelatorioCompleto :: TimeZone -> [LogEntry] -> String
formatarRelatorioCompleto tz logs =
    let total = length logs
        erros = logsDeErro logs
        totalErros = length erros
        itemTop = itemMaisMovimentado logs
    in
        unlines [ "========== RELATORIO DE ANALISE DE LOGS =========="
                , ""
                , "** Sumario Geral **"
                , "Total de entradas de log: " ++ show total
                , "Total de operacoes com falha: " ++ show totalErros
                , "Item mais movimentado (com sucesso): " ++ itemTop
                , ""
                , "---"
                , ""
                , "** Detalhe de Logs de Erro (" ++ show totalErros ++ ") **"
                , if null erros
                  then "Nenhum erro registrado."
                  -- Mapeia e indenta cada log de erro
                  else unlines (map (("  " ++) . formatarLog tz) erros)
                , "===================================================="
                ]