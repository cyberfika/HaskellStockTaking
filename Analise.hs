-- Módulo: Analise.hs
-- Responsável: Aluno 4 (Ângelo Piovezan Jorgeto)
-- Objetivo: Funções puras para análise e geração de relatórios de logs.

module Analise
    ( logsDeErro
    , historicoPorItem
    , itemMaisMovimentado
    , formatarRelatorioCompleto
    , formatarLog
    ) where

-- Importações
import InventarioTipos
import Data.List (group, sort, sortBy, filter, find, tails, isPrefixOf, drop, isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down(..))
import Data.Time (TimeZone, utcToZonedTime, formatTime)
import Data.Time.Format (defaultTimeLocale)

-- Funções de Análise Puras

-- Filtra a lista de logs, retornando apenas aqueles com status 'Falha'
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (ehFalha . status)
  where
    ehFalha :: StatusLog -> Bool
    ehFalha (Falha _) = True
    ehFalha Sucesso   = False

-- Filtra logs que mencionam um ID de item específico
historicoPorItem :: [LogEntry] -> String -> [LogEntry]
historicoPorItem logs idAlvo =
    let 
        marcador1 = "ID: " ++ idAlvo
        marcador2 = "(ID: " ++ idAlvo
    in 
        filter (\log -> (marcador1 `isInfixOf` detalhes log) || (marcador2 `isInfixOf` detalhes log)) logs


-- Encontra o itemID com mais logs de SUCESSO
itemMaisMovimentado :: [LogEntry] -> String
itemMaisMovimentado logs =
    let ids = mapMaybe extractSuccessItemID logs
    in case ids of
        [] -> "Nenhuma movimentacao de sucesso registrada."
        _  -> 
           let groupedIDs = group (sort ids)
               sortedGroups = sortBy (comparing (Down . length)) groupedIDs
           in 
           case sortedGroups of
               ((topID:_):_) -> 
                   let count = length (filter (== topID) ids)
                   in topID ++ " (com " ++ show count ++ " movimentacoes)"
               _ -> "Erro interno de analise de logs."

-- Helper para extrair ItemID de logs de Sucesso
extractSuccessItemID :: LogEntry -> Maybe String
extractSuccessItemID log
    | status log /= Sucesso = Nothing 
    | otherwise =
        let details = detalhes log
            marker = "(ID: "
        in 
            case find (isPrefixOf marker) (tails details) of
               Just match -> Just $ takeWhile (/= ')') (drop (length marker) match)
               Nothing    -> Nothing

-- Funções de Formatação de Relatório

-- Converte um LogEntry em uma String formatada para exibição
formatarLog :: TimeZone -> LogEntry -> String
formatarLog tz log =
    let 
        zonedTime = utcToZonedTime tz (timestamp log)
        formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" zonedTime
    in
        "[" ++ formattedTime ++ "] " ++
        "[" ++ show (acao log) ++ "] " ++
        "[" ++ formatarStatus (status log) ++ "] " ++
        detalhes log
  where
    formatarStatus Sucesso     = "SUCESSO"
    formatarStatus (Falha msg) = "FALHA: " ++ msg

-- Cria o texto completo do relatório para ser impresso pelo módulo de IO
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
                  else unlines (map (("  " ++) . formatarLog tz) erros)
                , "===================================================="
                ]