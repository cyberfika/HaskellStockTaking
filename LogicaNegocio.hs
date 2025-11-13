-- Módulo: LogicaNegocio.hs
-- Implementa a lógica de negócio pura do sistema de inventário
-- Responsável: Aluno 2

module LogicaNegocio
    ( ResultadoOperacao
    , addItem
    , removeItem
    , updateQty
    ) where

import InventarioTipos
import qualified Data.Map as Map
import Data.Time (UTCTime)


type ResultadoOperacao = (Inventario, LogEntry)


-- | Adiciona um item ao inventário.
-- Retorna Left com mensagem de erro se falhar.
-- Retorna Right com inventário atualizado e log de sucesso.
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem tempo novoItem inventario
    | quantidade novoItem < 0 =
        Left "Quantidade deve ser maior ou igual a zero"

    | Map.member (itemID novoItem) inventario =
        Left "Item com ID ja existe no inventario"

    | otherwise =
        let inventarioNovo = Map.insert (itemID novoItem) novoItem inventario
            logEntry = LogEntry
                { timestamp = tempo
                , acao = Add
                , detalhes = "Adicionado item '" ++ nome novoItem ++ "' (ID: " ++
                           itemID novoItem ++ ") com quantidade " ++
                           show (quantidade novoItem) ++ " na categoria " ++ categoria novoItem
                , status = Sucesso
                }
        in Right (inventarioNovo, logEntry)


-- | Remove uma quantidade de um item do inventário.
-- Retorna Left com mensagem de erro se falhar.
-- Retorna Right com inventário atualizado e log de sucesso.
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem tempo idItem qtdRemover inventario
    | qtdRemover <= 0 =
        Left "Quantidade a remover deve ser maior que zero"

    | otherwise =
        case Map.lookup idItem inventario of
            Nothing ->
                Left "Item nao encontrado no inventario"

            Just itemAtual
                | quantidade itemAtual < qtdRemover ->
                    Left "Estoque insuficiente"

                | otherwise ->
                    let novaQuantidade = quantidade itemAtual - qtdRemover
                        itemAtualizado = itemAtual { quantidade = novaQuantidade }
                        inventarioNovo = Map.insert idItem itemAtualizado inventario
                        logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Remove
                            , detalhes = "Removidas " ++ show qtdRemover ++
                                       " unidades do item '" ++ nome itemAtual ++
                                       "' (ID: " ++ idItem ++ ")"
                            , status = Sucesso
                            }
                    in Right (inventarioNovo, logEntry)


-- | Atualiza a quantidade de um item no inventário.
-- Retorna Left com mensagem de erro se falhar.
-- Retorna Right com inventário atualizado e log de sucesso.
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty tempo idItem novaQtd inventario
    | novaQtd < 0 =
        Left "Quantidade deve ser maior ou igual a zero"

    | otherwise =
        case Map.lookup idItem inventario of
            Nothing ->
                Left "Item nao encontrado no inventario"

            Just itemAtual ->
                let itemAtualizado = itemAtual { quantidade = novaQtd }
                    inventarioNovo = Map.insert idItem itemAtualizado inventario
                    logEntry = LogEntry
                        { timestamp = tempo
                        , acao = Update
                        , detalhes = "Atualizada quantidade do item '" ++ nome itemAtual ++
                                   "' (ID: " ++ idItem ++ ") para " ++ show novaQtd
                        , status = Sucesso
                        }
                in Right (inventarioNovo, logEntry)
