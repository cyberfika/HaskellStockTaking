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
-- Retorna Left com inventário inalterado e log de erro se falhar.
-- Retorna Right com inventário atualizado e log de sucesso.
addItem :: UTCTime -> Item -> Inventario -> Either ResultadoOperacao ResultadoOperacao
addItem tempo novoItem inventario
    | quantidade novoItem < 0 =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Add
                , detalhes = "Tentativa de adicionar item '" ++ nome novoItem ++
                           "' (ID: " ++ itemID novoItem ++ ") com quantidade negativa"
                , status = Falha "Quantidade deve ser maior ou igual a zero"
                }
        in Left (inventario, logEntry)

    | Map.member (itemID novoItem) inventario =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Add
                , detalhes = "Tentativa de adicionar item com ID duplicado: " ++ itemID novoItem
                , status = Falha "Item com ID ja existe no inventario"
                }
        in Left (inventario, logEntry)

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
-- Retorna Left com inventário inalterado e log de erro se falhar.
-- Retorna Right com inventário atualizado e log de sucesso.
removeItem :: UTCTime -> String -> Int -> Inventario -> Either ResultadoOperacao ResultadoOperacao
removeItem tempo idItem qtdRemover inventario
    | qtdRemover <= 0 =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Remove
                , detalhes = "Tentativa de remover quantidade invalida do item ID: " ++ idItem
                , status = Falha "Quantidade a remover deve ser maior que zero"
                }
        in Left (inventario, logEntry)

    | otherwise =
        case Map.lookup idItem inventario of
            Nothing ->
                let logEntry = LogEntry
                        { timestamp = tempo
                        , acao = Remove
                        , detalhes = "Tentativa de remover item inexistente. ID: " ++ idItem
                        , status = Falha "Item nao encontrado no inventario"
                        }
                in Left (inventario, logEntry)

            Just itemAtual
                | quantidade itemAtual < qtdRemover ->
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Remove
                            , detalhes = "Estoque insuficiente no item '" ++ nome itemAtual ++
                                       "' (ID: " ++ idItem ++ "). Disponivel: " ++
                                       show (quantidade itemAtual) ++ ", Solicitado: " ++
                                       show qtdRemover
                            , status = Falha "Estoque insuficiente"
                            }
                    in Left (inventario, logEntry)

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
-- Retorna Left com inventário inalterado e log de erro se falhar.
-- Retorna Right com inventário atualizado e log de sucesso.
updateQty :: UTCTime -> String -> Int -> Inventario -> Either ResultadoOperacao ResultadoOperacao
updateQty tempo idItem novaQtd inventario
    | novaQtd < 0 =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Update
                , detalhes = "Tentativa de atualizar item ID: " ++ idItem ++
                           " com quantidade negativa"
                , status = Falha "Quantidade deve ser maior ou igual a zero"
                }
        in Left (inventario, logEntry)

    | otherwise =
        case Map.lookup idItem inventario of
            Nothing ->
                let logEntry = LogEntry
                        { timestamp = tempo
                        , acao = Update
                        , detalhes = "Tentativa de atualizar item inexistente. ID: " ++ idItem
                        , status = Falha "Item nao encontrado no inventario"
                        }
                in Left (inventario, logEntry)

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
