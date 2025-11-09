-- Módulo: LogicaNegocio.hs
-- Descrição: Implementa a lógica de negócio pura do sistema de inventário
-- Responsável: Aluno 2
-- Objetivo: Funções puras de transação sem operações de IO

module LogicaNegocio
    ( ResultadoOperacao
    , addItem
    , removeItem
    , updateQty
    ) where

-- Importações
import InventarioTipos
import qualified Data.Map as Map
import Data.Time (UTCTime)


-- Tipo: ResultadoOperacao
-- Descrição: Tipo alias para o resultado de uma operação
-- Contém: (Inventario atualizado, LogEntry da operação)
type ResultadoOperacao = (Inventario, LogEntry)


-- Funções de Transação Puras

-- Função: addItem
-- Descrição: Adiciona um novo item ao inventário
-- Parâmetros:
--   - tempo: UTCTime da operação
--   - novoItem: O item a ser adicionado
--   - inventario: O inventário atual
-- Retorno: Either String ResultadoOperacao
--   - Left String: Mensagem de erro (ex: ID duplicado, quantidade inválida)
--   - Right ResultadoOperacao: Tupla com (inventário atualizado, log entry)
-- Validações:
--   - Verifica se o itemID já existe (ID duplicado)
--   - Verifica se a quantidade é válida (>= 0)
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem tempo novoItem inventario
    | quantidade novoItem < 0 =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Add
                , detalhes = "Tentativa de adicionar item '" ++ nome novoItem ++
                           "' (ID: " ++ itemID novoItem ++ ") com quantidade negativa: " ++
                           show (quantidade novoItem)
                , status = Falha "Quantidade deve ser maior ou igual a zero"
                }
        in Left "Erro: Quantidade deve ser maior ou igual a zero"
    | Map.member (itemID novoItem) inventario =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Add
                , detalhes = "Tentativa de adicionar item com ID duplicado: " ++
                           itemID novoItem
                , status = Falha "Item com ID ja existe no inventario"
                }
        in Left $ "Erro: Item com ID '" ++ itemID novoItem ++ "' ja existe no inventario"
    | otherwise =
        let inventarioNovo = Map.insert (itemID novoItem) novoItem inventario
            logEntry = LogEntry
                { timestamp = tempo
                , acao = Add
                , detalhes = "Adicionado item '" ++ nome novoItem ++ "' (ID: " ++
                           itemID novoItem ++ ") com quantidade " ++
                           show (quantidade novoItem) ++ " na categoria " ++
                           categoria novoItem
                , status = Sucesso
                }
        in Right (inventarioNovo, logEntry)

-- Função: removeItem
-- Descrição: Remove uma quantidade especificada de um item do inventário
-- Parâmetros:
--   - tempo: UTCTime da operação
--   - idItem: O ID do item a ser removido
--   - qtdRemover: A quantidade a ser removida
--   - inventario: O inventário atual
-- Retorno: Either String ResultadoOperacao
--   - Left String: Mensagem de erro (ex: item não encontrado, estoque insuficiente)
--   - Right ResultadoOperacao: Tupla com (inventário atualizado, log entry)
-- Validações:
--   - Verifica se o item existe
--   - Verifica se há estoque suficiente
--   - Verifica se a quantidade a remover é válida (> 0)
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem tempo idItem qtdRemover inventario
    | qtdRemover <= 0 =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Remove
                , detalhes = "Tentativa de remover quantidade invalida (" ++
                           show qtdRemover ++ ") do item ID: " ++ idItem
                , status = Falha "Quantidade a remover deve ser maior que zero"
                }
        in Left "Erro: Quantidade a remover deve ser maior que zero"
    | otherwise =
        case Map.lookup idItem inventario of
            Nothing ->
                let logEntry = LogEntry
                        { timestamp = tempo
                        , acao = Remove
                        , detalhes = "Tentativa de remover item inexistente. ID: " ++ idItem
                        , status = Falha "Item nao encontrado no inventario"
                        }
                in Left $ "Erro: Item com ID '" ++ idItem ++ "' nao encontrado no inventario"
            Just itemAtual ->
                if quantidade itemAtual < qtdRemover
                then
                    let logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Remove
                            , detalhes = "Tentativa de remover " ++ show qtdRemover ++
                                       " unidade(s) do item '" ++ nome itemAtual ++
                                       "' (ID: " ++ idItem ++ "). Estoque disponivel: " ++
                                       show (quantidade itemAtual)
                            , status = Falha "Estoque insuficiente"
                            }
                    in Left $ "Erro: Estoque insuficiente. Disponivel: " ++
                              show (quantidade itemAtual) ++ ", Solicitado: " ++
                              show qtdRemover
                else
                    let novaQuantidade = quantidade itemAtual - qtdRemover
                        itemAtualizado = itemAtual { quantidade = novaQuantidade }
                        inventarioNovo = Map.insert idItem itemAtualizado inventario
                        logEntry = LogEntry
                            { timestamp = tempo
                            , acao = Remove
                            , detalhes = "Removidas " ++ show qtdRemover ++
                                       " unidade(s) do item '" ++ nome itemAtual ++
                                       "' (ID: " ++ idItem ++ "). Quantidade anterior: " ++
                                       show (quantidade itemAtual) ++ ", Nova quantidade: " ++
                                       show novaQuantidade
                            , status = Sucesso
                            }
                    in Right (inventarioNovo, logEntry)

-- Função: updateQty
-- Descrição: Atualiza a quantidade de um item no inventário
-- Parâmetros:
--   - tempo: UTCTime da operação
--   - idItem: O ID do item a ser atualizado
--   - novaQtd: A nova quantidade do item
--   - inventario: O inventário atual
-- Retorno: Either String ResultadoOperacao
--   - Left String: Mensagem de erro (ex: item não encontrado, quantidade inválida)
--   - Right ResultadoOperacao: Tupla com (inventário atualizado, log entry)
-- Validações:
--   - Verifica se o item existe
--   - Verifica se a nova quantidade é válida (>= 0)
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty tempo idItem novaQtd inventario
    | novaQtd < 0 =
        let logEntry = LogEntry
                { timestamp = tempo
                , acao = Update
                , detalhes = "Tentativa de atualizar item ID: " ++ idItem ++
                           " com quantidade negativa: " ++ show novaQtd
                , status = Falha "Quantidade deve ser maior ou igual a zero"
                }
        in Left "Erro: Quantidade deve ser maior ou igual a zero"
    | otherwise =
        case Map.lookup idItem inventario of
            Nothing ->
                let logEntry = LogEntry
                        { timestamp = tempo
                        , acao = Update
                        , detalhes = "Tentativa de atualizar item inexistente. ID: " ++ idItem
                        , status = Falha "Item nao encontrado no inventario"
                        }
                in Left $ "Erro: Item com ID '" ++ idItem ++ "' nao encontrado no inventario"
            Just itemAtual ->
                let itemAtualizado = itemAtual { quantidade = novaQtd }
                    inventarioNovo = Map.insert idItem itemAtualizado inventario
                    qtdAnterior = quantidade itemAtual
                    logEntry = LogEntry
                        { timestamp = tempo
                        , acao = Update
                        , detalhes = "Atualizada quantidade do item '" ++ nome itemAtual ++
                                   "' (ID: " ++ idItem ++ ") de " ++ show qtdAnterior ++
                                   " para " ++ show novaQtd
                        , status = Sucesso
                        }
                in Right (inventarioNovo, logEntry)
