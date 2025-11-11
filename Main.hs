-- Arquivo: Main.hs
-- Descrição: Arquivo principal que integra todos os módulos do sistema

-- Importação usando qualified para evitar conflitos de nomes
import qualified IOPersistencia

-- O main do programa simplesmente chama o main do módulo IOPersistencia
main :: IO ()
main = IOPersistencia.main