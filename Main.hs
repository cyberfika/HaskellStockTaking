-- Arquivo: Main.hs
-- Descrição: Arquivo principal que integra todos os módulos do sistema
-- Equipe: Alunos 1, 2 e 3

import IOPersistencia (main)

-- O main do programa simplesmente chama o main do módulo IOPersistencia
main :: IO ()
main = IOPersistencia.main
