# Sistema de Gerenciamento de Inventário em Haskell

## Informações do Projeto

**Instituição:** Pontifícia Universidade Católica do Paraná (PUCPR)  
**Disciplina:** Programação Funcional  
**Professor:** Frank Coelho de Alcantara  
**Atividade:** RA2 - Sistema de Inventário em Haskell

### Equipe (em ordem de Responsabilidade)

| Nome do Aluno | GitHub Username | Responsabilidade |
|---------------|-----------------|------------------|
| [Aluno 1 Nome] | @usuario1 | Arquiteto de Dados |
| [Aluno 2 Nome] | @usuario2 | Lógica de Negócio Pura |
| [Aluno 3 Jafte Carneiro Fagundes da Silva] | @cyberfika | Módulo de I/O e Persistência |
| [Aluno 4 Ângelo Piovezan Jorgeto] | @PJorgeto | Validação, Documentação e Gerenciamento |

---

## 📋 Descrição do Sistema

Sistema de gerenciamento de inventário desenvolvido em Haskell que implementa:

- ✅ Separação clara entre lógica pura e operações de I/O
- ✅ Persistência de dados em arquivo (`Inventario.dat`)
- ✅ Log de auditoria completo (`Auditoria.log`)
- ✅ Tratamento de erros com `Either`
- ✅ Validação de estoque e operações
- ✅ Interface interativa via terminal

---

## 🔗 Links de Execução

### Online GDB
🔗 [Executar no Online GDB](https://www.onlinegdb.com/YOUR_PROJECT_LINK)

### Repl.it
🔗 [Executar no Repl.it](https://repl.it/@YOUR_USERNAME/YOUR_PROJECT)

---

## 📁 Estrutura do Projeto

```
projeto/
├── InventarioTipos.hs      # Aluno 1 - Definição de tipos de dados
├── LogicaNegocio.hs        # Aluno 2 - Funções puras de lógica
├── IOPersistencia.hs       # Aluno 3 - I/O e persistência
├── Analise.hs              # Aluno 4 - Análise de logs e relatórios
├── Main.hs                 # Arquivo principal
├── Inventario.dat          # Arquivo de dados (gerado em runtime)
├── Auditoria.log           # Arquivo de log (gerado em runtime)
└── README.md               # Este arquivo
```

---

## 🚀 Como Compilar e Executar

### Opção 1: Online GDB

1. Acesse: https://www.onlinegdb.com/
2. Selecione linguagem: **Haskell**
3. Copie todos os arquivos `.hs` para o editor
4. Certifique-se de que `Main.hs` está como arquivo principal
5. Clique em **Run**

### Opção 2: Repl.it

1. Acesse: https://repl.it/
2. Crie um novo Repl em **Haskell**
3. Faça upload de todos os arquivos `.hs`
4. Execute o comando: `runhaskell Main.hs`

### Opção 3: Ambiente Local

```bash
# Compilar
ghc --make Main.hs -o inventario

# Executar
./inventario
```

---

## 📖 Comandos Disponíveis

### Adicionar Item
```
add <ID> <nome> <quantidade> <categoria>
```
**Exemplo:** `add 001 Teclado 10 Perifericos`

### Remover Item
```
remove <ID> <quantidade>
```
**Exemplo:** `remove 001 5`

### Atualizar Quantidade
```
update <ID> <nova_quantidade>
```
**Exemplo:** `update 001 20`

### Listar Inventário
```
list
```

### Gerar Relatório
```
report
```

### Ajuda
```
help
```

### Sair
```
exit
```

---

## 🧪 Cenários de Teste

### Cenário 1: Persistência de Estado (Sucesso)

**Objetivo:** Verificar se o estado do inventário é persistido corretamente entre execuções.

**Passos:**
1. Iniciar o programa (sem arquivos de dados)
2. Executar os seguintes comandos:
   ```
   add 001 Teclado 10 Perifericos
   add 002 Mouse 15 Perifericos
   add 003 Monitor 5 Monitores
   ```
3. Sair do programa com `exit`
4. Verificar se os arquivos `Inventario.dat` e `Auditoria.log` foram criados
5. Reiniciar o programa
6. Executar comando `list`

**Resultado Esperado:**
- ✅ Arquivos `Inventario.dat` e `Auditoria.log` criados
- ✅ Ao reiniciar, o sistema carrega os 3 itens
- ✅ Comando `list` exibe os 3 itens adicionados

**Resultado Obtido:**
```
==========================================
   SISTEMA DE GERENCIAMENTO DE INVENTARIO
==========================================

[INICIALIZACAO] Carregando dados do sistema...
[INFO] Inventario carregado com sucesso!
[INFO] Log de auditoria carregado com sucesso!
[INFO] Inventario contem 3 item(ns)
[INFO] Log contem 3 entrada(s)

Digite 'help' para ver os comandos disponiveis.

> list

========== INVENTARIO ATUAL ==========
  ID: 001
  Nome: Teclado
  Quantidade: 10
  Categoria: Perifericos
  ---
  ID: 002
  Nome: Mouse
  Quantidade: 15
  Categoria: Perifericos
  ---
  ID: 003
  Nome: Monitor
  Quantidade: 5
  Categoria: Monitores
  ---
======================================
```

**Status:** [X] Passou | [ ] Falhou

---

### Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Objetivo:** Verificar se o sistema trata corretamente a tentativa de remover mais itens do que há em estoque.

**Passos:**
1. Adicionar um item com 10 unidades:
   ```
   add 004 Teclado_Gamer 10 Perifericos
   ```
2. Tentar remover 15 unidades:
   ```
   remove 004 15
   ```
3. Verificar mensagem de erro
4. Executar `list` para confirmar que ainda há 10 unidades
5. Verificar o arquivo `Auditoria.log`

**Resultado Esperado:**
- ✅ Sistema exibe mensagem: "Erro: Estoque insuficiente..."
- ✅ Comando `list` mostra que o item ainda tem 10 unidades
- ✅ `Auditoria.log` contém entrada com `StatusLog (Falha ...)`
- ✅ `Inventario.dat` permanece inalterado (10 unidades)

**Resultado Obtido:**
```
==========================================
   SISTEMA DE GERENCIAMENTO DE INVENTARIO
==========================================

[INICIALIZACAO] Carregando dados do sistema...
[INFO] Inventario carregado com sucesso!
[INFO] Log de auditoria carregado com sucesso!
[INFO] Inventario contem 3 item(ns)
[INFO] Log contem 3 entrada(s)

Digite 'help' para ver os comandos disponiveis.

> add 004 Teclado_Gamer 10 Perifericos

[SUCESSO] Item adicionado: Teclado_Gamer (ID: 004)
[PERSISTENCIA] Inventario salvo em Inventario.dat
[AUDITORIA] Log registrado em Auditoria.log

> remove 004 15

[ERRO] Erro: Estoque insuficiente. Disponivel: 10, Solicitado: 15
[AUDITORIA] Log registrado em Auditoria.log
```
```
Arquivo Auditoria.log:

LogEntry {timestamp = 2025-11-11 22:01:57.435906613 UTC, acao = Add, detalhes = "Adicionado item 'Teclado' (ID: 001) com quantidade 10 na categoria Perifericos", status = Sucesso}
LogEntry {timestamp = 2025-11-11 22:02:08.882197285 UTC, acao = Add, detalhes = "Adicionado item 'Mouse' (ID: 002) com quantidade 15 na categoria Perifericos", status = Sucesso}
LogEntry {timestamp = 2025-11-11 22:02:17.65382862 UTC, acao = Add, detalhes = "Adicionado item 'Monitor' (ID: 003) com quantidade 5 na categoria Monitores", status = Sucesso}
LogEntry {timestamp = 2025-11-11 22:05:45.181329383 UTC, acao = Add, detalhes = "Adicionado item 'Teclado_Gamer' (ID: 004) com quantidade 10 na categoria Perifericos", status = Sucesso}
LogEntry {timestamp = 2025-11-11 22:05:56.640684341 UTC, acao = Remove, detalhes = "Falha ao remover do item ID: 004", status = Falha "Erro: Estoque insuficiente. Disponivel: 10, Solicitado: 15"}
```

**Status:** [X] Passou | [ ] Falhou

---

### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar se o relatório exibe corretamente as entradas de log de falhas.

**Passos:**
1. Após executar o Cenário 2, executar:
   ```
   report
   ```
2. Verificar se o relatório gerado pela função `logsDeErro` exibe a entrada referente à falha do Cenário 2

**Resultado Esperado:**
- ✅ Relatório exibe a tentativa de remover estoque insuficiente
- ✅ Log mostra `StatusLog (Falha "Estoque insuficiente")`

**Resultado Obtido:**
```
==========================================
   SISTEMA DE GERENCIAMENTO DE INVENTARIO
==========================================

[INICIALIZACAO] Carregando dados do sistema...
[INFO] Inventario carregado com sucesso!
[INFO] Log de auditoria carregado com sucesso!
[INFO] Inventario contem 4 item(ns)
[INFO] Log contem 5 entrada(s)

Digite 'help' para ver os comandos disponiveis.

> report

[INFO] Carregando logs para gerar relatorio...
[INFO] Log de auditoria carregado com sucesso!

========== RELATORIO DE ANALISE DE LOGS ==========

** Sumario Geral **
Total de entradas de log: 5
Total de operacoes com falha: 1
Item mais movimentado (com sucesso): 001 (com 1 movimentacoes)

---

** Detalhe de Logs de Erro (1) **
  [2025-11-11 19:05:56] [Remove] [FALHA: Erro: Estoque insuficiente. Disponivel: 10, Solicitado: 15] Falha ao remover do item ID: 004

====================================================


Digite um ID de item para ver seu historico (ou deixe em branco para pular):
> 004

--- Historico para Item ID: 004 ---
  [2025-11-11 19:05:45] [Add] [SUCESSO] Adicionado item 'Teclado_Gamer' (ID: 004) com quantidade 10 na categoria Perifericos
  [2025-11-11 19:05:56] [Remove] [FALHA: Erro: Estoque insuficiente. Disponivel: 10, Solicitado: 15] Falha ao remover do item ID: 004
---------------------------------------------
```

**Status:** [X] Passou | [ ] Falhou

**Nota:** A implementação detalhada das funções de relatório (`logsDeErro`, `historicoPorItem`, `itemMaisMovimentado`) será realizada pelo Aluno 4.

---

## 🗂️ Dados Mínimos para Teste

O sistema foi populado com **10 itens distintos** para permitir testes adequados:

```haskell
add 001 Teclado_Mecanico 15 Perifericos
add 002 Mouse_Gamer 20 Perifericos
add 003 Monitor_LED_24 8 Monitores
add 004 Headset_USB 12 Audio
add 005 Webcam_HD 10 Video
add 006 Mousepad_Grande 25 Acessorios
add 007 Hub_USB 18 Conectividade
add 008 Cabo_HDMI 30 Cabos
add 009 Adaptador_VGA 15 Adaptadores
add 010 SSD_500GB 6 Armazenamento
```

---

## 🏗️ Arquitetura do Sistema

### Módulo 1: InventarioTipos.hs (Aluno 1)

Define os tipos de dados fundamentais:

```haskell
data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq)

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq)
```

### Módulo 2: LogicaNegocio.hs (Aluno 2)

Implementa a lógica de negócio pura:

```haskell
type ResultadoOperacao = (Inventario, LogEntry)

addItem :: UTCTime -> Item -> Inventario 
        -> Either String ResultadoOperacao

removeItem :: UTCTime -> String -> Int -> Inventario 
           -> Either String ResultadoOperacao

updateQty :: UTCTime -> String -> Int -> Inventario 
          -> Either String ResultadoOperacao
```

**Validações implementadas:**
- ✅ Quantidade deve ser >= 0
- ✅ Item ID não pode ser duplicado
- ✅ Item deve existir antes de remover/atualizar
- ✅ Estoque deve ser suficiente para remoção

### Módulo 3: IOPersistencia.hs (Aluno 3)

Gerencia I/O e persistência:

**Funções de Inicialização:**
- `carregarInventario :: IO Inventario`
- `carregarLogs :: IO [LogEntry]`

**Funções de Persistência:**
- `salvarInventario :: Inventario -> IO ()`
- `adicionarLogAuditoria :: LogEntry -> IO ()`

**Loop Principal:**
- `main :: IO ()`
- `loopPrincipal :: Inventario -> IO ()`
- `processarComando :: Comando -> Inventario -> IO Inventario`

**Tratamento de Exceções:**
- Usa `catch` para lidar com `IOException`
- Inicia com estado vazio se arquivos não existirem

### Módulo 4: Analise.hs (Aluno 4)

Funções de análise de logs:

- `historicoPorItem :: [LogEntry] -> String -> [LogEntry]`
- `logsDeErro :: [LogEntry] -> [LogEntry]`
- `itemMaisMovimentado :: [LogEntry] -> String`

---

## ✅ Checklist de Requisitos

### Lógica e Funcionalidade (70 pontos)

- [x] Tipos de dados bem modelados
- [x] Serialização (Show/Read) funciona
- [x] Nomenclatura conforme especificação
- [x] Separação clara entre funções puras e impuras
- [x] Sistema carrega dados corretamente
- [x] Sistema salva dados corretamente
- [x] Log de auditoria funcional
- [X] Relatórios implementados

### Organização e Legibilidade (15 pontos)

- [x] Código Haskell claro e comentado
- [x] Uso adequado de `where`, `let`, `case`
- [x] README.md completo
- [x] Commits significativos no GitHub

### Robustez (15 pontos)

- [x] Tratamento de exceções de I/O (catch)
- [x] Tratamento de erros de lógica (Either)
- [x] Sistema não quebra com arquivos inexistentes
- [x] Validações de entrada implementadas

---

## 📝 Observações Importantes

### Separação de Responsabilidades

O projeto segue rigorosamente o princípio de separação entre:

1. **Funções Puras** (LogicaNegocio.hs, Analise.hs):
   - Não contêm nenhuma operação de I/O
   - Retornam `Either String ResultadoOperacao` (Logica) ou dados (Analise)
   - Totalmente testáveis e previsíveis

2. **Funções Impuras** (IOPersistencia.hs):
   - Gerenciam toda a interação com arquivos
   - Controlam o loop principal
   - Fazem a ponte entre usuário e lógica pura

### Persistência de Dados

- **Inventario.dat:** Sobrescrito a cada operação bem-sucedida (writeFile)
- **Auditoria.log:** Modo append-only, nunca sobrescrito (appendFile)
- Ambos os arquivos usam `Show` e `Read` para serialização

### Tratamento de Erros

O sistema implementa dois níveis de tratamento de erros:

1. **Erros de Lógica:** Tratados com `Either String`
2. **Erros de I/O:** Tratados com `catch` e `IOException`

---

## 🐛 Troubleshooting

### Erro: "File not found"
**Solução:** É esperado na primeira execução. O sistema cria os arquivos automaticamente.

### Erro: "Parse error in Inventario.dat" / "a.out: Prelude.read: no parse"
**Solução:** Deletar os arquivos `.dat` e `.log` e reiniciar o sistema.

### Erro: "Item já existe"
**Solução:** Usar IDs únicos para cada item.

---

## 📚 Referências

- Real World Haskell - O'Sullivan, Stewart, Goerzen
- Learn You a Haskell for Great Good - Miran Lipovača
- Haskell Documentation: https://www.haskell.org/documentation/

---

## 📧 Contato

Para dúvidas sobre o projeto, entre em contato com os membros da equipe através do GitHub.

---

**Última atualização:** 11/11/2025  
**Versão:** 1.0
