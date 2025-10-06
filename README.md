# Trabalho Mundo dos Blocos Variável em Model Checking

## Integrantes

Carolina Falabelo Maycá - <carolina.mayca@icomp.ufam.edu.br>

Luiza da Costa Caxeixa - <luiza.caxeixa@icomp.ufam.edu.br>

Fernando Lucas Nascimento Almeida - <fernando.almeida@icomp.ufam.edu.br>

## Distribuição de Tarefas
- Código Prolog: Carolina Falabelo Maycá
- Tabela de Estados: Luiza da Costa Caxeixa
- Diagrama de Estados: Luiza da Costa Caxeixa
- Modelo SMV: Fernando Lucas Nascimento Almeida
- Documentação: Carolina Falabelo Maycá e Fernando Lucas Nascimento Almeida

## Como rodar

```bash
prolog
```

```prolog
?- [blocks_var_domain_and_planner].
```

### Situação 1 (S0 -> Sf4)

```prolog
?- s0_sit1(S0), sf4_sit1(G), plan(S0, G, P).
```

### Situação 2

```prolog
?- s0_sit2(S0), s5_sit2(G), plan(S0, G, P0).
```

### Situação 3

```prolog
?- s0_sit3(S0), s7_sit3(G), plan(S0, G, P0)
```

## Artefatos Entregues

- **Código Prolog** (`blocks_var_domain_and_planner.pl`)
- **Modelo SMV** (`blocos_var_model.smv`)  
- **Tabela de Estados** (`tabela_transicao.md`)
- **Diagrama de Estados** (`Mundo_Blocos_Variaveis.png`)
- **Documentação** (README.md)

## Como visualizar

### Tabela de Estados
```bash
code tabela_transicao.md
# Depois Ctrl+Shift+V para preview
```

### Diagrama
```bash
xdg-open Mundo_Blocos_Variaveis.png
```