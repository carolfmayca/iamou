# iamou

## Integrantes

Carolina Falabelo Maycá - <carolina.mayca@icomp.ufam.edu.br>

Luiza da Costa Caxeixa - <luiza.caxeixa@icomp.ufam.edu.br>

Fernando Lucas Nascimento Almeida - <fernando.almeida@icomp.ufam.edu.br>

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
?- 0_sit2(S0), s5_sit2(G), plan(S0, G, P0).
```

### Situação 3

```prolog
?- s0_sit3(S0), s7_sit3(G), plan(S0, G, P0).
```
