/* =============================================================
   Mundo dos Blocos com Tamanhos Variáveis
   Domínio STRIPS + Planner BFS
   ------------------------------------------------------------- */

% ---------- Parte 1: blocos e mesa (estáticos) ----------
block(a). block(b). block(c). block(d).
size(a,1). size(b,1). size(c,2). size(d,2).

table_width(7).
table_slot(X) :- integer(X), X >= 0, table_width(W), X < W.

% ---------- Parte 2: estado mínimo e auxiliares ----------
% Estado é lista de fatos dinâmicos: pos/2 e clear/1
%   pos(B, table(X))  -> B apoiado na mesa na coluna X (à esquerda)
%   pos(B, on(Sup))   -> B apoiado sobre Sup (B != Sup)
%   clear(B)          -> topo de B está livre

% Posição absoluta em X (herdada do suporte quando não está na mesa)
absolute_pos(B, S, X) :- member(pos(B, table(X)), S).
absolute_pos(B, S, X) :- member(pos(B, on(Sup)), S),
                         absolute_pos(Sup, S, XS),
                         X = XS.

% Conjunto de slots ocupados por B na mesa
busy_slots(B, S, Slots) :-
    absolute_pos(B, S, X),
    size(B, W),
    XE is X + W - 1,
    findall(T, between(X, XE, T), Slots).

% Um slot está ocupado?
slot_busy(Slot, S) :-
    member(pos(B,_), S),
    busy_slots(B, S, Bs),
    member(Slot, Bs).

% Faixa X..X+W-1 está livre na mesa?
range_free(X, W, S) :-
    XE is X + W - 1,
    table_width(TW),
    XE < TW,
    forall(between(X, XE, T), \+ slot_busy(T, S)).

% ---------- Parte 3: Operadores (pré-condições) ----------
can(move(B, on(TB)), S) :-
    member(clear(B), S),
    B \== TB,
    member(clear(TB), S),
    size(B, W1),
    size(TB, W2),
    W1 =< W2.

can(move(B, table(X)), S) :-
    member(clear(B), S),
    size(B, W),
    range_free(X, W, S).

% ---------- Parte 4: Efeitos (adds/deletes) ----------
deletes(move(B,_), S, [pos(B, OldPos)]) :-
    % remove a posição antiga de B
    member(pos(B, OldPos), S).

adds(move(B, on(TB)), [pos(B, on(TB))]).
adds(move(B, table(X)), [pos(B, table(X))]).

% Quando movemos um bloco, o que estava embaixo fica clear
clear_effect(move(B, _), S, [clear(Sup)]) :-
    member(pos(B, on(Sup)), S).
clear_effect(move(B, _), S, []) :-
    member(pos(B, table(_)), S).

% ---------- Parte 5: Aplicação de ação ----------
apply(S, A, S2) :-
    can(A, S),
    deletes(A, S, ToDel),
    subtract(S, ToDel, S1),
    adds(A, Add),
    clear_effect(A, S, ClearAdd),
    append(Add, ClearAdd, AllAdds),
    % destino (TB) deixa de estar clear se A colocar algo sobre ele
    ( A = move(_, on(TB)) -> subtract(S1, [clear(TB)], S1b) ; S1b = S1 ),
    % adiciona novos fatos
    append(AllAdds, S1b, S2tmp),
    % remove duplicatas usando sort
    sort(S2tmp, S2).

% ---------- Parte 6: Satisfação do objetivo ----------
satisfies(State, Goals) :- forall(member(G, Goals), member(G, State)).

% ---------- Parte 7: Planner BFS ----------
plan(State, Goals, Plan) :-
    bfs([[State, []]], Goals, [State], Plan).

bfs([[S, P]|_], G, _, P) :- 
    satisfies(S, G), !.

bfs([[S, P]|Rest], G, Visited, Plan) :-
    findall([S2, [A|P]], 
            (action_template(A), 
             can(A, S), 
             apply(S, A, S2),
             \+ member_state(S2, Visited)
            ), 
            Successors),
    extract_states(Successors, NewStates),
    append(Visited, NewStates, NewVisited),
    append(Rest, Successors, Queue),
    bfs(Queue, G, NewVisited, Plan).

% Extrai apenas os estados dos sucessores
extract_states([], []).
extract_states([[S, _]|Rest], [S|States]) :-
    extract_states(Rest, States).

% Verifica se um estado já foi visitado
member_state(S, [H|_]) :- 
    same_facts(S, H), !.
member_state(S, [_|T]) :-
    member_state(S, T).

% Compara dois estados
same_facts(S1, S2) :-
    length(S1, L),
    length(S2, L),
    forall(member(F, S1), member(F, S2)),
    forall(member(F, S2), member(F, S1)).

% Action templates limitados para evitar explosão
action_template(move(B, on(TB))) :- 
    block(B), 
    block(TB), 
    B \== TB.

action_template(move(B, table(X))) :- 
    block(B), 
    member(X, [0,1,2,3,4]).  % Apenas 5 posições

/* =============================================================
   CENÁRIO DE TESTE (válido com estabilidade)
   -------------------------------------------------------------
   Inicial:
     d na mesa em 0; b em d; a em b; c em a; clear(c)
   Objetivo:
     Torre final (c como base): c na mesa em 2; d sobre c; b sobre d; a sobre b
   Obs.: Respeita size: a=1, b=1, c=2, d=2 (sempre <= no suporte).
   ============================================================= */

s0([ pos(d, table(0)),
     pos(b, on(d)),
     pos(a, on(b)),
     pos(c, on(a)),
     clear(c)
   ]).

goal([ pos(c, table(2)),
       pos(d, on(c)),
       pos(b, on(d)),
       pos(a, on(b))
     ]).

/* Plano manual válido (8 passos) para s0 -> goal:

  1) move(c, table(2))
  2) move(a, on(c))
  3) move(b, table(4))
  4) move(a, on(b))
  5) move(d, on(c))
  6) move(a, table(5))
  7) move(b, on(d))
  8) move(a, on(b))

Sugestão: rode "plan(S0, G, P)." após definir s0(S0), goal(G).
*/
