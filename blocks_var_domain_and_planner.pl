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
    sort(State, S0),
    bfs([[S0, []]], Goals, [S0], Plan).

bfs([[S, P]|_], G, _, Plan) :- 
    satisfies(S, G), !,
    reverse(P, Plan).

bfs([[S, P]|Rest], G, Visited, Plan) :-
    findall([S2, [A|P]], 
            ( action_template(A), 
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
    member(X, [0,1,2,3,4,5,6]).

/* --- Situação 1: S0 -> Sf4 --- */

% S0
s0_sit1([
  pos(c, table(0)),   % c cobre [0..1]
  pos(a, table(3)),   % a em 3
  pos(d, table(4)),   % d cobre [4..5]
  pos(b, table(6)),   % b em 6
  clear(a), clear(b), clear(c), clear(d)
]).

% Sf4 (objetivo)
sf4_sit1([
  pos(a, table(0)),   % a em 0
  pos(c, table(1)),   % c cobre [1..2]
  pos(d, table(3)),   % d cobre [3..4]
  pos(b, table(6)),   % b em 6
  clear(a), clear(b), clear(c), clear(d)
]).

/* --- Situação 2: S0 -> S5 --- */

% S0
s0_sit2([
  pos(c, table(0)),      % c ocupa [0..1]
  pos(a, on(c)),         % a empilhado em c
  pos(b, table(2)),      % b em 2
  pos(d, table(4)),      % d ocupa [4..5]
  clear(a), clear(b), clear(d)
]).

% S5 (alvo):
s5_sit2([
  pos(d, table(3)),      % d ocupa [3..4]
  pos(c, table(5)),      % c ocupa [5..6]
  pos(b, table(1)),      % b em 1
  pos(a, on(b)),         % a sobre b (1 <= 1)
  clear(a), clear(c), clear(d)
]).

/* --- Situação 3: S0 -> S7 --- */

% S0
s0_sit3([
  pos(c, table(0)),    % c ocupa [0..1]
  pos(a, table(2)),    % a em 2
  pos(d, table(3)),    % d ocupa [3..4]
  pos(b, table(6)),    % b em 6 (ok ficar fora do range permitido, só não será destino)
  clear(a), clear(b), clear(c), clear(d)
]).

% S7
s7_sit3([
  pos(c, table(0)),    % [0..1]
  pos(d, table(3)),    % [3..4]
  pos(b, table(2)),    % b em 2
  pos(a, on(b)),       % a sobre b (1 <= 1)
  clear(a), clear(c), clear(d)
]).