:- use_module(library(avl)).

get_avl(AVL) :-
  empty_avl(AVL).

store_alphabet(AVL, _) :-
  asserta(alphabet_avl(AVL)),
  asserta(alphabet_number(0)),
  member(Char, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]),
  retract(alphabet_avl(StoredAVL)),
  retract(alphabet_number(Number)),
  NewNumber is Number + 1,
  avl_store(Char, StoredAVL, NewNumber, NewAVL),
  asserta(alphabet_avl(NewAVL)),
  asserta(alphabet_number(NewNumber)),
  fail.
store_alphabet(_, ResultAVL) :-
  retract(alphabet_avl(ResultAVL)),
  retract(alphabet_number(_)).

get_char_position(Char, AVL, Pos) :-
  avl_fetch(Char, AVL, Pos).

next_char(Char, AVL, NextChar) :-
  avl_next(Char, AVL, NextChar).

count_alphabet(AVL, Size) :-
  avl_size(AVL, Size).
