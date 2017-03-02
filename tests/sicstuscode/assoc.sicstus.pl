:- use_module(library(assoc)).

get_assoc(Assoc) :-
  empty_assoc(Assoc).

store_alphabet(Assoc, _) :-
  asserta(alphabet_assoc(Assoc)),
  asserta(alphabet_number(0)),
  member(Char, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]),
  retract(alphabet_assoc(StoredAssoc)),
  retract(alphabet_number(Number)),
  NewNumber is Number + 1,
  put_assoc(Char, StoredAssoc, NewNumber, NewAssoc),
  asserta(alphabet_assoc(NewAssoc)),
  asserta(alphabet_number(NewNumber)),
  fail.
store_alphabet(_, ResultAssoc) :-
  retract(alphabet_assoc(ResultAssoc)),
  retract(alphabet_number(_)).

get_char_position(Char, Assoc, Pos) :-
  get_assoc(Char, Assoc, Pos).

next_char(Char, Assoc, NextChar) :-
  get_next_assoc(Char, Assoc, NextChar, _).

count_alphabet(Assoc, Size) :-
  assoc_to_list(Assoc, List), length(List, Size).
