:- module(sicstuscode, [get_empty/1]).

:- use_module(library(avl)).

get_empty(AVL) :-
  empty_avl(AVL).

tesd(sss(A)).

test_strings:-
  A="test double quotes",
  write(A).

a(A) --> "test", {empty_avl(A)}.
a(A) --> "test", a(A).
