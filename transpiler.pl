do_expand((:-use_module(library(avl))), (:-use_module(library(assoc)))).
do_expand((A:-B), (A:-NB)) :-
  expand_body(B,NB).
do_expand((A-->B), (A-->NB)) :-
  expand_body(B,NB).
do_expand(A,A).

expand_body((A,B),(NA,NB)) :- !,
  expand_body(A,NA), expand_body(B,NB).
expand_body({A},{NA}) :- !,
  expand_body(A,NA).
expand_body(A=B,A=NB) :- !,
  expand_expression(B, NB).
expand_body(empty_avl(X), empty_assoc(X)) :- !.
expand_body(A,A).

expand_expression(String, List) :-
  string(String),
  string_codes(String, List).
expand_expression(A, A).

term_expansion(A, B) :-
  format('was asked to transpile ~w\n', [A]),
  do_expand(A,B),
  format('from ~w to ~w\n',[A,B]).
