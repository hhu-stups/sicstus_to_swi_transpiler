:- module(transpiler_core, [transpile_term/2]).

:- use_module(transpiler_avl).

transpile_term((Term:-Body), (NTerm:-NBody)) :-
	transpile_term(Term, NTerm),
	transpile_body(Body, NBody), !.
transpile_term((Term-->Body), (NTerm-->NBody)) :-
	transpile_term(Term, NTerm),
	transpile_body(Body, NBody), !.
transpile_term(Term, NNTerm) :-
	transpile_avl_term(Term, NTerm),
	transpile_term(NTerm, NNTerm), !.
transpile_term(Term, Term).

transpile_body((A,B), (NA,NB)) :-
	transpile_body(A, NA),
	transpile_body(B, NB), !.
transpile_body({Term}, {NTerm}) :-
	transpile_body(Term, NTerm), !.
transpile_body(A=B, NA=NB) :-
	when(nonvar(A), transpile_expression(A, NA)),
	when(nonvar(B), transpile_expression(B, NB)), !.
transpile_body(Term, NTerm) :-
	transpile_expression(Term, NTerm), !.
transpile_body(Term, NTerm) :-
	transpile_term(Term, NTerm), !.
transpile_body(Body, Body).

transpile_expression(String, List) :-
  string(String),
  string_codes(String, List), !.
transpile_expression(Expression, NExpression) :-
	transpile_term(Expression, NExpression), !.
transpile_expression(Expression, Expression).
