:- module(test_aggregate_extension, [test_aggregate_extension/0]).

:- use_module(library(plunit)).
:- use_module("../../src/swi_prolog_extensions/aggregate_extension.pl").

test_aggregate_extension :-
	run_tests([term_variables_ext]).

:- begin_tests(term_variables_ext).

test(empty_list, [true(List == [B, A]), nondet]) :-
  term_variables_ext(f(A, B), [], List).

test(invalid_second_argument, [true(List == a), nondet]) :-
  term_variables_ext(f(_, _), a, List).

test(union, [true(List == [B, A, C, D, E]), nondet]) :-
  term_variables_ext(f(a, A, B), [A, C, D, E], List).

test(union2, [true(List == [B, A, D, C, E]), nondet]) :-
  term_variables_ext(f(a, A, B), [D, C, E], List).

test(union3, [true(List == [B, A, D, C, E]), nondet]) :-
  term_variables_ext(f(a, g(h(A, B))), [D, C, E], List).

test(var_first_argument, [true(List == [A, B, C]), nondet]) :-
  term_variables_ext(A, [B, C], List).

test(var_second_argument, [true(List == [A]), nondet]) :-
  term_variables_ext(f(a, A), _, List).

:- end_tests(term_variables_ext).
