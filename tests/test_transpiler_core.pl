:- module(test_transpiler_core, [test_transpiler_core/0]).

:- use_module(library(plunit)).
:- use_module("../src/transpiler_core.pl").

test_transpiler_core :-
	run_tests([transpile_expression, transpile_term]).

:- begin_tests(transpile_expression).

test(string_to_codes) :-
	transpiler_core:transpile_expression("test", [116, 101, 115, 116]).

:- end_tests(transpile_expression).

:- begin_tests(transpile_term).

test(equal_operator) :-
	transpile_term(a:-A="test", a:-A=[116, 101, 115, 116]),
	transpile_term(a:-"test"=A, a:-[116, 101, 115, 116]=A),
	transpile_term(a:-"test"="test", a:-[116, 101, 115, 116]=[116, 101, 115, 116]).

:- end_tests(transpile_term).
