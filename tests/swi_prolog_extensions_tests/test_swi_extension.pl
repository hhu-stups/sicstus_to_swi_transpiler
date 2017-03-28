:- module(test_swi_extension, [test_swi_extension/0]).

:- use_module(library(plunit)).
:- use_module("../../src/swi_prolog_extensions/swi_extension.pl").

test_swi_extension :-
	run_tests([convert_arithmetic_expression]).

:- begin_tests(convert_arithmetic_expression).

test(vars) :-
  convert_arithmetic_expression(A+sin(cos(B)), A+sin(cos(B))).

test(integer) :-
  convert_arithmetic_expression(sin(integer(3.4555))+4.0, sin(truncate(3.4555))+4.0).

test(xor) :-
  convert_arithmetic_expression(sin(4\5)+4.0, sin(xor(4, 5))+4.0).

test(round) :-
  convert_arithmetic_expression(sin(round(3))+4.0, sin(floor(3+1/2))+4.0).

test(cot) :-
  convert_arithmetic_expression(sin(cot(3))+4.0, sin(cos(3)/sin(3))+4.0).

test(coth) :-
  convert_arithmetic_expression(sin(coth(3))+4.0, sin(cosh(3)/sinh(3))+4.0).

test(acot) :-
  convert_arithmetic_expression(sin(acot(3))+4.0, sin(pi/2-atan(3))+4.0).

test(acoth) :-
  convert_arithmetic_expression(sin(acoth(3))+4.0, sin(1/2*log((3+1)/(3-1)))+4.0).

test(log) :-
  convert_arithmetic_expression(sin(log(3, 4))+4.0, sin(log(4)/log(3))+4.0).

test(exp) :-
  convert_arithmetic_expression(sin(exp(3, 4))+4.0, sin(3 ** 4)+4.0).

:- end_tests(convert_arithmetic_expression).
