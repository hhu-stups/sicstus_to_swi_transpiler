:- module(test_transpiler_core, [test_transpiler_core/0]).

:- use_module(library(plunit)).
:- use_module("../src/transpiler_core.pl").

test_transpiler_core :-
	run_tests([transpile_expression, transpile_term_operators]).

:- begin_tests(transpile_expression).

test(string_to_codes) :-
	transpiler_core:transpile_expression("test", [116, 101, 115, 116]).

:- end_tests(transpile_expression).

:- begin_tests(transpile_term_operators).

test(arithmetic_equal_greaterthan_operator) :-
	transpile_term(a:-A>=empty_avl(B), a:-A>=empty_assoc(B)),
	transpile_term(a:-empty_avl(B)>=A, a:-empty_assoc(B)>=A),
	transpile_term(a:-empty_avl(A)>=empty_avl(B), a:-empty_assoc(A)>=empty_assoc(B)).

test(arithmetic_equal_lessthan_operator) :-
	transpile_term(a:-A=<empty_avl(B), a:-A=<empty_assoc(B)),
	transpile_term(a:-empty_avl(B)=<A, a:-empty_assoc(B)=<A),
	transpile_term(a:-empty_avl(A)=<empty_avl(B), a:-empty_assoc(A)=<empty_assoc(B)).

test(arithmetic_equal_operator) :-
	transpile_term(a:-A=:=empty_avl(B), a:-A=:=empty_assoc(B)),
	transpile_term(a:-empty_avl(B)=:=A, a:-empty_assoc(B)=:=A),
	transpile_term(a:-empty_avl(A)=:=empty_avl(B), a:-empty_assoc(A)=:=empty_assoc(B)).

test(arithmetic_greaterthan_operator) :-
	transpile_term(a:-A>empty_avl(B), a:-A>empty_assoc(B)),
	transpile_term(a:-empty_avl(B)>A, a:-empty_assoc(B)>A),
	transpile_term(a:-empty_avl(A)>empty_avl(B), a:-empty_assoc(A)>empty_assoc(B)).

test(arithmetic_lessthan_operator) :-
	transpile_term(a:-A<empty_avl(B), a:-A<empty_assoc(B)),
	transpile_term(a:-empty_avl(B)<A, a:-empty_assoc(B)<A),
	transpile_term(a:-empty_avl(A)<empty_avl(B), a:-empty_assoc(A)<empty_assoc(B)).

test(arithmetic_not_equal_operator) :-
	transpile_term(a:-A=\=empty_avl(B), a:-A=\=empty_assoc(B)),
	transpile_term(a:-empty_avl(B)=\=A, a:-empty_assoc(B)=\=A),
	transpile_term(a:-empty_avl(A)=\=empty_avl(B), a:-empty_assoc(A)=\=empty_assoc(B)).

test(bitwise_and_operator) :-
	transpile_term(a:-A/\empty_avl(B), a:-A/\empty_assoc(B)),
	transpile_term(a:-empty_avl(B)/\A, a:-empty_assoc(B)/\A),
	transpile_term(a:-empty_avl(A)/\empty_avl(B), a:-empty_assoc(A)/\empty_assoc(B)).

test(bitwise_or_operator) :-
	transpile_term(a:-A\/empty_avl(B), a:-A\/empty_assoc(B)),
	transpile_term(a:-empty_avl(B)\/A, a:-empty_assoc(B)\/A),
	transpile_term(a:-empty_avl(A)\/empty_avl(B), a:-empty_assoc(A)\/empty_assoc(B)).

test(disjunction_operator) :-
	transpile_term(a:-empty_avl(A);empty_avl(B), a:-empty_assoc(A);empty_assoc(B)).

test(division_operator) :-
	transpile_term(a:-A/empty_avl(B), a:-A/empty_assoc(B)),
	transpile_term(a:-empty_avl(B)/A, a:-empty_assoc(B)/A),
	transpile_term(a:-empty_avl(A)/empty_avl(B), a:-empty_assoc(A)/empty_assoc(B)).

test(equal_greaterthan_operator) :-
	transpile_term(a:-A@>=empty_avl(B), a:-A@>=empty_assoc(B)),
	transpile_term(a:-empty_avl(B)@>=A, a:-empty_assoc(B)@>=A),
	transpile_term(a:-empty_avl(A)@>=empty_avl(B), a:-empty_assoc(A)@>=empty_assoc(B)).

test(equal_lessthan_operator) :-
	transpile_term(a:-A@=<empty_avl(B), a:-A@=<empty_assoc(B)),
	transpile_term(a:-empty_avl(B)@=<A, a:-empty_assoc(B)@=<A),
	transpile_term(a:-empty_avl(A)@=<empty_avl(B), a:-empty_assoc(A)@=<empty_assoc(B)).

test(equal_operator) :-
	transpile_term(a:-A==empty_avl(B), a:-A==empty_assoc(B)),
	transpile_term(a:-empty_avl(B)==A, a:-empty_assoc(B)==A),
	transpile_term(a:-empty_avl(A)==empty_avl(B), a:-empty_assoc(A)==empty_assoc(B)).

test(greaterthan_operator) :-
	transpile_term(a:-A@>empty_avl(B), a:-A@>empty_assoc(B)),
	transpile_term(a:-empty_avl(B)@>A, a:-empty_assoc(B)@>A),
	transpile_term(a:-empty_avl(A)@>empty_avl(B), a:-empty_assoc(A)@>empty_assoc(B)).

test(ifthen_operator) :-
	transpile_term(a:-empty_avl(A)->empty_avl(B), a:-empty_assoc(A)->empty_assoc(B)).

test(integer_division_operator) :-
	transpile_term(a:-A//empty_avl(B), a:-A//empty_assoc(B)),
	transpile_term(a:-empty_avl(B)//A, a:-empty_assoc(B)//A),
	transpile_term(a:-empty_avl(A)//empty_avl(B), a:-empty_assoc(A)//empty_assoc(B)).

test(is_operator) :-
	transpile_term(a:-A is empty_avl(B), a:-A is empty_assoc(B)),
	transpile_term(a:-empty_avl(B) is A, a:-empty_assoc(B) is A),
	transpile_term(a:-empty_avl(A) is empty_avl(B), a:-empty_assoc(A) is empty_assoc(B)).

test(left_shift_operator) :-
	transpile_term(a:-A<<empty_avl(B), a:-A<<empty_assoc(B)),
	transpile_term(a:-empty_avl(B)<<A, a:-empty_assoc(B)<<A),
	transpile_term(a:-empty_avl(A)<<empty_avl(B), a:-empty_assoc(A)<<empty_assoc(B)).

test(lessthan_operator) :-
	transpile_term(a:-A@<empty_avl(B), a:-A@<empty_assoc(B)),
	transpile_term(a:-empty_avl(B)@<A, a:-empty_assoc(B)@<A),
	transpile_term(a:-empty_avl(A)@<empty_avl(B), a:-empty_assoc(A)@<empty_assoc(B)).

test(minus_operator) :-
	transpile_term(a:-A-empty_avl(B), a:-A-empty_assoc(B)),
	transpile_term(a:-empty_avl(B)-A, a:-empty_assoc(B)-A),
	transpile_term(a:-empty_avl(A)-empty_avl(B), a:-empty_assoc(A)-empty_assoc(B)).

test(minus2_operator) :-
	transpile_term(a:-(-empty_avl(A)), a:-(-empty_assoc(A))),
	transpile_term(a:-(-A), a:-(-A)).

test(mod_operator) :-
	transpile_term(a:-A mod empty_avl(B), a:-A mod empty_assoc(B)),
	transpile_term(a:-empty_avl(B) mod A, a:-empty_assoc(B) mod A),
	transpile_term(a:-empty_avl(A) mod empty_avl(B), a:-empty_assoc(A) mod empty_assoc(B)).

test(multiply_operator) :-
	transpile_term(a:-A*empty_avl(B), a:-A*empty_assoc(B)),
	transpile_term(a:-empty_avl(B)*A, a:-empty_assoc(B)*A),
	transpile_term(a:-empty_avl(A)*empty_avl(B), a:-empty_assoc(A)*empty_assoc(B)).

test(not_equal_operator) :-
	transpile_term(a:-A\==empty_avl(B), a:-A\==empty_assoc(B)),
	transpile_term(a:-empty_avl(B)\==A, a:-empty_assoc(B)\==A),
	transpile_term(a:-empty_avl(A)\==empty_avl(B), a:-empty_assoc(A)\==empty_assoc(B)).

test(not_provable_operator) :-
	transpile_term(a:-(\+empty_avl(A)), a:-(\+empty_assoc(A))).

test(not_unify_operator) :-
	transpile_term(a:-A\=empty_avl(B), a:-A\=empty_assoc(B)),
	transpile_term(a:-empty_avl(B)\=A, a:-empty_assoc(B)\=A),
	transpile_term(a:-empty_avl(A)\=empty_avl(B), a:-empty_assoc(A)\=empty_assoc(B)).

test(plus_operator) :-
	transpile_term(a:-A+empty_avl(B), a:-A+empty_assoc(B)),
	transpile_term(a:-empty_avl(B)+A, a:-empty_assoc(B)+A),
	transpile_term(a:-empty_avl(A)+empty_avl(B), a:-empty_assoc(A)+empty_assoc(B)).

test(power_operator) :-
	transpile_term(a:-A**empty_avl(B), a:-A**empty_assoc(B)),
	transpile_term(a:-empty_avl(B)**A, a:-empty_assoc(B)**A),
	transpile_term(a:-empty_avl(A)**empty_avl(B), a:-empty_assoc(A)**empty_assoc(B)).

test(power_operator) :-
	transpile_term(a:-A^empty_avl(B), a:-A^empty_assoc(B)),
	transpile_term(a:-empty_avl(B)^A, a:-empty_assoc(B)^A),
	transpile_term(a:-empty_avl(A)^empty_avl(B), a:-empty_assoc(A)^empty_assoc(B)).

test(rem_operator) :-
	transpile_term(a:-A rem empty_avl(B), a:-A rem empty_assoc(B)),
	transpile_term(a:-empty_avl(B) rem A, a:-empty_assoc(B) rem A),
	transpile_term(a:-empty_avl(A) rem empty_avl(B), a:-empty_assoc(A) rem empty_assoc(B)).

test(right_shift_operator) :-
	transpile_term(a:-A>>empty_avl(B), a:-A>>empty_assoc(B)),
	transpile_term(a:-empty_avl(B)>>A, a:-empty_assoc(B)>>A),
	transpile_term(a:-empty_avl(A)>>empty_avl(B), a:-empty_assoc(A)>>empty_assoc(B)).

test(slash_operator) :-
	transpile_term(a:-(\empty_avl(A)), a:-(\empty_assoc(A))),
	transpile_term(a:-(\A), a:-(\A)).

test(unify_operator) :-
	transpile_term(a:-A=empty_avl(B), a:-A=empty_assoc(B)),
	transpile_term(a:-empty_avl(B)=A, a:-empty_assoc(B)=A),
	transpile_term(a:-empty_avl(A)=empty_avl(B), a:-empty_assoc(A)=empty_assoc(B)).

test(univ_operator) :-
	transpile_term(a:-A=..empty_avl(B), a:-A=..empty_assoc(B)),
	transpile_term(a:-empty_avl(B)=..A, a:-empty_assoc(B)=..A),
	transpile_term(a:-empty_avl(A)=..empty_avl(B), a:-empty_assoc(A)=..empty_assoc(B)).
:- end_tests(transpile_term_operators).
