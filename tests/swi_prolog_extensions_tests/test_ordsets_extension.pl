:- module(test_ordsets_extension, [test_ordsets_extension/0]).

:- use_module(library(plunit)).
:- use_module("../../src/swi_prolog_extensions/ordsets_extension.pl").

test_ordsets_extension :-
	run_tests([ord_disjoint_union, ord_nonmemberchk, ord_setproduct, ordset_order]).

:- begin_tests(ord_disjoint_union).

test(not_disjoint, [fail]) :-
	ord_disjoint_union([b, c, d], [a, d, g, h], _).

test(union, [true(Union == [a, b, c, d, g, h])]) :-
	ord_disjoint_union([b, c, d], [a, g, h], Union).

test(var_first_argument, [true(Union == [a, g, h])]) :-
	ord_disjoint_union(_, [a, g, h], Union).

test(var_second_argument, [true(Union == [b, c, d])]) :-
	ord_disjoint_union([b, c, d], _, Union).

:- end_tests(ord_disjoint_union).

:- begin_tests(ord_nonmemberchk).

test(invalid_set) :-
	ord_nonmemberchk(e, [b, b, c, d]).

test(invalid_set2) :-
	ord_nonmemberchk(a, [b, b, a]).

test(element_does_not_exist) :-
	ord_nonmemberchk(c, [a, b, d]).

test(element_exists, [fail]) :-
	ord_nonmemberchk(b, [a, b, c, d]).

test(var_first_argument) :-
	ord_nonmemberchk(_, [a, b]).

test(var_second_argument, [true(Set == []), nondet]) :-
	ord_nonmemberchk(b, Set).

:- end_tests(ord_nonmemberchk).

:- begin_tests(ord_setproduct).

test(setproduct, [true(Product = [a-a,a-b,b-a,b-b,c-a,c-b])]) :-
	ord_setproduct([a, b, c], [a, b], Product).

test(var_first_argument, [true((Set1 == [], Product == []))]) :-
	ord_setproduct(Set1, [a, b], Product).

test(var_second_argument, [true((Set2 == [], Product == []))]) :-
	ord_setproduct([a, b], Set2, Product).

test(var_arguments, [true((Set1 == [], Product == []))]) :-
	ord_setproduct(Set1, _, Product).

:- end_tests(ord_setproduct).

:- begin_tests(ordset_order).

test(equal, [true(==(R, =))]) :-
	ordset_order([a, b], [a, b], R).

test(no_order, [fail]) :-
	ordset_order([a, b, f, r], [e, m, o], _).

test(subset, [true(==(R, <))]) :-
	ordset_order([a, b], [a, b, c, f], R).

test(superset, [true(==(R, >))]) :-
	ordset_order([a, b, c], [a, b], R).

:- end_tests(ordset_order).
