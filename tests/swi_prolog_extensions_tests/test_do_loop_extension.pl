:- module(test_do_loop_extension, [test_do_loop_extension/0]).

:- use_module(library(plunit)).
:- use_module("../../src/swi_prolog_extensions/do_loop_extension.pl").

a.

b.

c(t, a).
c(t, b).

test_do_loop_extension :-
	run_tests([conjunction_and_param, count, do_operator, errors, for, foreach,
						foreacharg, fromto]).

:- begin_tests(conjunction_and_param).

test(for, [all(Result == [3])]) :-
	do((for(Index, 1, 3), count(Index, 1, Result)), member(Index, [1, 2, 3])).

test(for_param, [all(Result == [2, 2, 2, 2])]) :-
	P = t,
	do((for(Index, 1, 2), count(_, 1, Result), param(P)), (c(P, H), H \== Index)).

test(foreach, [all(Result == [3])]) :-
	do((foreach(Item, [a, b, c]), count(Index, 1, Result)), (member(Item, [a, b, c]), Index < 4)).

test(foreach2, [all(Result == [[1, 2, 3]])]) :-
	do((foreach(H, [1, 2, 3]), foreach(X, Result)), X = H).

test(foreach_param, [all(Result == [2, 2, 2, 2])]) :-
	P = t,
	do((foreach(Item, [x, y]), count(_, 1, Result), param(P)), (c(P, H), Item \== H)).

test(foreacharg, [all(Result == [3])]) :-
	do((foreacharg(Item, f(a, b, c)), count(Index, 1, Result)), (member(Item, [a, b, c]), Index < 4)).

test(foreacharg_param, [all(Result == [2, 2, 2, 2])]) :-
	P = t,
	do((foreacharg(Item, f(x, y)), count(_, 1, Result), param(P)), (c(P, H), Item \== H)).

test(fromto_to_ground, [all(Result == [4])]) :-
	do((count(Index, 0, Result), fromto(0, In, Out, 10)), Out is In+Index).

test(fromto_to_nonground, [all(Result == [10])]) :-
	do((count(Index, 0, 5), fromto(0, In, Out, g(Result))), (In == 10 -> Out = g(10); Out is In+Index)).

test(fromto_param, [all(Result == [1, 1, 1, 1])]) :-
	P = t,
	do((count(_, 0, Result), param(P), fromto(0, In, Out, 2)), (c(P, H), H \== In, Out is In+1)).

test(param_list, [nondet]) :-
	P1=1,
	P2=2,
	do((for(Index, 1, 2), param([P1, P2])), member(Index, [P1, P2])).

test(var_argument_specifier) :-
	do(param(_), true).

test(var_first_argument_specifier, [error(instantiation_error)]) :-
	do((param(_), _), true).

test(var_second_argument_specifier, [error(instantiation_error)]) :-
	do((_, param(_)), true).

:- end_tests(conjunction_and_param).

:- begin_tests(count).

test(call_user_definied_goal) :-
	do(count(X, 1, 3), (c(t, H), H \== X)).

test(float_first_argument_specifier) :-
	do(count(_, 1.8, 0), true).

test(iterate) :-
	do(count(Index, 1, 3), member(Index, [1, 2, 3])).

test(invalid_first_argument_specifier, [fail]) :-
	do(count(t, 1, 3), true).

test(invalid_second_argument_specifier,  [error(type_error(evaluable, _))]) :-
	do(count(_, t, 3), true).

test(var_second_argument_specifier, [error(instantiation_error)]) :-
	do(count(_, _, 2), true).

test(var_third_argument_specifier) :-
	do(count(_, 1, _), true).

:- end_tests(count).

:- begin_tests(do_operator).

test(do_operator) :-
	(foreach(_,   [a]) do a, b).

test(do_operator2) :-
	(foreach(_, [a]) do (foreach(_, [a]) do a, b)).

:- end_tests(do_operator).

:- begin_tests(errors).

test(existence_error_second_argument, [error(existence_error(procedure, plunit_errors:t/0))]) :-
	do(foreach(_,   [a]), t).

test(instantiation_error_first_argument, [error(instantiation_error)]) :-
	do(_, true).

test(type_error_first_argument, [error(type_error(iterator, _))]) :-
 	do(t, true).

:- end_tests(errors).

:- begin_tests(for).

test(arithmetic_expression) :-
	do(for(Index, 1, 1+2), member(Index, [1, 2, 3])).

test(call_user_definied_goal) :-
	do(for(X, 1, 3), (c(t, H), H \== X)).

test(float_first_argument_specifier) :-
	do(for(Index, 1.9, 1), Index == 1).

test(float_second_argument_specifier) :-
	do(for(Index, 1, 1.9), Index == 1).

test(iterate) :-
	do(for(Index, 1, 3), member(Index, [1, 2, 3])).

test(invalid_first_argument_specifier, [fail]) :-
	do(for(t, 1, 3), true).

test(invalid_second_argument_specifier,  [error(type_error(evaluable, _))]) :-
	do(for(_, t, 3), true).

test(invalid_third_argument_specifier,  [error(type_error(evaluable, _))]) :-
	do(for(_, 1, t), true).

test(var_second_argument_specifier, [error(instantiation_error)]) :-
	do(for(_, _, 2), true).

test(var_third_argument_specifier, [error(instantiation_error)]) :-
	do(for(_, 1, _), true).

:- end_tests(for).

:- begin_tests(foreach).

test(call_user_definied_goal) :-
	do(foreach(X, [x, y]), (c(t, H), H \== X)).

test(empty_list) :-
	do(foreach(Item, []), member(Item, [a, b, c])).

test(enumerate) :-
	do(foreach(Item, [a, b, c]), member(Item, [a, b, c])).

test(invalid_first_argument_specifier, [fail]) :-
	do(foreach(t, [a, b]), true).

test(var_first_argument_specifier) :-
	do(foreach(a-X, [a-b, a-b]), member(X, [a, b, c, d])).

test(var_second_argument_specifier, [all(List == [[]])]) :-
	do(foreach(Item, List), member(Item, [a, b, c, d])).

test(var_second_argument_specifier2, [all(List == [[]])]) :-
	do(foreach(Item, [a, b|List]), member(Item, [a, b, c, d])).

:- end_tests(foreach).

:- begin_tests(foreacharg).

test(call_user_definied_goal) :-
	do(foreacharg(Item, f(x, y)), (c(t, H), H \== Item)).

test(call_user_definied_goal2) :-
	do(foreacharg(Item, f(x, y), Index), (c(t, H), H \== Item, member(Index, [1, 2]))).

test(empty_list) :-
	do(foreacharg(Item, f), member(Item, [a, b, c])).

test(empty_list2) :-
	do(foreacharg(Item, f, _), member(Item, [a, b, c])).

test(enumerate) :-
	do(foreacharg(Item, f(a, b, c)), member(Item, [a, b, c])).

test(enumerate2) :-
	do(foreacharg(Item, f(a, b, c), Index), (member(Item, [a, b, c]), member(Index, [1, 2, 3]))).

test(invalid_first_argument_specifier, [fail]) :-
	do(foreacharg(t, f(a, b)), true).

test(invalid_first_argument_specifier2, [fail]) :-
	do(foreacharg(t, f(a, b), _), true).

test(invalid_third_argument_specifier, [fail]) :-
	do(foreacharg(Item, f(a, b, c), t), (member(Item, [a, b, c]))).

test(list) :-
	do(foreacharg(Item, [a, b]), member(Item, [a, [b]])).

test(list2) :-
	do(foreacharg(Item, [a, b], Index), (member(Item, [a, [b]]), member(Index, [1, 2, 3]))).

test(var_first_argument_specifier) :-
	do(foreacharg(a-X, f(a-b, a-b)), member(X, [a, b])).

test(var_first_argument_specifier2) :-
	do(foreacharg(a-X, f(a-b, a-b), Index), (member(X, [a, b]), member(Index, [1, 2, 3]))).

test(var_second_argument_specifier, [error(instantiation_error)]) :-
	do(foreacharg(Item, _), member(Item, [a, b, c])).

test(var_second_argument_specifier2, [error(instantiation_error)]) :-
	do(foreacharg(Item, _, Index), (member(Item, [a, b, c]), member(Index, [1, 2, 3]))).

test(var_third_argument_specifier) :-
	do(foreacharg(Item, f(a, b, c), _), member(Item, [a, b, c])).

:- end_tests(foreacharg).

:- begin_tests(fromto).

test(call_user_definied_goal_to_ground, [nondet]) :-
	findall(_, (do(fromto(0, In, Out, 3), (c(t, H), H \== In, Out is In+1))), List),
	assertion(length(List, 8)).

test(call_user_definied_goal_to_nonground, [all(Result == [3, 3, 3, 3, 3, 3, 3, 3])]) :-
	do(fromto(0, In, Out, g(Result)), (In == 3 -> Out = g(3); (c(t, H), H \== In, Out is In+1))).

test(invalid_second_argument_specifier, [forall(member(To, [10, g(_)])), fail]) :-
	do(fromto(0, t, Out, To), Out is _+1).

test(to_ground) :-
	do(fromto(0, In, Out, 10), Out is In+1).

test(to_nonground, [all(Result == [10])]) :-
	do(fromto(0, In, Out, g(Result)), (In == 10 -> Out = g(10); Out is In+1)).

test(var_first_argument_specifier_to_ground, [all(List == [10])]) :-
	do(fromto(List, In, Out, 10), Out is In+1).

test(var_first_argument_specifier_to_nonground, [all(List = [g(X)])]) :-
	do(fromto(List, In, Out, g(X)), Out is In+1).

test(var_fourth_argument_specifier, [all(List == [0])]) :-
	do(fromto(0, In, Out, List), Out is In+1).

test(var_second_argument_specifier, [forall(member(To, [10, g(_)]))]) :-
	do(fromto(0, _, Out, To), Out = To).

test(var_third_argument_specifier_to_ground, [forall(member(To, [10, g(_)]))]) :-
	do(fromto(0, In, _, To), _ is In+1).

:- end_tests(fromto).
