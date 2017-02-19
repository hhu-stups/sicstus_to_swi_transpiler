:- module(test_assoc_extension, [test_assoc_extension/0]).

:- use_module(library(plunit)).
:- use_module("../../src/swi_prolog_extensions/assoc_extension.pl").

get_test_tree(Assoc2, 1) :-
 	empty_assoc(Assoc1),
	put_assoc(a, Assoc1, 1, Assoc2).
get_test_tree(Assoc3, 2-left) :-
 	empty_assoc(Assoc1),
	put_assoc(b, Assoc1, 2, Assoc2),
  put_assoc(a, Assoc2, 1, Assoc3).
get_test_tree(Assoc3, 2-right) :-
 	empty_assoc(Assoc1),
	put_assoc(a, Assoc1, 1, Assoc2),
	put_assoc(b, Assoc2, 2, Assoc3).
get_test_tree(Assoc5, 4) :-
 	empty_assoc(Assoc1),
	put_assoc(a, Assoc1, 1, Assoc2),
	put_assoc(b, Assoc2, 2, Assoc3),
	put_assoc(c, Assoc3, 3, Assoc4),
	put_assoc(d, Assoc4, 4, Assoc5).

test_assoc_extension :-
	run_tests([del_max_assoc_ext, del_min_assoc_ext, height_assoc, incr_assoc, next_assoc_3,
            next_assoc_4, portray_assoc, prev_assoc_3, prev_assoc_4, size_assoc]).

:- begin_tests(del_max_assoc_ext).

test(invalid_first_argument, [fail]) :-
	del_max_assoc_ext(a, _, _, _).

test(size0, [setup(empty_assoc(Assoc)), fail]) :-
  del_max_assoc_ext(Assoc, _, _, _).

test(size1, [setup(get_test_tree(Assoc, 1)), true((Key == a, Value = 1, NewAssoc = t))]) :-
  del_max_assoc_ext(Assoc, Key, Value, NewAssoc).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true((Key == b, Value == 2, is_assoc(NewAssoc)))]) :-
	del_max_assoc_ext(Assoc, Key, Value, NewAssoc).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true((Key == b, Value == 2, is_assoc(NewAssoc)))]) :-
	del_max_assoc_ext(Assoc, Key, Value, NewAssoc).

test(size4, [setup(get_test_tree(Assoc, 4)), true((Key == d, Value == 4, is_assoc(NewAssoc)))]) :-
	del_max_assoc_ext(Assoc, Key, Value, NewAssoc).

:- end_tests(del_max_assoc_ext).

:- begin_tests(del_min_assoc_ext).

test(invalid_first_argument, [fail]) :-
	del_min_assoc_ext(a, _, _, _).

test(size0, [setup(empty_assoc(Assoc)), fail]) :-
  del_min_assoc_ext(Assoc, _, _, _).

test(size1, [setup(get_test_tree(Assoc, 1)), true((Key == a, Value == 1, NewAssoc == t))]) :-
  del_min_assoc_ext(Assoc, Key, Value, NewAssoc).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true((Key == a, Value == 1, is_assoc(NewAssoc)))]) :-
	del_min_assoc_ext(Assoc, Key, Value, NewAssoc).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true((Key == a, Value == 1, is_assoc(NewAssoc)))]) :-
	del_min_assoc_ext(Assoc, Key, Value, NewAssoc).

test(size4, [setup(get_test_tree(Assoc, 4)), true((Key == a, Value == 1, is_assoc(NewAssoc)))]) :-
	del_min_assoc_ext(Assoc, Key, Value, NewAssoc).

:- end_tests(del_min_assoc_ext).

:- begin_tests(height_assoc).

test(height0, [setup(empty_assoc(Assoc)), true(Height == 0)]) :-
	height_assoc(Assoc, Height).

test(height1, [setup(get_test_tree(Assoc, 1)), true(Height == 1)]) :-
	height_assoc(Assoc, Height).

test(height2left, [setup(get_test_tree(Assoc, 2-left)), true(Height == 2)]) :-
	height_assoc(Assoc, Height).

test(height2right, [setup(get_test_tree(Assoc, 2-right)), true(Height == 2)]) :-
	height_assoc(Assoc, Height).

test(height3, [setup(get_test_tree(Assoc, 4)), true(Height == 3)]) :-
	height_assoc(Assoc, Height).

test(invalid_first_argument, [fail]) :-
	height_assoc(a, _).

test(var_first_argument, error(instantiation_error)) :-
	height_assoc(_, 1).

:- end_tests(height_assoc).

:- begin_tests(incr_assoc).

test(bound_fourth_argument, [setup(get_test_tree(Assoc, 4)), fail]) :-
	incr_assoc(a, Assoc, 1, a).

test(incr_float_add, [setup(get_test_tree(Assoc, 4)), true((get_assoc(e, NewAssoc, Value), Value == 2.1))]) :-
  incr_assoc(e, Assoc, 2.1, NewAssoc).

test(incr_float_change, [setup(get_test_tree(Assoc, 4)), true((get_assoc(a, NewAssoc, Value), Value == 2.1))]) :-
  incr_assoc(a, Assoc, 1.1, NewAssoc).

test(incr_integer_add, [setup(get_test_tree(Assoc, 4)), true((get_assoc(e, NewAssoc, Value), Value == 2))]) :-
  incr_assoc(e, Assoc, 2, NewAssoc).

test(incr_integer_change, [setup(get_test_tree(Assoc, 4)), true((get_assoc(a, NewAssoc, Value), Value == 2))]) :-
  incr_assoc(a, Assoc, 1, NewAssoc).

test(invalid_second_argument, [fail]) :-
	incr_assoc(_, a, _, _).

test(invalid_third_argument, [setup(get_test_tree(Assoc, 4)), error(type_error(evaluable, _))]) :-
	incr_assoc(a, Assoc, a, _).

:- end_tests(incr_assoc).

:- begin_tests(next_assoc_3).

test(invalid_first_argument, [setup(get_test_tree(Assoc, 1)), fail]) :-
	next_assoc(k, Assoc, _).

test(invalid_first_argument_2, [setup(get_test_tree(Assoc, 4)), true(Key == a)]) :-
	next_assoc(0, Assoc, Key).

test(invalid_second_argument, [fail]) :-
	next_assoc(_, a, _).

test(size1, [setup(get_test_tree(Assoc, 1)), fail]) :-
  next_assoc(a, Assoc, _).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true(Key == b)]) :-
	next_assoc(a, Assoc, Key).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true(Key == b)]) :-
	next_assoc(a, Assoc, Key).

test(size4_var, [setup(get_test_tree(Assoc, 4)), true(Key == a)]) :-
	next_assoc(_, Assoc, Key).

test(size4_a, [setup(get_test_tree(Assoc, 4)), true(Key == b)]) :-
	next_assoc(a, Assoc, Key).

test(size4_b, [setup(get_test_tree(Assoc, 4)), true(Key == c)]) :-
	next_assoc(b, Assoc, Key).

test(size4_c, [setup(get_test_tree(Assoc, 4)), true(Key == d)]) :-
	next_assoc(c, Assoc, Key).

test(size4_d, [setup(get_test_tree(Assoc, 4)), fail]) :-
	next_assoc(d, Assoc, _).

:- end_tests(next_assoc_3).

:- begin_tests(next_assoc_4).

test(invalid_first_argument, [setup(get_test_tree(Assoc, 1)), fail]) :-
	next_assoc(k, Assoc, _, _).

test(invalid_second_argument, [fail]) :-
	next_assoc(_, a, _, _).

test(invalid_first_argument_2, [setup(get_test_tree(Assoc, 4)), true((Key == a, Value == 1))]) :-
	next_assoc(0, Assoc, Key, Value).

test(size1, [setup(get_test_tree(Assoc, 1)), fail]) :-
  next_assoc(a, Assoc, _, _).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true((Key == b, Value == 2))]) :-
	next_assoc(a, Assoc, Key, Value).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true((Key == b, Value == 2))]) :-
	next_assoc(a, Assoc, Key, Value).

test(size4_var, [setup(get_test_tree(Assoc, 4)), true((Key == a, Value == 1))]) :-
	next_assoc(_, Assoc, Key, Value).

test(size4_a, [setup(get_test_tree(Assoc, 4)), true((Key == b, Value == 2))]) :-
	next_assoc(a, Assoc, Key, Value).

test(size4_b, [setup(get_test_tree(Assoc, 4)), true((Key == c, Value == 3))]) :-
	next_assoc(b, Assoc, Key, Value).

test(size4_c, [setup(get_test_tree(Assoc, 4)), true((Key == d, Value == 4))]) :-
	next_assoc(c, Assoc, Key, Value).

test(size4_d, [setup(get_test_tree(Assoc, 4)), fail]) :-
	next_assoc(d, Assoc, _, _).

:- end_tests(next_assoc_4).

:- begin_tests(portray_assoc).

test(portray, [setup(get_test_tree(Assoc, 4))]) :-
	portray_assoc(Assoc).

test(var_argument, [true(Assoc == t)]) :-
  portray_assoc(Assoc).

:- end_tests(portray_assoc).

:- begin_tests(prev_assoc_3).

test(invalid_first_argument, [setup(get_test_tree(Assoc, 4)), true(Key == d)]) :-
	prev_assoc(k, Assoc, Key).

test(invalid_first_argument_2, [setup(get_test_tree(Assoc, 4)), fail]) :-
	prev_assoc(0, Assoc, _).

test(invalid_second_argument, [fail]) :-
	prev_assoc(_, a, _).

test(size1, [setup(get_test_tree(Assoc, 1)), fail]) :-
  prev_assoc(a, Assoc, _).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true(Key == a)]) :-
	prev_assoc(b, Assoc, Key).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true(Key == a)]) :-
	prev_assoc(b, Assoc, Key).

test(size4_var, [setup(get_test_tree(Assoc, 4)), fail]) :-
	prev_assoc(_, Assoc, _).

test(size4_a, [setup(get_test_tree(Assoc, 4)), fail]) :-
	prev_assoc(a, Assoc, _).

test(size4_b, [setup(get_test_tree(Assoc, 4)), true(Key == a)]) :-
	prev_assoc(b, Assoc, Key).

test(size4_c, [setup(get_test_tree(Assoc, 4)), true(Key == b)]) :-
	prev_assoc(c, Assoc, Key).

test(size4_d, [setup(get_test_tree(Assoc, 4)), true(Key == c)]) :-
	prev_assoc(d, Assoc, Key).

:- end_tests(prev_assoc_3).

:- begin_tests(prev_assoc_4).

test(invalid_first_argument, [setup(get_test_tree(Assoc, 4)), true((Key == d, Value == 4))]) :-
	prev_assoc(k, Assoc, Key, Value).

test(invalid_first_argument_2, [setup(get_test_tree(Assoc, 4)), fail]) :-
	prev_assoc(0, Assoc, _, _).

test(invalid_second_argument, [fail]) :-
	prev_assoc(_, a, _, _).

test(size1, [setup(get_test_tree(Assoc, 1)), fail]) :-
  prev_assoc(a, Assoc, _, _).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true((Key == a, Value == 1))]) :-
	prev_assoc(b, Assoc, Key, Value).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true((Key == a, Value == 1))]) :-
	prev_assoc(b, Assoc, Key, Value).

test(size4_var, [setup(get_test_tree(Assoc, 4)), fail]) :-
	prev_assoc(_, Assoc, _, _).

test(size4_a, [setup(get_test_tree(Assoc, 4)), fail]) :-
	prev_assoc(a, Assoc, _, _).

test(size4_b, [setup(get_test_tree(Assoc, 4)), true((Key == a, Value == 1))]) :-
	prev_assoc(b, Assoc, Key, Value).

test(size4_c, [setup(get_test_tree(Assoc, 4)), true((Key == b, Value == 2))]) :-
	prev_assoc(c, Assoc, Key, Value).

test(size4_d, [setup(get_test_tree(Assoc, 4)), true((Key == c, Value == 3))]) :-
	prev_assoc(d, Assoc, Key, Value).

:- end_tests(prev_assoc_4).

:- begin_tests(size_assoc).

test(invalid_first_argument, [fail]) :-
	size_assoc(a, _).

test(size0, [setup(empty_assoc(Assoc)), true(Size == 0)]) :-
	size_assoc(Assoc, Size).

test(size1, [setup(get_test_tree(Assoc, 1)), true(Size == 1)]) :-
	size_assoc(Assoc, Size).

test(size2left, [setup(get_test_tree(Assoc, 2-left)), true(Size == 2)]) :-
	size_assoc(Assoc, Size).

test(size2right, [setup(get_test_tree(Assoc, 2-right)), true(Size == 2)]) :-
	size_assoc(Assoc, Size).

test(size4, [setup(get_test_tree(Assoc, 4)), true(Size == 4)]) :-
	size_assoc(Assoc, Size).

test(size_assoc_bound, [setup(get_test_tree(Assoc, 4))]) :-
	size_assoc(Assoc, 4).

:- end_tests(size_assoc).
