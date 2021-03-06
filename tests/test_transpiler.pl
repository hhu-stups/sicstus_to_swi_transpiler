:- module(test_transpiler, [test_transpiler/0]).

:- use_module(library(plunit)).
:- use_module("../src/transpiler.pl").

create_output_directory :-
	\+ exists_directory("output"), !,
	make_directory("output").
create_output_directory :-
	delete_directory_and_contents("output"),
	make_directory("output").

get_sicstuscode_path(Path, FileName) :-
	source_file(test_transpiler:test_transpiler, TestTranspilerPath),
	file_directory_name(TestTranspilerPath, Directory),
	directory_file_path(Directory, FileName, Path).

test_transpiler :-
	run_tests([transpile_file]).

:- begin_tests(transpile_file, [setup(create_output_directory)]).

test(aggregate_file, [setup(get_sicstuscode_path(Path, "sicstuscode/aggregate.sicstus.pl")), nondet]) :-
	transpile_file(Path, "output/aggregate.swi.pl"),
	consult("output/aggregate.swi.pl"),
	get_variables(f(A, B, C), [C, B, A]),
	list_of_integers([1, 2, 3, 5]),
	unload_file("output/aggregate.swi.pl").

test(assoc_file, [setup(get_sicstuscode_path(Path, "sicstuscode/assoc.sicstus.pl"))]) :-
	transpile_file(Path, "output/assoc.swi.pl"),
	consult("output/assoc.swi.pl"),
	get_assoc(Assoc),
	store_alphabet(Assoc, NewAssoc),
	get_char_position(z, NewAssoc, 26),
	get_char_position(b, NewAssoc, 2),
	next_char(d, NewAssoc, e),
	count_alphabet(NewAssoc, 26),
	unload_file("output/assoc.swi.pl").

test(avl_file, [setup(get_sicstuscode_path(Path, "sicstuscode/avl.sicstus.pl"))]) :-
	transpile_file(Path, "output/avl.swi.pl"),
	consult("output/avl.swi.pl"),
	get_avl(AVL),
	store_alphabet(AVL, AAVL),
	get_char_position(z, AAVL, 26),
	next_char(d, AAVL, e),
	count_alphabet(AAVL, 26),
	unload_file("output/avl.swi.pl").

test(control_file, [setup(get_sicstuscode_path(Path, "sicstuscode/builtin/control.sicstus.pl"))]) :-
	transpile_file(Path, "output/control.swi.pl"),
	consult("output/control.swi.pl"),
	test_if,
	test_block,
	unload_file("output/control.swi.pl").

test(do_loop_file, [setup(get_sicstuscode_path(Path, "sicstuscode/builtin/do_loop.sicstus.pl"))]) :-
	transpile_file(Path, "output/do_loop.swi.pl"),
	consult("output/do_loop.swi.pl"),
	next_integer([1, 2, 3, 4], [2, 3, 4, 5]),
	sum_list([10, 100, 50], 160),
	integer_list_between(3, 7, [3, 4, 5, 6, 7]),
	count_items([1, 2, 3, 4], 4),
	count_arguments(a(b, c, d), 3),
	count_arguments([1, 2, 3], 2),
	number_arguments(a(a, b), [(1, a), (2, b)]),
	add_to_items([1,2,3], 3, [4, 5, 6]),
	unload_file("output/do_loop.swi.pl").

test(grammar_file, [setup(get_sicstuscode_path(Path, "sicstuscode/builtin/grammar.sicstus.pl")), nondet]) :-
	transpile_file(Path, "output/grammar.swi.pl"),
	consult("output/grammar.swi.pl"),
	phrase(expr(154), `-4+8+5*+5*6`),
	test_expr_grammar,
	test_do,
	test_if,
	test_once,
	unload_file("output/grammar.swi.pl").

test(hooks_file, [setup(get_sicstuscode_path(Path, "sicstuscode/builtin/hooks.sicstus.pl"))]) :-
	transpile_file(Path, "output/hooks.swi.pl"),
	use_module("output/hooks.swi.pl"),
	test_goal_expansion,
	unload_file("output/hooks.swi.pl").

test(load_file, [setup(get_sicstuscode_path(Path, "sicstuscode/builtin/load_file.sicstus.pl"))]) :-
	transpile_file(Path, "output/load_file.swi.pl"),
	get_sicstuscode_path(PathTestModule, "sicstuscode/builtin/test_module.pl"),
	copy_file(PathTestModule, "output/test_module.pl"),
	get_sicstuscode_path(PathTest, "sicstuscode/builtin/test.pl"),
	copy_file(PathTest, "output/test.pl"),
	get_sicstuscode_path(PathInclude1, "sicstuscode/builtin/include_file1.pl"),
	copy_file(PathInclude1, "output/include_file1.pl"),
	get_sicstuscode_path(PathInclude2, "sicstuscode/builtin/include_file2.pl"),
	copy_file(PathInclude2, "output/include_file2.pl"),
	use_module("output/load_file.swi.pl"),
	test_compile("output/test.pl"),
	unload_file("output/load_file.swi.pl").

test(math_file, [setup(get_sicstuscode_path(Path, "sicstuscode/builtin/math.sicstus.pl"))]) :-
	transpile_file(Path, "output/math.swi.pl"),
	consult("output/math.swi.pl"),
	test_comparison,
	test_arithmetic,
	test_integer,
	test_xor,
	test_floor,
	test_log,
	unload_file("output/math.swi.pl").

test(ordsets_file, [setup(get_sicstuscode_path(Path, "sicstuscode/ordsets.sicstus.pl"))]) :-
	transpile_file(Path, "output/ordsets.swi.pl"),
	consult("output/ordsets.swi.pl"),
	test_ordsets_predicates([1, 2, 3, 4, 5, 10, 12]),
	test_ord_member,
	test_ord_nonmember,
	unload_file("output/ordsets.swi.pl").

test(inputfile_does_not_exist, [error(existence_error(_, "sicstuscode/test.pl"))]) :-
	transpile_file("sicstuscode/test.pl", "output/test.pl").

test(invalid_inputfile, [error(syntax_error(_)), setup(get_sicstuscode_path(Path, "sicstuscode/syntax.pl"))]) :-
	transpile_file(Path, "output/syntax.swi.pl").

test(outputfile_directory_does_not_exitst, [error(existence_error(_, "directory/avl.swi.pl")), setup(get_sicstuscode_path(Path, "sicstuscode/avl.sicstus.pl"))]) :-
	transpile_file(Path, "directory/avl.swi.pl").

test(var_inputfilepath_argument, [error(instantiation_error)]) :-
	transpile_file(_, "test.pl").

test(var_outputfilepath_argument, [error(instantiation_error), setup(get_sicstuscode_path(Path, "sicstuscode/avl.sicstus.pl"))]) :-
	transpile_file(Path, _).

:- end_tests(transpile_file).
