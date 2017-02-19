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

test(avl_file, [setup(get_sicstuscode_path(Path, "sicstuscode/avl.sicstus.pl"))]) :-
	transpile_file(Path, "output/avl.swi.pl"),
	consult("output/avl.swi.pl"),
	get_avl(AVL),
	store_alphabet(AVL, AAVL),
	get_char_position(z, AAVL, 26),
	next_char(d, AAVL, e),
	count_alphabet(AAVL, 26).

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
