:-module(load_file, [test_compile/1], [hidden(true)]).
:- load_files(test_module, [load_type(source), compilation_mode(consult), encoding_signature(true), eol(lf)]).

:- include([include_file1, include_file2]).

test_compile(A) :-
  compile(A).
