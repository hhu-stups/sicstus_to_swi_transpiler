:- module(transpiler, [transpile_file/2]).

:- use_module(transpiler_core).

% transpile the sicstus file, specified by InputFilePath, write the result to the file specified by OutputFilePath
transpile_file(InputFilePath, OutputFilePath) :-
	open(InputFilePath, read, InputStream),
	open(OutputFilePath, write, OutputStream),
	read_terms(InputStream, TermList),
	write_terms(OutputStream, TermList),
	close(InputStream),
	close(OutputStream).

read_terms(Stream, []) :-
	at_end_of_stream(Stream), !.
read_terms(Stream, [TranspiledTerm|TermList]) :-
	\+ at_end_of_stream(Stream),
	read_term(Stream, Term, []),
  transpile_term(Term, TranspiledTerm),
	read_terms(Stream, TermList).

write_terms(_, [end_of_file|_]) :- !.
write_terms(Stream, [Term|TermList]) :-
	% remove attribute variables
	copy_term(Term, CTerm, _),
	% number all variables for better output
	numbervars(CTerm, 0, _, [attvar(skip)]),
	% TODO: insert variable names from input file
	write_term(Stream, CTerm, [back_quotes(symbol_char), nl(true), quoted(true), fullstop(true), numbervars(true), spacing(next_argument)]),
	write_terms(Stream, TermList).
