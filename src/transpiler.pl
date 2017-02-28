/** <module> Sicstus to Swi prolog transpiler module
*/

:- module(transpiler, [transpile_file/2]).

:- use_module(transpiler_core).

:- op(1100, xfy, do).

%! transpile_file(+InputFilePath, +OutputFilePath) is det
% Transpile the Sicstus prolog code of the input file, specified by InputFilePath,
% and write the result to the file specified by OutputFilePath.
transpile_file(InputFilePath, OutputFilePath) :-
	open(InputFilePath, read, InputStream),
	open(OutputFilePath, write, OutputStream),
	read_terms(InputStream, TermList),
	get_additional_directives(DirectivesList),
	write("DL:"), writeln(DirectivesList),
	append(DirectivesList, TermList, NewTermList),
	% extract directory of output source file path
	absolute_file_name(OutputFilePath, AbsoluteOutputFilePath),
	file_directory_name(AbsoluteOutputFilePath, Directory),
	write_terms(OutputStream, NewTermList),
	close(InputStream),
	close(OutputStream),
	% copy necessary module files
	copy_extension_module_files(Directory).

read_terms(Stream, []) :-
	at_end_of_stream(Stream), !.
read_terms(Stream, [TranspiledTerm|TermList]) :-
	\+ at_end_of_stream(Stream),
	read_term(Stream, Term, [module(transpiler)]),
  transpile_term(Term, TranspiledTerm),
	read_terms(Stream, TermList).

write_terms(_, [end_of_file|_]) :- !.
write_terms(Stream, [Term|TermList]) :-
	% remove attribute variables
	copy_term(Term, CTerm, _),
	% number all variables for better output
	numbervars(CTerm, 0, _, [attvar(skip)]),
	% TODO: insert variable names from input file
	write_term(Stream, CTerm, [back_quotes(symbol_char), nl(true), quoted(true), fullstop(true), module(transpiler), numbervars(true), spacing(next_argument)]),
	write_terms(Stream, TermList).
