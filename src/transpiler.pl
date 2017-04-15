/** <module> Sicstus to SWI prolog transpiler module
*/
:- module(transpiler, [transpile_file/2, transpile_file/3]).

:- use_module(transpiler_core).
:- use_module(transpiler_messages).

read_terms(Stream, []) :-
	at_end_of_stream(Stream), !.
read_terms(Stream, [TermTree|TermList]) :-
	read_term(Stream, Term, [comments(Comments), dotlists(true), module(transpiler_core),
													subterm_positions(TermPosisiton), term_position(StartPosition),
													variable_names(VarNames)]),
	stream_property(Stream, position(EndPosition)),
	% process operators
	preprocess_term(Term),
  construct_term_tree(Term, StartPosition, EndPosition, TermPosisiton, VarNames, Comments, TermTree),
	read_terms(Stream, TermList).

% initialization predicate to handle arguments
start :-
	current_prolog_flag(argv, Argv),
	[InputFilePath, OutputFilePath] = Argv, !,
	catch(transpile_file(InputFilePath, OutputFilePath), Error, print_message(error, Error)),
	halt.
start :-
	current_prolog_flag(argv, Argv),
	[InputFilePath, OutputFilePath|TranspilationDefinitionFiles] = Argv, !,
	catch(transpile_file(InputFilePath, TranspilationDefinitionFiles, OutputFilePath), Error,
				print_message(error, Error)),
	halt.
start :-
	print_message(error, "The command line arguments are not valid or missing."),
	halt.

%! transpile_file(+InputFilePath, +OutputFilePath) is det
%
% Transpile the Sicstus prolog code of the input file, specified by InputFilePath,
% and write the result to the file specified by OutputFilePath.
transpile_file(InputFilePath, OutputFilePath) :-
	% load tranpilation modules for built-in libraries of Sicstus prolog
	module_property(transpiler_core, file(ModulePath)),
	file_directory_name(ModulePath, Directory),
	atom_concat(Directory, '/transpiler_avl.pl', PathAVL),
	atom_concat(Directory, '/transpiler_assoc.pl', PathAssoc),
	atom_concat(Directory, '/transpiler_aggregate.pl', PathAggregate),
	atom_concat(Directory, '/transpiler_ordsets.pl', PathOrdsets),
	List = [PathAVL, PathAssoc, PathAggregate, PathOrdsets],
	transpile_file(InputFilePath, List, OutputFilePath).

%! transpile_file(+InputFilePath, +TranspilationDefinitionFiles, +OutputFilePath) is det
%
% Transpile the Sicstus prolog code of the input file, specified by InputFilePath,
% using the given user-definied transpilation files by TranspilationDefinitionFiles
% and write the result to the file specified by OutputFilePath.
transpile_file(InputFilePath, TranspilationDefinitionFiles, OutputFilePath) :-
	% extend transpiler loading given transpilation definition files
	load_transpilation_definition_files(TranspilationDefinitionFiles),
	% read terms of input file
	open(InputFilePath, read, InputStream),
	read_terms(InputStream, TermList),
	close(InputStream),
	% transpile each term
	transpile_trees(TermList, TranspiledTermList),
	add_additional_terms(TranspiledTermList, ExtendedTermList),
	% write transpiled tems to output file
	open(OutputFilePath, write, OutputStream),
	write_terms(OutputStream, ExtendedTermList),
	close(OutputStream),
	% extract directory of output source file path
	absolute_file_name(OutputFilePath, AbsoluteOutputFilePath),
	file_directory_name(AbsoluteOutputFilePath, Directory),
	% copy necessary module files
	copy_extension_module_files(Directory),
	% write errors and warnings
	write_messages.

write_comments(_, []) :- !.
write_comments(Stream, [_-Comment|Tail]) :-
	writeln(Stream, Comment),
	write_comments(Stream, Tail).

% write all comments between last term and  the first character of the current term
write_comments(_, CurrentLine, _, [], CurrentLine, []).
write_comments(_, CurrentLine, StartLine, [Position-Comment|Tail], CurrentLine, [Position-Comment|Tail]) :-
	% the comment appears behind the first character of the current term.
	stream_position_data(line_count, Position, CommentLine),
	CommentLine >= StartLine, !.
write_comments(Stream, CurrentLine, StartLine, [Position-Comment|Tail], FinalCurrentLine, CommentsTail) :-
	% the comment is behind the current line => write newline
	stream_position_data(line_count, Position, CommentLine),
	CurrentLine < CommentLine,
	nl(Stream),
	NewCurrentLine is CurrentLine+1,
	write_comments(Stream, NewCurrentLine, StartLine, [Position-Comment|Tail], FinalCurrentLine, CommentsTail), !.
write_comments(Stream, CurrentLine, StartLine, [Position-Comment|Tail], FinalCurrentLine, CommentsTail) :-
	% comment is befor the current line => write comment
	stream_position_data(line_count, Position, CommentLine),
	CurrentLine >= CommentLine,
	line_count(Stream, CurrentOutputLine),
	writeln(Stream, Comment),
	line_count(Stream, NewCurrentOutputLine),
	NewCurrentLine is CommentLine+(NewCurrentOutputLine-CurrentOutputLine),
	write_comments(Stream, NewCurrentLine, StartLine, Tail, FinalCurrentLine, CommentsTail).

% write newlines until the line, where the current term starts, is reached
write_lines(Stream, CurrentLine, StartLine) :-
	CurrentLine < StartLine,
	nl(Stream),
	NewCurrentLine is CurrentLine+1,
	write_lines(Stream, NewCurrentLine, StartLine), !.
write_lines(_, _, _).

write_terms(Stream, TermList) :-
	write_terms(Stream, 1, TermList).

write_terms(Stream, _, [end_of_file(AttAssoc)|_]) :-
	get_assoc(comments, AttAssoc, Comments),
	write_comments(Stream, Comments), !.
write_terms(Stream, CurrentLine, [dir(Name, AttAssoc, SubTermTree)|TermList]) :-
	% write a directive
	% process lines befor the first character and comments
	get_assoc(startpos, AttAssoc, StartPosition),
	stream_position_data(line_count, StartPosition, StartLine),
	get_assoc(comments, AttAssoc, Comments),
	write_comments(Stream, CurrentLine, StartLine, Comments, CurrentLine2, CommentsTail),
	write_lines(Stream, CurrentLine2, StartLine),
	write_comments(Stream, CommentsTail),
	% construct term and write term
	construct_terms([SubTermTree], SubTermList),
	Term =.. [Name|SubTermList],
	get_assoc(varnames, AttAssoc, VarNames),
	write_term(Stream, Term, [back_quotes(string), character_escapes(true), fullstop(true),
														module(transpiler_core), nl(true), quoted(true), numbervars(true),
														spacing(next_argument), variable_names(VarNames)]),
	get_assoc(endpos, AttAssoc, EndPosition),
	stream_position_data(line_count, EndPosition, EndLine),
	NewCurrentLine is EndLine+1,
	write_terms(Stream, NewCurrentLine, TermList), !.
write_terms(Stream, CurrentLine, [pri_term(Term, AttAssoc)|TermList]) :-
	% write an atom
	% process lines befor the first character and comments
	get_assoc(startpos, AttAssoc, StartPosition),
	stream_position_data(line_count, StartPosition, StartLine),
	get_assoc(comments, AttAssoc, Comments),
	write_comments(Stream, CurrentLine, StartLine, Comments, CurrentLine2, CommentsTail),
	write_lines(Stream, CurrentLine2, StartLine),
	write_comments(Stream, CommentsTail),
	% write term
	write_term(Stream, Term, [back_quotes(string), character_escapes(true), fullstop(true),
														module(transpiler_core), nl(true), quoted(true),
														numbervars(true), spacing(next_argument)]),
	get_assoc(endpos, AttAssoc, EndPosition),
	stream_position_data(line_count, EndPosition, EndLine),
	NewCurrentLine is EndLine+1,
	write_terms(Stream, NewCurrentLine, TermList), !.
write_terms(Stream, CurrentLine, [additional_term(Term)|TermList]) :-
	% write a term, which was added by the transpiler
	write_term(Stream, Term, [back_quotes(string), character_escapes(true), fullstop(true),
														module(transpiler_core), nl(true),
														quoted(true), numbervars(true), spacing(next_argument)]),
	write_terms(Stream, CurrentLine, TermList), !.
write_terms(Stream, CurrentLine, [term(Name, AttAssoc, ArgTermTrees)|TermList]) :-
	% write all other terms
	% process lines befor the first character and comments
	get_assoc(startpos, AttAssoc, StartPosition),
	stream_position_data(line_count, StartPosition, StartLine),
	get_assoc(comments, AttAssoc, Comments),
	write_comments(Stream, CurrentLine, StartLine, Comments, CurrentLine2, CommentsTail),
	write_lines(Stream, CurrentLine2, StartLine),
	write_comments(Stream, CommentsTail),
	% construct term and the term
	construct_terms(ArgTermTrees, ArgTerms),
	Term =.. [Name|ArgTerms],
	get_assoc(varnames, AttAssoc, VarNames),
	write_term(Stream, Term, [back_quotes(string), character_escapes(true), fullstop(true),
														module(transpiler_core), nl(true), quoted(true), numbervars(true),
														spacing(next_argument), variable_names(VarNames)]),
	get_assoc(endpos, AttAssoc, EndPosition),
	stream_position_data(line_count, EndPosition, EndLine),
	NewCurrentLine is EndLine+1,
	write_terms(Stream, NewCurrentLine, TermList), !.
