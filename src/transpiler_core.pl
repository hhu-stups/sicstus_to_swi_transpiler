/** <module> Provides core functionality for the transpiler.
*/

:- module(transpiler_core, [copy_extension_module_files/1, transpile_term/2]).

:- use_module(transpiler_avl).

:- dynamic(required_module/2).

copy_extension_module_file(SourceDirectory, DestinationDirectory, FileName) :-
	atom_concat(SourceDirectory, FileName, SourcePath),
	atom_concat(DestinationDirectory, FileName, DestinationPath),
	copy_file(SourcePath, DestinationPath).

%! copy_extension_module_files(+Directory) is det
%
% Copy all necessary modules to Directory.
copy_extension_module_files(Directory) :-
	atom_concat(Directory, '/', DestinationDirectory),
	% locate directory of module files
	source_file_property(TranspilerCoreFile, module(transpiler_core)),
	file_directory_name(TranspilerCoreFile, TranspilerCoreFileDirectory),
	atom_concat(TranspilerCoreFileDirectory, '/swi_prolog_extensions/', ModuleDirectory),
	% determine necessary modules
	extension_module_avl(ModuleList),
	% copy modules
	forall(member(Member, ModuleList), copy_extension_module_file(ModuleDirectory, DestinationDirectory, Member)).


%! transpile_term(+Term, -TranspiledTerm) is det
%
% Transpile sictus prolog Term to SWI prolog TranspiledTerm.
% If Term is a valid swi prolog term, TranspiledTerm will be unified with Term,
% otherwise TranspiledTerm will be unified with a term with equal meaning
% in SWI prolog.
transpile_term((Term:-Body), (NTerm:-NBody)) :-
	transpile_term(Term, NTerm),
	transpile_body(Body, NBody), !.
transpile_term((Term-->Body), (NTerm-->NBody)) :-
	transpile_term(Term, NTerm),
	transpile_body(Body, NBody), !.
transpile_term(Term, NNTerm) :-
	transpile_avl_term(Term, NTerm),
	transpile_term(NTerm, NNTerm), !.
transpile_term(Term, Term).

% predefined operators in order of priority (cp. ISO standard 6.3.4.4 (p. 19))
transpile_body(A;B, NA;NB) :-
	transpile_body(A, NA),
	transpile_body(B, NB), !.
transpile_body(A->B, NA->NB) :-
	transpile_body(A, NA),
	transpile_body(B, NB), !.
transpile_body((A,B), (NA,NB)) :-
	transpile_body(A, NA),
	transpile_body(B, NB), !.
transpile_body(\+Term, \+NTerm) :-
	transpile_body(Term, NTerm), !.
transpile_body({Term}, {NTerm}) :-
	transpile_body(Term, NTerm), !.
transpile_body(A=B, NA=NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A\=B, NA\=NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A==B, NA==NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A\==B, NA\==NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A@<B, NA@<NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A@=<B, NA@=<NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A@>B, NA@>NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A@>=B, NA@>=NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A=..B, NA=..NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A is B, NA is NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A=:=B, NA=:=NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A=\=B, NA=\=NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A<B, NA<NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A=<B, NA=<NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A>B, NA>NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A>=B, NA>=NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A+B, NA+NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A-B, NA-NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A/\B, NA/\NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A\/B, NA\/NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A*B, NA*NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A/B, NA/NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A//B, NA//NB) :-
	(nonvar(A) -> transpile_expression(A, NA); A = NA),
	(nonvar(B) -> transpile_expression(B, NB); B = NB), !.
transpile_body(A rem B, NA rem NB) :-
	(nonvar(A)-> transpile_expression(A, NA); A = NA),
	(nonvar(B)-> transpile_expression(B, NB); B = NB), !.
transpile_body(A mod B, NA mod NB) :-
	(nonvar(A)-> transpile_expression(A, NA); A = NA),
	(nonvar(B)-> transpile_expression(B, NB); B = NB), !.
transpile_body(A<<B, NA<<NB) :-
	(nonvar(A)-> transpile_expression(A, NA); A = NA),
	(nonvar(B)-> transpile_expression(B, NB); B = NB), !.
transpile_body(A>>B, NA>>NB) :-
	(nonvar(A)-> transpile_expression(A, NA); A = NA),
	(nonvar(B)-> transpile_expression(B, NB); B = NB), !.
transpile_body(A**B, NA**NB) :-
	(nonvar(A)-> transpile_expression(A, NA); A = NA),
	(nonvar(B)-> transpile_expression(B, NB); B = NB), !.
transpile_body(A^B, NA^NB) :-
	(nonvar(A)-> transpile_expression(A, NA); A = NA),
	(nonvar(B)-> transpile_expression(B, NB); B = NB), !.
transpile_body(-Term, -NTerm) :-
	(nonvar(Term) -> transpile_expression(Term, NTerm); Term = NTerm), !.
transpile_body(\Term, \NTerm) :-
	(nonvar(Term) -> transpile_expression(Term, NTerm); Term = NTerm), !.
transpile_body(Term, NTerm) :-
	transpile_expression(Term, NTerm), !.
transpile_body(Term, NTerm) :-
	transpile_term(Term, NTerm), !.
transpile_body(Body, Body).

transpile_expression(String, List) :-
  string(String),
  string_codes(String, List), !.
transpile_expression(Expression, NExpression) :-
	transpile_term(Expression, NExpression), !.
transpile_expression(Expression, Expression).
