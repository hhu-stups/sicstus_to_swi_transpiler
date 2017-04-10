/** <module> Provides core functionality for the transpiler.
*/
:- module(transpiler_core, [add_additional_terms/2, construct_term_tree/7, construct_terms/2,
														copy_extension_module_files/1, create_transpilation_predicate/3,
														load_transpilation_definition_files/1, preprocess_term/1,
														register_module_file/2, register_transpilation_predicate/2,
														register_trigger/2, transpile_trees/2]).

:- use_module(transpiler_builtin).
:- use_module(transpiler_messages).

:- dynamic(additional_module_file_path/2).
:- dynamic(trigger/1).
:- dynamic(trigger/2).
:- dynamic(transpile_predicate_name/2).
:- dynamic(transpile_term/3).

:- op(1150, fx, block).
:- op(1150, fx, mode).
:- op(1100, xfy, do).
:- op(500, yfx, \).

%! add_additional_terms(+TermList, -ExtendedTermList) is det.
%
% Adds neccessary Terms to TermList and unifies the result with ExtendedTermList.
add_additional_terms(TermList, ExtendedTermList) :-
	findall(TriggerName, trigger(TriggerName), TriggerNameList),
	add_additional_terms_builtin(TriggerNameList, TermList, ExtendedTermList).

% check if a trigger must be fired.
check_for_triggers(Term) :-
	(trigger(Term, TriggerName); trigger_for_builtin_predicates(Term, TriggerName)),
	\+ trigger(TriggerName),
	asserta(trigger(TriggerName)), !.

% construct the term tree without position information
construct_term_tree(Term, pri_term(Term, AttAssoc)) :-
	(var(Term);
	number(Term);
	atom(Term);
	string(Term);
	is_of_type(codes, Term)),
	empty_assoc(AttAssoc), !.
construct_term_tree({Term}, brace_term(TermTree, AttAssoc)) :-
	construct_term_tree(Term, TermTree),
	empty_assoc(AttAssoc), !.
construct_term_tree(List, list(List, AttAssoc)) :-
	is_list(List),
	empty_assoc(AttAssoc), !.
construct_term_tree(Term, term(Name, AttAssoc, ArgTermTrees)) :-
	Term =.. [Name|ArgTerms],
	construct_term_trees(ArgTerms, ArgTermTrees),
	empty_assoc(AttAssoc).

% construct term tree for inner terms.
construct_term_tree(Term, From-To, pri_term(Term, AttAssoc)) :-
	list_to_assoc([termpos-(From-To)], AttAssoc), !.
construct_term_tree(Term, string_position(From, To), pri_term(Term, AttAssoc)) :-
	list_to_assoc([termpos-(From-To)], AttAssoc), !.
construct_term_tree({Term}, brace_term_position(From, To, ArgPosition), brace_term(TermTree, AttAssoc)) :-
	construct_term_tree(Term, ArgPosition, TermTree),
	list_to_assoc([termpos-(From-To)], AttAssoc), !.
construct_term_tree(List, list_position(From, To, _, _), list(List, AttAssoc)) :-
	list_to_assoc([termpos-(From-To)], AttAssoc), !.
construct_term_tree(Term, term_position(From, To, FFrom, FTo, SubTermPosList),
										term(Name, AttAssoc, ArgTermTrees)) :-
	Term =.. [Name|ArgTerms],
	construct_term_trees(ArgTerms, SubTermPosList, ArgTermTrees),
	list_to_assoc([termpos-(From-To), functorpos-(FFrom-FTo)], AttAssoc), !.
construct_term_tree(Term, parentheses_term_position(From, To, ContentPos), par_term(TermTree, AttAssoc)) :-
	construct_term_tree(Term, ContentPos, TermTree),
	list_to_assoc([termpos-(From-To)], AttAssoc).

%! construct_term_tree(+Term, +StartPosition, +EndPosition, +TermPosition, +VarNames, +Comments, -Tree) is det.
%
% True, if Tree is a tree representing Term with additional information, which is given
% by StartPosition, EndPosition, TermPosition, VarNames, Comments.
%
% @var Term A Term which is represent by Tree
% @var StartPosition The position where the term starts.
% @var EndPosition The position where thTermPositione term ends.
% @var TermPosition The position term, which is given by read_term.
% @var VarNames The list of variable names, which is given by read_term.
% @var Comments The list of comments, which is given by read_term.
% @var Tree The tree representation of the term.
construct_term_tree(end_of_file, _, _, _, _, Comments, end_of_file(AttAssoc)) :-
	list_to_assoc([comments-Comments], AttAssoc), !.
construct_term_tree(:-Term, StartPosition, EndPosition, term_position(_, _, _, _, [SubTermPos]),
										VarNames, Comments, dir(:-, AttAssoc, SubTermTree)) :-
	list_to_assoc([startpos-StartPosition, endpos-EndPosition, varnames-VarNames,
								comments-Comments], AttAssoc),
	construct_term_tree(Term, SubTermPos, SubTermTree), !.
construct_term_tree(?-Term, StartPosition, EndPosition, term_position(_, _, _, _, [SubTermPos]),
										VarNames, Comments, dir(?-, AttAssoc, SubTermTree)) :-
	list_to_assoc([startpos-StartPosition, endpos-EndPosition, varnames-VarNames,
								comments-Comments], AttAssoc),
	construct_term_tree(Term, SubTermPos, SubTermTree), !.
construct_term_tree(Term, StartPosition, EndPosition, _, _, Comments,
										pri_term(Term, AttAssoc)) :-
	functor(Term, _, 0),
	list_to_assoc([startpos-StartPosition, endpos-EndPosition, comments-Comments], AttAssoc), !.
construct_term_tree(Term, StartPosition, EndPosition,
										term_position(From, To, FFrom, FTo, SubTermPosList), VarNames, Comments,
										term(Name, AttAssoc, ArgTermTrees)) :-
	Term =.. [Name|ArgTerms],
	construct_term_trees(ArgTerms, SubTermPosList, ArgTermTrees),
	list_to_assoc([startpos-StartPosition, endpos-EndPosition, termpos-(From-To),
								functorpos-(FFrom-FTo), varnames-VarNames, comments-Comments], AttAssoc), !.

construct_term_trees([], []).
construct_term_trees([Term|TermTail], [Tree|TreeTail]) :-
	construct_term_tree(Term, Tree),
	construct_term_trees(TermTail, TreeTail).
construct_term_trees([], _, []).
construct_term_trees([Term|TermTail], [Position|PositionTail], [Tree|TreeTail]) :-
	construct_term_tree(Term, Position, Tree),
	construct_term_trees(TermTail, PositionTail, TreeTail).

%! construct_terms(+TermTrees, +TermList)
%
% True, if the terms of TermList represents the trees of TermTrees. For each Tree
% in TermTrees, a corresponding term is constructed.
construct_terms([], []).
construct_terms([pri_term(Term, _)|TreeTail], [Term|TermTail]) :-
	construct_terms(TreeTail, TermTail).
construct_terms([brace_term(TermTree, _)|TreeTail], [{Term}|TermTail]) :-
	construct_terms([TermTree], [Term]),
	construct_terms(TreeTail, TermTail).
construct_terms([list(List, _)|TreeTail], [List|TermTail]) :-
	construct_terms(TreeTail, TermTail).
construct_terms([term(Name, _, Trees)|TreeTail], [Term|TermTail]) :-
	construct_terms(Trees, TermList),
	Term =.. [Name|TermList],
	construct_terms(TreeTail, TermTail).
construct_terms([par_term(TermTree, _)|TreeTail], [Term|TermTail]) :-
	construct_terms([TermTree], [Term]),
	construct_terms(TreeTail, TermTail).

copy_extension_module_file(ModulePath, DestinationDirectory) :-
	file_base_name(ModulePath, FileName),
	atom_concat(DestinationDirectory, FileName, DestinationFilePath),
	copy_file(ModulePath, DestinationFilePath).

%! copy_extension_module_files(+Directory) is det
%
% Copy all necessary modules to Directory.
copy_extension_module_files(Directory) :-
	atom_concat(Directory, '/', DestinationDirectory),
	% determine necessary module files
	trigger(TriggerName),
	additional_module_file_path(TriggerName, Path),
	% copy modules
	copy_extension_module_file(Path, DestinationDirectory),
	fail.
copy_extension_module_files(Directory) :-
	atom_concat(Directory, '/', DestinationDirectory),
	% check if do trigger fired
	trigger(TriggerName),
	% lookup path
	module_file_path(TriggerName, Path),
	% copy module
	copy_extension_module_file(Path, DestinationDirectory),
	fail.
copy_extension_module_files(_).

%! create_transpilation_predicate(TriggerName, Term, TranspiledTerm) is det.
%
% If the trigger, specified by TriggerName, is fired, each term, which can be unified
% with Term, is replaced by TranspiledTerm.
create_transpilation_predicate(TriggerName, Term, TranspiledTerm) :-
		\+ transpile_term(TriggerName, Term, TranspiledTerm),
		asserta(transpile_term(TriggerName, Term, TranspiledTerm)).

%! load_transpilation_definition_files(+DefinitionFiles) is det.
%
% True, if DefinitionFiles is list of paths to transpilation modules, which load
% the transpiler_extension module.
load_transpilation_definition_files(DefinitionFiles) :-
	% retract all predicates from previous transpiled files
	retractall(trigger(_)),
	retractall(trigger(_, _)),
	retractall(transpile_predicate_name(_, _)),
	retractall(transpile_term(_, _, _)),
	retractall(additional_module_file_path(_, _)),
	% load all DefinitionFiles
	load_files(DefinitionFiles, [if(true)]).

%! preprocess_term(+Term) is det.
%
% Preprocess Term to support operators.
preprocess_term(:-op(Priority, Type, Name)) :-
	op(Priority, Type, Name), !.
preprocess_term(_).

%! register_module_file(TriggerName, Path) is det.
%
% register a module file, which will be copied to the location of the output file,
% if trigger, specified by TriggerName, is fired.
register_module_file(TriggerName, Path) :-
 \+ additional_module_file_path(TriggerName, Path),
 asserta(additional_module_file_path(TriggerName, Path)).

%! register_transpilation_predicate(TriggerName, PredicateName) is det.
%
% Register a new predicate with name PredicateName, which is called for each term
% of the transpilation process. The predicate is only called if the specified trigger
% is fired.
register_transpilation_predicate(TriggerName, PredicateName) :-
	\+ transpile_predicate_name(TriggerName, PredicateName),
	asserta(transpile_predicate_name(TriggerName, PredicateName)).

%! register_transpilation_predicate(TriggerName, Term) is det.
% Register a new trigger. When Term is transpiled, the trigger is fired.
register_trigger(TriggerName, Term) :-
	\+ trigger(Term, TriggerName),
	asserta(trigger(Term, TriggerName)).

% helper predicate, which works like the built-in select/4, but depends on term equality
% to handle variables
select_var(Element, [Element2|Tail], 0, Tail) :-
	Element == Element2, !.
select_var(Element, [Element2|Tail], NewIndex, [Element2|NewTail]) :-
	select_var(Element, Tail, Index, NewTail),
	NewIndex is Index+1.

% sync_tree(TermList, TranspiledTermList, TermTrees, SyncedTermTrees)
% Depending on the changes of TranspiledTermList with respect to TermList, the list SyncedTermTrees
% is created sharing the most trees of the list TermTrees as possible.
sync_tree(_, [], _, []):- !.
sync_tree(TermList, TranspiledTermList, ArgTermTrees, ArgTermTrees) :-
	% When TermList is the same list as TranspiledTermList, the corresponding tree list
	% could be the same.
	TermList == TranspiledTermList, !.
sync_tree(TermList, [TranspiledElement|TranspiledTermTail],
					ArgTermTrees, [ArgTermTree|SyncedTreeTail]) :-
	% When TranspiledElement is in TermList, the  corresponding tree is used.
	select_var(TranspiledElement, TermList, Index, TermTail),
	nth0(Index, ArgTermTrees, ArgTermTree, ArgTermTreeTail),
	sync_tree(TermTail, TranspiledTermTail, ArgTermTreeTail, SyncedTreeTail), !.
sync_tree(TermList, [TranspiledElement|TranspiledTermTail],
					ArgTermTrees, [ArgTermTree|SyncedTreeTail]) :-
	% Otherwise: A new tree is constructed.
	construct_term_tree(TranspiledElement, ArgTermTree),
	sync_tree(TermList, TranspiledTermTail, ArgTermTrees, SyncedTreeTail), !.

transpile_term(Term, _) :-
	check_for_triggers(Term),
	fail.
transpile_term(Term, TranspiledTerm) :-
	transpile_builtin_term(Term, TranspiledTerm).
transpile_term(Term, TranspiledTerm) :-
	trigger(TriggerName),
	transpile_term(TriggerName, Term, TranspiledTerm).
transpile_term(Term, TranspiledTerm) :-
	trigger(TriggerName),
	transpile_predicate_name(TriggerName, TranspileTermName),
	call(TranspileTermName, Term, TranspiledTerm).
transpile_term(Term, Term).

transpile_tree(dir(Sign, AttAssoc, SubTermTree), dir(Sign, AttAssoc, TranspiledSubTerm)) :-
	get_assoc(startpos, AttAssoc, StartPos),
	set_current_line(StartPos),
	transpile_tree(SubTermTree, TranspiledSubTerm), !.
transpile_tree(pri_term(Term, AttAssoc), pri_term(Term, AttAssoc)) :-
	var(Term), !.
transpile_tree(pri_term(Term, AttAssoc), pri_term(TranspiledTerm, AttAssoc)) :-
	(get_assoc(startpos, AttAssoc, StartPos) -> set_current_line(StartPos); true),
	transpile_term(Term, TranspiledTerm), !.
transpile_tree(brace_term(TermTree, AttAssoc), brace_term(TranspiledTermTree, AttAssoc)) :-
	transpile_tree(TermTree, TranspiledTermTree), !.
transpile_tree(list(List, AttAssoc), list(TranspiledList, AttAssoc)) :-
	transpile_term(List, TranspiledList), !.
transpile_tree(par_term(TermTree, AttAssoc),
							par_term(TranspiledTermTree, AttAssoc)) :-
	transpile_trees([TermTree], [TranspiledTermTree]), !.
transpile_tree(term(Name, AttAssoc, ArgTermTrees),
							term(TranspiledName, AttAssoc, TranspiledArgTermTrees)) :-
	(get_assoc(startpos, AttAssoc, StartPos) -> set_current_line(StartPos); true),
	construct_terms(ArgTermTrees, TermList),
	Term =.. [Name|TermList],
	transpile_term(Term, TranspiledTerm),
	TranspiledTerm =.. [TranspiledName|TranspiledTermList],
	sync_tree(TermList, TranspiledTermList, ArgTermTrees, NewArgTermTrees),
	transpile_trees(NewArgTermTrees, TranspiledArgTermTrees), !.
transpile_tree(Tree, Tree).

%! transpile_trees(+Trees, -TranspiledTree) is det.
%
% True, if TranspiledTree is a list, which contains all transpiled trees of Trees.
transpile_trees([], []).
transpile_trees([Tree|TreeTail], [TranspiledTree2|TranspiledTail]) :-
	transpile_tree(Tree, TranspiledTree1),
	transpile_tree_builtin_predicates(TranspiledTree1, TranspiledTree2),
	transpile_trees(TreeTail, TranspiledTail).
