/** <module> Provides functionality to transpile built-in predicates of Sicstus.
*/
:- module(transpiler_builtin, [add_additional_terms_builtin/3, module_file_path/2, transpile_builtin_term/2,
															transpile_tree_builtin_predicates/2, trigger_for_builtin_predicates/2]).

:- use_module(transpiler_core).
:- use_module(transpiler_messages).

%! add_additional_terms_builtin(+TriggerName, +TermList, -ExtendedTermList) is semidet.
%
% True, if it is neccessary to add terms, which supports built-in predicates of Sicstus in SWI,
% to TermList, when the trigger, specifies by TriggerName, has been fired.
% Unifies the result with ExtendedTermList.
add_additional_terms_builtin(TriggerNameList, [dir(Sign, AttAssoc, term(module, AttAssocTerm, TreeList))|TermTail],
														ExtendedTermList) :-
	select(block_directive, TriggerNameList, NewTriggerNameList),
	add_additional_terms_builtin(NewTriggerNameList, [dir(Sign, AttAssoc, term(module, AttAssocTerm, TreeList)),
																										additional_term(:-use_module(block_directive))|TermTail],
															ExtendedTermList), !.
add_additional_terms_builtin(TriggerNameList, TermList, ExtendedTermList) :-
	select(block_directive, TriggerNameList, NewTriggerNameList),
	add_additional_terms_builtin(NewTriggerNameList, [additional_term(:-use_module(block_directive))|TermList],
															ExtendedTermList), !.
add_additional_terms_builtin(TriggerNameList, [dir(Sign, AttAssoc, term(module, AttAssocTerm, TreeList))|TermTail],
														ExtendedTermList) :-
	select(do, TriggerNameList, NewTriggerNameList),
	add_additional_terms_builtin(NewTriggerNameList, [dir(Sign, AttAssoc, term(module, AttAssocTerm, TreeList)),
																										additional_term(:-use_module(do_loop_extension))|TermTail],
															ExtendedTermList), !.
% if the input file is not a module, it will add use_module(do_loop_extension)
add_additional_terms_builtin(TriggerNameList, TermList, ExtendedTermList) :-
	select(do, TriggerNameList, NewTriggerNameList),
	add_additional_terms_builtin(NewTriggerNameList, [additional_term(:-use_module(do_loop_extension))|TermList],
															ExtendedTermList), !.
add_additional_terms_builtin(TriggerNameList, [dir(Sign, AttAssoc, term(module, AttAssocTerm, TreeList))|TermTail],
														ExtendedTermList2) :-
	select(swi_extension, TriggerNameList, NewTriggerNameList),
	add_additional_terms_builtin(NewTriggerNameList, [dir(Sign, AttAssoc, term(module, AttAssocTerm, TreeList)),
																										additional_term(:-use_module(swi_extension))|TermTail],
															ExtendedTermList),
	add_include_directives(ExtendedTermList, ExtendedTermList2), !.
add_additional_terms_builtin(TriggerNameList, TermList, ExtendedTermList2) :-
	select(swi_extension, TriggerNameList, NewTriggerNameList),
	add_additional_terms_builtin(NewTriggerNameList, [additional_term(:-use_module(swi_extension))|TermList],
															ExtendedTermList),
	add_include_directives(ExtendedTermList, ExtendedTermList2), !.
add_additional_terms_builtin(_, TermList, TermList).

add_include_directives([], []).
add_include_directives([dir(Sign, AttAssoc, term(include, AttAssocTerm, [list(List, _)]))|TermListTail], ResultTermList) :-
	List = [FirstFile|FilesTail],
	findall(additional_term(:-include(File)), member(File, FilesTail), ResultList),
	add_include_directives(TermListTail, ExtendedTermListTail),
	empty_assoc(EmptyAttAssoc),
	append([dir(Sign, AttAssoc, term(include, AttAssocTerm, [pri_term(FirstFile, EmptyAttAssoc)]))|ResultList],
				ExtendedTermListTail, ResultTermList), !.
add_include_directives([Term|TermListTail], [Term|ExtendedTermListTail]) :-
	add_include_directives(TermListTail, ExtendedTermListTail).

%! module_file_path(do, Path) is semidet.
%
% True, if there is a module, which must be copied to the output, when the trigger,
% specifies by TriggerName, has been fired.
module_file_path(block_directive, Path) :-
	module_property(transpiler_core, file(ModulePath)),
	file_directory_name(ModulePath, Directory),
	atom_concat(Directory, '/swi_prolog_extensions/block_directive.pl', Path).
module_file_path(do, Path) :-
	module_property(transpiler_core, file(ModulePath)),
	file_directory_name(ModulePath, Directory),
	atom_concat(Directory, '/swi_prolog_extensions/do_loop_extension.pl', Path).
module_file_path(swi_extension, Path) :-
	module_property(transpiler_builtin, file(ModulePath)),
	file_directory_name(ModulePath, Directory),
	atom_concat(Directory, '/swi_prolog_extensions/swi_extension.pl', Path).

%! transpile_builtin_term(+Term, -TranspiledTerm) is semidet.
%
% True, if Term is a built-in term of Sicstus, but is not supported by SWI.
% The term Term is replaced by TranspiledTerm.
transpile_builtin_term(acot2(X, Y), acot2(X, Y)) :-
	create_warning("Arithmetic functor acot2/2 is not supported.",
								"SWI-Prolog has no arithmetic functor acot2/2.").
transpile_builtin_term(compile(A), consult(A)).
transpile_builtin_term(load_files(user), [user]).
transpile_builtin_term(load_files(Files), ([user], load_files(FilesWithoutUser))) :-
	member(user, Files),
	select(user, Files, FilesWithoutUser).
transpile_builtin_term(load_files(user, _), [user]).
transpile_builtin_term(load_files(Files, Options), ([user], load_files(FilesWithoutUser, TranspiledOptions))) :-
	member(user, Files),
	select(user, Files, FilesWithoutUser),
	transpile_options_load_files(Options, TranspiledOptions), !.
transpile_builtin_term(load_files(Files, Options), load_files(Files, TranspiledOptions)) :-
	transpile_options_load_files(Options, TranspiledOptions).
transpile_builtin_term(if(X, Y, Z), (X*->Y;Z)).
transpile_builtin_term((M:goal_expansion(Goal, Layout, Module, Goal2, Layout2):-Body),
											(M:goal_expansion(Goal, Layout, Goal2, Layout2):-prolog_load_context(module, Module), Body)) :-
	create_warning("The hook predicate goal_expansion/5 is not fully supported.",
								"The transpiler transpiles goal_expansion/5 to the hook predicate goal_expansion/4 of SWI-Prolog, which is will not be called by SWI-Prolog, if the goal is imported and then called by a differnt module.").
transpile_builtin_term(module(A, B, _), module(A, B)).
transpile_builtin_term(read_term(A, Options), read_term(A, Options)) :-
		(member(layout(_), Options); member(consume_layout(_), Options)),
		create_warning("The layout or consume_layout are not supported as options for read_term/2.",
									"SWI-Prolog has no option consume_layout(Boolean) or layout(Layout).").
transpile_builtin_term(read_term(A, B, Options), read_term(A, B, Options)) :-
		(member(layout(_), Options); member(consume_layout(_), Options)),
		create_warning("The layout or consume_layout are not supported as options for read_term/3.",
									"SWI-Prolog has no option consume_layout(Boolean) or layout(Layout).").
transpile_builtin_term(restore(A), restore(A)) :-
		create_warning("The predicate restore/1 is not supported.",
									"SWI-Prolog has no predicate with the same semantics.").
transpile_builtin_term(user:runtime_entry(A), user:runtime_entry(A)) :-
		create_warning("The hook predicate user:runtime_entry/1 is not supported.",
									"SWI-Prolog has no predicate with the same semantics.").
transpile_builtin_term(save_files(A, B), save_files(A, B)) :-
	create_warning("The predicate save_files/2 is not supported.",
								"SWI-Prolog has no predicate with the same semantics.").
transpile_builtin_term(save_predicates(A, B), save_predicates(A, B)) :-
	create_warning("The predicate save_predicates/2 is not supported.",
								"SWI-Prolog has no predicate with the same semantics.").
transpile_builtin_term(save_program(A, B), qsave_program(A, [goal(B)])) :-
		create_warning("The predicate save_modules/1 is not supported.",
									"SWI-Prolog has no predicate with the same semantics. In a few cases qsave_program/1 could be good replacement.").
transpile_builtin_term(save_program(A), qsave_program(A)) :-
		create_warning("The predicate save_program/1 is not supported.",
									"SWI-Prolog has no predicate with the same semantics. In a few cases qsave_program/1 could be good replacement.").
transpile_builtin_term(save_program(A, B), qsave_program(A, B)) :-
		create_warning("The predicate save_modules/1 is not supported.",
									"SWI-Prolog has no predicate with the same semantics. In a few cases qsave_program/1 could be good replacement.").
transpile_builtin_term(set_module(Module), set_module(base(Module))).
transpile_builtin_term(user:term_expansion(A, B, C, D, E, F), user:term_expansion(A, B, C, D, E, F)) :-
	create_warning("The hook predicate user:term_expansion/6 is not supported.",
								"SWI-Prolog supports a different term expansion mechanism in comparision to Sicstus prolog.").
transpile_builtin_term(use_module(File), load_files(File, [if(changed)])).
transpile_builtin_term(use_module(File, Imports), load_files(File, [imports(Imports), if(changed)])).
transpile_builtin_term(write_term(A, Options), write_term(A, TranspiledOptions)) :-
	subtract(Options, [legacy_modulevars(_), indented(_), quoted_charset(_), loat_format(_)], TranspiledOptions).
transpile_builtin_term(write_term(A, B, Options), write_term(A, B, TranspiledOptions)) :-
	subtract(Options, [legacy_modulevars(_), indented(_), quoted_charset(_), loat_format(_)], TranspiledOptions).

% remove options, which are not supported by SWI-Prolog
transpile_options_load_files([], []).
transpile_options_load_files([when(_)|OptionsTail], TranspiledOptionsTail) :-
	transpile_options_load_files(OptionsTail, TranspiledOptionsTail), !.
transpile_options_load_files([load_type(_)|OptionsTail], TranspiledOptionsTail) :-
	transpile_options_load_files(OptionsTail, TranspiledOptionsTail), !.
transpile_options_load_files([compilation_mode(_)|OptionsTail], TranspiledOptionsTail) :-
	transpile_options_load_files(OptionsTail, TranspiledOptionsTail), !.
transpile_options_load_files([eol(_)|OptionsTail], TranspiledOptionsTail) :-
	transpile_options_load_files(OptionsTail, TranspiledOptionsTail), !.
transpile_options_load_files([encoding_signature(_)|OptionsTail], TranspiledOptionsTail) :-
	transpile_options_load_files(OptionsTail, TranspiledOptionsTail), !.
transpile_options_load_files([Option|OptionsTail], [Option|TranspiledOptionsTail]) :-
	transpile_options_load_files(OptionsTail, TranspiledOptionsTail).

%! transpile_tree_builtin_predicates(+Tree, -TranspiledTree) is det.
transpile_tree_builtin_predicates(term(is, AttAssoc, [LeftTree, RightTree]),
																	term(',', EmptyAssoc, [NewLeftTree, NewRightTree])) :-
	empty_assoc(EmptyAssoc),
	NewLeftTree = term(convert_arithmetic_expression, EmptyAssoc, [RightTree, pri_term(Expr, EmptyAssoc)]),
	NewRightTree = term(is, AttAssoc, [LeftTree, pri_term(Expr, EmptyAssoc)]), !.
transpile_tree_builtin_predicates(term(Sign, AttAssoc, [LeftTree, RightTree]),
																	term(',', EmptyAssoc, [NewLeftTree, NewRightTree])) :-
	(==(Sign, =:=); ==(Sign, =\=); ==(Sign, <); ==(Sign, >=); ==(Sign, >); ==(Sign, =<)),
	empty_assoc(EmptyAssoc),
	NewLeftTree = term(convert_arithmetic_expression, EmptyAssoc, [LeftTree, pri_term(ExprLeft, EmptyAssoc)]),
	NewRightTree = term(',', EmptyAssoc, [NewLeftTree2, NewRightTree2]),
	NewLeftTree2 = term(convert_arithmetic_expression, EmptyAssoc, [RightTree, pri_term(ExprRight, EmptyAssoc)]),
	NewRightTree2 = term(Sign, AttAssoc, [pri_term(ExprLeft, EmptyAssoc), pri_term(ExprRight, EmptyAssoc)]), !.
transpile_tree_builtin_predicates(Tree, Tree).

%! trigger_for_builtin_predicates(Term, TriggerName) is semidet.
%
% True, if Term have to fire a trigger to support built-in predicates of Sicstus.
trigger_for_builtin_predicates(Term, block_directive) :-
	functor(Term, block, _).
trigger_for_builtin_predicates(do(_, _), do).
trigger_for_builtin_predicates(_, swi_extension).
