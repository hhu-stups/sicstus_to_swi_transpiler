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
add_additional_terms_builtin(do, [dir(Sign, StartPosition, EndPosition, VarNames, Comments,
																	term(module, Position, FPosition, TreeList))|TermTail],
														[dir(Sign, StartPosition, EndPosition, VarNames, Comments,
														term(module, Position, FPosition, TreeList)),
														additional_term(:-use_module(do_loop_extension))|
											TermTail]) :- !.
% if the input file is not a module, it will add use_module(do_loop_extension
add_additional_terms_builtin(do, TermList, [additional_term(:-use_module(do_loop_extension))|TermList]).
add_additional_terms_builtin(swi_extension, [dir(Sign, StartPosition, EndPosition, VarNames, Comments,
																	term(module, Position, FPosition, TreeList))|TermTail],
														[dir(Sign, StartPosition, EndPosition, VarNames, Comments,
														term(module, Position, FPosition, TreeList)),
														additional_term(:-use_module(swi_extension))|
											TermTail]) :- !.
% if the input file is not a module, it will add use_module(do_loop_extension
add_additional_terms_builtin(swi_extension, TermList, [additional_term(:-use_module(swi_extension))|TermList]).


%! module_file_path(do, Path) is semidet.
%
% True, if there is a module, which must be copied to the output, when the trigger,
% specifies by TriggerName, has been fired.
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
transpile_tree_builtin_predicates(term(Name, AttAssoc, ArgTermTrees), term(Name, AttAssoc, TranspiledTrees)) :-
	transpile_trees_builtin_predicates(ArgTermTrees, TranspiledTrees), !.
transpile_tree_builtin_predicates(Tree, Tree).

transpile_trees_builtin_predicates([], []) :- !.
transpile_trees_builtin_predicates([Tree|TermTail], [TranspiledTree|TranspiledTail]) :-
	transpile_tree_builtin_predicates(Tree, TranspiledTree),
	transpile_tree_builtin_predicates(TermTail, TranspiledTail), !.

%! trigger_for_builtin_predicates(Term, TriggerName) is semidet.
%
% True, if Term have to fire a trigger to support built-in predicates of Sicstus.
trigger_for_builtin_predicates(do(_, _), do).
trigger_for_builtin_predicates(_, swi_extension).
