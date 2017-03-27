/** <module> Provides functionality to transpile built-in predicates of Sicstus.
*/
:- module(transpiler_builtin, [add_additional_terms_builtin/3, module_file_path/2,
															transpile_tree_builtin_predicates/2, trigger_for_builtin_predicates/2]).

:- use_module(transpiler_core).


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

%! module_file_path(do, Path) is semidet.
%
% True, if there is a module, which must be copied to the output, when the trigger,
% specifies by TriggerName, has been fired.
module_file_path(do, Path) :-
	module_property(transpiler_core, file(ModulePath)),
	file_directory_name(ModulePath, Directory),
	atom_concat(Directory, '/swi_prolog_extensions/do_loop_extension.pl', Path).

/*transpile_tree_builtin_predicates(term(is, TermPosition, FunctorPosition, ArgTermTrees),
																	term(is, TermPosition, FunctorPosition, TranspiledArgTermTrees)) :-
	ArgTermTrees = [FirstArgTree, SecondArgTree],
	transpile_arithmetic_expression(SecondArgTree, TranspiledSecondArgTree),
	TranspiledArgTermTrees = [FirstArgTree, TranspiledSecondArgTree]. */
transpile_tree_builtin_predicates(Tree, Tree).


%! trigger_for_builtin_predicates(Term, TriggerName) is semidet.
%
% True, if Term have to fire a trigger to support built-in predicates of Sicstus.
trigger_for_builtin_predicates(do(_, _), do).
