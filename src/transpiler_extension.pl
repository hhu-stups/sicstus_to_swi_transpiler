/** <module> Provides predicates to extend the transpiler.
*/
:- module(transpiler_extension, [additional_module_file/2, module_trigger/2, replace_module/3,
																transpilation_rule/2]).

:- use_module(transpiler_core).

:- meta_predicate(additional_module_file(?, :)).

%! additional_module_file(+TriggerName, +Path) is det.
%
% Declare a additional module file, which is copied to the directory of the output file(s).
% The file will be only copied, if the trigger, specified by TriggerName, is fired.
% Path is a relative (with respect to location of the module, which calls this predicate)
% or absoulute path to a prolog module file.
%
% @throws instantiation_error if TriggerName or Path insufficiently instantiated.
% @throws existence_error if the file specified by Path is not accessible.
additional_module_file(TriggerName, _:Path) :-
	(var(TriggerName); var(Path)),
	instantiation_error(_).
additional_module_file(TriggerName, _:Path) :-
	is_absolute_file_name(Path), !,
	% Test, if the file specified by Path is accessible.
	open(Path, read, Stream),
	close(Stream),
	register_module_file(TriggerName, Path), !.
additional_module_file(TriggerName, Module:Path) :-
	% construct absoulute path
	module_property(Module, file(ModulePath)),
	file_directory_name(ModulePath, Directory),
	atom_concat(Directory, '/', DirectoryPath),
	atom_concat(DirectoryPath, Path, AbsolutePath),
	additional_module_file(TriggerName, Module:AbsolutePath).

%! module_trigger(+TriggerName, +Module) is det.
%
% Define a new trigger with name TriggerName. The new trigger will be fired, if Module is loaded.
%
% @throws instantiation_error if TriggerName or Module is insufficiently instantiated.
module_trigger(TriggerName, Module) :-
	(var(TriggerName); var(Module)),
	instantiation_error(_).
module_trigger(TriggerName, Module) :-
	register_trigger(TriggerName, use_module(Module)).

%! replace_module(+TriggerName, +ReplacedModule, +NewModule) is det
%
% Declare ReplacedModule to be replaced by NewModule if trigger TriggerName is fired.
%
% @throws instantiation_error if TriggerName, ReplacedModule or NewModule insufficiently instantiated.
replace_module(TriggerName, ReplacedModule, NewModule) :-
	(var(TriggerName); var(ReplacedModule); var(NewModule)),
	instantiation_error(_).
replace_module(TriggerName, ReplacedModule, NewModule) :-
	create_transpilation_predicate(TriggerName, use_module(ReplacedModule), use_module(NewModule)).

%! transpilation_rule(+TriggerName, +PredicateName) is det.
%
% Adds a new tranpilation rule (a predicate) to the transpiler. The predicate must be
% a predicate with arity two.
%
% @throws instantiation_error if TriggerName or PredicateName insufficiently instantiated.
% @throws type_error if PredicateName is not an atom.
transpilation_rule(TriggerName, PredicateName) :-
	(var(TriggerName); var(PredicateName)),
	instantiation_error(_).
transpilation_rule(TriggerName, PredicateName) :-
	must_be(atom, PredicateName),
	register_transpilation_predicate(TriggerName, PredicateName).
