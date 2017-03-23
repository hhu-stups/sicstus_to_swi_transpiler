/** <module> Transpilation of the Sicstus assoc library to SWI assoc library.
*/
:- module(transpiler_assoc, []).

:- use_module(transpiler_extension).

:- module_trigger(assoc, library(assoc)).

:- additional_module_file(assoc, 'swi_prolog_extensions/assoc_extension.pl').

:- replace_module(assoc, library(assoc), assoc_extension).
