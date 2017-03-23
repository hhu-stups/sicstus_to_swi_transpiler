/** <module> Transpilation of the Sicstus aggregate library to SWI aggregate library and additional aggregate_extension module
*/
:- module(transpiler_aggregate, [transpile_aggregate_term/2]).

:- use_module(transpiler_extension).

:- module_trigger(aggregate, library(aggregate)).

:- additional_module_file(aggregate, 'swi_prolog_extensions/aggregate_extension.pl').

:- replace_module(aggregate, library(aggregate), aggregate_extension).

:- transpilation_rule(aggregate, transpile_aggregate_term).

%! transpile_aggregate_term(+Term, -TranspiledTerm) is semidet
%
% True if Term is a predicate of the aggregate library and TranspiledTerm is the
% corresponding predicate of the aggregate library in SWI prolog respectively the aggregate_extension module.
transpile_aggregate_term(term_variables(A, B, C), term_variables_ext(A, B, C)).
