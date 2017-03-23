/** <module> Transpilation of the Sicstus ordsets library to SWI ordsets library.
		(cp. 10.24 Sicstus user manual and A.20 SWI prolog reference manual)
*/
:- module(transpiler_ordsets, [transpile_ordsets_term/2]).

:- use_module(transpiler_extension).

:- module_trigger(ordsets, library(ordsets)).

:- additional_module_file(ordsets, 'swi_prolog_extensions/ordsets_extension.pl').

:- replace_module(ordsets, library(ordsets), ordsets_extension).

:- transpilation_rule(ordsets, transpile_ordsets_term).

%! transpile_ordsets_term(+Term, -TranspiledTerm) is semidet
%
% True if Term is a predicate of the ordsets library module of Sicstus and TranspiledTerm is the
% corresponding predicate of the ordsets library in SWI prolog.
transpile_ordsets_term(ord_member(A, B), ord_memberchk(A, B)).
transpile_ordsets_term(ord_nonmember(A, B), ord_nonmemberchk(A, B)).
