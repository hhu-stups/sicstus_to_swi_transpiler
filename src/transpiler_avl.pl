/** <module> Transpilation of the Sicstus AVL library to SWI assoc library.
		(cp. 10.4 Sicstus user manual and A.3 SWI prolog reference manual)
*/
:- module(transpiler_avl, [transpile_avl_term/2]).

:- use_module(transpiler_extension).

:- module_trigger(avl, library(avl)).

:- additional_module_file(avl, 'swi_prolog_extensions/assoc_extension.pl').

:- replace_module(avl, library(avl), assoc_extension).

:- transpilation_rule(avl, transpile_avl_term).

%! transpile_avl_term(+Term, -TranspiledTerm) is semidet
%
% True if Term is a predicate of the AVL library and TranspiledTerm is the
% corresponding predicate of the assoc library in SWI prolog.
transpile_avl_term(empty_avl(X), empty_assoc(X)).
transpile_avl_term(avl_to_list(X, Y), assoc_to_list(X, Y)).
transpile_avl_term(is_avl(X), is_assoc(X)).
transpile_avl_term(avl_domain(X, Y), assoc_to_keys(X, Y)).
transpile_avl_term(avl_range(X, Y), assoc_to_values(X, Y)).
transpile_avl_term(avl_min(X, Y), min_assoc(X, Y, _)).
transpile_avl_term(avl_min(X, Y, Z), min_assoc(X, Y, Z)).
transpile_avl_term(avl_max(X, Y), max_assoc(X, Y, _)).
transpile_avl_term(avl_max(X, Y, Z), max_assoc(X, Y, Z)).
transpile_avl_term(avl_height(X, Y), height_assoc(X, Y)).
transpile_avl_term(avl_size(X, Y), size_assoc(X, Y)).
transpile_avl_term(portray_avl(X), portray_assoc(X)).
transpile_avl_term(avl_member(X, Y), gen_assoc(X, Y, _)).
transpile_avl_term(avl_member(X, Y, Z), gen_assoc(X, Y, Z)).
transpile_avl_term(avl_fetch(X, Y), get_assoc(X, Y, _)).
transpile_avl_term(avl_fetch(X, Y, Z), get_assoc(X, Y, Z)).
transpile_avl_term(avl_next(X, Y, Z), get_next_assoc(X, Y, Z)).
transpile_avl_term(avl_next(A, B, C, D), get_next_assoc(A, B, C, D)).
transpile_avl_term(avl_prev(X, Y, Z), get_prev_assoc(X, Y, Z)).
transpile_avl_term(avl_prev(A, B, C, D), get_prev_assoc(A, B, C, D)).
transpile_avl_term(avl_change(A, B, C, D, E), get_assoc(A, B, C, D, E)).
transpile_avl_term(ord_list_to_avl(X, Y), ord_list_to_assoc(X, Y)). % SWI prolog checks ascending order of the keys.
transpile_avl_term(list_to_avl(X, Y), list_to_assoc(X, Y)).
transpile_avl_term(avl_store(A, B, C, D), put_assoc(A, B, C, D)).
transpile_avl_term(avl_incr(A, B, C, D), incr_assoc(A, B, C, D)).
transpile_avl_term(avl_delete(A, B, C, D), del_assoc(A, B, C, D)).
transpile_avl_term(avl_del_min(A, B, C, D), del_min_assoc_ext(A, B, C, D)).	% SWI prolog: predicate will succeed, if given association list is empty. sicstus: the predicate fails
transpile_avl_term(avl_del_max(A, B, C, D), del_max_assoc_ext(A, B, C, D)). % SWI prolog: predicate will succeed, if given association list is empty. sicstus: the predicate fails
transpile_avl_term(avl_map(X, Y), map_assoc(X, Y)).
transpile_avl_term(avl_map(X, Y, Z), map_assoc(X, Y, Z)).
