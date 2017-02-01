:- module(transpiler_avl, [transpile_avl_term/2]).

transpile_avl_term((:-use_module(library(avl))), (:-use_module(library(assoc)))).

% cp. 10.4 Sicstus user manual and A.3 swi prolog reference manual
transpile_avl_term(empty_avl(X), empty_assoc(X)).
transpile_avl_term(avl_to_list(X, Y), assoc_to_list(X, Y)).
transpile_avl_term(is_avl(X), is_assoc(X)).
transpile_avl_term(avl_domain(X, Y), assoc_to_keys(X, Y)).
transpile_avl_term(avl_range(X, Y), assoc_to_values(X, Y)).
transpile_avl_term(avl_min(X, Y), min_assoc(X, Y, _)).
transpile_avl_term(avl_min(X, Y, Z), min_assoc(X, Y, Z)).
transpile_avl_term(avl_max(X, Y), max_assoc(X, Y, _)).
transpile_avl_term(avl_max(X, Y, Z), max_assoc(X, Y, Z)).
% transpile_avl_term(avl_height(X, Y), ).
% transpile_avl_term(avl_size(X, Y), ).
% transpile_avl_term(portray_avl(X), ).
transpile_avl_term(avl_member(X, Y), gen_assoc(X, Y, _)).
transpile_avl_term(avl_member(X, Y, Z), gen_assoc(X, Y, Z)).
transpile_avl_term(avl_fetch(X, Y), get_assoc(X, Y, _)).
transpile_avl_term(avl_fetch(X, Y, Z), get_assoc(X, Y, Z)).
% transpile_avl_term(avl_next(X, Y, Z), ).
% transpile_avl_term(avl_next(A, B, C, D), ).
% transpile_avl_term(avl_prev(X, Y, Z), ).
% transpile_avl_term(avl_prev(A, B, C, D), ).
% transpile_avl_term(ord_list_to_avl(X, Y), ord_list_to_assoc(X, Y, Z)). swi prolog checks ascending order of the keys.
transpile_avl_term(list_to_avl(X, Y), list_to_assoc(X, Y)).
transpile_avl_term(avl_store(A, B, C, D), put_assoc(A, B, C, D)).
% transpile_avl_term(avl_incr(A, B, C, D), ).
transpile_avl_term(avl_delete(A, B, C, D), del_assoc(A, B, C, D)).
% transpile_avl_term(avl_del_min(A, B, C, D), del_min_assoc(A, B, C, D)). swi prolog: predicate will succeed, if given association list is empty. sicstus: the predicate fails
% transpile_avl_term(avl_del_max(A, B, C, D), del_max_assoc(A, B, C, D)). swi prolog: predicate will succeed, if given association list is empty. sicstus: the predicate fails
transpile_avl_term(avl_map(X, Y), map_assoc(X, Y)).
transpile_avl_term(avl_map(X, Y, Z), map_assoc(X, Y, Z)).
