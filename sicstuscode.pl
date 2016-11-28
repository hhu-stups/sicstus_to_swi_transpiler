:- module(sicstuscode, [get_empty/1]).

:- use_module(library(avl)).

get_empty(AVL) :-
    empty_avl(AVL).
