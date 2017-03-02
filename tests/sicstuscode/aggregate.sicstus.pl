:- use_module(library(aggregate)).

get_variables(Term, List) :-
	term_variables(Term, [], List).

list_of_integers(List) :-
	forall(member(Item, List), integer(Item)).
