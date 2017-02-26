/** <module> Extends SWI prolog by do loops
*/

:- module(do_loop_extension, [op(1100, xfy, do), do/2]).

:- meta_predicate do(?, :).

do(foreach(Item, List), Body) :-
	(asserta(do_1([]):-!),
	assertz((do_1([Item|T]):-
		Body,
		do_1(T))),
	call(do_1(List)),
	retractall(do_1(_)), !);
	retractall(do_1(_)),
	fail.
