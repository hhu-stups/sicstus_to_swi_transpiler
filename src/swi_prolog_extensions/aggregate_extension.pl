/** <module> Extends the SWI aggregate library by term_variables_ext/3 with the semantics of term_variables/3 in Sicstus.
*/

:- module(aggregate_extension, [term_variables_ext/3]).

do_1(_, A, A, B, B).
do_1(Term, I0, I2, I3, L1) :-
	I1 is I0+1,
	arg(I0, Term, Arg),
	term_variables_ext(Arg, I3, I4),
	do_1(Term, I1, I2, I4, L1).

do_2([], _).
do_2([Item|Tail], Term) :-
	Item \== Term,
	do_2(Tail, Term).

%! term_variables_ext(+Term, +VarList, -ResultVarList) is det
%
% ResultVarList is unified with a union of the variables of Term and VarList.
term_variables_ext(Term, VarList, ResultVarList) :-
	% Implementation follows the proposal of the Sicstus user's manual.
	nonvar(Term), !,
	(	functor(Term, _, Arity),
		N is Arity+1,
		do_1(Term, 1, N, VarList, ResultVarList)
	).
term_variables_ext(Term, VarList, ResultVarList) :-
	do_2(VarList, Term), !,
  ResultVarList = [Term|VarList].
term_variables_ext(_, ResultVarList, ResultVarList).
