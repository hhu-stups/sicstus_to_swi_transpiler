/** <module> Extends SWI prolog by do loops
*/

:- module(do_loop_extension, [op(1100, xfy, do), do/2, do/4]).

:- dynamic(aux_assoc/1).

:- meta_predicate do(?, :). % necessary to support user-definied goals in Body
:- meta_predicate do(?, :, ?, ?).

% Implementation follows 4.2.3.4 Sicstus user's manual.

construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name) :-
	aux_assoc(Assoc),
	% lookup, if predicate already exists, then use it.
	variant_sha1((BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body), Hash),
	get_assoc(Hash, Assoc, Name), !.
construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name) :-
	aux_assoc(Assoc),
	% generate a new name for the new auxiliary predicate
	assoc_to_keys(Assoc, List),
	length(List, Count),
	NewCount is Count+1,
	atom_concat(do_, NewCount, Name),
	% construct auxiliary predicate
	BaseTerm =.. [Name|BaseArgs],
	asserta(BaseTerm:-!),
	HeadTerm =.. [Name|HeadArgs],
	RecTerm =.. [Name|RecArgs],
	assertz((HeadTerm :- PreBodyGoals, Body, RecTerm)),
	% update assoc
	retract(aux_assoc(Assoc)),
	variant_sha1((BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body), Hash),
	put_assoc(Hash, Assoc, Name, NewAssoc),
	asserta(aux_assoc(NewAssoc)), !.
construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name) :-
	empty_assoc(Assoc),
	asserta(aux_assoc(Assoc)),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name).

%! do(+IterationSpecifier, :Body) is nondet
%
% Predicate for the do operator.
% IterationSpecifier determine how Body is iterated.
% The following specifiers are supported:
%
% * count(Index, Min, Max)
% Iterate over ascending integers from Min up to Max. Max can be uninstantiated. Index is unified with actual value of the iteration.
% * foreach(Item, List)
% Iterate over List. Item is a local variable unified with the item of the actual iteration.
% * foreacharg(Term, List)
% Iterate over all arguments of Term. Item is a local variable unified with the item of the actual iteration.
% * foreacharg(Term, List, Index)
% Iterate over all arguments of Term. Item is a local variable unified with the item of the actual iteration.
% Index is the actual argument number of Item.
% * for(Index, MinExpr, MaxExpr)
% Iterate over the intervall [MinExpr, MaxExpr] with step 1. Index is unified with actual value of the iteration.
% * fromto(First, In, Out, Last)
% At beginning In = First. In further iterations In is unified with Out from the last iteration.
% The iteration will end if Out=Last.
% * param(Variable)
% Declares Variable as a global one. Other variables are local by default.
do(Variable, _) :-
	var(Variable),
	instantiation_error(Variable).
do((LeftSpecifier, RightSpecifier), Body) :- !, % cut necessary to avoiding last clause throws type error.
	get_call_args(LeftSpecifier, LeftCallArgs),
	get_call_args(RightSpecifier, RightCallArgs),
	append(LeftCallArgs, RightCallArgs, CallArgs),
	% construct auxiliary predicate
	get_base_args(LeftSpecifier, LeftBaseArgs),
	get_base_args(RightSpecifier, RightBaseArgs),
	append(LeftBaseArgs, RightBaseArgs, BaseArgs),
	get_other_args(LeftSpecifier, LeftHeadArgs, LeftPreBodyGoals, LeftRecArgs),
	get_other_args(RightSpecifier, RightHeadArgs, RightPreBodyGoals, RightRecArgs),
	append(LeftHeadArgs, RightHeadArgs, HeadArgs),
	append(LeftRecArgs, RightRecArgs, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, (LeftPreBodyGoals, RightPreBodyGoals), RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal).
do(count(Index, Min, Max), Body) :- !,
	get_call_args(count(Index, Min, Max), CallArgs),
	% construct auxiliary predicate
	get_base_args(count(Index, Min, Max), BaseArgs),
	get_other_args(count(Index, Min, Max), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal), !. % cut necessary to be compatible with Sicstus
do(for(Index, Min, Max), Body) :- !,
	get_call_args(for(Index, Min, Max), CallArgs),
	% construct auxiliary predicate
	get_base_args(for(Index, Min, Max), BaseArgs),
	get_other_args(for(Index, Min, Max), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal), !. % cut necessary to be compatible with Sicstus
do(foreach(Item, List), Body) :- !,
	get_call_args(foreach(Item, List), CallArgs),
	% construct auxiliary predicate
	get_base_args(foreach(Item, List), BaseArgs),
	get_other_args(foreach(Item, List), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal), !. % cut necessary to be compatible with Sicstus
do(foreacharg(Item, Term), Body) :- !,
	get_call_args(foreacharg(Item, Term), CallArgs),
	% construct auxiliary predicate
	get_base_args(foreacharg(Item, Term), BaseArgs),
	get_other_args(foreacharg(Item, Term), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal), !. % cut necessary to be compatible with Sicstus
do(foreacharg(Item, Term, Index), Body) :- !,
	get_call_args(foreacharg(Item, Term, Index), CallArgs),
	% construct auxiliary predicate
	get_base_args(foreacharg(Item, Term, Index), BaseArgs),
	get_other_args(foreacharg(Item, Term, Index), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal), !. % cut necessary to be compatible with Sicstus
do(fromto(First, In, Out, Last), Body) :- !,
	get_call_args(fromto(First, In, Out, Last), CallArgs),
	% construct auxiliary predicate
	get_base_args(fromto(First, In, Out, Last), BaseArgs),
	get_other_args(fromto(First, In, Out, Last), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal).
do(param(P), Body) :- !,
	get_call_args(param(P), CallArgs),
	% construct auxiliary predicate
	get_base_args(param(P), BaseArgs),
	get_other_args(param(P), HeadArgs, PreBodyGoals, RecArgs),
	construct_auxiliary_predicate(BaseArgs, HeadArgs, PreBodyGoals, RecArgs, Body, Name),
	% call Predicate
	CallGoal =.. [Name|CallArgs],
	call(CallGoal).
do(Term, _) :-
	type_error(iterator, Term).

% sc. Sicstus manual 4.14.3; do for DCGs
do(Iterator, Body, S0, S) :-
	do((Iterator, fromto(S0, S1, S2, S)), phrase(Body, S1, S2)).

get_base_args(count(_, _, _), [L0, L0]).
get_base_args(for(_, _, _), [L0, L0]).
get_base_args(foreach(_, _), [[]]).
get_base_args(foreacharg(_, _), [_, I0, I0]).
get_base_args(foreacharg(_, _, _), [_, I0, I0]).
get_base_args(fromto(_, _, _, _), [L0, L0]).
get_base_args(param(P), [P]).
get_base_args((LeftSpecifier, RightSpecifier), BaseArgs) :-
	get_base_args(LeftSpecifier, LeftBaseArgs),
	get_base_args(RightSpecifier, RightBaseArgs),
	append(LeftBaseArgs, RightBaseArgs, BaseArgs).

get_call_args(count(_, FE, T), [F, T]) :-
	F is round(float_integer_part(FE))-1.
get_call_args(for(_, FE, TE), [F, S]) :-
	F is round(float_integer_part(FE)),
	S is max(F, round(float_integer_part(TE))+1).
get_call_args(foreach(_, List), [List]).
get_call_args(foreacharg(_, Term), [Term, 1, N1]) :-
	functor(Term, _, N),
	N1 is N+1.
get_call_args(foreacharg(_, Term, _), [Term, 1, N1]) :-
	functor(Term, _, N),
	N1 is N+1.
get_call_args(fromto(F, _, _, T), [F, T]).
get_call_args(param(P), [P]).
get_call_args((LeftSpecifier, RightSpecifier), CallArgs) :-
	get_call_args(LeftSpecifier, LeftCallArgs),
	get_call_args(RightSpecifier, RightCallArgs),
	append(LeftCallArgs, RightCallArgs, CallArgs).

get_other_args(count(I, _, _), [I0, L1], (I is I0+1), [I, L1]).
get_other_args(for(I, _, _),  [I, L1], (I1 is I+1), [I1, L1]).
get_other_args(foreach(X, _), [[X|T]], true, [T]).
get_other_args(foreacharg(Item, Term), [Term, I0, I2], (I1 is I0+1, arg(I0, Term, Item)), [Term, I1, I2]).
get_other_args(foreacharg(Item, Term, Index), [Term, Index, I2], (I1 is Index+1, arg(Index, Term, Item)), [Term, I1, I2]).
get_other_args(fromto(_, I0, I1, _), [I0, L1], true, [I1, L1]).
get_other_args(param(P), [P], true, [P]).
get_other_args((LeftSpecifier, RightSpecifier), HeadArgs, (LeftPreBodyGoals, RightPreBodyGoals), RecArgs) :-
	get_other_args(LeftSpecifier, LeftHeadArgs, LeftPreBodyGoals, LeftRecArgs),
	get_other_args(RightSpecifier, RightHeadArgs, RightPreBodyGoals, RightRecArgs),
	append(LeftHeadArgs, RightHeadArgs, HeadArgs),
	append(LeftRecArgs, RightRecArgs, RecArgs).
