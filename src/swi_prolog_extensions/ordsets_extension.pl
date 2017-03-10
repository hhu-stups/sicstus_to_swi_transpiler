/** <module> Extends the SWI ordsets library by functionality of the Sicstus ordsets library.
*/

:- module(ordsets_extension, [ord_disjoint_union/3, ord_nonmemberchk/2, ord_setproduct/3,
															ordset_order/3]).

%! ord_disjoint_union(+LeftSet, +RightSet, -Union) is semidet.
%
% True, when LeftSet and RightSet are disjoint and Union is the union of LeftSet
% and RightSet.
ord_disjoint_union(LeftSet, [], LeftSet) :- !.
ord_disjoint_union([], RightSet, RightSet) :- !.
ord_disjoint_union([LeftElement|LeftTail], [RightElement|RightTail], [LeftElement|Result]) :-
	LeftElement @< RightElement,
	ord_disjoint_union(LeftTail, [RightElement|RightTail], Result), !.
ord_disjoint_union([LeftElement|LeftTail], [RightElement|RightTail], [RightElement|Result]) :-
	LeftElement @> RightElement,
	ord_disjoint_union([LeftElement|LeftTail], RightTail, Result).

%! ord_nonmemberchk(+Element, +Set) is semidet
%
% True, when Element is a not in Set.
ord_nonmemberchk(_, []) :- !.
ord_nonmemberchk(Element1, [Element2|_]) :-
	Element1 @< Element2, !.
ord_nonmemberchk(Element1, [Element2|Tail]) :-
	Element1 \== Element2,
	ord_nonmemberchk(Element1, Tail).

%! ord_setproduct(+LeftSet, +RightSet, -Product) is det.
%
% Product is unified with a new set of pairs X-Y, where X is an element in LeftSet
% and Y is an element in RightSet.
ord_setproduct([], _, []) :- !.
ord_setproduct(_, [], []) :- !.
ord_setproduct(LeftSet, RightSet, Product) :-
	findall(LeftElement-RightElement, (member(LeftElement, LeftSet), member(RightElement, RightSet)), Product), !.

%! ordset_order(+LeftSet, +RightSet, -R)
%
% R will be unified with =, if LeftSet is equal to RightSet; R will be unified with <,
% if LeftSet is a subset of RightSet; R will be unified with >, if LeftSet is superset 
% of RightSet; otherwise the predicate fails.
ordset_order(LeftSet, RightSet, =) :-
	ord_seteq(LeftSet, RightSet), !.
ordset_order(LeftSet, RightSet, <) :-
	ord_subset(LeftSet, RightSet), !.
ordset_order(LeftSet, RightSet, >) :-
	ord_subset(RightSet, LeftSet), !.
