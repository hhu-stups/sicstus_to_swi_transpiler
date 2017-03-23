:- use_module(library(ordsets)).

test_ordsets_predicates(FinalSet) :-
	list_to_ord_set([1, 1, 3], Set1),
	is_ordset(Set1),
	ord_add_element(Set1, 4, Set2),
	ord_del_element(Set2, 1, Set3),
	ord_disjoint(Set3, [a, b]),
	ord_intersect(Set3, [3, 4, 5]),
	ord_intersection(Set3, [2, 3, 4, 5, 6, 7], _, Set4),
	ord_intersection(Set4, [6, 7, 9], Set5),
	ord_intersection([Set5, [3, 4, 5, 6, 7, 8, 9, 10]], Set6),
	ord_member(6, Set6),
	ord_nonmember(1, Set6),
	\+ ord_seteq(Set6, [1, 2, 3]),
	ord_setproduct([1, 2], [3, 4], [1-3, 1-4, 2-3, 2-4]),
	ord_subset(Set6, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ord_subtract([1, 2, 3, 4, 5, 6, 7], Set6, Set7),
	ord_symdiff(Set7, [10, 12], Set8),
	ord_disjoint_union([1, 2, 3], [4, 5], Set9),
	ord_union(Set8, Set9, Set10),
	ord_union(Set8, Set9, Set10, Set11),
	ord_union([Set10, Set11], FinalSet),
	ordset_order(FinalSet, Set11, >).

test_ord_member :-
	ord_member(b, [b, b, c, d]),
	\+ ord_member(a, [b, b, a]),
	\+ ord_member(c, [a, b, d]),
	ord_member(b, [a, b, c, d]),
	\+ ord_member(_, [a, b]).

test_ord_nonmember :-
	ord_nonmember(e, [b, b, c, d]),
	ord_nonmember(a, [b, b, a]),
	ord_nonmember(c, [a, b, d]),
	\+ ord_nonmember(c, [a, b, c, d]),
	ord_nonmember(_, [a, b]),
	ord_nonmember(b, []).
