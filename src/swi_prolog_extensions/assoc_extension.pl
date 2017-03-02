/** <module> Extends the SWI assoc library by functionality of the Sicstus AVL library.
*/

:- module(assoc_extension, [get_next_assoc/3, get_next_assoc/4, get_prev_assoc/3,
														get_prev_assoc/4, del_max_assoc_ext/4, del_min_assoc_ext/4,
														height_assoc/2, incr_assoc/4,  portray_assoc/1, size_assoc/2]).

%! del_max_assoc_ext(+Assoc, -Key, -Value, -NewAssoc) is semidet
%
% True if Key and Value is in Assoc and Key is the greatest key. NewAssoc is Assoc
% with removed node specified by Key. Fails when Assoc is empty.
del_max_assoc_ext(Assoc, _, _, _) :-
	empty_assoc(Assoc), !,
	fail.
del_max_assoc_ext(Assoc, Key, Value, NewAssoc) :-
	del_max_assoc(Assoc, Key, Value, NewAssoc).

%! del_max_assoc_ext(+Assoc, -Key, -Value, -NewAssoc) is semidet
%
% True if Key and Value is in Assoc and Key is the smallest key. NewAssoc is Assoc
% with removed node specified by Key. Fails when Assoc is empty.
del_min_assoc_ext(Assoc, _, _, _) :-
	empty_assoc(Assoc), !,
	fail.
del_min_assoc_ext(Assoc, Key, Value, NewAssoc) :-
	del_min_assoc(Assoc, Key, Value, NewAssoc).

%! get_next_assoc(+Key, +Assoc, -NextKey) is semidet
%
% True if PreviousKey is the next key to Key in Assoc.
get_next_assoc(Key, t(RootKey, _, _, t(LeftKey, _, _, _, t), _), NextKey) :-
	Key == LeftKey,
	NextKey = RootKey, !.
get_next_assoc(Key, t(RootKey, _, _, _, RightTree), NextKey) :-
	Key == RootKey,
	min_assoc(RightTree, NextKey, _), !.
get_next_assoc(Key, t(RootKey, _, _, LeftTree, _), NextKey) :-
	RootKey @> Key,
	get_next_assoc(Key, LeftTree, NextKey), !.
get_next_assoc(Key, t(RootKey, _, _, _, RightTree), NextKey) :-
	RootKey @< Key,
	get_next_assoc(Key, RightTree, NextKey), !.
get_next_assoc(Key, Assoc, NextKey) :-
	min_assoc(Assoc, NextKey, _),
	NextKey @> Key.

%! get_next_assoc(+Key, +Assoc, -NextKey, -Value) is semidet
%
% True if PreviousKey is the next key to Key in Assoc.
get_next_assoc(Key, t(RootKey, Value, _, t(LeftKey, _, _, _, t), _), NextKey, Value) :-
	Key == LeftKey,
	NextKey = RootKey, !.
get_next_assoc(Key, t(RootKey, _, _, _, RightTree), NextKey, Value) :-
	Key == RootKey,
	min_assoc(RightTree, NextKey, Value), !.
get_next_assoc(Key, t(RootKey, _, _, LeftTree, _), NextKey, Value) :-
	RootKey @> Key,
	get_next_assoc(Key, LeftTree, NextKey, Value), !.
get_next_assoc(Key, t(RootKey, _, _, _, RightTree), NextKey, Value) :-
	RootKey @< Key,
	get_next_assoc(Key, RightTree, NextKey, Value), !.
get_next_assoc(Key, Assoc, NextKey, Value) :-
	min_assoc(Assoc, NextKey, Value),
	NextKey @> Key.

%! get_prev_assoc(+Key, +Assoc, -PreviousKey) is semidet
%
% True if PreviousKey is the previous key to Key in Assoc.
get_prev_assoc(Key, t(RootKey, _, _, _, t(RightKey, _, _, t, _)), PreviousKey) :-
	Key == RightKey,
	PreviousKey = RootKey, !.
get_prev_assoc(Key, t(RootKey, _, _, LeftTree, _), PreviousKey) :-
	Key == RootKey,
	max_assoc(LeftTree, PreviousKey, _), !.
get_prev_assoc(Key, t(RootKey, _, _, LeftTree, _), PreviousKey) :-
	RootKey @> Key,
	get_prev_assoc(Key, LeftTree, PreviousKey), !.
get_prev_assoc(Key, t(RootKey, _, _, _, RightTree), PreviousKey) :-
	RootKey @< Key,
	get_prev_assoc(Key, RightTree, PreviousKey), !.
get_prev_assoc(Key, Assoc, PreviousKey) :-
	max_assoc(Assoc, PreviousKey, _),
	PreviousKey @< Key.

%! get_prev_assoc(+Key, +Assoc, -PreviousKey, -Value) is semidet
%
% True if PreviousKey is the previous key to Key in Assoc.
get_prev_assoc(Key, t(RootKey, Value, _, _, t(RightKey, _, _, t, _)), PreviousKey, Value) :-
	Key == RightKey,
	PreviousKey = RootKey, !.
get_prev_assoc(Key, t(RootKey, _, _, LeftTree, _), PreviousKey, Value) :-
	Key == RootKey,
	max_assoc(LeftTree, PreviousKey, Value), !.
get_prev_assoc(Key, t(RootKey, _, _, LeftTree, _), PreviousKey, Value) :-
	RootKey @> Key,
	get_prev_assoc(Key, LeftTree, PreviousKey, Value), !.
get_prev_assoc(Key, t(RootKey, _, _, _, RightTree), PreviousKey, Value) :-
	RootKey @< Key,
	get_prev_assoc(Key, RightTree, PreviousKey, Value), !.
get_prev_assoc(Key, Assoc, PreviousKey, Value) :-
	max_assoc(Assoc, PreviousKey, Value),
	PreviousKey @< Key.

%! height_assoc(+Assoc, -Height) is det
%
% True if Height is the number of nodes on the longest path in the AVL tree
% representing Assoc.
height_assoc(Assoc, _) :-
	var(Assoc), !,
	instantiation_error(Assoc).
height_assoc(Assoc, Height) :-
	height_assoc(Assoc, 0, Height).

height_assoc(t, Acc, Acc) :- !.
height_assoc(t(_, _, -, _, RightAssoc), Acc, Height) :-
	NewAcc is Acc + 1,
	height_assoc(RightAssoc, NewAcc, Height), !.
height_assoc(t(_, _, <, LeftAssoc, _), Acc, Height) :-
	NewAcc is Acc + 1,
	height_assoc(LeftAssoc, NewAcc, Height), !.
height_assoc(t(_, _, >, _, RightAssoc), Acc, Height) :-
	NewAcc is Acc + 1,
	height_assoc(RightAssoc, NewAcc, Height).

%! incr_assoc(+Key, +Assoc, +Increment, -NewAssoc) is det
%
% If Key is not in Assoc, NewAssoc is Assoc with added Key and Increment as value
% of the Key; otherwise changes the value of Key adding Increment to the value.
incr_assoc(Key, Assoc, Increment, NewAssoc) :-
	% Check Assoc avoiding get_assoc/3 throws an error.
	is_assoc(Assoc),
	get_assoc(Key, Assoc, Value),
	NewValue is Value + Increment,
	put_assoc(Key, Assoc, NewValue, NewAssoc), !.
incr_assoc(Key, Assoc, Increment, NewAssoc) :-
	put_assoc(Key, Assoc, Increment, NewAssoc).

list_nodes(t, []).
list_nodes(t(Key, Value, Balance, LeftTree, RightTree), Result) :-
	list_nodes(LeftTree, LeftNodes),
	append(LeftNodes, [t(Key, Value, Balance)], LeftNodes2),
	list_nodes(RightTree, RightNodes),
	append(LeftNodes2, RightNodes, Result).

%! portray_assoc(+Assoc) is det
%
% Assoc is written to the current output stream.
portray_assoc(t) :-
	write("Assoc{"),
	write("}"), !.
portray_assoc(t(Key, Value, Balance, LeftTree, RightTree)) :-
	write("Assoc{"),
	% Create a sorted list of all nodes ...
	list_nodes(t(Key, Value, Balance, LeftTree, RightTree), Nodes),
	% ... and write each member of the list to the output stream.
	portray_nodes(Nodes),
	write("}").

portray_nodes([t(Key, Value, -)|[]]) :-
	write(Key),
	write("->"),
	write(Value), !.
portray_nodes([t(Key, Value, _)|[]]) :-
	write(Key),
	write("->*"),
	write(Value).
portray_nodes([t(Key, Value, -)|Nodes]) :-
	write(Key),
	write("->"),
	write(Value),
	write(","),
	portray_nodes(Nodes), !.
portray_nodes([t(Key, Value, _)|Nodes]) :-
	write(Key),
	write("->*"),
	write(Value),
	write(","),
	portray_nodes(Nodes).

%! size_assoc(?Assoc, -Size) is nondet
%
% True if Size is the number of keys in Assoc.
size_assoc(t, 0) :- !.
size_assoc(t(_, _, _, LeftAssoc, RightAssoc), Size) :-
	size_assoc(RightAssoc, RightSize),
 	size_assoc(LeftAssoc, LeftSize),
	Size is RightSize + LeftSize + 1.
