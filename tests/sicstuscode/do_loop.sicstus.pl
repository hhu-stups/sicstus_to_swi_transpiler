next_integer(List, NewList) :-
  (foreach(Item, List), foreach(NewItem, NewList) do NewItem is Item+1).

sum_list(List, Result) :-
  (foreach(Item, List), fromto(0, In, Out, Result) do Out is In+Item).

integer_list_between(Min, Max, Result) :-
  (for(Index, Min, Max), foreach(Index, Result) do true).

count_items(List, Count) :-
  (foreach(_, List), count(_, 1, Count) do true).

count_arguments(Term, Count) :-
  (foreacharg(_, Term), count(_, 1, Count) do true).

number_arguments(Term, Result) :-
  (foreacharg(Argument, Term, Index), foreach((Index, Argument), Result)  do true).

add_to_items(List, Summand, Result) :-
  (foreach(Item, List), foreach(NewItem, Result), param([Summand]) do NewItem is Item+Summand).
