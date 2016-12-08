:- [transpiler].

:- use_module(sicstuscode).

test(X) :-
  get_empty(X),
  test_strings.

test_strings :-
  A = "test double quotes",
  write(A).

b --> "test", {X="test", write(X)}.
b --> "test", b.
