
:-block block_test(-, ?).
block_test(A, A).

test_block :-
  block_test(A, B),
  B \== test,
  A = test,
  B == test.

test_if :-
  findall(R, if_test([a, b], R), [a, b]),
  findall(R, if_test(test, R), [none]),
  findall(R, if_test2([a, b], R), [a]).

if_test(List, Result) :-
  if(member(E, List), Result=E, Result=none).

if_test2(List, Result) :-
  if(member(E, List), (Result=E, !), Result=none).
