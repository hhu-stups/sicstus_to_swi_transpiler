:-module(hooks_tests,  [test/1, test_goal_expansion/0]).


hooks_tests:goal_expansion(test(A), Layout, _, test(2), Layout) :-
  A =\= 2.

test(A):-A is 2.

test_goal_expansion :-
  test(4).
