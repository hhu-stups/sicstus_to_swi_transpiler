
% sc. Sicstus manual 4.14.3
expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
expr(Z) --> term(X), "-", expr(Y), {Z is X - Y}.
expr(X) --> term(X).
term(Z) --> number(X), "*", term(Y), {Z is X * Y}.
term(Z) --> number(X), "/", term(Y),  {Z is X / Y}.
term(Z) --> number(Z).
number(C) --> "+", number(C).
number(C) --> "-", number(X), {C is -X}.
number(X) --> [C], {"0"=<C, C=<"9", X is C - "0"}.

test_expr_grammar :-
  phrase(expr(54),  "-4+8+5*+2*5"),
  phrase(expr(14),  "-4+8+5*+2*5", "*5"),
  phrase(f, [aint], [not, g]).

test_do :-
  phrase(a, [test, test, test], [test]).


a --> fromto(0, In, Out, 2) do b(In, Out).

b(In, Out) --> [test], {Out is In+1}.

test_if :-
  phrase(c(1), [test, test, test], [test]),
  phrase(c(tdddt), [test, test], [test, test]).

c(Cond) --> if(b(0, Cond), [test], []).

test_once :-
  findall(R, phrase(d, [test, test, test], R), [[test]]).

d --> once(e).

e --> [test, test].
e --> [test].

f, [not, g] --> [aint].
