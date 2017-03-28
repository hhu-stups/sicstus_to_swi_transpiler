/** <module> Extends SWI prolog by predicates, which are built-in predicates in Sicstus prolog.
*/

:- module(swi_extension, [op(500, yfx, \), convert_arithmetic_expression/2]).

convert_arithmetic_expression(Expression, Expression) :-
  (var(Expression); atomic(Expression)), !.
convert_arithmetic_expression(integer(Expression), truncate(ConvertedExpression)) :-
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(\(LeftExpression, RightExpression),
                              xor(ConvertedLeftExpression, ConvertedRightExpression)) :-
  convert_arithmetic_expression(LeftExpression, ConvertedLeftExpression),
  convert_arithmetic_expression(RightExpression, ConvertedRightExpression), !.
convert_arithmetic_expression(round(Expression), floor(ConvertedExpression+1/2)) :-
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(cot(Expression), (cos(ConvertedExpression)/sin(ConvertedExpression))) :-
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(coth(Expression), (cosh(ConvertedExpression)/sinh(ConvertedExpression))) :-
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(acot(Expression), ((pi/2)-atan(ConvertedExpression))) :-
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(acoth(Expression),
                              (1/2)*log((ConvertedExpression+1)/(ConvertedExpression-1))) :-
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(log(BaseExpression, Expression),
                              log(ConvertedExpression)/log(ConvertedBaseExpression)) :-
  convert_arithmetic_expression(BaseExpression, ConvertedBaseExpression),
  convert_arithmetic_expression(Expression, ConvertedExpression), !.
convert_arithmetic_expression(exp(LeftExpression, RightExpression),
                              **(ConvertedLeftExpression, ConvertedRightExpression)) :-
  convert_arithmetic_expression(LeftExpression, ConvertedLeftExpression),
  convert_arithmetic_expression(RightExpression, ConvertedRightExpression), !.
convert_arithmetic_expression(Expression, ConvertedExpression) :-
  Expression =.. [Functor|Args],
  findall(NewArg, (member(Arg, Args), convert_arithmetic_expression(Arg, NewArg)), NewArgs),
  ConvertedExpression =.. [Functor|NewArgs].
