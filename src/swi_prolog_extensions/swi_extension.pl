/** <module> Extends SWI prolog by predicates, which are built-in predicates in Sicstus prolog.
*/

:- module(swi_extension, [op(500, yfx, \), op(1150, fx, mode), convert_arithmetic_expression/2, once/3, use_module/3]).

:- meta_predicate(once(:, ?, ?)).

%! convert_arithmetic_expression(+Expression, -ConvertedExpression) is det.
%
% Convert the arithmetic Expression of Sicstus prolog to an equivalent arithmetic ConvertedExpression of SWI prolog.
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

% sc. Sicstus manual 4.14.3; once for DCG
once(Goal, S0, S) :-
	phrase(Goal, S0, S1) -> S1=S.

/* use_module/3 is copied from the sicstus compatibility library of SWI-Prolog. License:
Copyright (c)  2010-2014, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

%% use_module(+Module, -File, +Imports) is det.
%% use_module(-Module, +File, +Imports) is det.
%
%	This predicate can be used to import   from a named module while
%	the file-location of the module is unknown   or to get access to
%	the module-name loaded from a file.
%
%	If both Module and File are  given,   we  use  Module and try to
%	unify File with the absolute  canonical   path  to the file from
%	which Module was loaded. However, we   succeed regardless of the
%	success of this unification.
use_module(Module, File, Imports) :-
	atom(Module), !,
	module_property(Module, file(Path)),
	load_files(Path, [imports(Imports), if(changed)]),
	ignore(File = Path).
use_module(Module, File, Imports) :-
	ground(File), !,
	absolute_file_name(File, Path, [file_type(prolog), access(read)]),
	load_files(Path, [imports(Imports), if(changed)]),
	module_property(Module, file(Path)).
use_module(Module, _, _) :-
	instantiation_error(Module).
