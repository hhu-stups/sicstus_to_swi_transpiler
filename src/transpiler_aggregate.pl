/** <module> Transpilation of the Sicstus aggregate library to SWI aggregate library and additional aggregate_extension module
*/

:- module(transpiler_aggregate, [extension_module_aggregate/1, transpile_aggregate_term/2]).

%! extension_module_aggregate(-FileName) is det
%
% True, if FileName is a list containing the file name of the aggregate_extension module.
extension_module_aggregate(['aggregate_extension.pl']).

%! transpile_aggregate_term(+Term, -TranspiledTerm) is semidet
%
% True if Term is a predicate of the aggregate library and TranspiledTerm is the
% corresponding predicate of the aggregate library in SWI prolog respectively the aggregate_extension module.
transpile_aggregate_term((:-use_module(library(aggregate))), (:-use_module(aggregate_extension))).
transpile_aggregate_term(term_variables(A, B, C), term_variables_ext(A, B, C)).
