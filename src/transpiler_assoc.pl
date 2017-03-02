/** <module> Transpilation of the Sicstus assoc library to SWI assoc library
*/
:- module(transpiler_assoc, [extension_module_assoc/1, transpile_assoc_term/2]).

%! extension_module_avl(-FileName) is det
%
% True, if FileName is a list containing the file name of the assoc_extension module.
extension_module_assoc(['assoc_extension.pl']).

%! transpile_avl_term(+Term, -TranspiledTerm) is semidet
%
% True if Term is a predicate of the assoc library and TranspiledTerm is the
% corresponding predicate of the assoc library in SWI prolog.
transpile_assoc_term((:-use_module(library(assoc))), (:-use_module(assoc_extension))).
