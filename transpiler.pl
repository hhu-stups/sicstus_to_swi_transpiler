transpiler(InFilePath, OutFilePath) :-
  parse_file(InFilePath, AST),
  translation(AST, NAST),
  write_ast_to_file(NAST, OutFilePath).

parse_file(InFilePath, AST) :-
  phrase_from_file(parse(AST), InFilePath).

translation(file(Sentences), file(NSentences)) :-
  translate_sentences(Sentences, NSentences).

write_ast_to_file(file(Sentences), OutFilePath) :-
  open(OutFilePath, write, Stream),
  write_sentences(Stream, Sentences), !,
  close(Stream).

%translation
translate_sentences([], []).
translate_sentences([Sentence|Tail], [NSentence|NTail]) :-
  translate_sentence(Sentence, NSentence),
  translate_sentences(Tail, NTail).

translate_sentence(dir(cterm("use_module", [cterm("library", [atom("avl")])])),
  dir(cterm("use_module", [cterm("library", [atom("assoc")])]))) :- !.
translate_sentence(rule(Term, Body), rule(Term, NBody)) :-
  translate_body(Body, NBody), !.
translate_sentence(grrule(Term, Body), grrule(Term, NBody)) :-
  translate_grbody(Body, NBody), !.
translate_sentence(Sentence, Sentence).

translate_body([], []).
translate_body([cterm("empty_avl", [Argument])|Tail], [cterm("empty_assoc", [Argument])|NTail]) :-
  translate_body(Tail, NTail), !.
translate_body([oper(Right, Op, str(String))|Tail], [oper(Right, Op, codes(String))|NTail]) :-
    translate_body(Tail, NTail), !.
translate_body([A|Tail], [A|NTail]) :-
  translate_body(Tail, NTail).

translate_grbody([], []).
translate_grbody([str(String)|Tail], [codes(String)|NTail]) :-
  translate_grbody(Tail, NTail), !.
translate_grbody([cond(Body)|Tail], [cond(NBody)|NTail]) :-
  translate_body(Body, NBody),
  translate_grbody(Tail, NTail), !.
translate_grbody([A|Tail], [A|NTail]) :-
  translate_grbody(Tail, NTail).

%write output.
write_sentences(_, []).
write_sentences(Stream, [Sentence|Tail]) :-
  write_dir(Stream, Sentence),
  write_sentences(Stream, Tail).
write_sentences(Stream, [Sentence|Tail]) :-
  write_fact(Stream, Sentence),
  write_sentences(Stream, Tail).
write_sentences(Stream, [Sentence|Tail]) :-
  write_grrule(Stream, Sentence),
  write_sentences(Stream, Tail).
write_sentences(Stream, [Sentence|Tail]) :-
  write_rule(Stream, Sentence),
  write_sentences(Stream, Tail).

write_dir(Stream, dir(Term)) :-
  write(Stream, ":- "),
  write_term2(Stream, Term),
  writeln(Stream, "."),
  nl(Stream).

write_fact(Stream, fact(Term)) :-
  write_term2(Stream, Term),
  writeln(Stream, "."),
  nl(Stream).

write_grrule(Stream, grrule(Term, Body)) :-
  write_term2(Stream, Term),
  write(Stream, " --> "),
  write_grbody(Stream, Body),
  writeln(Stream, "."),
  nl(Stream).

write_rule(Stream, rule(Term, Body)) :-
  write_term2(Stream, Term),
  writeln(Stream, " :- "),
  write_body(Stream, Body),
  writeln(Stream, "."),
  nl(Stream).

write_term2(Stream, cterm(Functor, Arguments)) :-
  write(Stream, Functor),
  write(Stream, "("),
  write_arguments(Stream, Arguments),
  write(Stream, ")").
write_term2(Stream, atom(Atom)) :-
  write(Stream, Atom).

write_grbody(Stream, [cond(Condition)|[]]) :-
  write(Stream, "{"),
  write_body(Stream, Condition),
  write(Stream, "}").
write_grbody(Stream, [String|[]]) :-
  write_string(Stream, String).
write_grbody(Stream, [Term|[]]) :-
  write_term2(Stream, Term).
write_grbody(Stream, [cond(Condition)|Tail]) :-
  Tail \= [],
  write(Stream, "{"),
  write_body(Stream, Condition),
  write(Stream, "}, "),
  write_body(Stream, Tail).
write_grbody(Stream, [String|Tail]) :-
  Tail \= [],
  write_string(Stream, String),
  write(Stream, ", "),
  write_grbody(Stream, Tail).
write_grbody(Stream, [Term|Tail]) :-
  Tail \= [],
  write_term2(Stream, Term),
  write(Stream, ", "),
  write_grbody(Stream, Tail).

write_body(Stream, [Operator|[]]) :-
  write_operator(Stream, Operator).
write_body(Stream, [Term|[]]) :-
  write_term2(Stream, Term).
write_body(Stream, [Operator|Tail]) :-
  write_operator(Stream, Operator),
  writeln(Stream, ", "),
  write_body(Stream, Tail).
write_body(Stream, [Term|Tail]) :-
  write_term2(Stream, Term),
  writeln(Stream, ", "),
  write_body(Stream, Tail).

write_arguments(Stream, [list(List)|[]]) :-
  write(Stream, "["),
  write_arguments(Stream, List),
  write(Stream, "]").
write_arguments(Stream, [Term|[]]) :-
  write_term2(Stream, Term).
write_arguments(Stream, [String|[]]) :-
  write_string(Stream, String).
write_arguments(Stream, [variable(Name)|[]]) :-
  write(Stream, Name).
write_arguments(Stream, [list(List)|Tail]) :-
  write(Stream, "["),
  write_arguments(Stream, List),
  write(Stream, "], "),
  write_arguments(Stream, Tail).
write_arguments(Stream, [Term|Tail]) :-
  write_term2(Stream, Term),
  write(Stream, ", "),
  write_arguments(Stream, Tail).
write_arguments(Stream, [String|Tail]) :-
  write_string(Stream, String),
  write(Stream, ", "),
  write_arguments(Stream, Tail).
write_arguments(Stream, [variable(Name)|Tail]) :-
  write(Stream, Name),
  write(Stream, ", "),
  write_arguments(Stream, Tail).

write_string(Stream, str(String)) :-
  write(Stream, "\""),
  write(Stream, String),
  write(Stream, "\"").
write_string(Stream, codes(String)) :-
  write(Stream, "`"),
  write(Stream, String),
  write(Stream, "`").

write_operator(Stream, oper(Left, Op, Right)) :-
  write_term2(Stream, Left),
  write(Stream, " "),
  write(Stream, Op),
  write(Stream, " "),
  write_term2(Stream, Right).
write_operator(Stream, oper(variable(Name), Op, Right)) :-
  Right \= str(_),
  Right \= codes(_),
  write(Stream, Name),
  write(Stream, " "),
  write(Stream, Op),
  write(Stream, " "),
  write_term2(Stream, Right).
write_operator(Stream, oper(variable(Name), Op, Right)) :-
  write(Stream, Name),
  write(Stream, " "),
  write(Stream, Op),
  write(Stream, " "),
  write_string(Stream, Right).

% GRAMMAR
parse(file([Sentence|Sentences])) -->  optws, sentence(Sentence), ws, sentences(Sentences), optws, !.
parse(file([Sentence])) -->  optws, sentence(Sentence), optws, !.

sentences([Sentence|Sentences]) --> sentence(Sentence), ws, sentences(Sentences).
sentences([Sentence]) --> sentence(Sentence).

sentence(rule(Head, Body)) --> term(Head), optws, `:-`, optws, body(Body), optws, `.`.
sentence(grrule(Head, Body)) --> term(Head), optws, `-->`, optws, grbody(Body), optws, `.`.
sentence(dir(Term)) --> `:-`, optws, term(Term), optws, `.`.
sentence(fact(Term)) --> term(Term), `.`.

term(cterm(Functor, Arguments)) --> name(Functor), `(`, args(Arguments), `)`.
term(atom(Atom)) --> name(Atom).

body([Operator|Tail]) --> oper(Operator), optws, `,`, optws, body(Tail).
body([Term|Tail]) --> term(Term), optws, `,`, optws, body(Tail).
body([Operator]) --> oper(Operator).
body([Term]) --> term(Term).

grbody([cond(Condition)|Tail]) --> `{`, optws, body(Condition), optws, `}`, optws, grbody(Tail).
grbody([String|Tail]) --> str(String), optws, `,`, optws, grbody(Tail).
grbody([Term|Tail]) --> term(Term), optws, `,`, optws, grbody(Tail).
grbody([cond(Condition)]) --> `{`, optws, body(Condition), optws, `}`.
grbody([String]) --> str(String).
grbody([Term]) --> term(Term).

name(String) --> [Char], {Char > 96, Char < 123}, chars(Tail), {string_codes(String, [Char|Tail])}.
name(String) --> [Char], {Char > 96, Char < 123}, {string_codes(String, [Char])}.

chars([Char|Tail]) -->
  [Char],
  {(Char > 46, Char < 58); (Char > 64, Char < 91); (Char > 96, Char < 123); Char = 95},
  chars(Tail).
chars([Char]) -->
  [Char],
  {(Char > 46, Char < 58); (Char > 64, Char < 91); (Char > 96, Char < 123); Char = 95}.

allchars([Char|Tail]) -->
  [Char],
  {(Char \= 34)},
  allchars(Tail).
allchars([Char]) -->
  [Char],
  {Char \= 34}.

args([List|Tail]) --> list(List), optws, `,`, optws, args(Tail).
args([Term|Tail]) --> term(Term), optws, `,`, optws, args(Tail).
args([String|Tail]) --> str(String), optws, `,`, optws, args(Tail).
args([Variable|Tail]) --> variable(Variable), optws, `,`, optws, args(Tail).
args([List]) --> list(List).
args([Term]) --> term(Term).
args([String]) --> str(String).
args([Variable]) --> variable(Variable).

oper(oper(Left, "=", Right)) --> term(Left), optws, `=`, optws, term(Right).
oper(oper(Left, "=", Right)) --> variable(Left), optws, `=`, optws, term(Right).
oper(oper(Left, "=", Right)) --> variable(Left), optws, `=`, optws, str(Right).

str(str(String)) --> `"`, allchars(Codes), `"`, {string_codes(String, Codes)}.

list(list(List)) --> `[`, optws, args(List), optws, `]`.

variable(variable(String)) --> [Char], {(Char > 64, Char < 91); Char=95},
  chars(Tail),
  {string_codes(String, [Char|Tail])}.
variable(variable(String)) --> [Char],
  {(Char > 64, Char < 91); Char=95}, {string_codes(String, [Char])}.

% whitespace
ws --> [13], optws.
ws --> [10], optws.
ws --> [9], optws.
ws --> ` `, optws.

% optional whitespace
optws --> [13], optws.
optws --> [10], optws.
optws --> [9], optws.
optws --> ` `, optws.
optws --> ``.
