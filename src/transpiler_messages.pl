/** <module> The modules handles warnings and error with respect to the transpilation process.
*/
:-module(transpiler_messages, [create_warning/2, set_current_line/1, write_messages/0]).

:- dynamic(current_line/1).
:- dynamic(warning/4).

:- multifile(prolog:message//1).

%! create_warning(+Title, +Notes) is det.
%
% Creates a new warning with title Title and further notes Notes. The warning is
% stored and can be displayed by calling write_messages/0.
create_warning(Title, Notes) :-
  current_line(Line),
  retract(current_index(Index)),
  assertz(warning(Index, Line, Title, Notes)),
  NewIndex is Index+1,
  asserta(current_index(NewIndex)), !.
create_warning(Title, Notes) :-
  current_line(Line),
  assertz(warning(1, Line, Title, Notes)),
  asserta(current_index(2)).

prolog:message(warning(Line, Title, Notes)) -->
  ['Predicate on line ~w: ~w'-[Line, Title], nl,
  '~w'-[Notes], nl].

% set_current_line(+StreamPosition) is det.
%
% Sets the a new line number given by StreamPosition. This line number is associated to all new
% warning or errors until a new line number is set.
set_current_line(StreamPosition) :-
  stream_position_data(line_count, StreamPosition, Line),
  retract(current_line(_)),
  asserta(current_line(Line)), !.
set_current_line(StreamPosition) :-
  stream_position_data(line_count, StreamPosition, Line),
  asserta(current_line(Line)).

% write_messages is det.
%
% Print all messages (warnings and errors) and delete the printed ones.
write_messages :-
  write_messages(1).

write_messages(Index) :-
  warning(Index, Line, Title, Notes),
  print_message(warning, warning(Line, Title, Notes)),
  NewIndex is Index+1,
  write_messages(NewIndex), !.
write_messages(_) :-
  retractall(warning(_, _, _, _)).
