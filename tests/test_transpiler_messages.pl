:- module(test_transpiler_messages, [test_transpiler_messages/0]).

:- dynamic(test_warning/2).
:- dynamic(user:message_hook/3).

:- multifile(user:message_hook/3).

:- use_module(library(plunit)).
:- use_module("../src/transpiler_messages.pl").

user:message_hook(warning(_, Title, Notes), warning, _) :-
  asserta(test_warning(Title, Notes)).

test_transpiler_messages :-
  run_tests([warning]).

:-begin_tests(warning).

test(create_warning) :-
  current_output(Stream),
  stream_property(Stream, position(Pos)),
  stream_position_data(line_count, Pos, Line),
  set_current_line(Pos),
  create_warning("Test Title", "Note 1"),
  write(" "),
  stream_property(Stream, position(Pos2)),
  stream_position_data(line_count, Pos2, Line2),
  set_current_line(Pos2),
  create_warning("Test Title 2", "Note 2"),
  transpiler_messages:warning(1, Line, "Test Title", "Note 1"),
  transpiler_messages:warning(2, Line2, "Test Title 2", "Note 2").

test(write_warnings) :-
  write_messages,
  test_warning("Test Title", "Note 1"),
  test_warning("Test Title 2", "Note 2").
:-end_tests(warning).
