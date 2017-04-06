-module(ex4).
-export([test/0]).

% Tests Frequency Server Assignment.

test() ->
  frequency:start(),
  timer:sleep(8000),
  io:format("allocated: ~w~n", [frequency:allocate()]),
  timer:sleep(8000),
  io:format("allocated: ~w~n", [frequency:allocate()]),
  timer:sleep(8000),
  io:format("allocated: ~w~n", [frequency:allocate()]),
  timer:sleep(8000),
  io:format("allocated: ~w~n", [frequency:allocate()]).

