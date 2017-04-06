-module(ex2).
-export([receiver/0, receiver2/0]).

% Simple server to check how ordering of messages can be controlled.

% Usage in REPL:
% Self = self().
% Pid = spawn(ex2, receiver, []).
% Pid ! {2, "hello"}.
% Pid ! {3, "qw"}.
% Pid ! {1, "foo"}.
% Pid ! stop.
% flush().

receiver() ->
  timer:sleep(5000),
  receive
    {1, Message} ->
      io:format("Priority 1 ~s~n", [Message]),
      receiver();
    {2, Message} ->
      io:format("Priority 2 ~s~n", [Message]),
      receiver();
    stop ->
      io:format("stopping~n")
  end.

% Ensure that message with Priority 1 is handled first.

% Usage in REPL:
% Self = self().
% Pid = spawn(ex2, receiver2, []).
% Pid ! {2, "hello"}.
% Pid ! {3, "qw"}.
% Pid ! {1, "foo"}.
% flush().

receiver2() ->
  receive
    {1, Message} ->
      io:format("Priority 1 ~s~n", [Message]),
      receiver3()
  end.

receiver3() ->
  receive
    {2, Message} ->
      io:format("Priority 2 ~s~n", [Message]),
      io:format("stopping~n")
  end.

