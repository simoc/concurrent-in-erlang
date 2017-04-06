-module(ex1).
-export([server/1]).

% Simplest possible server checks whether messages are palindromes
% and sends a reply to parent process.

% Usage in REPL:
% Self = self().
% Pid = spawn(ex1, server, [Self]).
% Pid ! {check, "banana"}.
% Pid ! {check, "abba"}.
% Pid ! stop.
% flush().

server(PID) ->
  receive
    {check, S} ->
      case palin:palindrome(S) of
        true -> PID ! {result, S ++ " is a palindrome"};
        false -> PID ! {result, S ++ " is not a palindrome"}
      end,
      server(PID);
    stop ->
      io:format("stopping~n");
    _ ->
      server(PID)
  end.


