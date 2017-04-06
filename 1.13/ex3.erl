% Use "erl < ex3.erl" to run this in REPL.

% Create and register frequency server.
register(frequency, spawn(frequency, init, [])).
Pid = whereis(frequency).

% Ask frequency server for two frequencies, the second request should fail.
Pid ! {request, self(), allocate}.
Pid ! {request, self(), allocate}.
receive
  {reply, Reply1} -> io:format("got reply ~w~n", [Reply1])
  after 5000 -> "timeout"
end.
receive
  {reply, Reply2} -> io:format("got reply ~w~n", [Reply2])
  after 5000 -> "timeout"
end.
flush().

% Release frequency back to server, then a second frequency that was not allocated.
Pid ! {request, self(), {deallocate, 10}}.
Pid ! {request, self(), {deallocate, 99}}.
receive
  {reply, Reply3} -> io:format("got reply ~w~n", [Reply3])
  after 5000 -> "timeout"
end.
receive
  {reply, Reply4} -> io:format("got reply ~w~n", [Reply4])
  after 5000 -> "timeout"
end.
Pid ! {request, self(), stop}.

