-module(router).
-export([init/0,start/0,allocate/0,deallocate/1,stop/0]).

start() ->
  register(frequency1, spawn(frequency, init, [[20,21,22,23,24,25]])),
  register(frequency2, spawn(frequency, init, [[30,31,32]])),
  register(router, spawn(router, init, [])).

init() ->
  loop12(frequency1, frequency2).

loop12(Server1, Server2) ->
  receive
    {request, Pid, allocate} ->
      Server1 ! {request, self(), allocate},
      receive
        {reply, {error, no_frequency}} ->
          % First server does not have any frequencies, try other server
          Server2 ! {request, self(), allocate},
          receive
            {reply, Reply} -> Pid ! {reply, Reply}
          end;
        {reply, Reply} -> Pid ! {reply, Reply}
      end,
      % Use other server for handling next request
      loop12(Server2, Server1);
    {request, Pid , {deallocate, Freq}} ->
      % Servers accept deallocate of any frequency, so deallocate
      % to first server, as simplest solution
      Server1 ! {request, self(), {deallocate, Freq}},
      receive
        {reply, Reply} -> Pid ! {reply, Reply}
      end,
      % Use other server for handling next request
      loop12(Server2, Server1);
    {request, Pid, stop} ->
      % Stop both servers
      Server1 ! {request, self(), stop},
      Server2 ! {request, self(), stop},
      receive
        {reply, Reply} ->
          receive
            {reply, Reply} -> Pid ! {reply, Reply}
          end
      end
  end.

allocate() ->
  router ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  end.

deallocate(Freq) ->
  router ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Reply} -> Reply
  end.

stop() ->
  router ! {request, self(), stop},
  receive
    {reply, Reply} -> Reply
  end.
  
