%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
%%BEGIN-ASSIGNMENT
  tryCatchLoop(Frequencies).

tryCatchLoop(Frequencies) ->
  try
    loop(Frequencies)
  catch
    throw:unknown_message ->
      io:format("~p server ignoring unknown_message~n", [self()]),
      tryCatchLoop(Frequencies)
  end.
%%END-ASSIGNMENT

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
%%BEGIN-ASSIGNMENT
      tryCatchLoop(NewFrequencies);
%%END-ASSIGNMENT
    {request, Pid , {deallocate, Freq}} ->
%%BEGIN-ASSIGNMENT
      try
        NewFrequencies = deallocate(Frequencies, Freq),
        Pid ! {reply, ok},
        tryCatchLoop(NewFrequencies)
      catch
        throw:not_allocated ->
          Pid ! {reply, not_allocated},
          tryCatchLoop(Frequencies)
      end;
%%END-ASSIGNMENT
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      NewFrequencies = exited(Frequencies, Pid), 
      tryCatchLoop(NewFrequencies);
%%BEGIN-ASSIGNMENT
    _ -> throw(unknown_message)
%%END-ASSIGNMENT
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply;
%%BEGIN-ASSIGNMENT
        _ -> throw(unknown_message)
%%END-ASSIGNMENT
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply;
%%BEGIN-ASSIGNMENT
        _ -> throw(unknown_message)
%%END-ASSIGNMENT
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply;
%%BEGIN-ASSIGNMENT
        _ -> throw(unknown_message)
%%END-ASSIGNMENT
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
%%BEGIN-ASSIGNMENT
  case lists:keysearch(Freq,1,Allocated) of
    false -> throw(not_allocated);
%  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
    {value,{Freq,Pid}} ->
      unlink(Pid),                                             %%% ADDED
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free],  NewAllocated}
  end.
%%END-ASSIGNMENT

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

