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
%%BEGIN-ASSIGNMENT
-export([test/0,startClient1/0,initClient/6]).
%%END-ASSIGNMENT

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      NewFrequencies = exited(Frequencies, Pid), 
      loop(NewFrequencies)
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
%%BEGIN-ASSIGNMENT
    io:format("~p pid ~p has exited~n", [self(), Pid]),
%%END-ASSIGNMENT
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

%%BEGIN-ASSIGNMENT
processCommand(Command) -> case Command of
  allocate -> io:format("~p allocated ~p~n", [self(), allocate()]);
  deallocate -> deallocate(10);
  sleep -> io:format("~p sleeping~n", [self()]), timer:sleep(10000)
end.

initClient(Id, C1, C2, C3, C4, C5) -> 
  io:format("~p starting client ~p~n", [self(), Id]),
  processCommand(C1),
  processCommand(C2),
  processCommand(C3),
  processCommand(C4),
  processCommand(C5),
  io:format("~p finished~n", [self()]).

startClient1() ->
  spawn(frequency, initClient, ["1", allocate, sleep, allocate, sleep, allocate]).

startClient2() ->
  spawn(frequency, initClient, ["2", sleep, sleep, sleep, sleep, allocate]).

test() ->
  io:format("start server~n"),
  start(),
  startClient1(),
  startClient2().
%%END-ASSIGNMENT


