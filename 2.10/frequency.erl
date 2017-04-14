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
-export([startSupervisor/0,initSupervisor/0,initClient/0,test/0]).
%%END-ASSIGNMENT

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
%%BEGIN-ASSIGNMENT
  io:format("~p server started~n", [self()]),
%%END-ASSIGNMENT
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
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

%%BEGIN-ASSIGNMENT
supervisorLoop() ->
  receive
    {'EXIT', FromPid, _Reason} ->
      io:format("~p supervisor detected server ~p exit, restart server~n", [self(), FromPid]),
      start(),
      linkServer()
  end,
  supervisorLoop().

linkServer() ->
  process_flag(trap_exit, true),
  link(whereis(frequency)),
  supervisorLoop().

initSupervisor() ->
  io:format("~p supervisor started~n", [self()]),
  linkServer().

% Start supervisor process to restart server if it terminates
startSupervisor() ->
  register(supervisor, spawn(frequency, initSupervisor, [])).
    
% Endless loop allocating and deallocating a frequency
clientLoop() ->
  receive
    {'EXIT', FromPid, _Reason} ->
      io:format("~p client detected server process ~p exit, exit client process too~n", [self(), FromPid])
    after 1000 ->
      io:format("~p client allocated ~p~n", [self(), allocate()]),
      receive
        {'EXIT', FromPid, _Reason} ->
          io:format("~p client detected server process ~p exit, exit client process too~n", [self(), FromPid])
      after 10000 ->
        deallocate(10),
        io:format("~p client deallocated ~p~n", [self(), 10]),
        clientLoop()
      end
  end.

initClient() ->
  io:format("~p client started~n", [self()]),
  process_flag(trap_exit, true),
  clientLoop().

% Start client process to allocate and deallocate frequencies
startClient() ->
  spawn(frequency, initClient, []).

% Test server being terminated unexpectedly
test() ->
  start(),
  startSupervisor(),
  startClient(),
  timer:sleep(25000),
  io:format("~p killing server process~n", [self()]),
  % Kill server and observe that supervisor starts it again
  exit(whereis(frequency), kill),
  timer:sleep(25000),
  % Start another client
  startClient(),
  timer:sleep(25000).

%%END-ASSIGNMENT

