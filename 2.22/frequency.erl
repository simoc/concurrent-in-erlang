%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
%%BEGIN-ASSIGNMENT
-export([inject/1,loop/1]).
%%END-ASSIGNMENT
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  io:format("~p server in loop~n", [self()]),
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      frequency:loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      frequency:loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
%%BEGIN-ASSIGNMENT
    {request, Pid , {inject, AdditionalFrequencies}} ->
      Pid ! {reply, ok},
      frequency:loop(injectFrequencies(Frequencies, AdditionalFrequencies))
%%END-ASSIGNMENT
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

%%BEGIN-ASSIGNMENT
inject(Frequencies) ->
    frequency ! { request, self(), {inject, Frequencies}},
    receive 
	    {reply, Reply} -> Reply
    end.
%%END-ASSIGNMENT

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%%BEGIN-ASSIGNMENT
injectFrequencies({Free, Allocated}, AdditionalFrequencies) ->
  {Free ++ AdditionalFrequencies, Allocated}.
%%END-ASSIGNMENT
