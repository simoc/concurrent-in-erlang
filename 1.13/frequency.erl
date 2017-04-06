%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0, allocate/2, isAllocatedPid/2]).

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      io:format("sending reply ~w~n", [Reply]),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      case isAllocatedFreqPid(Freq, Pid, Frequencies) of
        true -> NewFrequencies = deallocate(Frequencies, Freq),
                Pid ! {reply, ok},
                loop(NewFrequencies);
        false -> Pid ! {reply, not_allocated},
                 loop(Frequencies)
      end;
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

isAllocatedPid(_Pid, []) -> false;
isAllocatedPid(Pid, [{_Freq, Pid} | _Xs]) -> true;
isAllocatedPid(Pid, [_X | Xs]) -> isAllocatedPid(Pid, Xs).

isAllocatedFreqPid(Freq, Pid, {Free, Allocated}) -> isAllocatedFreqAndPid(Freq, Pid, Allocated).

isAllocatedFreqAndPid(_Freq, _Pid, []) -> false;
isAllocatedFreqAndPid(Freq, Pid, [{Freq, Pid} | _Xs]) -> true;
isAllocatedFreqAndPid(Freq, Pid, [_X | Xs]) -> isAllocatedFreqAndPid(Freq, Pid, Xs).

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case isAllocatedPid(Pid, Allocated) of
    true -> {{[Freq|Free], Allocated}, {error, already_allocated}};
    false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
