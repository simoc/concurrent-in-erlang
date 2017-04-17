% 3.11, implementation of frequency server using gen_server.
-module(frequency).
-behaviour(gen_server).
-export([init/1,start_link/0,allocate/0,deallocate/1,stop/0,report/0]).
-export([handle_call/3,terminate/2]).

init(_Args) ->
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

handle_call(allocate, Pid, Frequencies) ->
  case allocate(Frequencies, Pid) of
    {_NewFrequencies, {error, no_frequency}} -> {reply, {error, no_frequency}, Frequencies};
    {NewFrequencies, {ok, Freq}} -> {reply, Freq, NewFrequencies}
  end;
handle_call({deallocate, Freq}, _Pid, Frequencies) ->
  {reply, ok, deallocate(Frequencies, Freq)};
handle_call(report, _Pid, {Free, Allocated}) ->
  {reply, {length(Free), length(Allocated)}, {Free, Allocated}}.

terminate(Reason, _Frequencies) -> io:format("terminate ~p~n", [Reason]).

% Start server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

allocate() ->
  gen_server:call(?MODULE, allocate).

deallocate(N) ->
  gen_server:call(?MODULE, {deallocate, N}).

report() ->
  gen_server:call(?MODULE, report).

stop() ->
  gen_server:stop(?MODULE).

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
