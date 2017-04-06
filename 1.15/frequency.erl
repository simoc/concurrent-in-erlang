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
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      % BEGIN-ASSIGNMENT:
      % Simulate server being very busy, and slow to reply
      % The client will timeout before reply is sent and reply will remain
      % in client's mailbox until the the client tries to allocate again.
      timer:sleep(12000),
      % END-ASSIGNMENT:
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
    % BEGIN-ASSIGNMENT:
    % Remove any pending messages from mailbox, so they do not get
    % mixed up with the response to the request we are about to send.
    clear(),
    % END-ASSIGNMENT:
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
        % ASSIGNMENT:
        % add timeouts for the client code.
        after 10000 -> timeout
    end.

deallocate(Freq) -> 
    % BEGIN-ASSIGNMENT:
    % Remove any pending messages from mailbox, so they do not get
    % mixed up with the response to the request we are about to send.
    clear(),
    % END-ASSIGNMENT:
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
        % ASSIGNMENT:
        % add timeouts for the client code.
        after 10000 -> timeout
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
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

% BEGIN-ASSIGNMENT:
% Remove all queued messages from mailbox.
clear() ->
  receive
    Msg -> io:format("clearing message from mailbox ~w~n", [Msg]), clear()
  after 0 -> ok
  end.
% END-ASSIGNMENT:
