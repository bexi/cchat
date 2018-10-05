-module(server).
-export([start/1,stop/1]).

-record(server_state, {
      name,
      users
}).

%%
init_state(ServerName) ->
  #server_state{name = ServerName, users = [] } .

%%
%   - takes 2 params : state, request message
%   - returns a tuple: new state, response message
% need to have a function for each request type (connect, join, leave ...)
req_handler(State, {connect, Pid, Nick}) ->
  NewState = State#server_state{users = [Nick | State#server_state.users]},
  io:format("The users: ~p~n", [State#server_state.users]),
  {reply, ok, NewState} .

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, init_state(ServerAtom), fun req_handler/2) .

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    genserver:stop(ServerAtom),
    % Return ok
    ok .
