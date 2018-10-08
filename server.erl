-module(server).
-export([start/1,stop/1]).

-record(server_state, {
      name,
      users,
      channels
}).

-record(channel_state, {
      name,
      users
}).

% set intial state for the server
initState(ServerName) ->
  #server_state{name = ServerName, users = [], channels = [] } .

% Start a new server process with the given name
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initState(ServerAtom), fun serverHandle/2) .

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

%% Server handle functions -----------------------------------------------------

serverHandle(State, {createchannel, Pid, Channel}) ->
	% Just create new channel.
  genserver:start(list_to_atom(Channel), initChannel(list_to_atom(Channel)), fun channelhandle/2),
	NewState = State#server_state{channels = [ list_to_atom(Channel) | State#server_state.channels] }, %State#server_state.channels ++ [list_to_atom(Channel)]
	io:format("The [servers] list of channels: ~p~n", [State#server_state.channels]),
	{reply, ok, NewState};

%%
%   - takes 2 params : state, request message
%   - returns a tuple: new state, response message
% need to have a function for each request type (join, leave ...)
serverHandle(State, {join, UserPid}) ->
  NewState = State#channel_state{users = [UserPid | State#channel_state.users] },
  {reply, ok, NewState}.

%% Channel functions -----------------------------------------------------------
initChannel(ChannelName) ->
    #channel_state{
          name = ChannelName,
  				users = []
  }.

%% [Joining channel]
% Add client to the list of clients who have joined the channel.
channelhandle(State, {join, Pid}) ->
      UpdatedState = State#channel_state{users = State#channel_state.users ++ [Pid] },
      {reply, ok, UpdatedState}.
