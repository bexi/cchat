-module(server).
- export ( [ start / 1 , stop / 1 ] ) .

-record(server_st, {
    name,
    clients,
    channels
}).

-record(channel_st, {
    name,
    clients
}).

initServer(ServerAtom) ->
    #server_st{
        name = ServerAtom,
        clients = [],
        channels = []
    }.

initChannel(ChannelAtom) ->
    #channel_st{
        name = ChannelAtom,
        clients = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initServer(ServerAtom), fun handleServer/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

% handleServer -----------------------------------------------------------------

% Join a channel
% if no channel exists --> create new one & join
% if channel exists --> join
handleServer(St, {join, Channel, PID}) ->
  ChannelAtom = list_to_atom(Channel),
  case whereis(ChannelAtom) of
      undefined -> % channel do not exist --> create new one
        genserver:start(ChannelAtom, initChannel(Channel), fun handleChannel/2);
      _ -> % channel already exist
        ok
  end,

  case catch(genserver:request(ChannelAtom, {join, PID})) of
      ok ->
        case lists:member(Channel, St#server_st.channels) of
          false ->
            {reply, ok, St#server_st{
              channels = [ Channel | St#server_st.channels ],
              clients = [ PID | St#server_st.clients ]}};
          _ -> {reply, ok, St}
        end;
      {'EXIT', "Timeout"} ->
        {reply, {error, channel_not_reached, "Timeout"}, St}
  end;

% Leave a channel
handleServer(St, {leave, Channel, PID}) ->
 not_implemented ;

% Send a message
handleServer(St, {message_send, Msg, Channel, PID}) ->
  not_implemented .

% handleChannel ----------------------------------------------------------------
handleChannel(St, {join, PID}) ->
  % Check if user already is in channel
  case lists:member(PID, St#channel_st.clients) of
    false -> % add user
      {reply, ok, St#channel_st{clients = [PID | St#channel_st.clients]}};
     _ -> % user is already in channel
      {reply, {error, user_already_joined}, St}
  end;

handleChannel(St, {leave, PID}) ->
 not_implemented ;

handleChannel(St, {message_send, Msg, PID}) ->
  not_implemented .

sendMsg([Client|Clients], Msg, Channel) ->
  not_implemented .
