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
  % check if channel process exists
  case whereis(ChannelAtom) of
      undefined -> % channel do not exist --> create new one (new process)
        genserver:start(ChannelAtom, initChannel(ChannelAtom), fun handleChannel/2);
      _ -> % channel already exist
        ok
  end,
  % join channel (new or old)
  case catch(genserver:request(ChannelAtom, {join, PID})) of
      ok ->
        case lists:member(ChannelAtom, St#server_st.channels) of
          % this case might need/should be moved up to the creation of the new channel
          false -> % if there was a new channel update the state with the new channel
            {reply, ok, St#server_st{
              channels = [ ChannelAtom | St#server_st.channels ],
              clients = [ PID | St#server_st.clients ]}};
          _ -> {reply, ok, St}
            % add user to server_state here too ?
        end;
      {'EXIT', _} ->
        {reply, {error, channel_not_reached, "Server not reached"}, St}
  end;


% Leave a channel
handleServer(St, {leave, Channel, Pid}) ->
    case catch(genserver:request(list_to_atom(Channel), {leave, Pid})) of
      ok ->
        {reply, ok, St#server_st{clients = lists:delete(Pid, St#server_st.clients)}};

      {'EXIT', "Timeout"} ->
        {reply, {error, channel_not_reached, "Server not reached"}, St};
      _ ->
        {reply, {error, user_not_joined, "User was not joined from the beginning"}, St}
    end.
>>>>>>> Filips stuff


% handleChannel ----------------------------------------------------------------
handleChannel(St, {join, PID}) ->
  % Check if user already is in channel
  case lists:member(PID, St#channel_st.clients) of
    false -> % add user
      {reply, ok, St#channel_st{clients = [PID | St#channel_st.clients]}};
     {'EXIT', _} -> % user is already in channel
      {reply, {error, user_already_joined}, St}
  end;

handleChannel(St, {leave, Pid}) ->
    case lists:member(Pid, St#channel_st.clients) of
      true -> % remove user
        {reply, ok, St#channel_st{clients = lists:delete(Pid, St#channel_st.clients)}};
      _ -> % user was not added from the beginning (do not remove)
        {reply, {error, user_not_joined, "User was not joined from the beginning"}, St}
    end;


handleChannel(St, {message_send, PID, Msg, Sender}) ->
  IsMember = lists:member(Sender, St#channel_st.clients),
      case IsMember of
        true ->
          spawn(
            fun() ->
              [ genserver:request(
                  Receiver,
                  {message_receive, St#channel_st.name, PID, Msg}
                ) || Receiver <- St#channel_st.clients, Receiver =/= Sender]
            end
          ),
          {reply, message_send, St};
        false ->
          {reply, error, St}
      end.
