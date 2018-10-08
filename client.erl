-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % existing channels on the server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

addChannel(State, Channel) ->
  NewState = State#client_st{channels = [ Channel | State#client_st.channels ]} .

% TODO fix
leaveChannel(State, Channel) ->
  Update = lists:delete(Channel, State#client_st.channels ),
  NewState = State#client_st{channels = Update} .


isUserJoined(State, Channel) ->
  lists:member(Channel, State#client_st.channels).

% Join channel
% TODO REDO
handle(State, {join, Channel}) ->

  case isUserJoined(State, list_to_atom(Channel)) of
    % user is not in the channel
    false ->
    	case whereis(list_to_atom(Channel)) of
        % need to create channel
    	  undefined ->
    		case catch genserver:request(State#client_st.server, {createchannel, self(), Channel}) of
    		    ok -> % Request ok, createchannel operation succeeded. Then join channel.
      			  genserver:request(list_to_atom(Channel), {join, self()}),
      			  io:format("Now the client has joined an newly created channel. ~n"),
      			  % Then, update the clients channel list (insert new channel).
              NewState = addChannel(State, list_to_atom(Channel)),
      			  {reply, ok, NewState};
    		    {'EXIT', Reason} ->
    			    {reply, {error, server_not_reached, "Something went wrong when trying to join, probably no connection to server.~n"}, State}
    	  end;
	    _ ->
		  genserver:request(list_to_atom(Channel), {join, self()}),
		  io:format("Now the client has joined an already existing channel. ~n"),
      NewState = addChannel(State, list_to_atom(Channel)),
		  {reply, ok, NewState}
    end;
    true ->
	io:format("The channel has already been joined. ~n"),
	{reply, {error, user_already_joined, "Already joined~n"}, State}
end;


% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "leave not implemented"}, St} ;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "message sending not implemented"}, St} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
