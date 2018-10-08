-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % channels the user has joined
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

% handle -----------------------------------------------------------------------

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of
      true -> % retrun with the message user_already_joined
        {reply, {error, user_already_joined, "User is already joined in the channel"}, St};
      _ -> % continue
        ok
    end,

    % add user to server/channel
    case catch(genserver:request(St#client_st.server, {join, Channel, self()})) of
      ok ->
        {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
      {'EXIT',_} ->
        {reply, {error, server_not_reached, "Server not reached"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
  not_implemented ;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
  not_implemented ;

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
    io:fwrite("Client dontchange Channel variable: ~p~n", [Channel]),
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    io:fwrite("Catchall: ~p~n", [Data]),
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
