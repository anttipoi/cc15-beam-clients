-module(erl_client).

-export([start/0]).

-define(LOBBY_PORT, 4567).
-define(NAME, <<"Planet Express">>).

% Transport captures everything needed to send messages to a aserver.
-record(transport,
        {serverHost,
         serverPort,
         socket}).

% State captures the runtime state needed by client.
-record(state, 
        {lobbyTransport,
         gameTransport,
         connectionId}).

% I question your random.
randomGameColor() ->
    <<"red">>.

samplePlayers() ->
    [#{number => 1,
       name => <<"Teemu">>},
     #{number => 2,
       name => <<"Kaali">>},
     #{number => 3,
       name => <<"Mighty Eagleflow">>}].

% Starting point.
start()->
    InitialMessage = connectMessage(?NAME, <<"bendersgame">>),
    Transport = openTransport(localhost, ?LOBBY_PORT, InitialMessage),
    loop(#state{lobbyTransport = Transport}).

% Main loop: receive incoming messages and react
loop(State) ->
    receive
        {udp, _Socket, _IP, _InPortNo, JsonData} ->
            Message = jsx:decode(JsonData, [return_maps, {labels, atom}]),
            debug("received JSON", Message),
            NewState = onServerMessage(Message, State),
            loop(NewState);
        M ->
            debug("Don't know what to do with message", M),
            loop(State)
    end.

%
% Handle messages from server. Return updated state.
%

onServerMessage(#{type := <<"connect-ok">>, 
                  'connection-id' := ConnectionId}, 
                State) ->
    State#state{connectionId = ConnectionId};
onServerMessage(#{type := <<"ping">>}, 
                #state{connectionId = ConnectionId, 
                       lobbyTransport = LobbyTransport} = State) ->
    sendMessage(pongMessage(ConnectionId), LobbyTransport),
    State;
onServerMessage(#{type := <<"game-at">>,
                  address := _Address,
                  port := Port,
                  'game-id' := GameId},
                #state{connectionId = ConnectionId} = State) ->
    InitialMessage = joinMessage(GameId, ?NAME, randomGameColor(), samplePlayers(), ConnectionId),
    GameTransport = openTransport(localhost, Port, InitialMessage),
    State#state{gameTransport = GameTransport};
onServerMessage(#{type := <<"join-ok">>}, 
                State) ->
    debug("Join was successful, let the games begin"),
    State;
onServerMessage(#{type := <<"join-error">>, 
                  description := Description}, 
                State) ->
    debug("Join failed", Description),
    State;
onServerMessage(M, State) ->
    debug("Unknown msg from server", M),
    State.

%
% Communication with server
%

openTransport(ServerHost, ServerPort, InitialMessage) ->
    {ok, Socket} = gen_udp:open(0, [{active, true}, binary]),
    Transport = #transport{serverHost = ServerHost,
                           serverPort = ServerPort,
                           socket = Socket},
    sendMessage(InitialMessage, Transport),
    Transport.

sendMessage(JsonMessage, #transport{serverHost = ServerHost, serverPort = ServerPort, socket = Socket}) ->
    Message = jsx:encode(JsonMessage),
    debug("sending json", Message),
    gen_udp:send(Socket, ServerHost, ServerPort, Message).
  
%
% Messages we send to server
%

connectMessage(Name, GameId) ->
    #{type => <<"connect">>, 
      name => Name, 
      'game-id' => GameId}.

pongMessage(ConnectionId) ->
    #{type => <<"pong">>, 
      'connection-id' => ConnectionId}.

joinMessage(GameId, Name, Color, Players, ConnectionId) ->
    #{type => <<"join">>, 
      color => Color, 
      'connection-id' => ConnectionId, 
      'game-id' => GameId, 
      name => Name, 
      players => Players}.

%
% Utils
%

debug(Message) ->
    io:format(string:concat(Message, "~n")).

debug(Message, Arg) ->
    Format = string:concat(Message, ": ~p~n"),
    io:format(Format, [Arg]).

