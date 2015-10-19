-module(erl_client).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    InitialMessage = connectMessage(?NAME, <<"bendersgame">>),
    Transport = openTransport(localhost, ?LOBBY_PORT, InitialMessage),
    {ok, #state{lobbyTransport = Transport}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({udp, _Socket, _IP, _InPortNo, JsonData}, State) ->
    Message = jsx:decode(JsonData, [return_maps, {labels, atom}]),
    debug("received JSON", Message),
    NewState = onServerMessage(Message, State),
    {noreply, NewState};
handle_info(M, State) ->
    debug("Don't know what to do with message", M),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

