%%%-------------------------------------------------------------------
%% @doc beam_clients public API
%% @end
%%%-------------------------------------------------------------------

-module(beam_clients_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    beam_clients_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
