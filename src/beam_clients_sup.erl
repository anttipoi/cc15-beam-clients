%%%-------------------------------------------------------------------
%% @doc beam_clients top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(beam_clients_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [#{id => erl_client,
                    start => {erl_client, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill,
                    type => worker}],
    {ok, { {one_for_all, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
