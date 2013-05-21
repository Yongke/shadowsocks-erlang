%%%-------------------------------------------------------------------
%%% @author Yongke <wangyongke@gmail.com>
%%% @copyright (C) 2013, Yongke
%%% @doc
%%% Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(shadowsocks_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link([_, ListeningPort | _] = Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, 
                          [ListeningPort, shadowsocks_fsm, Args]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Port, Module, Args]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Child = {tcp_server_sup, {tcp_listener,start_link,[Port,Module,Args]},
              Restart, Shutdown, Type, [tcp_listener]},

    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

