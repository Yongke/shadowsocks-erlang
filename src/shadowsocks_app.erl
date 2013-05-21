%%%-------------------------------------------------------------------
%%% @author Yongke <wangyongke@gmail.com>
%%% @copyright (C) 2013, Yongke
%%% @doc
%%% Application
%%% @end
%%%-------------------------------------------------------------------
-module(shadowsocks_app).

-behaviour(application).

%% Exported interfaces
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Interfaces
%%%===================================================================
start() ->
    application:start(shadowsocks).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    Args = case get_app_env(type, local) of
               local -> 
                   [local, get_app_env(local_port, 1080), 
                    get_app_env(server, "localhost"),
                    get_app_env(server_port, 8080),
                    shadowsocks_crypt:cipher_table(
                      get_app_env(method, default), get_app_env(password, "123456")),
                    get_app_env(method, default)];
               remote ->
                   [remote, get_app_env(server_port, 8080),
                    shadowsocks_crypt:cipher_table(
                      get_app_env(method, default), get_app_env(password, "123456")),
                    get_app_env(method, default)]
           end,
    case shadowsocks_sup:start_link(Args) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_app_env(Opt, Default) ->
    case application:get_env(shadowsocks, Opt) of
        {ok, Val} ->
            Val;
        _ -> 
            case init:get_argument(Opt) of
                {ok, [[Val | _]]} -> Val;
                error       -> Default
            end
    end.
