%%%-------------------------------------------------------------------
%%% @author Yongke <wangyongke@gmail.com>
%%% @copyright (C) 2013, Yongke
%%% @doc
%%% Proxying process, can be running as LOCAL instance or REMOTE instance
%%% - LOCAL instance provide an socks5 server to CLIENT and comunication 
%%%   with REMOTE instance;
%%% - REMOTE instance relay requests from LOCAL instance to TARGET
%%%   server which CLIENT want to visit
%%% @end
%%%-------------------------------------------------------------------
-module(shadowsocks_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
         'WAIT_FOR_SOCKET'/2,
         'WAIT_FOR_DATA'/2,
         'WAIT_FOR_SOCKS5_AUTH'/2,
         'WAIT_FOR_SOCKS5_REQUEST'/2,
         'WAIT_FOR_TARGET_INFO'/2
        ]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 120000).

-record(state, {
          type,             % server type, local or remote
          client_socket,    % client socket
          client_addr,      % client address
          remote_socket,    % remote socket
          remote_addr,      % remote addr
          remote_port,      % remote port
          method,
          encode_table,
          decode_table,
          buff = <<>>       % buffer used when handling socks5 handshake
         }).

-include("shadowsocks.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([local, _, ServerAddr, ServerPort, {EncodeTable, DecodeTable}, Method]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{type = local, 
                                   remote_addr = ServerAddr,
                                   remote_port = ServerPort,
                                   encode_table = EncodeTable,
                                   decode_table = DecodeTable,
                                   method = Method}};
init([remote, _, {EncodeTable, DecodeTable}, Method]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{type = remote,
                                   encode_table = EncodeTable,
                                   decode_table = DecodeTable,
                                   method = Method}}.

%% For LOCAL instance:
%%  WAIT_FOR_SOCKET -> WAIT_FOR_SOCKS5_AUTH -> WAIT_FOR_SOCKS5_REQUEST ->
%%  WAIT_FOR_DATA <-> WAIT_FOR_DATA
%%
%% For REMOTE instance: 
%%  WAIT_FOR_SOCKET -> WAIT_FOR_TARGET_INFO -> 
%%  WAIT_FOR_DATA <-> WAIT_FOR_DATA
'WAIT_FOR_SOCKET'({socket_ready, Socket}, #state{type=local}=State) 
  when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_SOCKS5_AUTH', 
     State#state{client_socket=Socket, client_addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'({socket_ready, Socket}, #state{type=remote}=State) 
  when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_TARGET_INFO', 
     State#state{client_socket=Socket, client_addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    ?ERROR("Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

'WAIT_FOR_SOCKS5_AUTH'({client, Data}, #state{
                         type=local, client_socket=S} = State) ->
    Buffer = <<(State#state.buff)/binary, Data/binary>>,
    case decode_socks5_auth(Buffer) of 
        incomplete ->
            {next_state, 'WAIT_FOR_SOCKS5_AUTH', 
             State#state{buff=Buffer}, ?TIMEOUT};
        {?SOCKS5_VER, _, _, Rest}->
            gen_tcp:send(S, <<?SOCKS5_VER, ?SOCKS5_AUTH_NONE>>),
            {next_state, 'WAIT_FOR_SOCKS5_REQUEST', 
             State#state{buff=Rest}, ?TIMEOUT};
        Error ->
            ?ERROR("'WAIT_FOR_SOCKS5_AUTH' with error: ~p\n", [Error]),
            {stop, Error, State}
    end;
'WAIT_FOR_SOCKS5_AUTH'(timeout, #state{client_addr=Addr}=State) ->
    ?ERROR("Client connection timeout: 'WAIT_FOR_SOCKS5_AUTH', ~p\n", 
           [Addr]),
    {stop, normal, State}.

'WAIT_FOR_SOCKS5_REQUEST'({client, Data}, #state{
                            type=local,
                            client_socket=S, remote_addr=RemoteAddr,
                            remote_port=RemotePort,
                            encode_table=EncTable} = State) ->
    Buffer = <<(State#state.buff)/binary, Data/binary>>,
    case decode_socks5_req(Buffer) of
        incomplete ->
            {next_state, 'WAIT_FOR_SOCKS5_REQUEST',
             State#state{buff=Buffer}, ?TIMEOUT};
        {?SOCKS5_VER, AddrType, Addr, Port, Rest}->
            Socks5Rsp = <<?SOCKS5_VER:8, ?SOCKS5_REP_OK:8, 
                          ?SOCKS5_RESERVED_FIELD:8>>,
            Target = case AddrType of 
                         ?SOCKS5_ATYP_V4 ->
                             BinAddr = list_to_binary(tuple_to_list(Addr)),
                             <<?SOCKS5_ATYP_V4:8, BinAddr/binary, Port:16>>;
                         ?SOCKS5_ATYP_DOM ->
                             BinAddr = list_to_binary(Addr),
                             AddrSize = size(BinAddr),
                             <<?SOCKS5_ATYP_DOM:8, AddrSize:8, BinAddr/binary, Port:16>>
                     end,
            gen_tcp:send(S, [Socks5Rsp, Target]),
            %% connect to remote server & send first message
            case gen_tcp:connect(RemoteAddr, RemotePort, [{active, once}, 
                                                          {packet, raw}, binary]) of
                {ok, RemoteSocket} ->
                    ?INFO("Connected to remote ~p:~p for proxying ~p:~p\n", 
                          [RemoteAddr, RemotePort, Addr, Port]),
                    gen_tcp:send(RemoteSocket, 
                                 shadowsocks_crypt:transform(EncTable,Target)),
                    gen_tcp:send(RemoteSocket, Rest),
                    {next_state, 'WAIT_FOR_DATA', 
                     State#state{buff= <<>>, remote_socket=RemoteSocket}, ?TIMEOUT};
                {error, Reason} ->
                    ?ERROR("'WAIT_FOR_SOCKS5_REQUEST' with error: ~p\n", [Reason]),
                    {stop, Reason, State}
            end;
        Error ->
            ?ERROR("'WAIT_FOR_SOCKS5_REQUEST' with error: ~p\n", [Error]),
            {stop, Error, State}            
    end;
'WAIT_FOR_SOCKS5_REQUEST'(timeout, #state{client_addr=Addr}=State) ->
    ?ERROR("Client connection timeout: 'WAIT_FOR_SOCKS5_AUTH', ~p\n", 
           [Addr]),
    {stop, normal, State}.

'WAIT_FOR_TARGET_INFO'({client, Data}, #state{type=remote, 
                                              decode_table=DecTable}=State) ->
    Data1 = shadowsocks_crypt:transform(DecTable, Data),
    Buffer = <<(State#state.buff)/binary, Data1/binary>>,
    case decode_target_info(Buffer) of
        incomplete ->            
            {next_state, 'WAIT_FOR_TARGET_INFO',
             State#state{buff=Buffer}, ?TIMEOUT};
        {error, _, _} = Error ->
            {stop, Error, State};
        {TargetAddr, TargetPort, Rest} ->
            case gen_tcp:connect(TargetAddr, TargetPort, 
                                 [{active, once}, {packet, raw}, binary]) of
                {ok, RemoteSocket} ->
                    ?INFO("Connected to remote ~p:~p\n", [ TargetAddr, TargetPort]),
                    gen_tcp:send(RemoteSocket, Rest),
                    {next_state, 'WAIT_FOR_DATA',
                     State#state{buff= <<>>, remote_socket=RemoteSocket,
                                remote_addr=TargetAddr, remote_port=TargetPort}, 
                     ?TIMEOUT};
                {error, Reason} ->
                    ?ERROR("Can not connect to remote ~p:~p, ~p\n", 
                           [ TargetAddr, TargetPort, Reason]),
                    {stop, Reason, State}
            end
    end.

'WAIT_FOR_DATA'({client, Data}, #state{type=local, 
                                       remote_socket=RemoteSocket, 
                                       encode_table=EncTable} = State) ->
    EncData = shadowsocks_crypt:transform(EncTable, Data),
    gen_tcp:send(RemoteSocket, EncData),
    {next_state, 'WAIT_FOR_DATA', State};
'WAIT_FOR_DATA'({remote, Data}, #state{type=local,
                                       client_socket=ClientSocket, 
                                       decode_table=DecTable} = State) ->
    DecData = shadowsocks_crypt:transform(DecTable, Data),
    gen_tcp:send(ClientSocket, DecData),
    {next_state, 'WAIT_FOR_DATA', State};
'WAIT_FOR_DATA'({client, Data}, #state{type=remote, 
                                       remote_socket=RemoteSocket, 
                                       decode_table=DecTable} = State) ->
    DecData = shadowsocks_crypt:transform(DecTable, Data),
    gen_tcp:send(RemoteSocket, DecData),
    {next_state, 'WAIT_FOR_DATA', State};
'WAIT_FOR_DATA'({remote, Data}, #state{type=remote,
                                       client_socket=ClientSocket, 
                                       encode_table=EncTable} = State) ->
    EncData = shadowsocks_crypt:transform(EncTable, Data),
    gen_tcp:send(ClientSocket, EncData),
    {next_state, 'WAIT_FOR_DATA', State};
'WAIT_FOR_DATA'(timeout, State) ->
    {stop, normal, State};
'WAIT_FOR_DATA'(Data, State) ->
    ?WARNING("Ignoring data: ~p\n", [Data]),
    {next_state, 'WAIT_FOR_DATA', State}.

handle_event(Event, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

handle_info({tcp, ClientSocket, Bin}, StateName, 
            #state{client_socket=ClientSocket} = StateData) ->
    inet:setopts(ClientSocket, [{active, once}]),
    ?MODULE:StateName({client, Bin}, StateData);
handle_info({tcp_closed, ClientSocket}, _StateName,
            #state{client_socket=ClientSocket, client_addr=Addr, 
                   remote_addr=RemoteAddr} = StateData) ->
    ?INFO("Client ~p disconnected(for ~p).\n", [Addr,RemoteAddr]),
    {stop, normal, StateData};
handle_info({tcp, RemoteSocket, Bin}, StateName, 
            #state{remote_socket=RemoteSocket} = StateData) ->
    inet:setopts(RemoteSocket, [{active, once}]),
    ?MODULE:StateName({remote, Bin}, StateData);
handle_info({tcp_closed, RemoteSocket}, _StateName,
            #state{remote_socket=RemoteSocket, remote_addr=Addr} = StateData) ->
    ?INFO("Remote ~p disconnected.\n", [Addr]),
    {stop, normal, StateData};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{client_socket=ClientSocket,
                                      remote_socket=RemoteSocket}) ->
    (catch gen_tcp:close(ClientSocket)),
    (catch gen_tcp:close(RemoteSocket)),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_socks5_auth(<<Ver:8/big, _/binary>>) when Ver =/= ?SOCKS5_VER ->
    {error, not_supported_version, Ver};
decode_socks5_auth(<<?SOCKS5_VER:8/big, NMethods:8/big, 
                     Methods:NMethods/binary, Rest/binary>>) ->
    {?SOCKS5_VER, NMethods, Methods, Rest};
decode_socks5_auth(_) ->
    incomplete.

decode_socks5_req(<<Ver:8/big, _/binary>>) 
  when Ver =/= ?SOCKS5_VER ->
    {error, not_supported_version, Ver};
decode_socks5_req(<<_:8/big, Cmd:8/big, _/binary>>) 
  when Cmd =/= ?SOCKS5_REQ_CONNECT ->
    {error, not_supported_command, Cmd};
decode_socks5_req(<<?SOCKS5_VER:8/big, ?SOCKS5_REQ_CONNECT:8/big, _:8/big, 
                    ?SOCKS5_ATYP_V6:8/big, _/binary>>) ->
    {error, not_supported_ipv6, ?SOCKS5_ATYP_V6};
decode_socks5_req(<<?SOCKS5_VER:8/big, ?SOCKS5_REQ_CONNECT:8/big, _:8/big, 
                     ?SOCKS5_ATYP_V4:8/big, 
                     DestAddr:4/binary, DestPort:16/big, Rest/binary>>) ->
    {?SOCKS5_VER, ?SOCKS5_ATYP_V4, list_to_tuple(binary_to_list(DestAddr)), 
     DestPort, Rest};
decode_socks5_req(<<?SOCKS5_VER:8/big, ?SOCKS5_REQ_CONNECT:8/big, _:8/big, 
                     ?SOCKS5_ATYP_DOM:8/big, DomLen:8/big, 
                     Domain:DomLen/binary, DestPort:16/big,
                     Rest/binary>>) ->
    {?SOCKS5_VER, ?SOCKS5_ATYP_DOM, binary_to_list(Domain), DestPort, Rest};
decode_socks5_req(_) ->
    incomplete.


decode_target_info(<<AddrType:8/big, _/binary>>) 
  when AddrType =/= ?SOCKS5_ATYP_V4, AddrType =/= ?SOCKS5_ATYP_DOM->
    {error, not_supported_address_type, AddrType};
decode_target_info(<<?SOCKS5_ATYP_V4:8/big, DestAddr:4/binary, DestPort:16/big, 
                     Rest/binary>>) ->
    {list_to_tuple(binary_to_list(DestAddr)), DestPort, Rest};
decode_target_info(<<?SOCKS5_ATYP_DOM:8/big, DomLen:8/big, Domain:DomLen/binary, 
                     DestPort:16/big, Rest/binary>>) ->
    {binary_to_list(Domain), DestPort, Rest};
decode_target_info(_) ->
    incomplete.
