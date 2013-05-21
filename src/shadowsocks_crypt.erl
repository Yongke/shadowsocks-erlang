%%% @author Yongke <wangyongke@gmail.com>
%%% @copyright (C) 2013, Yongke
%%% @doc
%%% APIs for encrypt and decrypt
%%% @end

-module(shadowsocks_crypt).

%% API
-export([cipher_table/2, transform/2]).

%%--------------------------------------------------------------------
%% @doc
%% Return the cipher table
%% 
%% @spec cipher_table(Method, Key::string()) -> 
%%                       {EncTable::list(), DecTable::list()}
%%      Method = default | rc4 (TODO)
%% @end
%%--------------------------------------------------------------------
cipher_table(default, Key) ->
    <<Value:64/little-unsigned-integer, _:64/little-unsigned-integer>> 
        = crypto:md5(Key),
    Init = lists:seq(0, 255),
    Table = lists:foldl(
              fun(I, Acc) ->
                      lists:sort(
                        fun(X, Y) ->
                                Value rem (X + I) =< Value rem (Y + I)
                        end, Acc) 
              end, Init, lists:seq(1,1023)),
    EncTable = lists:zip(Init, Table),
    DecTable = lists:keysort(1, [{N, M} || {M, N} <- EncTable]),
    {EncTable, DecTable}.

%%--------------------------------------------------------------------
%% @doc
%% Decode or encode the data with specified cipher table
%%
%% @spec transform(Table, Data) -> Data
%%      Table = [tuple()]
%%      Data = binary()     
%% @end
%%--------------------------------------------------------------------
%% proplists:get_value is very very slow!!! as it's not BIF
%% transform(Table, Data) when is_binary(Data)->
%%     << <<(proplists:get_value(X, Table))>> || <<X>> <= Data >>;
%% transform(Table, Data) when is_list(Data) ->
%%     [ proplists:get_value(X, Table) || X <- Data ].

transform(Table, Data) when is_binary(Data)->
    << <<(begin 
              {X, Y} = lists:keyfind(X, 1, Table), Y
          end)>> || <<X>> <= Data >>;
transform(Table, Data) when is_list(Data) ->
    [ begin 
          {X, Y} = lists:keyfind(X, 1, Table), Y
      end || X <- Data ].
