%%% @author Yongke <wangyongke@gmail.com>
%%% @copyright (C) 2013, Yongke
%%% @doc
%%% APIs for encrypt and decrypt
%%% @end

-module(shadowsocks_crypt).

%% API
-export([init_cipher_info/2, encode/2, decode/2, key_iv_len/1, stream_init/3]).

-include("shadowsocks.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Return the cipher information
%% 
%% @spec cipher_info(Method, Password::string()) -> 
%%                       {default, EncTable::list(), DecTable::list()} |
%%                       {Method, Key::binary(), IvEnc::binary(), IvDec::binanry()}
%%      Method := default | rc4 | des_cfb
%% @end
%%--------------------------------------------------------------------
init_cipher_info(default, Password) ->
    <<Value:64/little-unsigned-integer, _:64/little-unsigned-integer>> 
        = crypto:hash(md5,Password),
    Init = lists:seq(0, 255),
    EncTable = lists:foldl(
                 fun(I, Acc) ->
                         lists:sort(
                           fun(X, Y) ->
                                   Value rem (X + I) =< Value rem (Y + I)
                           end, Acc) 
                 end, Init, lists:seq(1,1023)),
    ZipTable = lists:zip(Init, EncTable),
    ZipDecTable = lists:keysort(1, [{N, M} || {M, N} <- ZipTable]),
    DecTable = [M || {_, M} <- ZipDecTable],
    #cipher_info{method=default, 
                 table={list_to_tuple(EncTable), list_to_tuple(DecTable)}};
init_cipher_info(Method, Password) ->
    {KeyLen, IvLen} = key_iv_len(Method),
    {Key, _} = evp_bytestokey(md5, Password, KeyLen, IvLen),
    %% use another random Iv, but not the one returned from evp_bytestokey()
    Iv = crypto:rand_bytes(IvLen),
    #cipher_info{method=Method, key=Key, encode_iv=Iv, decode_iv=undefined,
                stream_enc_state = stream_init(Method, Key, Iv),
                stream_dec_state = stream_init(Method, Key, Iv)}.

%%--------------------------------------------------------------------
%% @doc 
%% Encode function
%% @spec encode(CipherInfo, Data) -> Data
%%      CipherInfo := cipher_info()
%%      Data := iolist() | binary()
%% @end
%%--------------------------------------------------------------------
encode(#cipher_info{method=default, table={EncTable, _}}=CipherInfo, Data) ->
    {CipherInfo, transform(EncTable, Data)};
encode(#cipher_info{method=rc4, stream_enc_state=S}=CipherInfo, Data) ->
    {S1, EncData} = crypto:stream_encrypt(S, Data),
    {CipherInfo#cipher_info{stream_enc_state=S1}, EncData};
encode(#cipher_info{iv_sent = false, encode_iv=Iv}=CipherInfo, Data) ->
    NewCipherInfo = CipherInfo#cipher_info{iv_sent=true},
    {NewCipherInfo1, EncData} = encode(NewCipherInfo, Data), 
    {NewCipherInfo1, <<Iv/binary, EncData/binary>>};
encode(#cipher_info{method=des_cfb, key=Key, encode_iv=Iv}=CipherInfo, Data) ->
    EncData = crypto:block_encrypt(des_cfb, Key, Iv, Data),
    NextIv = crypto:next_iv(des_cfb, EncData, Iv),
    {CipherInfo#cipher_info{encode_iv=NextIv}, EncData}.

%%--------------------------------------------------------------------
%% @doc 
%% Decode function
%% @spec decode(CipherInfo, Data) -> Data
%%      CipherInfo := {default, EncTable::list(), DecTable::list()} |
%%                    {Method, Key::binary(), Iv::binary()}
%%      Method := default | rc4 | des_cfb
%%      Data := iolist() | binary()
%% @end
%%--------------------------------------------------------------------
decode(#cipher_info{method=default, table={_, DecTable}}=CipherInfo, EncData) ->
    {CipherInfo, transform(DecTable, EncData)};
decode(#cipher_info{method=rc4, stream_dec_state=S}=CipherInfo, EncData) ->
    {S1, Data} = crypto:stream_decrypt(S, EncData),
    {CipherInfo#cipher_info{stream_dec_state=S1}, Data};
decode(#cipher_info{method=des_cfb, key=Key, decode_iv=Iv}=CipherInfo, EncData) ->
    Data = crypto:block_decrypt(des_cfb, Key, Iv, EncData),
    NextIv = crypto:next_iv(des_cfb, Data, Iv),
    {CipherInfo#cipher_info{decode_iv=NextIv}, Data}.

%%--------------------------------------------------------------------
%% @doc
%% Decode or encode the data with default table ciphe method
%%
%% @spec transform(Table, Data) -> Data
%%      Table := [tuple()]
%%      Data := iolist() | binary()
%% @end
%%--------------------------------------------------------------------
transform(Table, Data) when is_binary(Data)->
    << <<(element(X+1, Table))>> || <<X>> <= Data >>;
transform(Table, Data) when is_list(Data) ->
    [ element(X+1, Table) || X <- Data ].

%%--------------------------------------------------------------------
%% @doc 
%% Creates a key and an IV for doing encryption, from a password, 
%% using a hashing function.
%% @spec evp_bytestokey(HashMethod::hash_method(), Password::string(), 
%%                      KeyLen::integer(), IvLen::integer()) ->
%%      {Key::binary(), Iv::binary()}
%% @end
%%--------------------------------------------------------------------
evp_bytestokey(md5, Password, KeyLen, IvLen) ->
    evp_bytestokey_aux(md5, list_to_binary(Password), KeyLen, IvLen, <<>>).

evp_bytestokey_aux(md5, _, KeyLen, IvLen, Acc) 
  when KeyLen + IvLen =< size(Acc) ->
    <<Key:KeyLen/binary, Iv:IvLen/binary, _/binary>> = Acc,
    {Key, Iv};
evp_bytestokey_aux(md5, Password, KeyLen, IvLen, Acc) ->
    Digest = crypto:hash(md5, <<Acc/binary, Password/binary>>),
    NewAcc = <<Acc/binary, Digest/binary>>,
    evp_bytestokey_aux(md5, Password, KeyLen, IvLen, NewAcc).

key_iv_len(des_cfb) ->
    {8, 8};
key_iv_len(rc4) ->
    {16, 0}.

stream_init(rc4, Key, _) ->
    crypto:stream_init(rc4, Key);
stream_init(_, _, _) ->
    undefined.
