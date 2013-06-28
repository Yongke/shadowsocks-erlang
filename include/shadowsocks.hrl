%% Logging macro
-define(INFO(Format, Data), error_logger:info_msg(Format, Data)).
-define(WARNING(Format, Data), error_logger:warning_msg(Format, Data)).
-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).

%% Definition of macros and constants for socksv5
-define(SOCKS5_VER, 16#05).

-define(SOCKS5_AUTH_NONE,   16#00).
-define(SOCKS5_AUTH_GSSAPI, 16#01).
-define(SOCKS5_AUTH_USER,   16#02).
-define(SOCKS5_AUTH_ERR,    16#ff).

-define(SOCKS5_REQ_CONNECT,  16#01).
-define(SOCKS5_REQ_BIND,     16#02).
-define(SOCKS5_REQ_UDP_ASSOC,16#03).

-define(SOCKS5_ATYP_V4,  16#01).
-define(SOCKS5_ATYP_DOM, 16#03).
-define(SOCKS5_ATYP_V6,  16#04).

-define(SOCKS5_REP_OK,   16#00).
-define(SOCKS5_REP_FAIL, 16#01).
-define(SOCKS5_REP_NOT_ALLOWED, 16#02).
-define(SOCKS5_REP_NET_UNREACHABLE, 16#03).
-define(SOCKS5_REP_HOST_UNREACHABLE, 16#04).
-define(SOCKS5_REP_REFUSED, 16#05).
-define(SOCKS5_REP_TTL_EXPIRED, 16#06).
-define(SOCKS5_REP_CMD_NOT_SUPPORTED, 16#07).
-define(SOCKS5_REP_ATYP_NOT_SUPPORTED, 16#08).

-define(SOCKS5_RESERVED_FIELD, 16#00).

%% cipher info
-record(cipher_info, {
          method=default,      %% default | rc4 | aes_128_cfb | bf_cfb | des_cfb
          table = {[], []},    %% table for default method only
          key,
          encode_iv,
          iv_sent = false,     %% true | false
          decode_iv,
          stream_enc_state,    %% used in AES CTR and RC4 mode
          stream_dec_state     %% used in AES CTR and RC4 mode
         }).
