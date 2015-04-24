%% ===================================================================
%% erlonion_app.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_app).
-behaviour(application).

%% Includes
-include_lib("public_key/include/public_key.hrl").

%% Application callbacks
-export([start/2, stop/1, recv_loop/4, get_env_val/2,
         pub_encrypt_message/2]).

%% Macros
-define(NUM_AESKEY_BYTES, 32).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("start erlonion_app~n", []),
    case get_env_val(rsa_pass, "") of
        "" -> RSAPass = "", io:format("error, need rsa password~n"); % exit and die
        RSAPass -> ok
    end,
    {ok, RSAPrivPem} = file:read_file("keys/private.pem"),
    [RSAPrivEntry] = public_key:pem_decode(RSAPrivPem),
    PrivKey = public_key:pem_entry_decode(RSAPrivEntry, RSAPass),
    {ok, RSAPubPem} = file:read_file("keys/public.pem"),
    [RSAPubEntry] = public_key:pem_decode(RSAPubPem),
    PubKey = public_key:pem_entry_decode(RSAPubEntry),
    AESKey = crypto:strong_rand_bytes(?NUM_AESKEY_BYTES),
    % ranch options
    Ref = erlonion_listener,
    NbAcceptors = get_env_val(num_acceptors, 20),
    Transport  = ranch_tcp,
    Port = get_env_val(port, 0),
    TransOpts = [{port, Port}],
    Protocol = case get_env_val(type, path) of
                   directory ->
                       TableOpts = [public, named_table],
                       erlonion_pathnodes = ets:new(erlonion_pathnodes, TableOpts),
                       erlonion_dir;
                   path ->
                       ok = erlonion_path:register_node(Transport, PrivKey, PubKey, AESKey),
                       erlonion_path;
                   _ -> error % print error message and die
               end,
    ProtoOpts = [{priv_key, PrivKey}, {pub_key, PubKey}, {aes_key, AESKey}],
    {ok, _Pid} = ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts),
    io:format("started ranch listener~n", []),
    erlonion_sup:start_link().

stop(_State) ->
    ok.

recv_loop(Transport, SockRec, Timeout, RetData) ->
    case Transport:recv(SockRec, 0, Timeout) of
        {ok, Data} ->
            recv_loop(Transport, SockRec, Timeout, <<RetData/binary, Data/binary>>);
        _ -> RetData
    end.

get_env_val(Key, Default) ->
    case application:get_env(Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

pub_encrypt_message(#'RSAPublicKey'{modulus=M, publicExponent=PubE}, MessageParts) ->
    Message = string:join(MessageParts, ";"),
    crypto:public_encrypt(rsa, list_to_binary(Message), [M, PubE], rsa_pkcs1_padding).
