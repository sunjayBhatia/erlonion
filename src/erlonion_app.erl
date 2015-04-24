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
         pub_encrypt_message/2, priv_decrypt_message/2]).

%% Macros
-define(NUM_AESKEY_BYTES, 16).
-define(RSA_OPTS, [{rsa_pad, 'rsa_pkcs1_padding'}]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("start erlonion_app~n", []),
    AESKey = crypto:strong_rand_bytes(?NUM_AESKEY_BYTES),
    case get_env_val(block_key_init, "") of
        "" -> IVec = ""; % print error message and die
        IVec ->
            case byte_size(IVec) of
                ?NUM_AESKEY_BYTES -> ok;
                _ -> error % print error message and die
            end
    end,
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
                       ProtoOpts = [{aes_key, AESKey}],
                       erlonion_dir;
                   path ->
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
                       DirAddr = erlonion_path:register_node(Transport, PrivKey, PubKey, AESKey, IVec),
                       ProtoOpts = [{dir_addr, DirAddr}, {priv_key, PrivKey}, {pub_key, PubKey}, {aes_key, AESKey}],
                       erlonion_path;
                   _ -> ProtoOpts = [], error % print error message and die
               end,
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

pub_encrypt_message(#'RSAPublicKey'{modulus=M, publicExponent=PubE}, Message) ->
    io:format("size of message: ~p~n", [byte_size(Message)]),
    crypto:public_encrypt(rsa, Message, [PubE, M], rsa_pkcs1_padding).

priv_decrypt_message(#'RSAPrivateKey'{version=_V, modulus=M, publicExponent=PubE,
                                      privateExponent=PrivE, prime1=P1, prime2=P2,
                                      exponent1=E1, exponent2=E2, coefficient=C,
                                      otherPrimeInfos=_O}, Message) ->
    crypto:private_decrypt(rsa, Message, [PubE, M, PrivE, P1, P2, E1, E2, C], rsa_pkcs1_padding).
