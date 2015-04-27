%% ===================================================================
%% erlonion_path_msghandler.erl
%% Sunjay Bhatia 4/10/2015
%% ===================================================================

-module(erlonion_path_msghandler).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(PORT, 8080).
-define(TIMEOUT, 5000).
-define(RECV_TIMEOUT, 1000).
-define(TCP_OPTS, [binary, {active, false}, {nodelay, true}, {reuseaddr, true}, {packet, raw}]).
-define(HTTP_PORT, 80).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

init(_) ->
    {ok, []}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast({tcp, Parent, Data, Transport, DirIP, AESKey, IVec}, State) ->
    case is_http_req(Data) of
        true ->
            case gen_tcp:connect(DirIP, ?PORT, ?TCP_OPTS, ?TIMEOUT) of
                {ok, DirSock} ->
                    Transport:send(DirSock, <<"PATH">>),
                    CryptPathBin = erlonion_app:recv_loop(Transport, DirSock, 2000, <<>>),
                    PathBin = crypto:block_decrypt(aes_cfb128, AESKey, IVec, CryptPathBin),
                    PathNodes = binary_to_term(PathBin),
                    [{SendIP, _} | _] = PathNodes,
                    TransReq = erlonion_parse:http_transform_req(Data),
                    HostName = erlonion_parse:http_get_fieldval(true, <<"Host">>, TransReq, <<>>),
                    case inet:gethostbyname(HostName) of
                        {ok, {hostent, _, _, _, _, [HostIP | _]}} ->
                            LayeredReq = erlonion_app:layer_encrypt_message(PathNodes, HostIP, TransReq, IVec),
                            case gen_tcp:connect(SendIP, ?PORT, ?TCP_OPTS, ?TIMEOUT) of
                                {ok, NewSock} ->
                                    Transport:send(NewSock, LayeredReq),
                                    Resp = erlonion_app:recv_loop(Transport, NewSock, 2000, <<>>),
                                    HTTPResp = erlonion_app:delayer_encrypt_resp(PathNodes, Resp, IVec),
                                    gen_server:cast(Parent, {response, HTTPResp});
                                _ -> gen_server:cast(Parent, timeout)
                            end;
                        _ -> gen_server:cast(Parent, invalid_host)
                    end;
                _ -> gen_server:cast(Parent, dir_node_unavailable)
            end;
        false ->
            {IP, Msg} = binary_to_term(crypto:block_decrypt(aes_cfb128, AESKey, IVec, Data)),
            case is_http_req(Msg) of
                true -> Port = ?HTTP_PORT;
                false -> Port = ?PORT
            end,
            case gen_tcp:connect(IP, Port, ?TCP_OPTS, ?TIMEOUT) of
                {ok, NewSock} ->
                    Transport:send(NewSock, Msg),
                    Resp = erlonion_app:recv_loop(Transport, NewSock, 2000, <<>>),
                    RespCrypt = crypto:block_encrypt(aes_cfb128, AESKey, IVec, Resp),
                    gen_server:cast(Parent, {response, RespCrypt});
                _ -> gen_server:cast(Parent, invalid_path_host_ip)
            end
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.



is_http_req(<<"GET", _Rest/binary>>) -> true;
is_http_req(<<"HEAD", _Rest/binary>>) -> true;
is_http_req(<<"POST", _Rest/binary>>) -> true;
is_http_req(<<"PUT", _Rest/binary>>) -> true;
is_http_req(<<"DELETE", _Rest/binary>>) -> true;
is_http_req(<<"TRACE", _Rest/binary>>) -> true;
is_http_req(<<"CONNECT", _Rest/binary>>) -> true;
is_http_req(_) -> false.
