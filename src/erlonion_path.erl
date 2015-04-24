%% ===================================================================
%% erlonion_path.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_path).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, register_node/5]).

%% Gen Server Callbacks
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(PORT, 8080).
-define(TIMEOUT, 5000).
-define(TCP_OPTS, [binary, {active, false}, {nodelay, true}, {reuseaddr, true}, {packet, raw}]).
-define(RECV_TIMEOUT, 1000).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link(Ref, Sock, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Sock, Transport, Opts]).

register_node(Transport, PrivKey, PubKey, AESKey, IVec) ->
    IpAddr = case erlonion_app:get_env_val(dir_addr, {error, none}) of
                {error, none} -> {error, none}; % throw/print error and die
                DirAddr -> DirAddr
            end,
    case gen_tcp:connect(IpAddr, ?PORT, ?TCP_OPTS, ?TIMEOUT) of
        {ok, NewSock} ->
            PubKeyBin = list_to_binary(erlonion_parse:stringify_rsa_public(PubKey)),
            Transport:send(NewSock, <<"REGISTER", PubKeyBin/binary>>),
            DirAESKeyBin = erlonion_app:recv_loop(Transport, NewSock, 2000, <<>>),
            DirAESKey = erlonion_app:priv_decrypt_message(PrivKey, DirAESKeyBin),
            AESKeyCrypt = crypto:block_encrypt(aes_cfb128, DirAESKey, IVec, AESKey),
            Transport:send(NewSock, <<AESKeyCrypt/binary>>),
            io:format("AESKey: ~p~n", [AESKey]);
        _ -> % print error and die
            io:format("timed out or error connecting to directory node~n")
    end,
    IpAddr.


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {socket, transport, dir_addr, aes_key, i_vec, msghandlers}).

init([]) -> {ok, undefined}.

init(Ref, Sock, Transport, [{dir_addr, DirAddr}, {aes_key, AESKey},
                             {i_vec, IVec}]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Sock, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Sock, transport=Transport, dir_addr=DirAddr, aes_key=AESKey, i_vec=IVec, msghandlers=[]},
        ?TIMEOUT).

handle_info({tcp, Sock, Data}, State) ->
    Transport = State#state.transport, MsgHandlers = State#state.msghandlers,
    DataRest = erlonion_app:recv_loop(Transport, Sock, ?RECV_TIMEOUT, <<>>),
    ok = Transport:setopts(Sock, [{active, once}]),
    {ok, MsgHandlerPid, MsgHandlerId} = erlonion_sup:start_path_msghandler(),
    gen_server:cast(MsgHandlerPid, {tcp, self(), <<Data/binary, DataRest/binary>>, Transport,
                                    State#state.dir_addr, State#state.aes_key, State#state.i_vec}),
    {noreply, State#state{msghandlers=[MsgHandlerId | MsgHandlers]}, ?TIMEOUT};
handle_info(Info, State) ->
    % lists:map(fun erlonion_sup:stop_child/1, MsgHandlers),
    case Info of
        {tcp_closed, _Sock} -> {stop, normal, State};
        {tcp_error, _, Reason} -> {stop, Reason, State};
        _ -> {stop, normal, State}
    end.

handle_cast({response, Data}, State) ->
    Transport = State#state.transport,
    Transport:send(State#state.socket, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
