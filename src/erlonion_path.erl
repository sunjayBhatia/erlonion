%% ===================================================================
%% erlonion_path.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_path).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, register_node/4]).

%% Gen Server Callbacks
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(TIMEOUT, 5000).
-define(TCP_OPTS, [binary, {active, false}, {nodelay, true}, {reuseaddr, true}, {packet, raw}]).
-define(RECV_TIMEOUT, 1000).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link(Ref, Sock, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Sock, Transport, Opts]).

register_node(Transport, PrivKey, PubKey, AESKey) ->
    {IpAddr, Port} = case erlonion_app:get_env_val(dir_addr, {error, none}) of
                           {error, none} -> {error, none}; % throw/print error and die
                           DirAddr -> DirAddr
                       end,
    case gen_tcp:connect(IpAddr, Port, ?TCP_OPTS, ?TIMEOUT) of
        {ok, NewSock} ->
            Transport:send(NewSock, <<"REGISTER">>),
            DirPubKeyBin = erlonion_app:recv_loop(Transport, NewSock, 2000, <<>>),
            DirPubKey = erlonion_parse:destringify_rsa_public(binary_to_list(DirPubKeyBin)),
            PrivKeyStr = erlonion_parse:stringify_rsa_private(PrivKey),
            PubKeyStr = erlonion_parse:stringify_rsa_private(PubKey),
            AESKeyStr = binary_to_list(AESKey),
            RegMessage = erlonion_app:pub_encrypt_message(DirPubKey, [PrivKeyStr, PubKeyStr, AESKeyStr]),
            Transport:send(NewSock, RegMessage);
        _ -> % print error and die
            io:format("timed out or error connecting to directory node~n")
    end,
    ok.


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {socket, transport, msghandlers}).

init([]) -> {ok, undefined}.

init(Ref, Sock, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Sock, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Sock, transport=Transport, msghandlers=[]},
        ?TIMEOUT).

handle_info({tcp, Sock, Data}, State=#state{socket=Sock, transport=Transport, msghandlers=MsgHandlers}) ->
    DataRest = erlonion_app:recv_loop(Transport, Sock, ?RECV_TIMEOUT, <<>>),
    % ask for a path
    ok = Transport:setopts(Sock, [{active, once}]),
    {ok, MsgHandlerPid, MsgHandlerId} = erlonion_sup:start_path_msghandler(),
    gen_server:cast(MsgHandlerPid, {tcp, self(), <<Data/binary, DataRest/binary>>, Transport}),
    {noreply, State#state{msghandlers=[MsgHandlerId | MsgHandlers]}, ?TIMEOUT};
handle_info(Info, State=#state{socket=_, transport=_, msghandlers=MsgHandlers}) ->
    % lists:map(fun erlonion_sup:stop_child/1, MsgHandlers),
    case Info of
        {tcp_closed, _Sock} -> {stop, normal, State};
        {tcp_error, _, Reason} -> {stop, Reason, State};
        _ -> {stop, normal, State}
    end.

handle_cast({http_response, Data}, State=#state{socket=Sock, transport=Transport, msghandlers=_}) ->
    Transport:send(Sock, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
