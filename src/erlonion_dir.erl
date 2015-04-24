%% ===================================================================
%% erlonion_dir.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_dir).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4]).

%% Gen Server Callbacks
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(TIMEOUT, 5000).
-define(RECV_TIMEOUT, 1000).
-define(TAB, erlonion_pathnodes).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link(Ref, Sock, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Sock, Transport, Opts]).


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {socket, transport, priv_key, pub_key}).

init([]) -> {ok, undefined}.

init(Ref, Sock, Transport, [{priv_key, PrivKey}, {pub_key, PubKey}, _]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Sock, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Sock, transport=Transport, priv_key=PrivKey, pub_key=PubKey},
        ?TIMEOUT).

handle_info({tcp, Sock, Data}, State=#state{socket=Sock, transport=Transport,
                                            priv_key=PrivKey, pub_key=PubKey}) ->
    DataRest = erlonion_app:recv_loop(Transport, Sock, ?RECV_TIMEOUT, <<>>),
    ok = Transport:setopts(Sock, [{active, once}]),
    {ok, MsgHandlerPid, MsgHandlerId} = erlonion_sup:start_dir_msghandler(),
    Req = <<Data/binary, DataRest/binary>>,
    case Req of
        <<"REGISTER">> ->
            {ok, {IP, Port}} = inet:peername(Sock),
            io:format("ip: ~p, port: ~p~n", [IP, Port]),
            PubKeyStr = erlonion_parse:stringify_rsa_public(PubKey),
            Transport:send(Sock, <<PubKeyStr/binary>>);
            % send public key
            % wait for response, decrypt and add to list
        _ -> ok
    end,
    {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

