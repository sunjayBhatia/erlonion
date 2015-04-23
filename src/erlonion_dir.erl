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

-record(state, {socket, transport}).

init([]) -> {ok, undefined}.

init(Ref, Sock, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Sock, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Sock, transport=Transport},
        ?TIMEOUT).

handle_info({tcp, Sock, Data}, State=#state{socket=Sock, transport=Transport}) ->
    DataRest = erlonion_app:recv_loop(Transport, Sock, ?RECV_TIMEOUT, <<>>),
    ok = Transport:setopts(Sock, [{active, once}]),
    {ok, MsgHandlerPid, MsgHandlerId} = erlonion_sup:start_dir_msghandler(),
    gen_server:cast(MsgHandlerPid, {tcp, self(), <<Data/binary, DataRest/binary>>, Transport}),
    {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_cast({register_ack, Data}, State=#state{socket=Sock, transport=Transport}) ->
    Transport:send(Sock, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

