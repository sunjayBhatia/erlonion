%% ===================================================================
%% erlonion_path.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_path).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4]).

%% Gen Server Callbacks
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(TIMEOUT, 5000).
-define(TAB, erlonion_ets).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link(Ref, Sock, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Sock, Transport, Opts]).


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
    ok = Transport:setopts(Sock, [{active, once}]),
    {ok, MsgHandlerPid, MsgHandlerId} = erlonion_sup:start_path_msghandler(),
    gen_server:cast(MsgHandlerPid, {tcp, self(), Data, Transport}),
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
