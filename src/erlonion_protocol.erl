%% ===================================================================
%% erlonion_protocol.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4]).

%% Gen Server Callbacks
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(TIMEOUT, 5000).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {socket, transport}).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport}, ?TIMEOUT).

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport}) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    io:format("data: ~p~n", [erlonion_parse:http_request(Data)]),
    % start new process to handle data?
        % parse request and get the hostname of server we want content from
        % connect to server, send http request
    Transport:send(Socket, Data), % do case
    {noreply, State, ?TIMEOUT};
% handl_info({this_from_childsdjfksh, Socket, <<HTTP/>>})
    % we have our tcp connection and our response, send back to client on socket
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    io:format("timeout~n", []),
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
