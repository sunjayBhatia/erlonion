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
-define(TIMEOUT, 5000).
-define(TCP_OPTS, [binary, {active, once}, {nodelay, true}, {reuseaddr, true}, {packet, raw}]).
-define(HTTP_SOCK, 80).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {parent, socket, transport}).

init(_) ->
    {ok, #state{parent=none, socket=none, transport=none}}.

handle_info({tcp, Sock, Data}, State=#state{parent=Parent, socket=Sock, transport=Transport}) ->
    ok = Transport:setopts(Sock, [{active, once}]),
    TransResp = erlonion_parse:http_transform_resp(Data),
    io:format("response: ~p~n", [Data]),
    io:format("trans resp: ~p~n", [TransResp]),
    gen_server:cast(Parent, {http_response, Data}),
    {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_cast({tcp, Parent, Data = <<"GET", _Rest/binary>>, Transport}, State) ->
    TransReq = erlonion_parse:http_transform_req(Data),
    % io:format("~p~n", [TransReq]),
    HostName = erlonion_parse:http_get_fieldval(true, <<"Host">>, TransReq, <<>>),
    {ok, {hostent, HName, _, _, _, _}} = inet:gethostbyname(HostName),
    case gen_tcp:connect(HName, ?HTTP_SOCK, ?TCP_OPTS, ?TIMEOUT) of
        {ok, NewSock} ->
            Transport:send(NewSock, Data),
            NewState = State#state{parent=Parent, socket=NewSock, transport=Transport};
        _ ->
            NewState = State#state{socket=none, transport=Transport},
            gen_server:cast(Parent, timeout)
    end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
