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


%% ===================================================================
%% API Functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {s}).

init(_) ->
    {ok, #state{s=0}}.

handle_info({tcp, _Sock, Data}, State) ->
    io:format("tcp message: ~p~n", [Data]),
    {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

recv_loop(Transport, SockRec, RetData) ->
    case Transport:recv(SockRec, 0, 5000) of
        {ok, Data} ->
            recv_loop(Transport, SockRec, <<RetData/binary, Data/binary>>);
        _ -> RetData
    end.

handle_cast({tcp_msg, Parent, Data = <<"GET", _Rest/binary>>, Transport}, State) ->
    HostName = erlonion_parse:http_get_fieldval(<<"Host">>, erlonion_parse:http_request(Data), <<>>),
    {ok, {hostent, HName, _, _, _, _}} = inet:gethostbyname(HostName),
    case gen_tcp:connect(HName, 80, [binary, {active, false}, {nodelay, true}, {packet, raw}], 5000) of
        {ok, NewSock} ->
            Transport:send(NewSock, Data),
            gen_server:cast(Parent, {http_response, recv_loop(Transport, NewSock, <<>>)});
        _ ->
            io:format("timed out or error connecting to server~n"),
            gen_server:cast(Parent, timeout)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
