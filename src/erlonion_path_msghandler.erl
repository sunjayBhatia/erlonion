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
-define(RECV_TIMEOUT, 1000).
-define(TCP_OPTS, [binary, {active, false}, {nodelay, true}, {reuseaddr, true}, {packet, raw}]).
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

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast({tcp, Parent, Data, Transport}, State) ->
    % io:format("request: ~p~n", [Data]),
    TransReq = erlonion_parse:http_transform_req(Data),
    HostName = erlonion_parse:http_get_fieldval(true, <<"Host">>, TransReq, <<>>),
    case inet:gethostbyname(HostName) of
        {ok, {hostent, HName, _, _, _, _}} ->
            case gen_tcp:connect(HName, ?HTTP_SOCK, ?TCP_OPTS, ?TIMEOUT) of
                {ok, NewSock} ->
                    Transport:send(NewSock, erlonion_parse:http_flatten(TransReq)),
                    Resp = erlonion_app:recv_loop(Transport, NewSock, ?RECV_TIMEOUT, <<>>),
                    % io:format("response: ~p~n", [Resp]),
                    case Resp of
                        <<"HTTP", _Rest/binary>> ->
                            TransResp = erlonion_parse:http_transform_resp(Resp),
                            gen_server:cast(Parent, {http_response, erlonion_parse:http_flatten(TransResp)});
                        _ -> gen_server:cast(Parent, {http_response, Resp})
                    end,
                    NewState = State#state{parent=Parent, socket=NewSock, transport=Transport};
                _ ->
                    NewState = State#state{socket=none, transport=Transport},
                    gen_server:cast(Parent, timeout)
            end;
        _ ->
            NewState = State#state{socket=none, transport=Transport},
            gen_server:cast(Parent, invalid_host)
    end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
