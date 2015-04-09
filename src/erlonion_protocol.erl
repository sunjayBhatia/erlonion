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

-record(state, {socket, port, transport}).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
    io:format("opts: ~p~n", [Opts]),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Socket, port=proplists:get_value(port, Opts), transport=Transport},
        ?TIMEOUT).

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, port=Port, transport=Transport}) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    <<Type:4/binary, _Rest/binary>> = Data,
    io:format("type: ~p~n", [Type]),
    case Type of
        <<"GET ">> ->
            % start new process later
            HostName = erlonion_parse:http_get_host(erlonion_parse:http_request(Data)),
            io:format("hostname: ~p~n", [HostName]),
            {ok, {hostent, HName, _, _, _, [_HostIP | _]}} = inet:gethostbyname(HostName),
            io:format("host name: ~p~n", [HName]),
            case gen_tcp:connect(HName, Port, [binary, {active, false}, {packet, raw}], 5000) of
                {ok, NewSocket} ->
                    io:format("new socket: ~p~n", [NewSocket]),
                    Transport:send(NewSocket, Data),
                    {ok, Pkt} = Transport:recv(NewSocket, 5000, 5000),
                    io:format("server returned: ~p~n", [Pkt]),
                    Transport:send(Socket, Pkt);
                _ -> io:format("timed out or error connecting to server~n")
            end;
        <<"HTTP">> -> % HTTP response from destination server
            case Transport:send(Socket, Data) of
                ok -> ok;
                {error, _} -> io:format("error sending~n")
            end;
            % eventually this case could be encrypted responses from our onion network
        _D -> io:format("not get data: ~p~n", [_D]) % decode message and check unique header id, if there continue, else
                                    % throw message away and close the connection
    end,
    % start new process to handle data?
        % parse request and get the hostname of server we want content from
        % connect to server, send http request
    {noreply, State, ?TIMEOUT};
% handle_info({this_from_childsdjfksh, Socket, <<HTTP/>>})
    % we have our tcp connection and our response, send back to client on socket
handle_info({tcp_closed, _Socket}, State) ->
    io:format("tcp_closed~n", []),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    io:format("tcp_error~n", []),
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
