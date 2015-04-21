%% ===================================================================
%% erlonion_app.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("start erlonion_app~n", []),
    % local storage for connected process info
    TableOpts = [public, named_table],
    erlonion_keytab = ets:new(erlonion_keytab, TableOpts),
    % ranch options
    Ref = erlonion_listener,
    NbAcceptors = get_env_val(num_acceptors, 20),
    Transport  = ranch_tcp,
    Port = get_env_val(port, 0),
    TransOpts = [{port, Port}],
    Protocol = case get_env_val(type, path) of
                   directory -> erlonion_dir;
                   path ->
                       % ok = register_node(Transport),
                       erlonion_path;
                   _ -> error % print error message and die
               end,
    ProtoOpts = [],
    {ok, _Pid} = ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts),
    io:format("started ranch listener~n", []),
    erlonion_sup:start_link().

stop(_State) ->
    ok.


%% ===================================================================
%% Internal Functions
%% ===================================================================

get_env_val(Key, Default) ->
    case application:get_env(Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

% register_node(Transport) ->
    % {HostName, Port} = case erlonion_app:get_env_val(dir_addr, {error, none}) of
    %                        {error, none} -> {error, none}; % throw/print error and die
    %                        DirAddr -> DirAddr
    %                    end,
    % {ok, MsgHandlerPid} = erlonion_sup:start_path_msghandler(),
    % gen_server:cast(MsgHandlerPid, {tcp_msg, self(), Data, Transport}),
    % ok.

    % {ok, {hostent, HName, _, _, _, _}} = inet:gethostbyname(HostName),
    % case gen_tcp:connect(HName, Port, [binary, {active, once}, {nodelay, true}, {packet, raw}], 5000) of
    %     {ok, NewSock} ->
    %         Transport:send(NewSock, "");
    %     _ ->
    %         io:format("timed out or error connecting to directory node~n")
    % end,
    % ok.
