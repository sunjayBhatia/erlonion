%% ===================================================================
%% erlonion_app.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, recv_loop/4, get_env_val/2]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % get encryption key seed and generate key

    io:format("start erlonion_app~n", []),
    % local storage for connected process info
    % ranch options
    Ref = erlonion_listener,
    NbAcceptors = get_env_val(num_acceptors, 20),
    Transport  = ranch_tcp,
    Port = get_env_val(port, 0),
    TransOpts = [{port, Port}],
    Protocol = case get_env_val(type, path) of
                   directory ->
                       TableOpts = [public, named_table],
                       erlonion_pathnodes = ets:new(erlonion_pathnodes, TableOpts),
                       erlonion_dir;
                   path ->
                       ok = erlonion_path:register_node(Transport),
                       erlonion_path;
                   _ -> error % print error message and die
               end,
    ProtoOpts = [],
    {ok, _Pid} = ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts),
    io:format("started ranch listener~n", []),
    erlonion_sup:start_link().

stop(_State) ->
    ok.

recv_loop(Transport, SockRec, Timeout, RetData) ->
    case Transport:recv(SockRec, 0, Timeout) of
        {ok, Data} ->
            io:format("got some data~n"),
            recv_loop(Transport, SockRec, Timeout, <<RetData/binary, Data/binary>>);
        _ ->
            io:format("error or end of stream~n"),
            RetData
    end.

get_env_val(Key, Default) ->
    case application:get_env(Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.
