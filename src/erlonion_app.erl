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
    Ref = erlonion,
    NbAcceptors = get_env_val(num_acceptors, 20),
    Transport  = ranch_tcp,
    Port = get_env_val(port, 0),
    TransOpts = [{port, Port}],
    Protocol = case get_env_val(type, path) of
                   directory -> erlonion_directory;
                   path -> erlonion_path;
                   _ -> error % print error message and die
               end,
    ProtoOpts = [],
    case ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) of
        {ok, _Pid} -> ok;
        _ -> error % print error message and die
    end,
    erlonion_sup:start_link(Protocol, Transport).

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
