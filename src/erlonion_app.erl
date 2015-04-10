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
    % lager:start(),
    % lager:error("Some message"),

    Ref = erlonion,
    NbAcceptors = 100, % get from application env?
    Transport  = ranch_tcp,
    Port = get_env_val(port, 0),
    TransOpts = [{port, Port}],
    Protocol = erlonion_protocol,
    ProtoOpts = [], % ??
    case ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) of
        {ok, _Pid} -> ok;
        {error, badarg} -> error % print error message and die
    end,
    % ranch takes care of supervising our tcp listen/accept processes
    % we need to supervise the message parsing/fetching processes
    erlonion_msghandler_sup:start_link().

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
