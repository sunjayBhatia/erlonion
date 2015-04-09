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
    Port = case application:get_env(port) of
               {ok, P} -> P;
               _ -> 0
           end,
    TransOpts = [{port, Port}],
    Protocol = erlonion_protocol,
    ProtoOpts = [], % ??
    case ranch:start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) of
        {ok, _Pid} -> ok;
        {error, badarg} -> error % print error message and die
    end,
    erlonion_sup:start_link().

stop(_State) ->
    ok.
