%% ===================================================================
%% erlonion_sup.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_sup).
-behaviour(supervisor).

%% API
-export([start_link/2, start_msghandler/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Protocol, Transport) ->
    case erlang:function_exported(Protocol, register_node, 1) of
        true -> Protocol:register_node(Transport);
        false -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_msghandler() ->
    supervisor:start_child(?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % local storage for connected process info
    TableOpts = [ordered_set, public, named_table],
    erlonion_ets = ets:new(erlonion_ets, TableOpts),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    AChild = {erlonion_msghandler, {erlonion_msghandler, start_link, []},
                Restart, Shutdown, Type, [erlonion_msghandler]},
    {ok, {SupFlags, [AChild]}}.
