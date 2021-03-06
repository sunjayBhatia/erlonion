%% ===================================================================
%% erlonion_sup.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_path_msghandler/0, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(RESTART_STRATEGY, one_for_one).
-define(MAX_RESTARTS, 10).
-define(MAX_SECS_BTW_RESTARTS, 10).
-define(SUP_FLAGS, {?RESTART_STRATEGY, ?MAX_RESTARTS, ?MAX_SECS_BTW_RESTARTS}).
-define(RESTART, permanent).
-define(SHUTDOWN, brutal_kill).
-define(TYPE, worker).
-define(PATH_MSGHANDLER_CHILD(Id), {{erlonion_path_msghandler, Id}, {erlonion_path_msghandler, start_link, []},
                                   ?RESTART, ?SHUTDOWN, ?TYPE, [erlonion_path_msghandler]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_path_msghandler() ->
    Id = ets:update_counter(erlonion_subproc_ids, path_msghandler, 1),
    {ok, Pid} = supervisor:start_child(?MODULE, ?PATH_MSGHANDLER_CHILD(Id)),
    {ok, Pid, {erlonion_path_msghandler, Id}}.

stop_child(ChildId) ->
    supervisor:terminate_child(erlonion_sup, ChildId),
    supervisor:delete_child(erlonion_sup, ChildId).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % local storage for generating unique process ids
    TableOpts = [public, named_table],
    ets:new(erlonion_subproc_ids, TableOpts),
    ets:insert(erlonion_subproc_ids, {path_msghandler, -1}),
    {ok, {?SUP_FLAGS, []}}.
