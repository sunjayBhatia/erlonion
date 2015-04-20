%% ===================================================================
%% erlonion_sup.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_path_msghandler/0, start_dir_msghandler/0]).

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
-define(PATH_MSGHANDLER_CHILD, {erlonion_path_msghandler, {erlonion_path_msghandler, start_link, []},
                                   ?RESTART, ?SHUTDOWN, ?TYPE, [erlonion_path_msghandler]}).
-define(DIR_MSGHANDLER_CHILD, {erlonion_dir_msghandler, {erlonion_dir_msghandler, start_link, []},
                                  ?RESTART, ?SHUTDOWN, ?TYPE, [erlonion_dir_msghandler]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_path_msghandler() ->
    supervisor:start_child(?MODULE, [?PATH_MSGHANDLER_CHILD]).

start_dir_msghandler() ->
    supervisor:start_child(?MODULE, [?DIR_MSGHANDLER_CHILD]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {?SUP_FLAGS, []}}.
