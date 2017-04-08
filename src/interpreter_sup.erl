-module(interpreter_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 1000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    MaxRestart = 1000,
    MaxTime = 1000,
    InterpreterServer = ?CHILD(interpreter, worker),
    {ok, {{one_for_one, MaxRestart, MaxTime}, [InterpreterServer]}}.