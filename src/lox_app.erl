-module(lox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, repl/0]).

repl() -> application:start(lox).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) -> interpreter_sup:start_link().

stop(_State) -> ok.