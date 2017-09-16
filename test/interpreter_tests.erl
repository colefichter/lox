-module(interpreter_tests).

-compile([export_all]).

-define(EXT, ".out").

% Unit tests
%-------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").


setup() -> ok.
cleanup(_) -> ok.

programs_test_() ->
    Funs = test_util:generate_asserts_from_files("data/program_results/*" ++ ?EXT, ?EXT, fun ?MODULE:exec/1),
    {foreach, fun setup/0, fun cleanup/1, Funs}.


% Test callback
exec(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, _Env} = interpreter:init(), % Create the global env. This will stay alive as long as the interpreter/REPL is alive.
    interpreter:interpret(Bin),
    ok.