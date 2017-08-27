-module(lox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, repl/0, run_file/1]).

repl() -> application:start(lox).

run_file([Name]) when is_atom(Name) ->
    run(atom_to_list(Name));
run_file([Name]) when is_list(Name) ->
    run(Name).

run(Name) ->
    Path = format_name(Name),
    case file:read_file(Path) of
        {ok, Bin} ->
            {ok, Env} = interpreter:init(), % Create the global env. This will stay alive as long as the interpreter/REPL is alive.
            interpreter:interpret(Bin);
        _ ->
            io:format("File not found: ~p~n", [Path])
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) -> interpreter_sup:start_link().

stop(_State) -> ok.


% UTIL
format_name(Name) ->
    Name1 = case string:sub_string(Name, 1, 9) of
        "programs/" -> Name;
        _any -> "programs/" ++ Name
    end,
    Reversed = lists:reverse(Name1),
    case string:sub_string(Reversed, 1, 4) of
        "xol." -> Name1;
        _ -> Name1 ++ ".lox"
    end.