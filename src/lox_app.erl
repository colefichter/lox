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
    % io:format("Name: ~s", [format_name(Name)]).
    Path = format_name(Name),
    interpreter:interpret_file(Path).


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