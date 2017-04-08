-module(test_util).

-export([replace_extension/3, generate_asserts_from_files/3]).

% Unit tests
%-------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

generate_asserts_from_files(Path, Ext, TestFun) ->
    Files = filelib:wildcard(Path),
    lists:map(fun(F) -> build_test_fun(F, Ext, TestFun) end, Files).

build_test_fun(File, Ext, TestFun) ->
    fun() -> run(File, Ext, TestFun) end.

run(OutFile, Ext, TestFun) ->
    InputFile = test_util:replace_extension(OutFile, Ext, ".lox"),
    {ok, [Expected]} = file:consult(OutFile),
    Result = TestFun(InputFile),
    io:format("~p evaluating ~p~n", [TestFun, InputFile]),
    ?assertEqual(Expected, Result).

replace_extension(S, OldExt, NewExt) ->
    string:sub_string(S, 1, string:len(S)-string:len(OldExt)) ++ NewExt.