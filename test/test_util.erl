-module(test_util).

-export([convert_outpath_to_sourcepath/3, generate_asserts_from_files/3]).

% Unit tests
%-------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

generate_asserts_from_files(Path, Ext, TestFun) ->
    Files = filelib:wildcard(Path),
    lists:map(fun(F) -> build_test_fun(F, Ext, TestFun) end, Files).

build_test_fun(File, Ext, TestFun) ->
    fun() -> run(File, Ext, TestFun) end.

run(OutFile, Ext, TestFun) ->
    InputFile = convert_outpath_to_sourcepath(OutFile, Ext, ".lox"),
    {ok, Bin} = file:read_file(OutFile),
    Expected = binary_to_list(Bin),
    % Result = TestFun(InputFile),
    {ok, TestOutput} = exec_test(TestFun, InputFile),
    io:format("~p evaluating ~p~n", [TestFun, InputFile]),
    ?assertEqual(Expected, TestOutput).

convert_outpath_to_sourcepath(S, OldExt, NewExt) ->
    S1 = string:sub_string(S, 1, string:len(S)-string:len(OldExt)) ++ NewExt,
    Index = string:rchr(S1, $/), % Find last /
    "programs" ++ string:sub_string(S1, Index). % Returns something like "/hello.lox"

exec_test(TestFun, InputFile) ->
    {IoServer, OldGroupLeader} = capture_io(),
    TestFun(InputFile), % Run the unit test while output is being captured.
    ok = release_io(OldGroupLeader),
    capture:get_output(IoServer).

capture_io() ->
    IoServer = capture:start(),
    OldGroupLeader = erlang:group_leader(),
    erlang:group_leader(IoServer, self()), % Send io:format output to our IoServer
    timer:sleep(25),
    {IoServer, OldGroupLeader}.

release_io(OldGroupLeader) ->
    erlang:group_leader(OldGroupLeader, self()),
    timer:sleep(25),
    ok.

