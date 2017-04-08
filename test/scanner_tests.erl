-module(scanner_tests).
-compile([export_all]).

-define(EXT, ".tokens").

% Unit tests
%-------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

setup() -> ok.
cleanup(_) -> ok.

statments_test_() ->
    Funs = test_util:generate_asserts_from_files("data/statements/*" ++ ?EXT, ?EXT, fun ?MODULE:exec/1),
    {foreach, fun setup/0, fun cleanup/1, Funs}.


% Test callback
exec(File) ->
    {ok, Tokens} = scanner:lex_file(File),
    Tokens.