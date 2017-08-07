-module(lox_callable).

-export([call/4]).

-include("records.hrl").

% The java version passes the interpreter as a parameter when invoking a function call...
call(Interpreter, Callee, Arguments, T) ->
    invoke(Interpreter, Callee, Arguments, T).


invoke(Interpreter, {native_function, {M, F, Parameters}}, Arguments, T) ->
    fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T),
    erlang:apply(M, F, Arguments);

invoke(Interpreter, {function_decl, _Name, Parameters, Body}, Arguments, T) ->
    fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T),

    % TODO: in the book, the env is always based on globals... is this right?
    environment:enclose(),
    define_all(Parameters, Arguments),
    % TODO: is hte execute_block function even needed?
    % Interpreter:execute_block()
    Interpreter:visit(Body), % Body should be a block AST node.
    environment:unenclose(),
    ok_what_return_value.

fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T) when length(Parameters) =/= length(Arguments) ->
    Message = io_lib:format("Wrong number of arguments. Expected ~p but got ~p", [length(Parameters), length(Arguments)]),
    Interpreter:rte(function_arity, Message, T);
fail_on_argument_mismatch(_Interpreter, _Parameters, _Arguments, _T) ->
    ok.

define_all([], []) -> 
    ok;
define_all([P|Parameters], [A|Arguments]) ->
    environment:define(P, A),
    define_all(Parameters, Arguments).