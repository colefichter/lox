-module(lox_callable).

-export([call/4, is_instance/1]).

-include("records.hrl").


is_instance({lox_instance, _Name, _StateRef}) -> true;
is_instance(_AnythingElse) -> false.


% The java version passes the interpreter as a parameter when invoking a function call...
call(Interpreter, Callee, Arguments, T) ->
    invoke(Interpreter, Callee, Arguments, T).

% This match is for class instantiation, for example: var x = Bagel(); //Bagel should be defined as a class.
invoke(_Interpreter, {class, Name, _Methods}=Class, _Arguments, _T) ->
    %Instantiate an instance of the class.
    environment:register_class(Class),
    %TODO: constructors go here eventually?
    % The methods live in the class and end up in the environment. Let's put a state dict in here (this is how the author does it).
    R = environment:init_object_state(),
    ReturnVal = {lox_instance, Name, R}, % TODO: this will change when we add constructor logic...
    ReturnVal;


invoke(Interpreter, {native_function, {M, F, Parameters}}, Arguments, T) ->
    fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T),
    % Native functions are always declared in the global scope:
    PreviousScope = environment:create_new_scope(T),
    define_all(Parameters, Arguments),
    ReturnVal = try erlang:apply(M, F, Arguments) of
        Any -> Any
    after
        environment:replace_scope(PreviousScope)
    end,
    ReturnVal;

invoke(Interpreter, {function_decl, _Name, Parameters, Body, Closure}, Arguments, T) ->
    fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T),
    % Programmer-defined functions run in the scope they are declared in:
    PreviousScope = environment:create_new_scope(Closure, T),
    define_all(Parameters, Arguments),
    ReturnVal = try Interpreter:visit(Body) of % Body should be a block AST node.
        ok -> nil
    catch
        {return, Val} ->
            Val
    after
        environment:replace_scope(PreviousScope)
    end,
    ReturnVal.

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