-module(lox_callable).

-export([call/4, is_instance/1]).

-include("records.hrl").

is_instance({lox_instance, _Name, _StateRef}) -> true;
is_instance(_AnythingElse) -> false.

% The java version passes the interpreter as a parameter when invoking a function call...
call(Interpreter, Callee, Arguments, T) ->
    invoke(Interpreter, Callee, Arguments, T).

% This match is for class instantiation, for example: var x = Bagel(); //Bagel should be defined as a class.
invoke(Interpreter, {class, Name, Methods}=Class, Arguments, T) ->
    environment:register_class(Class), %Instantiate an instance of the class.
    R = environment:init_object_state(),
    Instance = {lox_instance, Name, R},
    case find_init_method(Methods) of % If the class has a constructor, bind it and run it
        nil -> ok;
        InitMethod ->
            Initializer = Interpreter:bind_method_to_this(InitMethod, Instance),
            invoke(Interpreter, Initializer, Arguments, T),
            ok
    end,
    Instance;

invoke(Interpreter, {native_function, {M, F, Parameters}}, Arguments, T) ->
    fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T),
    PreviousScope = environment:create_new_scope(), % Native functions are always declared in the global scope
    define_all(Parameters, Arguments),
    ReturnVal = try erlang:apply(M, F, Arguments) of
        Any -> Any
    after
        environment:replace_scope(PreviousScope)
    end,
    ReturnVal;

invoke(Interpreter, {function_decl, Name, Parameters, Body, Closure}, Arguments, T) ->
    fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T),
    PreviousScope = environment:create_new_scope(Closure), % Programmer-defined functions run in the scope they are declared in
    define_all(Parameters, Arguments),
    ReturnVal = try Interpreter:visit(Body) of % Body should be a block AST node.
        ok -> 
            % constructors should always return a reference to "this", in case it's called directly, like: print f.init();
            % (init shouldn't explicitly return a value; the resolver throws an error if it does)
            case Name of 
                "init" -> environment:get("this", T);
                _Any -> nil % the function/method didn't have a return value
            end
    catch
        {return, Val} -> Val
    after
        environment:replace_scope(PreviousScope)
    end,
    ReturnVal.

fail_on_argument_mismatch(Interpreter, Parameters, Arguments, T) when length(Parameters) =/= length(Arguments) ->
    Message = io_lib:format("Wrong number of arguments. Expected ~p but got ~p", [length(Parameters), length(Arguments)]),
    Interpreter:rte(function_arity, Message, T);
fail_on_argument_mismatch(_Interpreter, _Parameters, _Arguments, _T) -> ok.

define_all([], []) -> ok;
define_all([P|Parameters], [A|Arguments]) ->
    environment:define(P, A),
    define_all(Parameters, Arguments).

find_init_method(Methods) ->
    case lists:keyfind("init", 2, Methods) of
        false -> nil;
        Method -> Method
    end.