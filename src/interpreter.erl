-module(interpreter).

-export([init/0, interpret/1, visit/1, error/2, error/4, warn/4, rte/3]).

-include("records.hrl").


init() ->
    ok = environment:init(),
    loader:load_all(), % These end up in the global scope.
    Env = environment:current(),
    {ok, Env}.

interpret(BinOrSourceCode) ->
    {ok, Tokens} = scanner:lex(BinOrSourceCode),
    run_parser(Tokens),
    ok.

run_parser(Tokens) ->
    try parser:parse(Tokens) of
        {ok, Statements} ->
            run_resolver(Statements)
    catch
        {parse_error, Message, Line, Literal} ->
            ?MODULE:error(parse_error, Line, Literal, Message);
        {parse_error, Message} ->
            ?MODULE:error(parse_error, Message);
         error:Reason ->
            ?MODULE:error(parser_crashed, Reason)
    end,
    ok.

run_resolver(Statements) ->
    try resolver:run(Statements) of
        ok ->
            interpret_statements(Statements)
    catch
        {resolve_error, Line, Literal, Message} ->
            ?MODULE:error(resolve_error, Line, Literal, Message);
        error:Reason ->
            ?MODULE:error(resolver_crashed, Reason)
    end,
    ok.

interpret_statements([]) -> ok;
interpret_statements([S|Statements]) ->
    try visit(S) of
        _Any -> ok % Some statements, like assignment, will return an actual value. Ignore it.
    catch
        {runtime_error, Type, Message, Line, Literal} ->            
            ?MODULE:error(Type, Line, Literal, Message),
            io:format("  ~s:~p~n", [color:cyan("STATEMENT"), S]);
        error:Reason ->
            ?MODULE:error(interpreter_crashed, Reason),
            io:format("  ~s:~p~n", [color:cyan("STATEMENT"), S]),
            print_stacktrace()
    end,
    interpret_statements(Statements).


%%%%%%%%%%%%%%%%%%%%%
% Statements
%%%%%%%%%%%%%%%%%%%%%

visit({dumpenv, T}) ->
    environment:dump(T#t.line),
    ok;

visit({class_stmt, Name, Methods, T}) ->
    % This two-stage binding looks strange, but allows use of the class name inside its declaration.
    environment:define(Name, nil), 
    Class = {class, Name, Methods},
    environment:assign(Name, Class, T),
    ok;

visit({function_decl, Name, Parameters, Body, T}) ->
    Closure = environment:current(),
    NewF = {function_decl, Name, Parameters, Body, Closure},
    environment:define(Name, NewF),
    ok;

% A statement declaring a new variable (not to be confused with a variable expression, which looks up the value of a variable)
visit({var_stmt, Id, InitilizerExpr, _T}) -> 
    Val = visit(InitilizerExpr),
    environment:define(Id, Val),
    ok;

visit({if_stmt, ConditionalExpr, ThenBranch, ElseBranch, _T}) ->
    CVal = visit(ConditionalExpr),
    eval_if(CVal, ThenBranch, ElseBranch),
    ok;

visit({while_stmt, ConditionalExpr, LoopBody, _T}=AST) ->
    case visit(ConditionalExpr) of
        true -> 
            visit(LoopBody),
            visit(AST);
        false -> ok
    end,
    ok;

% Multiple print expressions isn't working correctly from inside a function:
%    fun x() { print 1; } //crashes!
% visit({print_stmt, Expressions, _}) ->
%     Values = [visit(E) || E <- Expressions],
%     [pretty_print(V) || V <- Values],
%     ok;
visit({print_stmt, Expr, _T}) ->
    Val = visit(Expr),
    pretty_print(Val),
    ok;

visit({block, Statements, _T}) ->
    environment:enclose(),
    % [visit(S) || S <- Statements],  %TODO: try/catch here? What if a statement throws an error?
    interpret_statements(Statements),
    environment:unenclose(),
    ok;

visit({expr_stmt, Expr, _T}) ->
    visit(Expr),
    ok;

% We'll use exceptions to return to the caller from any point in a function.
visit({return_stmt, nil, _T}) ->
    throw({return, nil});
visit({return_stmt, Expr, _T}) ->
    ReturnVal = visit(Expr),
    throw({return, ReturnVal});

%%%%%%%%%%%%%%%%%%%%%
% Expressions
%%%%%%%%%%%%%%%%%%%%%

% This is the invocation of a function
visit({call, CalleeExpr, Arguments, T}) ->
    Callee = visit(CalleeExpr),
    % TODO: how to check the type of the callee? See http://www.craftinginterpreters.com/functions.html#call-type-errors
    ArgumentVals = [visit(A) || A <- Arguments],
    lox_callable:call(?MODULE, Callee, ArgumentVals, T);

% Assignment expression (e.g. "a = 1;"). Name is the variable name to in which to store the evaluated results of Value.
visit({assign, R, Name, Value, T}) ->
    Val = visit(Value),
    environment:assign(R, Name, Val, T),
    % Return the assigned value so that the following works:
    %  var a = 1;
    %  print a = 2; //"2"
    Val; 

visit({LExp, logic_or, RExp, _T}) ->
    LVal = visit(LExp),
    case is_true(LVal) of
        true -> LVal;
        false -> visit(RExp)
    end; %Will return val with appropriate truthiness but may not actually be true/false. See not in Control Flow chapter.

visit({LExp, logic_and, RExp, _T}) ->
    LVal = visit(LExp),
    case is_true(LVal) of
        false -> LVal;
        true -> visit(RExp)
    end; %Will return val with appropriate truthiness but may not actually be true/false. See not in Control Flow chapter.

visit({conditional, ConditionalExpr, ThenBranch, ElseBranch, _T}) ->
    CVal = visit(ConditionalExpr),
    eval_if(CVal, ThenBranch, ElseBranch);

visit({binary, LExp, Op, RExp, T}) ->
    LVal = visit(LExp),
    RVal = visit(RExp),
    case Op of
        % Our == and != implementation is easier than in the book because we don't need to worry about calling .equals()
        % on a null reference. Here, nil == nil is fine and something like nil == "test" will be false with no error.
        bang_equal  -> LVal /= RVal;
        equal_equal -> LVal == RVal;
        greater -> 
            check_number_operands(Op, LVal, RVal, T),
            LVal > RVal;
        greater_equal -> 
            check_number_operands(Op, LVal, RVal, T),
            LVal >= RVal;
        less ->
            check_number_operands(Op, LVal, RVal, T),
            LVal < RVal;
        less_equal -> 
            check_number_operands(Op, LVal, RVal, T),
            LVal =< RVal;
        slash ->
            check_number_operands(Op, LVal, RVal, T),
            check_non_zero(Op, RVal, T),
            LVal / RVal;
        star  ->
            check_number_operands(Op, LVal, RVal, T),
            LVal * RVal;
        minus -> subtractOrTrim(LVal, RVal, T);
        plus  -> addOrConcat(LVal, RVal, T)
    end;

visit({unary, Op, RExp, T}) ->
    RVal = visit(RExp),
    case Op of
        minus -> 
            check_number_operand(Op, RVal, T),
            -RVal;
        bang  -> 
            check_boolean_operand(Op, RVal, T),
            not is_true(RVal)
    end;

% Prefix/postfix operators here don't work quite like C++. They are expressions only (rather than statements). 
% No side-effects. Also, they are equivalent.
visit({prefix, Op, RExp, T}) ->
    RVal = visit(RExp),
    check_number_operand(Op, RVal, T),
    case Op of
        minus_minus ->
            RVal - 1;
        plus_plus ->
            RVal + 1
    end;
% visit({postfix, LExp, Op, T}) ->
%     LVal = visit(LExp),
%     check_number_operand(Op, LVal, T),
%     case Op of
%         minus_minus ->
%             LVal - 1;
%         plus_plus ->
%             LVal + 1
%     end;


visit({grouping, E, _T}) ->
    visit(E);

visit({variable, R, Id, T}) -> % A variable expression (that is, lookup the value of the variable)
    % Val = environment:get(Id, T),
    Val = environment:get(R, Id, T),   
    Val;

visit({literal, Val, _T}) -> Val.   


eval_if(true, ThenBranch, _ElseBranch) ->
    visit(ThenBranch);
eval_if(false, _ThenBranch, nil) -> 
    ok;   
eval_if(false, _ThenBranch, ElseBranch) ->
    visit(ElseBranch).


addOrConcat(LVal, RVal, _) when is_list(LVal) and is_list(RVal) ->
    LVal ++ RVal;
addOrConcat(LVal, RVal, _) when is_number(LVal) and is_number(RVal) ->
    LVal + RVal;
addOrConcat(_, _, T) ->
    rte(type_mismatch, "Operands must both be numbers or strings", T).

subtractOrTrim(LVal, RVal, _T) when is_list(LVal) and is_list(RVal) ->
    LVal -- RVal;
subtractOrTrim(LVal, RVal, _T) when is_number(LVal) and is_number(RVal) ->
    LVal - RVal;
subtractOrTrim(_, _, T) ->
    rte(type_mismatch, "Operands must both be numbers or strings", T).

% Pretty simple: nil and false are false, everything else is true.
is_true(nil) -> false;
is_true(false) -> false;
is_true(_) -> true.


%TODO: clean up unused _Op params.

% ERROR HANDLING STUFF
check_number_operand(_Op, V, _T) when is_number(V) -> ok;
check_number_operand(_Op, _, T) ->
    rte(type_mismatch, "Operand must be a number", T).

% check_number_operands(_Op, LVal, RVal) when is_number(LVal) and is_number(RVal) -> ok;
% check_number_operands(Op, _, _) ->
%     rte(type_mismatch, "Operands must be numbers", Op).
check_number_operands(_Op, LVal, RVal, _T) when is_number(LVal) and is_number(RVal) -> ok;
check_number_operands(_Op, _, _, T) ->
    rte(type_mismatch, "Operands must be numbers", T).
    
check_boolean_operand(_Op, true, _T) -> ok;
check_boolean_operand(_Op, false, _T) -> ok;
check_boolean_operand(_Op, _, T) ->
    rte(type_mismatch, "Operand must be a Boolean", T).

check_non_zero(_Op, 0, T) ->
    rte(divide_by_zero, "Divide by zero is invalid", T);
check_non_zero(_, _, _T) -> ok.


% TODO: can we remove the Op param now that we have the token?
rte(Type, Message, T) ->
    throw({runtime_error, Type, Message, T#t.line, T#t.literal}).


% UTILS - TODO: move out of this module. The it doesn't make sense for the parser to call interpreter:error().
error(interpreter_crashed, Reason) ->
    Out = io_lib:format("INTERPRETER CRASHED| ~p at unknown location.~n", [Reason]),
    highlight(Out);
error(Line, Message) when is_integer(Line) -> 
    Out = io_lib:format("~p| ~s at unknown location.~n", [Line, Message]),
    highlight(Out);
error(Type, Message) -> 
    Out = io_lib:format("(~p) ~s at unknown location.~n", [Type, Message]),
    highlight(Out).
error(Type, Line, Literal, Message) ->
    Out = io_lib:format("~p| (~p) ~s near ~p.~n", [Line, Type, Message, Literal]),
    highlight(Out).

warn(Type, Line, Literal, Message) ->
    Out = io_lib:format("~p| (~p) ~s near ~p.~n", [Line, Type, Message, Literal]),
    io:format("~s", [color:yellow(Out)]).

highlight(Message) -> io:format("~s", [color:red(Message)]).


pretty_print({class, Name, _Methods}) ->
    io:format("~s~n", [Name]);
pretty_print({lox_instance, Name}) ->
    io:format("instance of ~s~n", [Name]);
pretty_print(V) when is_list(V) ->
    io:format("~s~n", [V]);
pretty_print(V) ->
    io:format("~p~n", [V]).


print_stacktrace() ->
    io:format("*** BEGIN STACKTRACE ***~n"),
    print_stacktrace(erlang:get_stacktrace()).

print_stacktrace([]) ->
    io:format("***  END STACKTRACE  ***~n");
print_stacktrace([H|T]) ->
    print_stackitem(H),
    print_stacktrace(T).

print_stackitem({M, F, A, L}) when is_integer(A) ->
    io:format("~p ~p/~p~n  ~p~n", [M, F, A, L]);
print_stackitem({M, F, A, L}) when is_list(A) ->
    io:format("~p ~p~n  ARGS: ~p~n  ~p~n", [M, F, A, L]).