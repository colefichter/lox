-module(interpreter).

-export([visit/1, error/2, error/3]).

-include("records.hrl").


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
            not isTrue(RVal)
    end;

visit({grouping, E, _}) ->
    visit(E);

visit({literal, Val, _}) -> Val.   


addOrConcat(LVal, RVal, _) when is_list(LVal) and is_list(RVal) ->
    LVal ++ RVal;
addOrConcat(LVal, RVal, _) when is_number(LVal) and is_number(RVal) ->
    LVal + RVal;
addOrConcat(_, _, T) ->
    rte(type_mismatch, "Operands must both be numbers or strings", "+", T).

subtractOrTrim(LVal, RVal, _T) when is_list(LVal) and is_list(RVal) ->
    LVal -- RVal;
subtractOrTrim(LVal, RVal, _T) when is_number(LVal) and is_number(RVal) ->
    LVal - RVal;
subtractOrTrim(_, _, T) ->
    rte(type_mismatch, "Operands must both be numbers or strings", "-", T).

% Pretty simple: nil and false are false, everything else is true.
isTrue(nil) -> false;
isTrue(false) -> false;
isTrue(_) -> true.




% ERROR HANDLING STUFF
check_number_operand(_Op, V, _T) when is_number(V) -> ok;
check_number_operand(Op, _, T) ->
    rte(type_mismatch, "Operand must be a number", Op, T).

% check_number_operands(_Op, LVal, RVal) when is_number(LVal) and is_number(RVal) -> ok;
% check_number_operands(Op, _, _) ->
%     rte(type_mismatch, "Operands must be numbers", Op).
check_number_operands(_Op, LVal, RVal, _T) when is_number(LVal) and is_number(RVal) -> ok;
check_number_operands(Op, _, _, T) ->
    rte(type_mismatch, "Operands must be numbers", Op, T).
    
check_boolean_operand(_Op, true, _T) -> ok;
check_boolean_operand(_Op, false, _T) -> ok;
check_boolean_operand(Op, _, T) ->
    rte(type_mismatch, "Operand must be a Boolean", Op, T).

check_non_zero(Op, 0, T) ->
    rte(divide_by_zero, "Divide by zero is invalid", Op, T);
check_non_zero(_, _, _T) -> ok.


% TODO: can we remove the Op param now that we have the token?
rte(Type, Message, Op, T) ->
    throw({runtime_error, Type, Message, Op, T#t.line, T#t.literal}).


% UTILS
error(Line, Message) -> 
    Out = io_lib:format("~p| ~s at unknown location.~n", [Line, Message]),
    highlight(Out).
error(Line, Literal, Message) -> 
    Out = io_lib:format("~p| ~s near ~p.~n", [Line, Message, Literal]),
    highlight(Out).

highlight(Message) -> io:format("~s", [color:red(Message)]).