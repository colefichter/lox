-module(interpreter).

-export([visit/1]).



% The book isn't far enough to actually run yet... let's bootstrap this thing for now:
% visit([E, eof]) ->
%     visit(E);



visit({binary, LExp, Op, RExp}) ->
    LVal = visit(LExp),
    RVal = visit(RExp),
    case Op of
        % Our == and != implementation is easier than in the book because we don't need to worry about calling .equals()
        % on a null reference. Here, nil == nil is fine and something like nil == "test" will be false with no error.
        bang_equal  -> 
            check_number_operands(Op, LVal, RVal),
            LVal /= RVal;
        equal_equal -> 
            check_number_operands(Op, LVal, RVal),
            LVal == RVal;
        greater -> 
            check_number_operands(Op, LVal, RVal),
            LVal > RVal;
        greater_equal -> 
            check_number_operands(Op, LVal, RVal),
            LVal >= RVal;
        less ->
            check_number_operands(Op, LVal, RVal),
            LVal < RVal;
        less_equal -> 
            check_number_operands(Op, LVal, RVal),
            LVal =< RVal;
        slash ->
            check_number_operands(Op, LVal, RVal),
            check_non_zero(Op, RVal),
            LVal / RVal;
        star  ->
            check_number_operands(Op, LVal, RVal),
            LVal * RVal;
        minus ->subtractOrTrim(LVal, RVal);
        plus  ->addOrConcat(LVal, RVal)
    end;

visit({unary, Op, RExp}) ->
    RVal = visit(RExp),
    case Op of
        minus -> 
            check_number_operand(Op, RVal),
            -RVal;
        bang  -> 
            check_boolean_operand(Op, RVal),
            not isTrue(RVal)
    end;
% visit({prefix, Op, RExp}) ->
%     RVal = visit(RExp),
%     check_number_operand(Op, RVal),
%     case Op of 
%         plus_plus -> RVal + 1;
%         minus_minus

visit({grouping, E}) ->
    visit(E);

visit({literal, Val}) -> Val.   


addOrConcat(LVal, RVal) when is_list(LVal) and is_list(RVal) ->
    LVal ++ RVal;
addOrConcat(LVal, RVal) when is_number(LVal) and is_number(RVal) ->
    LVal + RVal;
addOrConcat(_, _) ->
    rte(type_mismatch, "Operands must both be numbers or strings.", "+").

subtractOrTrim(LVal, RVal) when is_list(LVal) and is_list(RVal) ->
    LVal -- RVal;
subtractOrTrim(LVal, RVal) when is_number(LVal) and is_number(RVal) ->
    LVal - RVal;
subtractOrTrim(_, _) ->
    rte(type_mismatch, "Operands must both be numbers or strings.", "-").

% Pretty simple: nil and false are false, everything else is true.
isTrue(nil) -> false;
isTrue(false) -> false;
isTrue(_) -> true.




% ERROR HANDLING STUFF
check_number_operand(_Op, V) when is_number(V) -> ok;
check_number_operand(Op, _) ->
    rte(type_mismatch, "Operand must be a number.", Op).

check_number_operands(_Op, LVal, RVal) when is_number(LVal) and is_number(RVal) -> ok;
check_number_operands(Op, _, _) ->
    rte(type_mismatch, "Operands must be numbers.", Op).
    
check_boolean_operand(_Op, true) -> ok;
check_boolean_operand(_Op, false) -> ok;
check_boolean_operand(Op, _) ->
    rte(type_mismatch, "Operand must be a Boolean.", Op).

check_non_zero(Op, 0) ->
    rte(divide_by_zero, "Divide by zero is invalid.", Op);
check_non_zero(_, _) -> ok.

rte(Type, Message, Op) ->
    throw({runtime_error, Type, Message, Op}).