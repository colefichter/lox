-module(interpreter).

-export([visit/1]).





visit({binary, LExp, Op, RExp}) ->
    LVal = visit(LExp),
    RVal = visit(RExp),
    case Op of
        % Our == and != implementation is easier than in the book because we don't need to worry about calling .equals()
        % on a null reference. Here, nil == nil is fine and something like nil == "test" will be false with no error.
        bang_equal      -> LVal /= RVal;
        equal_equal     -> LVal == RVal;
        greater         -> LVal > RVal;
        greater_equal   -> LVal >= RVal;
        less            -> LVal < RVal;
        less_equal      -> LVal =< RVal;
        minus           -> LVal - RVal;
        slash           -> LVal / RVal;
        star            -> LVal * RVal;
        plus            ->  addOrConcat(LVal, RVal)
        %TODO string trim with minus sign. See ideas.md
    end;

visit({unary, Op, RExp}) ->
    RVal = visit(RExp),
    case Op of
        minus -> -RVal;
        bang  -> not isTrue(RVal)
    end;

visit({grouping, E}) ->
    visit(E).





addOrConcat(LVal, RVal) when is_list(LVal) andalso is_list(RVal) ->
    LVal ++ RVal;
addOrConcat(LVal, RVal) -> %strings above, numbers here.
    LVal + RVal.

% TODO: Is this right? Erlang doesn't have null... does this jive with our parser?
isTrue(nil) -> false;
isTrue(false) -> false;
isTrue(_) -> true. %Pretty simple: nil and false are false, everything else is true.


% isEqual(nil, nil) -> true;
% isEqual(nil, _) -> false;
% isEqual(LVal, RVal) -> LVal == RVal.