-module(resolver).

-export([run/1]).

-include("records.hrl").

run(Statements) ->
    push_current_function(none),
    resolve_all(Statements, [dict:new()]),
    ok.

%%%%%%%%%%%%%%%%%%%%%
% Statements
%%%%%%%%%%%%%%%%%%%%%

resolve({dumpenv, _T}, ScopeStack) ->
    ScopeStack;

resolve({class_stmt, Name, Methods, T}, ScopeStack) ->
    ScopeStack1 = declare(Name, T, ScopeStack),
    ScopeStack2 = define(Name, ScopeStack1),
    ScopeStack3 = begin_scope(ScopeStack2),
    ScopeStack4 = define("this", ScopeStack3), 
    ScopeStack5 = resolve_methods(Methods, ScopeStack4),
    end_scope(ScopeStack5);

resolve({function_decl, Name, _Parameters, _Body, T}=F, ScopeStack) ->
    ScopeStack1 = declare(Name, T, ScopeStack),
    ScopeStack2 = define(Name, ScopeStack1),
    resolve_function(F, function, ScopeStack2);

% A statement declaring a new variable (not to be confused with a variable expression, which looks up the value of a variable)
resolve({var_stmt, Id, InitilizerExpr, T}, ScopeStack) ->
    ScopeStack1 = declare(Id, T, ScopeStack),
    ScopeStack2 = case InitilizerExpr of
        nil -> ScopeStack1;
        _   -> resolve(InitilizerExpr, ScopeStack1)
    end,
    define(Id, ScopeStack2);

resolve({if_stmt, ConditionalExpr, ThenBranch, ElseBranch, _T}, ScopeStack) ->
    ScopeStack1 = resolve(ConditionalExpr, ScopeStack),
    ScopeStack2 = resolve(ThenBranch, ScopeStack1),
    case ElseBranch of
        nil -> ScopeStack2;
        _ -> resolve(ElseBranch, ScopeStack2)
    end;

resolve({while_stmt, ConditionalExpr, LoopBody, _T}, ScopeStack) ->
    ScopeStack1 = resolve(ConditionalExpr, ScopeStack),
    resolve(LoopBody, ScopeStack1);

resolve({print_stmt, Expr, _T}, ScopeStack) ->
    resolve(Expr, ScopeStack);

resolve({block, Statements, _T}, ScopeStack) ->
    ScopeStack1 = begin_scope(ScopeStack),
    ScopeStack2 = resolve_all(Statements, ScopeStack1),
    end_scope(ScopeStack2);

resolve({expr_stmt, Expr, _T}, ScopeStack) ->
    resolve(Expr, ScopeStack);

% We'll use exceptions to return to the caller from any point in a function.
resolve({return_stmt, Expr, T}, ScopeStack) ->
    case check_current_function() of
        none -> throw({resolve_error, T#t.line, T#t.literal, "Cannot return from top-level code"});
        _any -> ok
    end,
    case Expr of
        nil -> ScopeStack;
        _ -> resolve(Expr, ScopeStack)
    end;

%%%%%%%%%%%%%%%%%%%%%
% Expressions
%%%%%%%%%%%%%%%%%%%%%

% This is the invocation of a function
resolve({call, CalleeExpr, Arguments, _T}, ScopeStack) ->
    ScopeStack1 = resolve(CalleeExpr, ScopeStack),
    lists:foldl(fun(A, SS) -> 
        SS1 = resolve(A, SS),
        SS1
    end, ScopeStack1, Arguments);

resolve({get_expr, CalleeExpr, _Id, _T}, ScopeStack) ->
    resolve(CalleeExpr, ScopeStack);

resolve({set_expr, Expr, _Name, Value, _T}, ScopeStack) ->
    ScopeStack1 = resolve(Value, ScopeStack),
    resolve(Expr, ScopeStack1);

resolve({this, R, _T}, ScopeStack) ->
    resolve_local(R, "this", ScopeStack),
    ScopeStack;

% Assignment expression (e.g. "a = 1;"). Name is the variable name to in which to store the evaluated results of Value.
resolve({assign, R, Name, Value, _T}, ScopeStack) ->
    ScopeStack1 = resolve(Value, ScopeStack),
    resolve_local(R, Name, ScopeStack1),
    ScopeStack1;

resolve({LExp, logic_or, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(LExp, ScopeStack),
    resolve(RExp, ScopeStack1);

resolve({LExp, logic_and, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(LExp, ScopeStack),
    resolve(RExp, ScopeStack1);

resolve({conditional, ConditionalExpr, ThenBranch, ElseBranch, _T}, ScopeStack) ->
    ScopeStack1 = resolve(ConditionalExpr, ScopeStack),
    ScopeStack2 = resolve(ThenBranch, ScopeStack1),
    case ElseBranch of
        nil -> ScopeStack2;
        _ -> resolve(ElseBranch, ScopeStack2)
    end;

resolve({binary, LExp, _Op, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(LExp, ScopeStack),
    resolve(RExp, ScopeStack1);

resolve({unary, _Op, RExp, _T}, ScopeStack) ->
    resolve(RExp, ScopeStack);

% Prefix/postfix operators here don't work quite like C++. They are expressions only (rather than statements). 
% No side-effects. Also, they are equivalent.
resolve({prefix, _Op, RExp, _T}, ScopeStack) ->
    resolve(RExp, ScopeStack);

resolve({grouping, E, _T}, ScopeStack) ->
    resolve(E, ScopeStack);

% A variable expression (that is, lookup the value of the variable)
resolve({variable, R, Id, T}, ScopeStack) ->
    case is_initializing(Id, ScopeStack) of
        true -> throw({resolve_error, T#t.line, T#t.literal, "Cannot read local variable '" ++ Id ++ "' in its own initializer"});
        false -> ok
    end,
    resolve_local(R, Id, ScopeStack),
    ScopeStack;

resolve({literal, _Val, _T}, ScopeStack) ->
    ScopeStack.

% UTILS
resolve_all([], ScopeStack) -> 
    ScopeStack;
resolve_all([S|Statements], ScopeStack) ->
    ScopeStack1 = resolve(S, ScopeStack),
    resolve_all(Statements, ScopeStack1).

resolve_methods([], ScopeStack) ->
    ScopeStack;
resolve_methods([M|Methods], ScopeStack) ->
    ScopeStack1 = resolve_function(M, method, ScopeStack),
    resolve_methods(Methods, ScopeStack1).

declare(_Name, _T,  []) -> [];
declare(Name, T, [Dict|ScopeStack]) ->
    case dict:is_key(Name, Dict) of
        true -> throw({resolve_error, T#t.line, T#t.literal, "Variable named '" ++ Name ++ "' already exists in this scope"});
        false -> ok
    end,
    Dict1 = dict:store(Name, false, Dict),
    [Dict1|ScopeStack].

define(_Name, []) -> [];
define(Name, [Dict|ScopeStack]) ->
    Dict1 = dict:store(Name, true, Dict),
    [Dict1|ScopeStack].

begin_scope(ScopeStack) -> [dict:new()|ScopeStack].

end_scope([_H|T]) -> T.

is_initializing(_Name, []) -> false;
is_initializing(Name, [Dict|_ScopeStack]) ->
    case dict:is_empty(Dict) of
        true -> false;
        false ->
            case dict:find(Name, Dict) of
                {ok, false} -> true;
                {ok, true} -> false;
                % error -> throw({resolver, not_found, Name})
                error -> false
            end
    end.

resolve_local(R, Name, ScopeStack) ->
    resolve_local(R, Name, 1, ScopeStack).

resolve_local(_R, _Name, _I, []) ->
    % This should not fail. If we can't find a variable, assume it's global.
    % throw("resolve_local failed to find a variable! This should be impossible.");
    ok;
resolve_local(R, Name, I, [Dict|ScopeStack]) ->
    case dict:find(Name, Dict) of
        {ok, _Value} ->
            environment:resolve(R, I), % in the book, this call is interpreter.resolve(Expr, int)
            ok;
        error ->
            resolve_local(R, Name, I+1, ScopeStack)
    end.

resolve_function({function_decl, _Name, Parameters, Body, T}, Type, ScopeStack) ->
    push_current_function(Type), % Keep track of the function type so we can detact bad returns.
    ScopeStack1 = begin_scope(ScopeStack),
    ScopeStack2 = lists:foldl(fun(ParamId, SS) -> 
        SS1 = declare(ParamId, T, SS),
        SS2 = define(ParamId, SS1),
        SS2
    end, ScopeStack1, Parameters),
    ScopeStack3 = resolve(Body, ScopeStack2),
    ScopeStack4 = end_scope(ScopeStack3),
    pop_current_function(),
    ScopeStack4.



% I'm sick of changing every method signature each time we add state. Just use the process dictionary to keep
% track of the valid return type thing.
push_current_function(Type) ->
    Stack1 = case get(current_function) of
        undefined -> [Type];
        [] -> [Type];
        Stack -> [Type|Stack]
    end,
    put(current_function, Stack1),
    ok.

pop_current_function() ->
    Stack = case get(current_function) of
        undefined -> [];
        [] -> [];
        [_H|T] -> T
    end,
    put(current_function, Stack),
    ok.

check_current_function() ->
    case get(current_function) of
        undefined -> none;
        [] -> none;
        [H|_T] -> H
    end.