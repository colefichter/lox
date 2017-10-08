-module(resolver).

-export([start/1]).

-include("records.hrl").

start(Statements) -> 
    resolve_all(Statements, [dict:new()]),
    ok.

%%%%%%%%%%%%%%%%%%%%%
% Statements
%%%%%%%%%%%%%%%%%%%%%

resolve({dumpenv, _Line}, ScopeStack) ->
    ScopeStack;

resolve({function_decl, Name, _Parameters, _Body}=F, ScopeStack) ->
    ScopeStack1 = declare(Name, ScopeStack),
    ScopeStack2 = define(Name, ScopeStack1),
    ScopeStack3 = resolve_function(F, function, ScopeStack2),
    ScopeStack3;

% A statement declaring a new variable (not to be confused with a variable expression, which looks up the value of a variable)
resolve({var_stmt, Id, InitilizerExpr, _T}, ScopeStack) ->
    ScopeStack1 = declare(Id, ScopeStack),
    ScopeStack2 = case InitilizerExpr of
        nil -> ScopeStack1;
        _   -> resolve(InitilizerExpr, ScopeStack1)
    end,
    ScopeStack3 = define(Id, ScopeStack2),
    ScopeStack3;

resolve({if_stmt, ConditionalExpr, ThenBranch, ElseBranch, _T}, ScopeStack) ->
    ScopeStack1 = resolve(ConditionalExpr, ScopeStack),
    ScopeStack2 = resolve(ThenBranch, ScopeStack1),
    ScopeStack3 = case ElseBranch of
        nil -> ScopeStack2;
        _ -> resolve(ElseBranch, ScopeStack2)
    end,
    ScopeStack3;

resolve({while_stmt, ConditionalExpr, LoopBody, _T}, ScopeStack) ->
    ScopeStack1 = resolve(ConditionalExpr, ScopeStack),
    ScopeStack2 = resolve(LoopBody, ScopeStack1),
    ScopeStack2;

resolve({print_stmt, Expr, _}, ScopeStack) ->
    ScopeStack1 = resolve(Expr, ScopeStack),
    ScopeStack1;

resolve({block, Statements}, ScopeStack) ->
    ScopeStack1 = begin_scope(ScopeStack),
    ScopeStack2 = resolve_all(Statements, ScopeStack1),
    ScopeStack3 = end_scope(ScopeStack2),
    ScopeStack3;

resolve({expr_stmt, Expr, _}, ScopeStack) ->
    ScopeStack1 = resolve(Expr, ScopeStack),
    ScopeStack1;

% We'll use exceptions to return to the caller from any point in a function.
resolve({return_stmt, Expr, _T}, ScopeStack) ->
    ScopeStack1 = case Expr of
        nil -> ScopeStack;
        _ -> resolve(Expr, ScopeStack)
    end,
    ScopeStack1;

%%%%%%%%%%%%%%%%%%%%%
% Expressions
%%%%%%%%%%%%%%%%%%%%%

% This is the invocation of a function
resolve({call, CalleeExpr, Arguments, _T}, ScopeStack) ->
    ScopeStack1 = resolve(CalleeExpr, ScopeStack),
    ScopeStack2 = lists:foldl(fun(A, SS) -> 
        SS1 = resolve(A, SS),
        SS1
    end, ScopeStack1, Arguments),
    ScopeStack2;

% Assignment expression (e.g. "a = 1;"). Name is the variable name to in which to store the evaluated results of Value.
resolve({assign, R, Name, Value, _T}, ScopeStack) ->
    ScopeStack1 = resolve(Value, ScopeStack),
    resolve_local(R, Name, ScopeStack1),
    ScopeStack1;

resolve({LExp, logic_or, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(LExp, ScopeStack),
    ScopeStack2 = resolve(RExp, ScopeStack1),
    ScopeStack2;

resolve({LExp, logic_and, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(LExp, ScopeStack),
    ScopeStack2 = resolve(RExp, ScopeStack1),
    ScopeStack2;

resolve({conditional, ConditionalExpr, ThenBranch, ElseBranch, _T}, ScopeStack) ->
    ScopeStack1 = resolve(ConditionalExpr, ScopeStack),
    ScopeStack2 = resolve(ThenBranch, ScopeStack1),
    ScopeStack3 = case ElseBranch of
        nil -> ScopeStack2;
        _ -> resolve(ElseBranch, ScopeStack2)
    end,
    ScopeStack3;

resolve({binary, LExp, _Op, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(LExp, ScopeStack),
    ScopeStack2 = resolve(RExp, ScopeStack1),
    ScopeStack2;

resolve({unary, _Op, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(RExp, ScopeStack),
    ScopeStack1;

% Prefix/postfix operators here don't work quite like C++. They are expressions only (rather than statements). 
% No side-effects. Also, they are equivalent.
resolve({prefix, _Op, RExp, _T}, ScopeStack) ->
    ScopeStack1 = resolve(RExp, ScopeStack),
    ScopeStack1;

resolve({grouping, E, _}, ScopeStack) ->
    ScopeStack1 = resolve(E, ScopeStack),
    ScopeStack1;

% A variable expression (that is, lookup the value of the variable)
resolve({variable, R, Id, _T}, ScopeStack) ->
    case is_initializing(Id, ScopeStack) of
        true -> throw("Cannot read local variable in its own initializer.");
        false -> ok
    end,
    resolve_local(R, Id, ScopeStack),
    ScopeStack;

resolve({literal, _Val, _}, ScopeStack) ->
    ScopeStack.

% UTILS
resolve_all([], ScopeStack) -> 
    ScopeStack;
resolve_all([S|Statements], ScopeStack) ->
    ScopeStack1 = resolve(S, ScopeStack),
    ScopeStack2 = resolve_all(Statements, ScopeStack1),
    ScopeStack2.

declare(_Name, []) -> [];
declare(Name, [Dict|ScopeStack]) ->
    Dict1 = dict:store(Name, false, Dict),
    [Dict1|ScopeStack].

define(_Name, []) -> [];
define(Name, [Dict|ScopeStack]) ->
    Dict1 = dict:store(Name, true, Dict),
    [Dict1|ScopeStack].

begin_scope(ScopeStack) -> [dict:new()|ScopeStack].

end_scope([_H|T]) -> T.

is_initializing(_Name, []) -> false;
is_initializing(Name, [Dict|_ScopeStack]=SS) ->
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

% io:format("resolve_local ~p ~p ~p ~p ~n", [R, Name, I, Dict]),


    case dict:find(Name, Dict) of
        {ok, _Value} ->
            environment:resolve(R, I), % in the book, this call is interpreter.resolve(Expr, int)
            ok;
        error ->
            resolve_local(R, Name, I+1, ScopeStack)
    end.

resolve_function({function_decl, _Name, Parameters, Body}, Type, ScopeStack) ->
    ScopeStack1 = begin_scope(ScopeStack),
    ScopeStack2 = lists:foldl(fun(ParamId, SS) -> 
        SS1 = declare(ParamId, SS),
        SS2 = define(ParamId, SS1),
        SS2
    end, ScopeStack1, Parameters),
    ScopeStack3 = resolve(Body, ScopeStack2),
    ScopeStack4 = end_scope(ScopeStack3),
    ScopeStack4.