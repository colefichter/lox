-module(parser).

-export([parse/1, parse_file/1]).

% For tests
-export([expression/1]).

-include("records.hrl").

% Client API
parse(Tokens) -> {ok, expression(Tokens)}.

parse_file(File) ->
    {ok, Tokens} = scanner:lex_file(File),
    parse(Tokens).



expression(T) ->
    conditional(T).


conditional(T) ->
    {Expr, T1} = equality(T),
    {Expr1, T2} = conditional_if(Expr, T1),
    {Expr1, T2}.

conditional_if(CondExpr, [question|T]) ->
    {ThenBranch, T1} = expression(T),
    T2 = consume(colon, T1, "Expect ':' after then branch of conditional expression."),
    {ElseBranch, T3} = conditional(T2),
    Expr = {conditional, CondExpr, ThenBranch, ElseBranch},
    {Expr, T3}; %This one is an optional rather than a loop, so don't recurse!
conditional_if(Expr, T) ->
    {Expr, T}.


equality(T) ->
    {Expr, T1} = comparison(T),
    {Expr1, T2} = equality_while(Expr, T1),
    {Expr1, T2}.

equality_while(Left, [Op|T]) when Op == bang_equal orelse Op == equal_equal ->
    {Right, T1} = comparison(T),
    Expr = {binary, Left, Op, Right},
    equality_while(Expr, T1);
equality_while(Expr, T) ->
    {Expr, T}. 


comparison(T) ->
    {Expr, T1} = term(T),
    {Expr1, T2} = comparison_while(Expr, T1),
    {Expr1, T2}.

comparison_while(Left, [Op|T]) when Op == greater orelse Op == greater_equal orelse Op == less orelse Op == less_equal ->
    {Right, T1} = term(T),
    Expr = {binary, Left, Op, Right},
    comparison_while(Expr, T1);
comparison_while(Expr, T) ->
    {Expr, T}.


term(T) ->
    {Expr, T1} = factor(T),
    {Expr1, T2} = term_while(Expr, T1),
    {Expr1, T2}.

term_while(Left, [Op|T]) when Op == minus orelse Op == plus ->
    {Right, T1} = factor(T),
    Expr = {binary, Left, Op, Right},
    term_while(Expr, T1);
term_while(Expr, T) ->
    {Expr, T}.


factor(T) ->
    {Expr, T1} = unary(T),
    {Expr1, T2} = factor_while(Expr, T1),
    {Expr1, T2}.

factor_while(Left, [Op|T]) when Op == slash orelse Op == star ->
    {Right, T1} = unary(T),
    Expr = {binary, Left, Op, Right},
    factor_while(Expr, T1);
factor_while(Expr, T) ->
    {Expr, T}.


% Prefix increment (++) and decrement (--) are unary operators
unary([Op|T]) when Op == bang orelse Op == minus->
    {Right, T1} = unary(T),
    {{unary, Op, Right}, T1};
unary([Op|T]) when Op == plus_plus orelse Op == minus_minus->
    {Right, T1} = unary(T),
    {{prefix, Op, Right}, T1};
unary(T) ->
    postfix(T).


postfix(T) ->
    {Expr, T1} = primary(T),
    {Expr1, T2} = postfix_while(Expr, T1),
    {Expr1, T2}.
postfix_while(Left, [Op|T]) when Op == plus_plus orelse Op == minus_minus ->
    Expr = {{posfix, Left, Op}, T},
    postfix_while(Expr, T);
postfix_while(Expr, T) ->
    {Expr, T}.


primary([false|T])       -> {{literal, false}, T};
primary([true|T])        -> {{literal, true}, T};
primary([nil|T])         -> {{literal, nil}, T};
primary([{number, N}|T]) -> {{literal, N}, T};
primary([{string, S}|T]) -> {{literal, S}, T};
primary([lparen|T])      ->
    {Expr, T1} = expression(T),
    T2 = consume(rparen, T1, "Expect ')' after expression."),
    {{grouping, Expr}, T2}.





consume(Expected, [Expected|T], _Err) -> T;
consume(_Expected, Tokens, Err) ->
    err(Err),
    Tokens.

err(Error) ->
    interpreter:error(999, Error).