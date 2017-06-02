-module(parser).

-export([parse/1, parse_file/1]).

% For tests
-export([expression/1]).

-include("records.hrl").

% Client API
parse(Tokens) -> {ok, init(Tokens)}.

parse_file(File) ->
    {ok, Tokens} = scanner:lex_file(File),
    parse(Tokens).



% TODO: should we just get rid of the eof token? Empty list indicates EOF, no?
init(Tokens) when is_list(Tokens) ->
    Tokens1 = Tokens -- [eof],
    {E, []} = statement_list(Tokens1),
    E.


statement_list(Tokens) ->
    statement_list(Tokens, []).
statement_list([], Statements) ->
    lists:reverse(Statements);
statement_list(Tokens, Statements) ->
    {S, Tokens1} = statement(Tokens),
    statement_list(Tokens1, [S|Statements]).

statement([#t{type=print}=T|Tokens]) ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after expression"),
    {{print, Expr, T}, Tokens2}; %% TODO: indicate that it's a statement?
statement(Tokens) ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after expression"),
    {{expr_stmt, Expr, unknown_token}, Tokens2}. %TODO: is this a format for the tuple?


expression(Tokens) ->
    conditional(Tokens).


conditional(Tokens) ->
    {Expr, Tokens1} = equality(Tokens),
    {Expr1, Tokens2} = conditional_if(Expr, Tokens1),
    {Expr1, Tokens2}.

conditional_if(CondExpr, [#t{type=question}=T|Tokens]) ->
    {ThenBranch, Tokens1} = expression(Tokens),
    Tokens2 = consume(colon, Tokens1, "Expect ':' after then branch of conditional expression"),
    {ElseBranch, Tokens3} = conditional(Tokens2),
    Expr = {conditional, CondExpr, ThenBranch, ElseBranch, T},
    {Expr, Tokens3}; %This one is an optional rather than a loop, so don't recurse!
conditional_if(Expr, Tokens) ->
    {Expr, Tokens}.


equality(Tokens) ->
    {Expr, Tokens1} = comparison(Tokens),
    {Expr1, Tokens2} = equality_while(Expr, Tokens1),
    {Expr1, Tokens2}.

equality_while(Left, [#t{type=Op}=T|Tokens]) when Op == bang_equal orelse Op == equal_equal ->
    {Right, Tokens1} = comparison(Tokens),
    Expr = b_ast(Left, Op, Right, T),
    equality_while(Expr, Tokens1);
equality_while(Expr, Tokens) ->
    {Expr, Tokens}. 


comparison(Tokens) ->
    {Expr, Tokens1} = term(Tokens),
    {Expr1, Tokens2} = comparison_while(Expr, Tokens1),
    {Expr1, Tokens2}.

comparison_while(Left, [#t{type=Op}=T|Tokens]) when Op == greater orelse Op == greater_equal orelse Op == less orelse Op == less_equal ->
    {Right, Tokens1} = term(Tokens),
    Expr = b_ast(Left, Op, Right, T),
    comparison_while(Expr, Tokens1);
comparison_while(Expr, Tokens) ->
    {Expr, Tokens}.


term(Tokens) ->
    {Expr, Tokens1} = factor(Tokens),
    {Expr1, Tokens2} = term_while(Expr, Tokens1),
    {Expr1, Tokens2}.

term_while(Left, [#t{type=Op}=T|Tokens]) when Op == minus orelse Op == plus ->
    {Right, Tokens1} = factor(Tokens),
    Expr = b_ast(Left, Op, Right, T),
    term_while(Expr, Tokens1);
term_while(Expr, Tokens) ->
    {Expr, Tokens}.


factor(Tokens) ->
    {Expr, Tokens1} = unary(Tokens),
    {Expr1, Tokens2} = factor_while(Expr, Tokens1),
    {Expr1, Tokens2}.

factor_while(Left, [#t{type=Op}=T|Tokens]) when Op == slash orelse Op == star ->
    {Right, Tokens1} = unary(Tokens),
    Expr = b_ast(Left, Op, Right, T),
    factor_while(Expr, Tokens1);
factor_while(Expr, Tokens) ->
    {Expr, Tokens}.


% Prefix increment (++) and decrement (--) are unary operators
unary([#t{type=Op}=T|Tokens]) when Op == bang orelse Op == minus->
    {Right, Tokens1} = unary(Tokens),
    r_ast(unary, Op, Right, T, Tokens1);
unary([#t{type=Op}=T|Tokens]) when Op == plus_plus orelse Op == minus_minus->
    {Right, Tokens1} = unary(Tokens),
    r_ast(prefix, Op, Right, T, Tokens1);
unary(Tokens) ->
    postfix(Tokens).


postfix(Tokens) ->
    {Expr, Tokens1} = primary(Tokens),
    {Expr1, Tokens2} = postfix_while(Expr, Tokens1),
    {Expr1, Tokens2}.
postfix_while(Left, [#t{type=Op}=T|Tokens]) when Op == plus_plus orelse Op == minus_minus ->
    Expr = l_ast(postfix, Left, Op, T, Tokens),
    postfix_while(Expr, Tokens);
postfix_while(Expr, Tokens) ->
    {Expr, Tokens}.


primary([#t{type=Val}=T|Tokens]) when Val == false orelse Val == true orelse Val == nil -> 
    ast(literal, Val, T, Tokens);
primary([#t{type={Label, Val}}=T|Tokens]) when Label == number orelse Label == string -> 
    ast(literal, Val, T, Tokens);
primary([#t{type=lparen}=T|Tokens])      ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(rparen, Tokens1, "Expect ')' after expression"),
    ast(grouping, Expr, T, Tokens2).


consume(Expected, [#t{type=Expected}|Tokens], _Err) -> Tokens;
consume(_Expected, Tokens, ErrorMessage) ->
    [T|_] = Tokens,
    interpreter:error(T#t.line, T#t.literal, ErrorMessage),
    Tokens.


% Create an AST node and return it with the remaining tokens.
ast(Type, Value, T, Tokens) -> {{Type, Value, T}, Tokens}.

% Create an AST node with a Left expression.
l_ast(Type, Left, Op, T, Tokens) -> {{Type, Left, Op, T}, Tokens}.
%Create an AST node with a Right expression
r_ast(Type, Op, Right, T, Tokens) -> {{Type, Op, Right, T}, Tokens}.

% Create an AST node with both Left and Right expressions. Does not package up the remaining tokens!
b_ast(Left, Op, Right, T) -> {binary, Left, Op, Right, T}.

