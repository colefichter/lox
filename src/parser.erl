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
    program(Tokens1).


program(Tokens) ->
    declaration_list(Tokens).

declaration_list(Tokens) ->
    declaration_list(Tokens, []).
declaration_list([], Declarations) ->
    lists:reverse(Declarations);
declaration_list(Tokens, Declarations) ->
    try declaration(Tokens) of
        {D, Tokens1} ->
            declaration_list(Tokens1, [D|Declarations])    
    catch
        {parse_error, Message, Line, Literal} ->   
            %TODO: move error handling out of interpreter module.         
            interpreter:error(parse_error, Line, Literal, Message),
            Tokens2 = synchronize(Tokens),
            declaration_list(Tokens2, Declarations)
    end.

declaration([#t{type=var}=T|Tokens]) ->
    {Id, Tokens1} = identifier(Tokens), % TODO: How to correctly handle missing var name?
    {InitilizerExpr, Tokens2} = initializer(Tokens1),
    Tokens3 = consume(semi_colon, Tokens2, "Expect ';' after variable declaration"),
    {{var_stmt, Id, InitilizerExpr, T}, Tokens3}; % This is a variable declaration statement
declaration(Tokens) ->
    statement(Tokens).
%% TODO: Catch parse error and synchronize in declaration()? How to do it? Do we need it?

identifier([#t{type={id, Id}}|Tokens]) ->
    {Id, Tokens};
identifier([T|_Tokens]) ->
    % interpreter:error(T#t.line, T#t.literal, "Expect variable name."),
    pe("Expect variable name after 'var' keyword", T).

initializer([#t{type=equal}|Tokens]) ->
    {Expr, Tokens1} = expression(Tokens),
    {Expr, Tokens1};
initializer(Tokens) ->
    % {nil, Tokens}.
    ast(literal, nil, no_token, Tokens).


statement([#t{type='if'}=T|Tokens]) ->
    Tokens1 = consume(lparen, Tokens, "Expect '(' after if statement"),
    {ConditionalExpr, Tokens2} = expression(Tokens1),
    Tokens3 = consume(rparen, Tokens2, "Expect ')' after if condition"),
    {ThenBranch, Tokens4} = statement(Tokens3),
    % [Next|Rest] = Tokens4,
    {ElseBranch, Tokens5} = case Tokens4 of
        [#t{type=else}|Rest] -> statement(Rest);
        [_|_Rest1] -> {nil, Tokens4};
        [] -> {nil, Tokens4}
    end,
    {{if_stmt, ConditionalExpr, ThenBranch, ElseBranch, T}, Tokens5};
statement([#t{type=print}=T|Tokens]) ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after print statement"),
    {{print_stmt, Expr, T}, Tokens2};
statement([#t{type=while}=T|Tokens]) ->
    Tokens1 = consume(lparen, Tokens, "Expect '(' after while statement"),
    {ConditionalExpr, Tokens2} = expression(Tokens1),
    Tokens3 = consume(rparen, Tokens2, "Expect ')' after while condition"),
    {LoopBody, Tokens4} = statement(Tokens3),
    {{while_stmt, ConditionalExpr, LoopBody, T}, Tokens4}; 
statement([#t{type=lbrace}|Tokens]) -> % Start of a block
    {BlockStatement, Tokens1} = block(Tokens),
    {BlockStatement, Tokens1};
statement(Tokens) ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after expression statement"),
    {{expr_stmt, Expr, unknown_token}, Tokens2}.

block(Tokens) ->
    {Statements, Tokens1} = block(Tokens, []),
    {{block, Statements}, Tokens1}.
block([#t{type=rbrace}|Tokens], Statements) -> %TODO: handle empty list in case code is missing }
    {lists:reverse(Statements), Tokens};
block(Tokens, Statements) ->
    {S, Tokens1} = declaration(Tokens),
    block(Tokens1, [S|Statements]).

expression(Tokens) ->
    % conditional(Tokens).
    assignment(Tokens).


assignment(Tokens) ->
    {Expr, Tokens1} = conditional(Tokens),
    {Expr1, Tokens2} = assignment_if(Expr, Tokens1),
    {Expr1, Tokens2}.

assignment_if(AssignExpr, [#t{type=equal}=T|Tokens]) ->
    Equals = T, % Just to match the code in the book...
    {Value, Tokens1} = assignment(Tokens), % Value from the right side of the "=".
    case AssignExpr of % AssignExpr is the left side of the "=".
        {variable, Id, T1} ->
            Name = Id, % Just to match the code in the book...
            {{assign, Name, Value, T1}, Tokens1}; % TODO: is T1 the correct token to send back?
        {_any, _any, _T} ->
            % Is Equals the correct token to use? Should it be the _T in the pattern?
            pe("Invalid assignment target.", Equals)
    end;
assignment_if(Expr, Tokens) ->
    {Expr, Tokens}.


conditional(Tokens) ->
    % {Expr, Tokens1} = equality(Tokens),
    {Expr, Tokens1} = logic_or(Tokens),
    {Expr1, Tokens2} = conditional_if(Expr, Tokens1),
    {Expr1, Tokens2}.

conditional_if(ConditionalExpr, [#t{type=question}=T|Tokens]) ->
    {ThenBranch, Tokens1} = expression(Tokens),
    Tokens2 = consume(colon, Tokens1, "Expect ':' after then branch of conditional expression"),
    {ElseBranch, Tokens3} = conditional(Tokens2),
    Expr = {conditional, ConditionalExpr, ThenBranch, ElseBranch, T},
    {Expr, Tokens3}; %This one is an optional rather than a loop, so don't recurse!
conditional_if(Expr, Tokens) ->
    {Expr, Tokens}.


logic_or(Tokens) ->
    {Expr, Tokens1} = logic_and(Tokens),
    {Expr1, Tokens2} = logic_or_while(Expr, Tokens1),
    {Expr1, Tokens2}.

logic_or_while(Left, [#t{type='or'}=T|Tokens]) ->
    {Right, Tokens1} = logic_and(Tokens),
    Expr = {Left, logic_or, Right, T},
    logic_or_while(Expr, Tokens1);
logic_or_while(Expr, Tokens) ->
    {Expr, Tokens}.


logic_and(Tokens) ->
    {Expr, Tokens1} = equality(Tokens),
    {Expr1, Tokens2} = logic_and_while(Expr, Tokens1),
    {Expr1, Tokens2}.

logic_and_while(Left, [#t{type='and'}=T|Tokens]) ->
    {Right, Tokens1} = equality(Tokens),
    Expr = {Left, logic_and, Right, T},
    logic_and_while(Expr, Tokens1);
logic_and_while(Expr, Tokens) ->
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
primary([#t{type={id, Id}}=T|Tokens]) ->
    ast(variable, Id, T, Tokens);  % This is variable expression that will be looked up at runtime.
primary([#t{type=lparen}=T|Tokens])      ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(rparen, Tokens1, "Expect ')' after grouping expression"),
    ast(grouping, Expr, T, Tokens2).




synchronize([]) ->
    [];
synchronize([#t{type=Type}|Tokens]) when Type == semi_colon orelse Type == class orelse Type == 'fun' orelse
                                           Type == var orelse Type == for orelse Type == 'if' orelse
                                           Type == while orelse Type == print orelse Type == return ->
    Tokens;
synchronize([_T|Tokens]) ->
    synchronize(Tokens).


consume(Expected, [#t{type=Expected}|Tokens], _Err) -> Tokens;
consume(_Expected, Tokens, ErrorMessage) ->
    [T|_] = Tokens,
    % interpreter:error(T#t.line, T#t.literal, ErrorMessage),
    pe(ErrorMessage, T),
    Tokens.


% Create an AST node and return it with the remaining tokens.
ast(Type, Value, T, Tokens) -> {{Type, Value, T}, Tokens}.

% Create an AST node with a Left expression.
l_ast(Type, Left, Op, T, Tokens) -> {{Type, Left, Op, T}, Tokens}.
%Create an AST node with a Right expression
r_ast(Type, Op, Right, T, Tokens) -> {{Type, Op, Right, T}, Tokens}.

% Create an AST node with both Left and Right expressions. Does not package up the remaining tokens!
b_ast(Left, Op, Right, T) -> {binary, Left, Op, Right, T}.


pe(Message, T) ->
    throw({parse_error, Message, T#t.line, T#t.literal}).