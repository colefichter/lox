-module(parser).

-export([parse/1, parse_file/1]).

% For tests
-export([expression/1]).

-include("records.hrl").

% Client API
parse(Tokens) -> {ok, program(Tokens)}.

parse_file(File) ->
    {ok, Tokens} = scanner:lex_file(File),
    parse(Tokens).


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

declaration([#t{type=class}=T|Tokens]) ->
    {Name, Tokens1} = identifier(Tokens, "Expect class name after 'class' keyword"),
    Tokens2 = consume(lbrace, Tokens1, "Expect '{' before class body"),
    {Methods, Tokens3} = method_list(Tokens2),
    Tokens4 = consume(rbrace, Tokens3, "Expect '}' after class body"),
    {{class_stmt, Name, Methods, T}, Tokens4};    
declaration([#t{type='fun'}=T|Tokens]) ->
    function(function, T, Tokens);
declaration([#t{type=var}=T|Tokens]) ->
    {Id, Tokens1} = identifier(Tokens, "Expect variable name after 'var' keyword"), % TODO: How to correctly handle missing var name?
    {InitilizerExpr, Tokens2} = initializer(Tokens1),
    Tokens3 = consume(semi_colon, Tokens2, "Expect ';' after variable declaration"),
    {{var_stmt, Id, InitilizerExpr, T}, Tokens3}; % This is a variable declaration statement
declaration(Tokens) ->
    statement(Tokens).
%% TODO: Catch parse error and synchronize in declaration()? How to do it? Do we need it?

method_list(Tokens) ->
    method_list([], Tokens).
method_list(Methods, [#t{type=rbrace}|_Rest]=Tokens) ->
    % RBRACE marks end of class body... we're done
    {lists:reverse(Methods), Tokens};
method_list(Methods, [T|_Rest]=Tokens) ->
    {Method, Tokens1} = function(method, T, Tokens),
    method_list([Method|Methods], Tokens1).

function(Kind, T, Tokens) ->
    Label = atom_to_list(Kind),
    {Name, Tokens1} = identifier(Tokens, "Expect " ++ Label ++ " name"),
    Tokens2 = consume(lparen, Tokens1, "Expect '(' after " ++ Label ++ " name"),
    {Parameters, Tokens3} = function_parameters(Tokens2),
    Tokens4 = consume(lbrace, Tokens3, "Expect '{' before " ++ Label ++ " body"),
    {Body, Tokens5} = block(Tokens4),
    {{function_decl, Name, Parameters, Body, T}, Tokens5}.

function_parameters(Tokens) ->
    function_parameters([], Tokens).
function_parameters(Parameters, [#t{type=rparen}=_T|Tokens]) ->
    {lists:reverse(Parameters), Tokens};
function_parameters(Parameters, [#t{type=comma}|Tokens]) ->
    function_parameters(Parameters, Tokens);
function_parameters(Parameters, Tokens) ->
    case length(Parameters) >= 8 of
        true -> 
            [T|_] = Tokens,
            % Don't throw the error, just warn about it. When we use this parser for something
            % other than an interpreter, we'll need a better interface to handle this
            pw("Function cannot have more than 8 parameters", T);
        false -> ok
    end,
    {NewParamId, Tokens1} = identifier(Tokens, "Expect parameter name"),
    Parameters1 = [NewParamId|Parameters],
    function_parameters(Parameters1, Tokens1).

identifier([#t{type={id, Id}}|Tokens], _Message) ->
    {Id, Tokens};
identifier([T|_Tokens], Message) ->
    pe(Message, T).

initializer([#t{type=equal}|Tokens]) ->
    {Expr, Tokens1} = expression(Tokens),
    {Expr, Tokens1};
initializer(Tokens) ->
    % {nil, Tokens}.
    ast(literal, nil, no_token, Tokens).

statement([#t{type=dumpenv}=T|Tokens]) ->
    Tokens1 = consume(semi_colon, Tokens, "Expect ';' after dumpenv statement"),
    {{dumpenv, T}, Tokens1};

statement([#t{type=for}=T|Tokens]) ->
	Tokens1 = consume(lparen, Tokens, "Expect '(' after for statement"),
	{InitializerExpr, Tokens2} = for_initializer(Tokens1),
	{ConditionalExpr, Tokens3} = for_condition(Tokens2),
	Tokens4 = consume(semi_colon, Tokens3, "Expect ';' after for loop condition"),
	{IncrementExpr, Tokens5} = for_increment(Tokens4),
	Tokens6 = consume(rparen, Tokens5, "Expect ')' after for loop clauses"),
	{LoopBody, Tokens7} = statement(Tokens6),
	LoopBody1 = for_combine_body_and_increment(IncrementExpr, LoopBody, T),  % TODO: find a better T for this?
	% Represent for loops with existing primitives, rather than adding new ones.
	CompleteWhileLoop = {while_stmt, ConditionalExpr, LoopBody1, T},
	FinalStatement = case InitializerExpr of
		nil -> CompleteWhileLoop;
		_ -> {block, [InitializerExpr|[CompleteWhileLoop]], T}
	end,
	{FinalStatement, Tokens7};
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
    % {Expressions, Tokens1} = print_while(Tokens),
    % Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after print statement"),
    % {{print_stmt, Expressions, T}, Tokens2};
statement([#t{type=return}=T|Tokens]) ->
    [Next|_] = Tokens,
    {Expr, Tokens1} = case Next of
        #t{type=semi_colon} -> {nil, Tokens};
        _ ->
            expression(Tokens)
    end,
    Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after return value"),
    {{return_stmt, Expr, T}, Tokens2};
statement([#t{type=while}=T|Tokens]) ->
    Tokens1 = consume(lparen, Tokens, "Expect '(' after while statement"),
    {ConditionalExpr, Tokens2} = expression(Tokens1),
    Tokens3 = consume(rparen, Tokens2, "Expect ')' after while condition"),
    {LoopBody, Tokens4} = statement(Tokens3),    
    {{while_stmt, ConditionalExpr, LoopBody, T}, Tokens4}; 
statement([#t{type=lbrace}|Tokens]) -> % Start of a block
    {BlockStatement, Tokens1} = block(Tokens),
    {BlockStatement, Tokens1};
statement([T|_Rest]=Tokens) ->
    {Expr, Tokens1} = expression(Tokens),
    Tokens2 = consume(semi_colon, Tokens1, "Expect ';' after expression statement"),
    {{expr_stmt, Expr, T}, Tokens2}.

% Multiple print expressions isn't working correctly from inside a function:
%    fun x() { print 1; } //crashes!
% print_while(Tokens) ->
%     print_while([], Tokens).
% print_while(Expressions, [#t{type=comma}|Tokens]) ->
%     print_while(Expressions, Tokens);
% print_while(Expressions, [#t{type=semi_colon}]=Tokens) ->
%     {lists:reverse(Expressions), Tokens};
% print_while(Expressions, Tokens) ->
%     {Expr, Tokens1} = expression(Tokens),
%     print_while([Expr|Expressions], Tokens1).

for_initializer([#t{type=semi_colon}|Tokens]) ->
	{nil, Tokens};
for_initializer([#t{type=var}|_Tokens]=AllTokens) ->
	{Expr, Tokens1} = declaration(AllTokens),
	{Expr, Tokens1};
for_initializer(Tokens) ->
	{Expr, Tokens1} = expression(Tokens), % TODO: Is this correct? The book uses an expression statement here.
	{Expr, Tokens1}.	

for_condition([#t{type=semi_colon}=T|_Tokens]=AllTokens) -> %Just check for semi_colon, but don't remove it from the list.
	ast(literal, true, T, AllTokens);
for_condition(Tokens) ->
	{Expr, Tokens1} = expression(Tokens),
	{Expr, Tokens1}.

for_increment([#t{type=rparen}|_Tokens]=AllTokens) -> %Just check for rparen, but don't remove it from the list.
	{nil, AllTokens};
for_increment(Tokens) ->
	{Expr, Tokens1} = expression(Tokens),
	{Expr, Tokens1}.

for_combine_body_and_increment(nil, LoopBody, _T) ->
	LoopBody;
for_combine_body_and_increment(IncrementExpr, LoopBody, T) ->
	{block, [LoopBody|[IncrementExpr]], T}.


block(Tokens) ->
    [T|_] = Tokens,
    {Statements, Tokens1} = block(Tokens, []),
    {{block, Statements, T}, Tokens1}.
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
        {variable, _R1, Id1, _T1} ->
            Name1 = Id1, % Just to match the code in the book...
            {{assign, erlang:make_ref(), Name1, Value, T}, Tokens1};
        {get_expr, Expr2, Id2, _T2} ->
            Name2 = Id2, % Just to match the code in the book...
            {{set_expr, Expr2, Name2, Value, T}, Tokens1};
        {_any, _any, _T} ->
            % Is Equals the correct token to use? Should it be the _T in the pattern?
            pe("Invalid assignment target.", Equals)
    end;
assignment_if(Expr, Tokens) ->
    {Expr, Tokens}.


conditional(Tokens) ->
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
    % postfix(Tokens).
    call(Tokens).

% postfix(Tokens) ->
%     {Expr, Tokens1} = primary(Tokens),
%     {Expr1, Tokens2} = postfix_while(Expr, Tokens1),
%     {Expr1, Tokens2}.
% postfix_while(Left, [#t{type=Op}=T|Tokens]) when Op == plus_plus orelse Op == minus_minus ->
%     % Expr = l_ast(postfix, Left, Op, T, Tokens),
%     Expr = {postfix, Left, Op, T},
%     postfix_while(Expr, Tokens);
% postfix_while(Expr, Tokens) ->
%     {Expr, Tokens}.

% This is the invocation of a function.
call(Tokens) ->
    {Expr, Tokens1} = primary(Tokens),
    {CalleeExpr, Tokens2} = call_while(Expr, Tokens1),
    {CalleeExpr, Tokens2}.
call_while(Expr, [#t{type=lparen}|Tokens]) ->
    {Expr1,  Tokens1} = finish_call(Expr, Tokens),
    call_while(Expr1, Tokens1);
call_while(Expr, [#t{type=dot}=T|Tokens]) ->
    {Id, Tokens1} = identifier(Tokens, "Expect property name after '.'"),
    GetExpr = {get_expr, Expr, Id, T}, % Expr is the thing called to the left of the dot, like: expr.myMethod()
    call_while(GetExpr, Tokens1);
call_while(Expr, Tokens) ->
    {Expr, Tokens}.

finish_call(CalleeExpr, Tokens) ->
    finish_call(CalleeExpr, [], Tokens).

finish_call(CalleeExpr, Arguments, [#t{type=rparen}=T|Tokens]) ->
    Expr = {call, CalleeExpr, lists:reverse(Arguments), T},
    {Expr, Tokens};
finish_call(CalleeExpr, Arguments, [#t{type=comma}|Tokens]) ->
    finish_call(CalleeExpr, Arguments, Tokens);
finish_call(CalleeExpr, Arguments, Tokens) ->
    case length(Arguments) >= 8 of
        true -> 
            [T|_] = Tokens,
            % Don't throw the error, just warn about it. When we use this parser for something
            % other than an interpreter, we'll need a better interface to handle this
            pw("Function cannot have more than 8 arguments", T);
        false -> ok
    end,
    {NewArg, Tokens1} = expression(Tokens),
    Arguments1 = [NewArg|Arguments],
    finish_call(CalleeExpr, Arguments1, Tokens1).
    

primary([#t{type=Val}=T|Tokens]) when Val == false orelse Val == true orelse Val == nil -> 
    ast(literal, Val, T, Tokens);
primary([#t{type={Label, Val}}=T|Tokens]) when Label == number orelse Label == string -> 
    ast(literal, Val, T, Tokens);
primary([#t{type={id, Id}}=T|Tokens]) ->
    % ast(variable, Id, T, Tokens);  % This is variable expression that will be looked up at runtime.
    {{variable, erlang:make_ref(), Id, T}, Tokens};
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

consume(_Expected, [], ErrorMessage) ->
    % This case happens with syntax error like missing semi_colon: "print 1".
    pe(ErrorMessage),
    [];
consume(Expected, [#t{type=Expected}|Tokens], _Err) -> Tokens;
consume(_Expected, Tokens, ErrorMessage) ->
    [T|_] = Tokens,
    % interpreter:error(T#t.line, T#t.literal, ErrorMessage),
    pe(ErrorMessage, T),
    Tokens.


% Create an AST node and return it with the remaining tokens.
ast(Type, Value, T, Tokens) -> {{Type, Value, T}, Tokens}.

% Create an AST node with a Left expression.
% l_ast(Type, Left, Op, T, Tokens) -> {{Type, Left, Op, T}, Tokens}.
%Create an AST node with a Right expression
r_ast(Type, Op, Right, T, Tokens) -> {{Type, Op, Right, T}, Tokens}.

% Create an AST node with both Left and Right expressions. Does not package up the remaining tokens!
b_ast(Left, Op, Right, T) -> {binary, Left, Op, Right, T}.


pe(Message) ->
    throw({parse_error, Message}).
pe(Message, T) ->
    throw({parse_error, Message, T#t.line, T#t.literal}).
pw(Message, T) ->
    interpreter:warn(parse_warning, T#t.line, T#t.literal, Message).