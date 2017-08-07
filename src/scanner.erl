-module(scanner).

% Client API
-export([lex/1]).

-include("records.hrl").


% lex_file(File) ->
%     {ok, Bin} = file:read_file(File),
%     lex(Bin).

lex(Bin) when is_binary(Bin) -> 
    start(),
    lex(Bin, []);
lex(Input) when is_list(Input) -> lex(list_to_binary(Input)).


%Internal API
lex(<<>>, Tokens) -> {ok, lists:reverse([eof|Tokens])};

lex(<<"//",    B/binary>>, Tokens) -> 
    {_, B1} = comment(B),
    lex(B1, Tokens);

lex(<<" ",     B/binary>>, Tokens) -> lex(B, Tokens);
lex(<<"\t",    B/binary>>, Tokens) -> lex(B, Tokens);
lex(<<"\r",    B/binary>>, Tokens) -> lex(B, Tokens);
lex(<<"\n",    B/binary>>, Tokens) -> 
    next(),
    lex(B, Tokens);

lex(<<$",     B/binary>>, Tokens) ->
    {Literal, B1} = string(B),
    continue(B1, Literal, {string, Literal}, Tokens);

lex(<<",",    B/binary>>, Tokens) -> continue(B, ",", comma, Tokens);
lex(<<".",    B/binary>>, Tokens) -> continue(B, ".", dot, Tokens);
lex(<<";",    B/binary>>, Tokens) -> continue(B, ";", semi_colon, Tokens);
lex(<<"?",    B/binary>>, Tokens) -> continue(B, "?", question, Tokens);
lex(<<":",    B/binary>>, Tokens) -> continue(B, ":", colon, Tokens);

lex(<<"++",   B/binary>>, Tokens) -> continue(B, "++", plus_plus, Tokens);
lex(<<"--",   B/binary>>, Tokens) -> continue(B, "--", minus_minus, Tokens);
% lex(<<"&&",   B/binary>>, Tokens) -> continue(B, "--", amp_amp, Tokens);
% lex(<<"||",   B/binary>>, Tokens) -> continue(B, "--", bar_bar, Tokens);

lex(<<"!=",   B/binary>>, Tokens) -> continue(B, "!=", bang_equal, Tokens);
lex(<<"!",    B/binary>>, Tokens) -> continue(B, "!", bang, Tokens);

lex(<<"==",   B/binary>>, Tokens) -> continue(B, "==", equal_equal, Tokens);
lex(<<"=",    B/binary>>, Tokens) -> continue(B, "=", equal, Tokens);
lex(<<">=",   B/binary>>, Tokens) -> continue(B, ">=", greater_equal, Tokens);
lex(<<">",    B/binary>>, Tokens) -> continue(B, ">", greater, Tokens);
lex(<<"<=",   B/binary>>, Tokens) -> continue(B, "<=", less_equal, Tokens);
lex(<<"<",    B/binary>>, Tokens) -> continue(B, "<", less, Tokens);

lex(<<"+",    B/binary>>, Tokens) -> continue(B, "+", plus, Tokens);
lex(<<"-",    B/binary>>, Tokens) -> continue(B, "-", minus, Tokens);
lex(<<"*",    B/binary>>, Tokens) -> continue(B, "*", star, Tokens);
lex(<<"/",    B/binary>>, Tokens) -> continue(B, "/", slash, Tokens);

lex(<<"(",    B/binary>>, Tokens) -> continue(B, "(", lparen, Tokens);
lex(<<")",    B/binary>>, Tokens) -> continue(B, ")", rparen, Tokens);
lex(<<"{",    B/binary>>, Tokens) -> continue(B, "{", lbrace, Tokens);
lex(<<"}",    B/binary>>, Tokens) -> continue(B, "}", rbrace, Tokens);

lex(<<"var",   B/binary>>, Tokens) -> continue(B, "var", var, Tokens);
lex(<<"and",   B/binary>>, Tokens) -> continue(B, "and", 'and', Tokens);
lex(<<"class", B/binary>>, Tokens) -> continue(B, "class", class, Tokens);
lex(<<"else",  B/binary>>, Tokens) -> continue(B, "else", else, Tokens);
lex(<<"false", B/binary>>, Tokens) -> continue(B, "false", false, Tokens);
lex(<<"fun",   B/binary>>, Tokens) -> continue(B, "fun", 'fun', Tokens);
lex(<<"for",   B/binary>>, Tokens) -> continue(B, "for", for, Tokens);
lex(<<"if",    B/binary>>, Tokens) -> continue(B, "if", 'if', Tokens);
lex(<<"nil",   B/binary>>, Tokens) -> continue(B, "nil", nil, Tokens);
lex(<<"or",    B/binary>>, Tokens) -> continue(B, "or", 'or', Tokens);
lex(<<"print", B/binary>>, Tokens) -> continue(B, "print", print, Tokens);
lex(<<"return",B/binary>>, Tokens) -> continue(B, "return", return, Tokens);
lex(<<"super", B/binary>>, Tokens) -> continue(B, "super", super, Tokens);
lex(<<"this",  B/binary>>, Tokens) -> continue(B, "this", this, Tokens);
lex(<<"true",  B/binary>>, Tokens) -> continue(B, "true", true, Tokens);
lex(<<"while", B/binary>>, Tokens) -> continue(B, "while", while, Tokens);

lex(<<D:1/binary, B/binary>>, Tokens) when (D >= <<$0>>) and (D =< <<$9>>) ->
    {N, B1} = number(list_to_binary([D, B])),
    continue(B1, N, {number, N}, Tokens);

lex(B, Tokens) ->
    {Id, B1} = identifier(B),
    continue(B1, Id, {id, Id}, Tokens).


comment(Bin) -> 
    Result = match($\n, Bin),
    next(),
    Result.

string(Bin) -> string(Bin, []).

string(<<$", B/binary>>, Literal) -> 
    String = accumulated_literal_to_string(Literal),
    {String, B};
string(<<Char:1/binary, B/binary>>, Literal) -> 
    string(B, [Char|Literal]);
string(<<>>, Literal) ->
    % TODO: either throw an error or add a HasErrors property somewhere. Right now, this displays the
    % error but the interpreter conintues happily along.
    report("Unterminated string literal"),
    String = accumulated_literal_to_string(Literal),
    {String, <<>>}.


% identifier(Bin) -> match($ , Bin).
identifier(Bin) -> identifier(Bin, []).

identifier(<<C:1/binary, B/binary>>, Chars) when (C >= <<$0>>) and (C =< <<$9>>) 
                                            orelse (C >= <<$a>>) and (C =< <<$z>>)
                                            orelse (C >= <<$A>>) and (C =< <<$Z>>) ->
    identifier(B, [C|Chars]);
identifier(<<C:1/binary, B/binary>>, Chars) -> %anything other than digit or dot.
    R = lists:reverse(Chars),
    Flat = list_to_binary(R),
    Id = binary_to_list(Flat),
    {Id, list_to_binary([C, B])}.

number(Bin) -> number(Bin, []).

number(<<$., B/binary>>, Digits) ->
    number(B, [$.|Digits]);
number(<<D:1/binary, B/binary>>, Digits) when (D >= <<$0>>) and (D =< <<$9>>) ->
    number(B, [D|Digits]);
number(<<D:1/binary, B/binary>>, Digits) -> %anything other than digit or dot.
    R = lists:reverse(Digits),
    Flat = list_to_binary(R),
    String = binary_to_list(Flat),
    {string_to_number(String), list_to_binary([D, B])}.

% Keep matching a literal until we encounter the character Find. Eg. build strings or identifiers with this.
match(Find, Bin) -> match(Find, Bin, []).

match(Find, <<Find, B/binary>>, Literal) -> 
    String = accumulated_literal_to_string(Literal),
    {String, B};
match(Find, <<Char:1/binary, B/binary>>, Literal) -> 
    match(Find, B, [Char|Literal]);
match(_Find, <<>>, Literal) ->
    String = accumulated_literal_to_string(Literal),
    {String, <<>>}.


% Track the line number in the process dictionary
start() -> put(line_number, 1).
next() -> put(line_number, current_line()+1).
current_line() -> get(line_number).


% HELPERS

string_to_number(S) ->
    {N, []} = case string:chr(S, $.) > 0 of
        true  -> string:to_float(S);
        false -> string:to_integer(S)
    end,
    N.

accumulated_literal_to_string(Literal) ->
    R = lists:reverse(Literal),
    Flat = list_to_binary(R),
    String = binary_to_list(Flat),
    String.

report(Error) ->
    L = current_line(),
    interpreter:error(L, Error).


% Helpers for managing the token records

to_token(Type, Literal) ->
    Line = current_line(),
    #t{type=Type, line=Line, literal=Literal}.

continue(Bin, Literal, Type, Tokens) ->
    NewToken = to_token(Type, Literal),
    lex(Bin, [NewToken|Tokens]).