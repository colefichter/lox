-module(scanner).

% Client API
-export([lex/1, lex_file/1]).

-include("records.hrl").


lex_file(File) ->
    {ok, Bin} = file:read_file(File),
    lex(Bin).

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
    lex(B1, [{string, Literal}|Tokens]);

lex(<<",",    B/binary>>, Tokens) -> lex(B, [comma|Tokens]);
lex(<<".",    B/binary>>, Tokens) -> lex(B, [dot|Tokens]);
lex(<<";",    B/binary>>, Tokens) -> lex(B, [semi_colon|Tokens]);
lex(<<"?",    B/binary>>, Tokens) -> lex(B, [question|Tokens]);
lex(<<":",    B/binary>>, Tokens) -> lex(B, [colon|Tokens]);

lex(<<"++",   B/binary>>, Tokens) -> lex(B, [plus_plus|Tokens]);
lex(<<"--",   B/binary>>, Tokens) -> lex(B, [minus_minus|Tokens]);

lex(<<"!=",   B/binary>>, Tokens) -> lex(B, [bang_equal|Tokens]);
lex(<<"!",    B/binary>>, Tokens) -> lex(B, [bang|Tokens]);

lex(<<"==",   B/binary>>, Tokens) -> lex(B, [equal_equal|Tokens]);
lex(<<"=",    B/binary>>, Tokens) -> lex(B, [equal|Tokens]);
lex(<<">=",   B/binary>>, Tokens) -> lex(B, [greater_equal|Tokens]);
lex(<<">",    B/binary>>, Tokens) -> lex(B, [greater|Tokens]);
lex(<<"<=",   B/binary>>, Tokens) -> lex(B, [less_equal|Tokens]);
lex(<<"<",    B/binary>>, Tokens) -> lex(B, [less|Tokens]);

lex(<<"+",    B/binary>>, Tokens) -> lex(B, [plus|Tokens]);
lex(<<"-",    B/binary>>, Tokens) -> lex(B, [minus|Tokens]);
lex(<<"*",    B/binary>>, Tokens) -> lex(B, [star|Tokens]);
lex(<<"/",    B/binary>>, Tokens) -> lex(B, [slash|Tokens]);

lex(<<"(",    B/binary>>, Tokens) -> lex(B, [lparen|Tokens]);
lex(<<")",    B/binary>>, Tokens) -> lex(B, [rparen|Tokens]);
lex(<<"{",    B/binary>>, Tokens) -> lex(B, [lbrace|Tokens]);
lex(<<"}",    B/binary>>, Tokens) -> lex(B, [rbrace|Tokens]);

lex(<<"var",   B/binary>>, Tokens) -> lex(B, [var|Tokens]);
lex(<<"and",   B/binary>>, Tokens) -> lex(B, ['and'|Tokens]);
lex(<<"class", B/binary>>, Tokens) -> lex(B, [class|Tokens]);
lex(<<"else",  B/binary>>, Tokens) -> lex(B, [else|Tokens]);
lex(<<"false", B/binary>>, Tokens) -> lex(B, [false|Tokens]);
lex(<<"fun",   B/binary>>, Tokens) -> lex(B, ['fun'|Tokens]);
lex(<<"for",   B/binary>>, Tokens) -> lex(B, [for|Tokens]);
lex(<<"if",    B/binary>>, Tokens) -> lex(B, ['ir'|Tokens]);
lex(<<"nil",   B/binary>>, Tokens) -> lex(B, [nil|Tokens]);
lex(<<"or",    B/binary>>, Tokens) -> lex(B, ['or'|Tokens]);
lex(<<"print", B/binary>>, Tokens) -> lex(B, [print|Tokens]);
lex(<<"return",B/binary>>, Tokens) -> lex(B, [return|Tokens]);
lex(<<"super", B/binary>>, Tokens) -> lex(B, [super|Tokens]);
lex(<<"this",  B/binary>>, Tokens) -> lex(B, [this|Tokens]);
lex(<<"true",  B/binary>>, Tokens) -> lex(B, [true|Tokens]);
lex(<<"while", B/binary>>, Tokens) -> lex(B, [while|Tokens]);

lex(<<D:1/binary, B/binary>>, Tokens) when (D >= <<$1>>) and (D =< <<$9>>) ->
    {N, B1} = number(list_to_binary([D, B])),
    lex(B1, [{number, N}|Tokens]);

lex(Bin, Tokens) ->
    {Id, Bin1} = identifier(Bin),
    lex(Bin1, [{id, Id}|Tokens]).


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
    report("Unterminated string literal."),
    String = accumulated_literal_to_string(Literal),
    {String, <<>>}.


identifier(Bin) -> match($ , Bin).

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
add(Type, Tokens) -> 
    Token = #t{type=Type, line=current_line()},
    [Token|Tokens].
add(Type, Literal, Tokens) ->
    Token = #t{type=Type, line=current_line(), literal=Literal},
    [Token|Tokens].

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