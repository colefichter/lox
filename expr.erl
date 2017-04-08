-module(expr).
-compile(export_all).

%
% A suite of functions for handling arithmetical expressions
%

% Expressions are represented like this
%
%     {num, N}
%     {var, A}
%     {add, E1, E2}
%     {mul, E1, E2}
%
% where N is a number, A is an atom,
% and E1, E2 are themselves expressions,

-type expr() :: {'num',integer()}
             |  {'var',atom()}
             |  {'add',expr(),expr()}
             |  {'mul',expr(),expr()}.

% For example,
%   {add,{var,a},{mul,{num,2},{var,b}}
% represents the mathematical expression
%   (a+(2*b))

%
% Printing
%

% Turn an expression into a string, so that
%   {add,{var,a},{mul,{num,2},{var,b}}
% is turned into
%   "(a+(2*b))"

-spec print(expr()) -> string().

print({num,N}) ->
    integer_to_list(N);
print({var,A}) ->
    atom_to_list(A);
print({add,E1,E2}) ->
    "("++ print(E1) ++ "+" ++ print(E2) ++")";
print({mul,E1,E2}) ->
    "("++ print(E1) ++ "*" ++ print(E2) ++")".

%
% parsing
%

% recognise expressions
% deterministic, recursive descent, parser.

% the function returns two things
%   - an expression recognised at the beginning of the string
%     (in fact, the longers such expression)
%   - whatever of the string is left
%
% for example, parse("(-55*eeee)+1111)") is             
%   {{mul,{num,-55},{var,eeee}} , "+1111)"}


% recognise a fully-bracketed expression, with no spaces etc.

-spec parse(string()) -> {expr(), string()}.

parse([$(|Rest]) ->                            % starts with a '('
      {E1,Rest1}     = parse(Rest),            % then an expression
      [Op|Rest2]     = Rest1,                  % then an operator, '+' or '*'
      {E2,Rest3}     = parse(Rest2),           % then another expression
      [$)|RestFinal] = Rest3,                  % starts with a ')'
      {case Op of
	  $+ -> {add,E1,E2};
	  $* -> {mul,E1,E2}
        end,
       RestFinal};

% recognise an integer, a sequence of digits
% with an optional '-' sign at the start

parse([Ch|Rest]) when ($0 =< Ch andalso Ch =< $9) orelse Ch==$- ->
    {Succeeds,Remainder} = get_while(fun is_digit/1,Rest),
    {{num, list_to_integer([Ch|Succeeds])}, Remainder};


% recognise a variable: an atom built of small letters only.

parse([Ch|Rest])  when $a =< Ch andalso Ch =< $z ->
    {Succeeds,Remainder} = get_while(fun is_alpha/1,Rest),
    {{var, list_to_atom([Ch|Succeeds])}, Remainder}.

% auxiliary functions

% recognise a digit

-spec is_digit(integer()) -> boolean().

is_digit(Ch) ->
    $0 =< Ch andalso Ch =< $9.

% recognise a small letter

-spec is_alpha(integer()) -> boolean().

is_alpha(Ch) ->
    $a =< Ch andalso Ch =< $z.

% the longest initial segment of a list in which all
% elements have property P. Used in parsing integers
% and variables

-spec get_while(fun((T) -> boolean()),[T]) -> {[T],[T]}.    
%-spec get_while(fun((T) -> boolean()),[T]) -> [T].    
			 
get_while(P,[Ch|Rest]) ->
    case P(Ch) of
	true ->
	    {Succeeds,Remainder} = get_while(P,Rest),
	    {[Ch|Succeeds],Remainder};
	false ->
	    {[],[Ch|Rest]}
    end;
get_while(_P,[]) ->
    {[],[]}.

%
% Evaluate an expression
%

% First version commented out.

-spec eval(expr()) -> integer().

eval({num,N}) ->
    N;
eval({add,E1,E2}) ->
    eval(E1) + eval(E2);
eval({mul,E1,E2}) ->
    eval(E1) * eval(E2).

-type env() :: [{atom(),integer()}].

-spec eval(env(),expr()) -> integer().

eval(_Env,{num,N}) ->
    N;
eval(Env,{var,A}) ->
    lookup(A,Env);
eval(Env,{add,E1,E2}) ->
    eval(Env,E1) + eval(Env,E2);
eval(Env,{mul,E1,E2}) ->
    eval(Env,E1) * eval(Env,E2).

%
% Compiler and virtual machine
%
% Instructions
%    {push, N} - push integer N onto the stack
%    {fetch, A} - lookup value of variable a and push the result onto the stack
%    {add2} - pop the top two elements of the stack, add, and push the result
%    {mul2} - pop the top two elements of the stack, multiply, and push the result

-type instr() :: {'push',integer()}
              |  {'fetch',atom()}
              |  {'add2'}
              |  {'mul2'}.

-type program() :: [instr()].

% compiler

-spec compile(expr()) -> program().

compile({num,N}) ->
    [{push, N}];
compile({var,A}) ->
    [{fetch, A}];
compile({add,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}].

% run a code sequence in given environment and empty stack

-spec run(program(),env()) -> integer().
   
run(Code,Env) ->
    run(Code,Env,[]).

% execute an instruction, and when the code is exhausted,
% return the top of the stack as result.
% classic tail recursion

-type stack() :: [integer()].

-spec run(program(),env(),stack()) -> integer().

run([{push, N} | Continue], Env, Stack) ->
    run(Continue, Env, [N | Stack]);
run([{fetch, A} | Continue], Env, Stack) ->
    run(Continue, Env, [lookup(A,Env) | Stack]);
run([{add2} | Continue], Env, [N1,N2|Stack]) ->
    run(Continue, Env, [(N1+N2) | Stack]);
run([{mul2} | Continue], Env ,[N1,N2|Stack]) ->
    run(Continue, Env, [(N1*N2) | Stack]);
run([],_Env,[N]) ->
    N.

% compile and run ...
% should be identical to eval(Env,Expr)

-spec execute(env(),expr()) -> integer().
     
execute(Env,Expr) ->
    run(compile(Expr),Env).

%
% Simplify an expression
%

% first version commented out

% simplify({add,E1,{num,0}}) ->
%     E1;
% simplify({add,{num,0},E2}) ->
%     E2;
% simplify({mul,E1,{num,1}}) ->
%     E1;
% simplify({mul,{num,1},E2}) ->
%     E2;
% simplify({mul,_,{num,0}}) ->
%     {num,0};
% simplify({mul,{num,0},_}) ->
%     {num,0}.

% second version commented out

% simplify({add,E1,{num,0}}) ->
%     simplify(E1);
% simplify({add,{num,0},E2}) ->
%     simplify(E2);
% simplify({mul,E1,{num,1}}) ->
%     simplify(E1);
% simplify({mul,{num,1},E2}) ->
%     simplify(E2);
% simplify({mul,_,{num,0}}) ->
%     {num,0};
% simplify({mul,{num,0},_}) ->
%     {num,0};
% simplify(E) ->
%     E.

% -spec simplify(expr()) -> expr().

% simplify({add,E1,{num,0}}) ->
%     simplify(E1);
% simplify({add,{num,0},E2}) ->
%     simplify(E2);
% simplify({mul,E1,{num,1}}) ->
%     simplify(E1);
% simplify({mul,{num,1},E2}) ->
%     simplify(E2);
% simplify({mul,_,{num,0}}) ->
%     {num,0};
% simplify({mul,{num,0},_}) ->
%     {num,0};
% simplify({add,E1,E2}) ->
%     {add,simplify(E1),simplify(E2)};
% simplify({mul,E1,E2}) ->
%     {mul,simplify(E1),simplify(E2)};
% simplify(E) ->
%     E.

% -spec simplify(expr()) -> expr().

% zeroA({add,E1,{num,0}}) ->
%     E1;
% zeroA({add,{num,0},E2}) ->
%     E2;
% zeroA(E) ->
%     E.

% oneM({mul,E1,{num,1}}) ->
%     E1;
% oneM({mul,{num,1},E2}) ->
%     E2;
% oneM(E) ->
%     E.

% zeroM({mul,_,{num,0}}) ->
%     {num,0};
% zeroM({mul,{num,0},_}) ->
%     {num,0};
% zeroM(E) ->
%     E.

% combine([]) ->
%     fun (E) -> E end;
% combine([Rule|Rules]) ->
%     fun (E) -> (combine(Rules))(Rule(E)) end.
 
% rules() ->
%     combine([fun zeroA/1, fun oneM/1, fun zeroM/1]).

% simp(F,{add,E1,E2}) ->
%     F({add,simp(F,E1),simp(F,E2)});
% simp(F,{mul,E1,E2}) ->
%     F({mul,simp(F,E1),simp(F,E2)});
% simp(_F,E) ->
%     E.

% simplify(E) ->
%     simp(rules(),E).



% Auxiliary function: lookup a
% key in a list of key-value pairs.
% Fails if the key not present.

-spec lookup(atom(),env()) -> integer().

lookup(A,[{A,V}|_]) ->
    V;
lookup(A,[_|Rest]) ->
    lookup(A,Rest).

% Test data.

-spec env1() -> env().    
env1() ->
    [{a,23},{b,-12}].

-spec expr1() -> expr().    
expr1() ->
    {add,{var,a},{mul,{num,2},{var,b}}}.

-spec test1() -> integer().    
test1() ->
    eval(env1(),expr1()).

-spec expr2() -> expr().    
expr2() ->
    {add,{mul,{num,1},{var,b}},{mul,{add,{mul,{num,2},{var,b}},{mul,{num,1},{var,b}}},{num,0}}}.

% simplification ...

zeroA({add,E,{num,0}}) ->
    E;
zeroA({add,{num,0},E}) ->
    E;
zeroA(E) ->
    E.

mulO({mul,E,{num,1}}) ->
    E;
mulO({mul,{num,1},E}) ->
    E;
mulO(E) ->
    E.

mulZ({mul,_,{num,0}}) ->
    {num,0};
mulZ({mul,{num,0},_}) ->
    {num,0};
mulZ(E) ->
    E.

compose([]) ->
    fun (E) -> E end;
compose([Rule|Rules]) ->
    fun (E) -> (compose(Rules))(Rule(E)) end.

rules() ->
    [ fun zeroA/1, fun mulO/1, fun mulZ/1].

simp(F,{add,E1,E2}) ->
    F({add,simp(F,E1),simp(F,E2)});
simp(F,{mul,E1,E2}) ->
    F({mul,simp(F,E1),simp(F,E2)});
simp(_F,E) -> E.

simplify(E) ->
    simp(compose(rules()),E).
	     
