-module(environment).

-export([current/0, new/0, define/2, assign/3, get/2, enclose/0, unenclose/0]).

-export([global/0, create_new_scope/0, create_new_scope/1, replace_scope/1, dump/1]).

-include("records.hrl").

current() -> get(env).

global() ->
	global(current()).
global({Env, global}) ->
	{Env, global};
global({_Env, Enclosed}) ->
	global(Enclosed).


% Returns the previous scope for restoral later on...
create_new_scope() ->
	Global = global(),
	New = {dict:new(), Global}, % enclose the global scope
	put(env, New).
create_new_scope(Closure) ->
	% Global = global(),
	New = {dict:new(), Closure}, % enclose the global scope
	put(env, New).

% When ending a function call, we need to get the globals from the nested scope
% and place them into the scope that we're reverting back to (NewEnv). This way, if a global
% was updated in a function call, we don't lose the value when the function returns.
replace_scope(NewEnv) ->
	{CurrentGlobalVars, global} = global(),
	NewEnv1 = replace_global(CurrentGlobalVars, NewEnv),
	put(env, NewEnv1),
	ok.


replace_global(NewDict, {_OldDict, global}) ->
	{NewDict, global};
replace_global(NewDict, {NestedDict, Enclosed}) ->
	NewEnclosed = replace_global(NewDict, Enclosed),
	{NestedDict, NewEnclosed}.




dump(LineNumber) ->
	color:format(magenta, "~p|CURRENT ENVIRONMENT: ~p~n", [LineNumber, current()]).

% Create a new environment to hold state
new() -> 
	Env = {dict:new(), global},
	put(env, {dict:new(), global}),
	Env.

enclose() ->
	Enclosed = get(env),
	NewEnv = {dict:new(), Enclosed},
	put(env, NewEnv),
	ok.

unenclose() ->
	case get(env) of
		{_, global} -> ok;
		{_Env, Enclosed} ->
			put(env, Enclosed)
	end,
	ok.

% Add a new variable to the environment
define(Name, Value) ->
	% store/3 overwrites if the key already exists. This will enable a program to work in the REPL like:
	%	 var a = "before";
	%	 print a; // "before".
	%	 var a = "after";
	%	 print a; // "after".
	{Env, Enclosed} = get(env), % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
	Env1 = dict:store(Name, Value, Env),
	put(env, {Env1, Enclosed}),
	ok.

assign(Name, Value, Token) ->
	EnvTuple = get(env), % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
	NewEnvTuple = try_assign(Name, Value, EnvTuple, Token),
	put(env, NewEnvTuple),
	ok.

try_assign(Name, Value, {Env, global}, Token) ->
	case dict:is_key(Name, Env) of
		true -> 
			{dict:store(Name, Value, Env), global};
		false ->
			undefined(Token)
	end;

try_assign(Name, Value, {Env, Enclosed}, Token) ->
	case dict:is_key(Name, Env) of
		true -> 
			{dict:store(Name, Value, Env), Enclosed};
		false ->
			{Env, try_assign(Name, Value, Enclosed, Token)}
	end.

% Expects a TOKEN in addition to the literal name of the variable to lookup. This is just for error reporting.
get(Name, Token) ->
	EnvTuple = get(env), % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
	try_get(Name, EnvTuple, Token).

try_get(Name, {Env, global}, Token) ->
	case dict:find(Name, Env) of
		{ok, Value} -> Value;
		error ->
			undefined(Token)
	end;
try_get(Name, {Env, Enclosed}, Token) ->
	case dict:find(Name, Env) of
		{ok, Value} -> Value;
		error -> 
			try_get(Name, Enclosed, Token)
	end.

undefined(Token) ->
	interpreter:rte(undefined_variable, "Undefined variable", Token).