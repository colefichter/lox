-module(environment).

-export([new/0, define/2, assign/3, get/2, enclose/0, unenclose/0]).

-include("records.hrl").

% Create a new environment to hold state
new() -> 
	Env = {dict:new(), global},
	put(env, {dict:new(), global}),
	Env.

enclose() ->
	Enclosed = get(env),
	New = {dict:new(), Enclosed},
	put(env, New),
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