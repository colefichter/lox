-module(environment).

-export([new/0, define/2, assign/3, get/2]).

-include("records.hrl").

% Create a new environment to hold state
new() -> {dict:new(), global}.

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
	% Env1 = case dict:is_key(Name, Env) of % Must not all variable creation during assignment!
	% 	true ->
	% 		dict:store(Name, Value, Env);
	% 	false ->
	% 		undefined(Token)
	% end,
	% put(env, {Env1, Enclosed}),
	% ok.
	try_assign(Name, Value, EnvTuple, Token),
	ok.

try_assign(Name, Value, {Env, global}, Token) ->
	Env1 = case dict:is_key(Name, Env) of
		true -> 
			dict:store(Name, Value, Env);
		false ->
			undefined(Token)
	end,
	put(env, {Env1, global});
try_assign(Name, Value, {Env, Enclosed}, Token) ->
	Env1 = case dict:is_key(Name, Env) of
		true -> 
			dict:store(Name, Value, Env);
		false ->
			try_assign(Name, Value, Enclosed, Token)
	end,
	put(env, {Env1, Enclosed}).

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