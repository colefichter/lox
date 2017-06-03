-module(environment).

-export([new/0, define/2, assign/3, get/2]).

-include("records.hrl").

% Create a new environment to hold state
new() -> dict:new().

% Add a new variable to the environment
define(Name, Value) ->
	% store/3 overwrites if the key already exists. This will enable a program to work in the REPL like:
	%	 var a = "before";
	%	 print a; // "before".
	%	 var a = "after";
	%	 print a; // "after".
	Env = get(env), % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
	Env1 = dict:store(Name, Value, Env),
	put(env, Env1),
	ok.

assign(Name, Value, Token) ->
	Env = get(env), % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
	Env1 = case dict:is_key(Name, Env) of % Must not all variable creation during assignment!
		true ->
			dict:store(Name, Value, Env);
		false ->
			interpreter:rte(undefined_variable, "Undefined variable", Token)
	end,
	put(env, Env1),
	ok.

% Expects a TOKEN in addition to the literal name of the variable to lookup. This is just for error reporting.
get(Name, Token) ->
	Env = get(env), % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
	case dict:find(Name, Env) of
		{ok, Value} -> Value;
		error -> 
			interpreter:rte(undefined_variable, "Undefined variable", Token)
	end.