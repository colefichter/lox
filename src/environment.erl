-module(environment).

-export([new/0, define/3, get/3]).

-include("records.hrl").

% Create a new environment to hold state
new() -> dict:new().

% Add a new variable to the environment
define(Name, Value, Env) ->
	% store/3 overwrites if the key already exists. This will enable a program to work in the REPL like:
	%	 var a = "before";
	%	 print a; // "before".
	%	 var a = "after";
	%	 print a; // "after".
	dict:store(Name, Value, Env). 

% Expects a TOKEN in addition to the literal name of the variable to lookup. This is just for error reporting.
get(Name, Token, Env) ->
	case dict:find(Name, Env) of
		{ok, Value} -> Value;
		error -> 
			interpreter:rte(undefined_variable, "Undefined variable", Token)
	end.