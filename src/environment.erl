-module(environment).

-export([new/0]).

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

get(Name, Env) ->
	case dict:find(Name, Env) of
		{ok, Value} -> Value;
		error -> 
			interpreter:rte(undefined_variable, "Undefined variable", no_operator, Name)
	end.