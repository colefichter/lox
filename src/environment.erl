-module(environment).

-export([init/0, current/0, define/2, assign/3, get/2, enclose/0, unenclose/0]).

-export([create_new_scope/1, create_new_scope/2, replace_scope/1, dump/1]).

-include("records.hrl").

init() ->
	put(env, [start_dict()]),
	ok.

% Create a new environment based on the globals
create_new_scope(_Token) ->
	PreviousEnv = current(),
	put(env, [global()]),
	PreviousEnv.

% Create a new environment based on a given scope
create_new_scope(Closure, _Token) ->
	PreviousEnv = current(),
	put(env, [start_dict()|Closure]),
	PreviousEnv.

replace_scope(Env) -> put(env, Env).

current() -> get(env).

% Add a new variable to the environment
define(Name, Value) ->
	% store/3 overwrites if the key already exists. This will enable a program to work in the REPL like:
	%	 var a = "before";
	%	 print a; // "before".
	%	 var a = "after";
	%	 print a; // "after".
	[Pid|_] = current(),
	ok = env_dict:put(Pid, Name, Value),
	ok.

% Assign a value to an existing variable
assign(Name, Value, Token) ->
	try_assign(Name, Value, current(), Token),
	ok.

% Lookup the value of an existing variable
get(Name, Token) ->
	try_get(Name, current(), Token).

enclose() ->
	Enclosed = current(),
	{ok, Pid} = env_dict:start(),
	put(env, [Pid|Enclosed]),
	ok.

unenclose() ->
	case get(env) of
		[_Pid] -> ok;
		[_Pid|Enclosed] -> put(env, Enclosed)
	end,
	ok.

% Helper to debug the current interpreter state
dump(LineNumber) ->
	color:format(magenta, "~p|CURRENT ENVIRONMENT: ~n", [LineNumber]),
	[color:format(magenta, "  ~p~n", [env_dict:all(Pid)]) || Pid <- current()].


% UTILS
start_dict() ->
	{ok, Pid} = env_dict:start(),
	Pid.

try_assign(_Name, _Value, [], Token) ->
	undefined(Token);
try_assign(Name, Value, [Pid|Enclosed], Token) ->
	case env_dict:get(Pid, Name) of
		{ok, _CurrentValue} -> 
			env_dict:put(Pid, Name, Value);
		error ->
			try_assign(Name, Value, Enclosed, Token)
	end.

try_get(_Name, [], Token) ->
	undefined(Token);
try_get(Name, [Pid|Enclosed], Token) ->
	case env_dict:get(Pid, Name) of
		{ok, Value} -> Value;
		error -> 
			try_get(Name, Enclosed, Token)
	end.

undefined(Token) -> interpreter:rte(undefined_variable, "Undefined variable", Token).

global() -> lists:last(current()).