-module(environment).

-export([init/0, current/0, define/2, assign/3, assign/4, get/2, get/3, enclose/0, unenclose/0, resolve/2]).

-export([init_object_state/0, get_object_property/2, set_object_property/3]).

-export([create_new_scope/1, create_new_scope/2, replace_scope/1, dump/1]).

-export([get_class_methods/1, register_class/1]).

-include("records.hrl").

init() ->
	put(env, [start_dict()]),
	ok.

% Store an empty state dictionary for a new instance of a class
init_object_state() ->
	R = erlang:make_ref(),
	Pid = global(),
	env_dict:put(Pid, R, dict:new()),
	R.
get_object_property(R, PropName) when is_reference(R) ->
	Pid = global(),
	{ok, ObjectStateDict} = env_dict:get(Pid, R),
	dict:find(PropName, ObjectStateDict).
set_object_property(R, PropName, PropValue) when is_reference(R) ->
	Pid = global(),
	{ok, ObjectStateDict} = env_dict:get(Pid, R),
	ObjectStateDict1 = dict:store(PropName, PropValue, ObjectStateDict),
	env_dict:put(Pid, R, ObjectStateDict1),
	ok.

get_class_methods(ClassName) ->
	Pid = global(),
	{ok, Methods} = env_dict:get(Pid, {class, ClassName}),
	Methods.

register_class({class, Name, Methods}) ->
	Pid = global(),
	env_dict:put(Pid, {class, Name}, Methods),
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

assign(R, Name, Value, _Token) when is_reference(R) ->
	Globals = global(),
	case env_dict:get(Globals, R) of
		{ok, Distance} -> 
			Pid = lists:nth(Distance, current()),
			env_dict:put(Pid, Name, Value); % Distance tells us exactly where to store value
		error ->
			env_dict:put(Globals, Name, Value) % If we didn't find a distance, the thing should be global
	end.

resolve(R, Distance) when is_reference(R) ->
	Globals = global(),
	ok = env_dict:put(Globals, R, Distance),
	ok.

% Lookup the value of an existing variable
get(Name, Token) ->
	try_get(Name, current(), Token).


% Lookup the value of an existing variable, using the distance from the resolver (static analysis)
get(R, Name, Token) ->
	Globals = global(),
	case env_dict:get(Globals, R) of
		{ok, Distance} -> 
			Pid = lists:nth(Distance, current()),
			{ok, Value} = env_dict:get(Pid, Name), % Distance tells us exactly where to find value
			Value;
		error ->
			try_get(Name, [Globals], Token) % If we didn't find a distance, the thing should be global
	end.

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

undefined(T) -> interpreter:rte(undefined_variable, "Undefined variable", T).

global() -> lists:last(current()).