-module(env_dict).

-behaviour(gen_server).

%% API
-export([start/0, get/2, put/3, all/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions

start() -> gen_server:start(?MODULE, [], []). %returns {ok, Pid}

get(ServerPid, Key) -> gen_server:call(ServerPid, {get, Key}).

put(ServerPid, Key, Value) -> gen_server:call(ServerPid, {put, Key, Value}).

all(ServerPid) -> gen_server:call(ServerPid, {all}).

%% gen_server callbacks

init([]) -> {ok, dict:new()}.

handle_call({get, Key}, _From, State) ->
    Reply = dict:find(Key, State),
    {reply, Reply, State};

handle_call({put, Key, Value}, _From, State) ->
    State1 = dict:store(Key, Value, State),
    {reply, ok, State1};

handle_call({all}, _From, State) ->
    Reply = State,
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.