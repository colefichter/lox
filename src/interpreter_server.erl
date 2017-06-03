-module(interpreter_server).

-behaviour(gen_server).

% Client API
-export([start_link/0, repl/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("records.hrl").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

repl() -> gen_server:cast(?SERVER, {repl}).


% gen_server callbacks
init([]) ->     
    repl(), % Send the first repl message to get the loop going.
    Env = environment:new(), % Create the global env. This will stay alive as long as the interpreter/REPL is alive.
    {ok, Env}.
handle_call(_Request, _From, State) -> {reply, {error, unknown_call}, State}.
handle_cast({repl}, Env) ->
    Input = io:get_line("LOX > "),
    
    % TODO: replace the process dictionary with something better? It's not needed often, so why pass it to every visit method?
    put(env, Env),
    ok = interpreter:interpret(Input),
    Env1 = erase(env),       
    
    repl(), % Send a message to keep the REPL loop running.
    {noreply, Env1};
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.