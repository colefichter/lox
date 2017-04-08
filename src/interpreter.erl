-module(interpreter).

-behaviour(gen_server).

% Client API
-export([start_link/0, repl/0, error/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

repl() -> gen_server:cast(?SERVER, {repl}).


% gen_server callbacks
init([]) -> 
    % Send the first repl message to get the loop going.
    repl(),
    {ok, []}.
handle_call(_Request, _From, State) -> {reply, {error, unknown_call}, State}.
handle_cast({repl}, State) ->
    Input = io:get_line("LOX > "),
    {ok, Tokens} = scanner:lex(Input),
    %io:format("     ~p~n", [Tokens]),
    {ok, Ast} = parser:parse(Tokens),
    io:format("      ~p~n", [Ast]),
    repl(),
    {noreply, State};
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


% UTILS
error(Line, Message) -> report(Line, "", Message).

% TODO: figure out how to report some of the source at the error location.
report(Line, _Where, Message) -> io:format("~p| ~p~n", [Line, Message]).