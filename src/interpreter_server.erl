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
    % Send the first repl message to get the loop going.
    repl(),
    {ok, []}.
handle_call(_Request, _From, State) -> {reply, {error, unknown_call}, State}.
handle_cast({repl}, State) ->
    Input = io:get_line("LOX > "),
    {ok, Tokens} = scanner:lex(Input),
    % io:format("     ~s:~p~n", [color:cyan("TOKENS"), Tokens]),
    {ok, Ast} = parser:parse(Tokens),
    % io:format("        ~s:~p~n", [color:cyan("AST"), Ast]),

    try interpreter:visit(Ast) of
        Result ->
            io:format("~s~n", [color:green(io_lib:format("~p", [Result]))])
    catch
        % TESTING ERROR REPORTING WITH LINES
        {runtime_error, _RTEType, Message, _Op, Line, Literal} ->
            io:format("     ~s:~p~n", [color:cyan("TOKENS"), Tokens]),
            io:format("        ~s:~p~n", [color:cyan("AST"), Ast]),
            interpreter:error(Line, Literal, Message)

        % {runtime_error, _Type, Message, Op} ->
        %     % io:format("     ERRROR: ~p ~p near ~p~n", [Type, Message, Op])
        %     interpreter:error(999, Op, Message)
    end,
    
    repl(),
    {noreply, State};
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.