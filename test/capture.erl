-module(capture).
-compile([export_all]).

-record(io, {rdata=""}).

start() ->
    spawn_link(fun() -> loop(#io{}) end).


get_output(IoServer) ->
    IoServer ! {get_data_and_close, self()},
    Output = receive
        {IoServer, data, S} -> S
    end,
    {ok, Output}.


loop(State) ->
    receive
    {io_request, From, ReplyAs, Request} ->
        {_, Reply, NewState} =  io_request(Request,State),
        From ! {io_reply, ReplyAs, Reply},
        loop(NewState);
    {get_data_and_close, From} ->
        From ! {self(), data, cleanup_data(State#io.rdata)},
        normal;
    _Unknown ->
        loop(State)
    end.

io_request({put_chars, _Encoding, Chars}, State = #io{rdata=Data}) ->
    {ok, ok, State#io{rdata=[Chars|Data]}};
io_request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
    io_request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch _:_ ->
        {error, {error, Function}, State}
    end;
io_request(_Req, State) ->
    %% io:format("~p: Unknown req: ~tp ~n",[?LINE, _Req]),
    {ok, {error, request}, State}.


cleanup_data(Data) ->
    D1 = lists:reverse(Data),
    D2 = lists:flatten(D1),
    string:strip(D2, both, $\n). % Trim whitespace from start & end