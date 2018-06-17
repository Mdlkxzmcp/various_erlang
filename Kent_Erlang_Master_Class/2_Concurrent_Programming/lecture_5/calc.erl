-module(calc).
-export([start/0, stop/0, execute/1]).
-export([init/0]).

start() -> spawn(calc, init, []).

%%
%% @doc init/0
%%
%% Registers itself, which is equivalent to the DNS registration.
init() ->
    io:format("Starting...~n"),
    register(calc, self()),
    loop().

%%
%% @doc loop/0
%%
%% Calls code from `expr.erl` (so doesn't actually work since it is
%% in a different directory.)
loop() ->
    receive
        {request, From, Expr} ->
            From ! {reply, expr:eval(Expr)},
            loop();
        stop ->
            io:format("Terminating...~n")
    end.

stop() ->
    calc ! stop.

-spec execute(expr:expr()) -> integer().
execute(X) ->
    calc ! {request, self(), X},
    receive
        {reply, Reply} ->
            Reply
    end.
