-module(calc).
-export([start/1, stop/0, eval/1]).
-export([init/1, handle/2, terminate/1]).


-spec start(expr:env()) -> 'true'.
start(Env) ->
    server:start(?MODULE, Env).


%%
%% @doc init/1
%%
%% Initialise the state of the process.
-spec init(expr:env()) -> expr:env().
init(Env) ->
    io:format("Starting...~n"),
    Env.


-spec stop() -> 'stop'.
stop() ->
    server:stop(?MODULE).


-spec terminate(any()) -> 'stop'.
terminate(_Env) ->
    io:format("Terminating...~n").


-spec eval(expr:expr()) -> integer().
eval(Expr) ->
    server:request(?MODULE, {eval, Expr}).


-spec handle({'eval', expr:expr()}, expr:env()) -> integer().
handle({eval, Expr}, Env) ->
    {expr:eval(Env, Expr), Env}.
