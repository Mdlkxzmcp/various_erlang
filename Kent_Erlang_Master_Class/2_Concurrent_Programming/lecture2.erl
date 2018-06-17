-module(lecture2).
-export([for/3, rpc/2, promise/2, yield/1, pmap/1, do/2, peval/1]).


-type expr() :: {'num',integer()} | {'mul',expr(),expr()}.


%%
%% @doc for/3
%%
%% A for-loop like function.
%%
%% For example,
%%  for(1,5,fun(I) -> I*I end). is
%%  [1,4,9,16,25]
-spec for(number(), number(), fun()) -> [any()].
for(Max,Max,F) ->
    [F(Max)];
for(I,Max,F) ->
    [F(I)|for(I+1,Max,F)].


%%
%% @doc rpc/2
%%
%% Remote Procedure Call makes a local function call look like a remote one.
-spec rpc(pid(), any()) -> any() | 'timeout'.
rpc(Pid, Request) ->
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    receive
        {Tag, Response} ->
            Response
    after 5000 ->
        timeout
    end.


%%
%% @doc promise/2
%%
%% like rpc/2, but split into two parts – promise/2 and yield/1.
-spec promise(pid(), any()) -> pid().
promise(Pid, Request) ->
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    Tag.

%%
%% @doc yield/1
%%
%% like rpc/2, but split into two parts – promise/2 and yield/1.
-spec yield(pid()) -> any() | 'timeout'.
yield(Tag) ->
    receive
        {Tag, Response} ->
            Response
    after 5000 ->
        timeout
    end.


%%
%% @doc pmap/1
%%
%% Parallel version of a map function.
-spec pmap(list()) -> list().
pmap(L) ->
    S = self(),
    Pids = [do(S,F) || F <- L],
    [receive {Pid,Val} -> Val end || Pid <- Pids].

%%
%% @doc do/2
%%
%% Spawns a process that executes a given function `F` and sends
%% the result to the `Parent` process.
-spec do(pid(), fun()) -> pid().
do(Parent, F) ->
    spawn(fun() ->
    Parent ! {self(), F()}
    end).


%%
%% @doc peval/1
%%
%% Parallel evaluator. Uses `pmap/1` from above.
%% Handles only multiplication.
%%
%% For example,
%%  peval({mul,{num,2},{num,3}}). is
%%  6
-spec peval(expr()) -> integer().
peval({mul, X, Y}) ->
    [Xv,Yv] = pmap([
        fun() -> peval(X) end,
        fun() -> peval(Y) end
    ]),
    Xv * Yv;
    peval({num, X}) ->
    X.
