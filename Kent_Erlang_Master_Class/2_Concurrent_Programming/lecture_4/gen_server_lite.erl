-module(gen_server_lite).
-export([start/2, loop/2, rpc/2]).


%%
%% @doc start/2
%%
%% Registers given module `Mod` in the global registry, then spawns the `loop/2`
%% function, passing it the `Mod` and a given `State` as initial args.
-spec start(module(), number()) -> 'ok' | 'error'.
start(Mod, State) ->
    register(Mod, spawn(gen_server_lite, loop, [Mod, State])).

%%
%% @doc loop/2
%%
%% Serves as the body of the counter.
-spec loop(module(), number()) -> no_return().
loop(Mod, State) ->
    receive
        {From, Tag, Query} ->
            {Reply, State1} = Mod:handle(Query, State),
            From ! {Tag, Reply},
            loop(Mod, State1)
    end.

%%
%% @doc rpc/2
%%
-spec rpc(module(), any()) -> any().
rpc(Source, Query) ->
    Tag = make_ref(),
    Source ! {self(), Tag, Query},
    receive
        {Tag, Reply} ->
            Reply
    end.
