-module(counter1).
-import(gen_server_lite, [start/2, rpc/2]).
-export([start/0, tick/1, read/0, handle/2]).


%%% Types

-type tick() :: {'tick', number()}.
-type tock() :: {'ack', number()}.
-type commands() :: tick() | 'read'.
-type responses() :: tock() | {number(), number()}.


%%% API

%%
%% @doc start/0
%%
%% Calls `gen_server_lite:start/2` to initialize the counter globally.
start() -> start(counter1, 0).

%%
%% @doc tick/1
%%
%% Calls `gen_server_lite:rpc/2` with the module name, `tick` command,
%% and the incremention.
-spec tick(number()) -> tock().
tick(N) -> rpc(counter1, {tick, N}).

%%
%% @doc read/0
%%
%% Calls `gen_server_lite:rpc/2` with the module name and `read` command.
-spec read() -> number().
read() -> rpc(counter1, read).

%%
%% @doc handle/2
%%
-spec handle(commands(), number()) -> responses().
handle({tick, N}, State) -> {ack, State + N};
handle(read, State) -> {State, State}.
