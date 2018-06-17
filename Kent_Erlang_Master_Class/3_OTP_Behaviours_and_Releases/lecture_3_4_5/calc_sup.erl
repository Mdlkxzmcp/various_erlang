-module(calc_sup).
%%% @doc calc_sup.
-behaviour(supervisor).

%%% API
-export([start_link/1]).

%%% Callbacks
-export([init/1]).


%%% API

start_link(Env) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Env).


%%% Callbacks

init(Env) ->
    Calc = #{
        id => calc,
        start => {calc, start, [Env]},
        type => worker,
        restart => permanent,
        shutdown => 2000,
        module => [calc]
    },
    ChildSpec = [Calc],
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 3600},
    {ok, {SupFlags, ChildSpec}}.
