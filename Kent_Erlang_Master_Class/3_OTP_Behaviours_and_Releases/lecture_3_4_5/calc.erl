-module(calc).
-behaviour(gen_server).

%%% API
-export([start/1, eval/1, print/1, stop/0]).

%%% Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).


%%% Types

-type eval() :: {'eval', expr:expr()}.
-type print() :: {'print', expr:expr()}.

-type calls() :: eval().
-type call_replies() :: {'reply', integer(), expr:env()}.

-type casts() :: print() | 'stop'.
-type cast_replies() :: {'noreply', expr:env()} | {'stop', 'normal', expr:env()}.



%%% API

-spec start(expr:env()) -> {'ok', pid()}.
start(Env) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Env, []).


%%
%% eval/1
%%
%% Evaluate given expression. Follows rules of `eval/2` from `expr.erl`.
%%
%% For example,
%%   eval({mul,{num,6},{sub,{num,3},{num,1}}}). is
%%   12
-spec eval(expr:expr()) -> number().
eval(Expr) ->
    gen_server:call(?MODULE, {eval, Expr}).

%%
%% print/1
%%
%% Turn given expression into a string.
%% Follows rules of `print/1` from `expr.erl`.
%%
%% For example,
%%   print({add,{var,a},{mul,{num,2},{var,b}}). is
%%   "(a+(2*b))"
%%   ok
-spec print(expr:expr()) -> 'ok'.
print(Expr) ->
    gen_server:cast(?MODULE, {print, Expr}).


-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE, stop).



%%% Callbacks

-spec init(expr:env()) -> {'ok', expr:env()}.
init(Env) ->
    io:format("Starting...~n"),
    {ok, Env}.


-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _Env) ->
    io:format("Terminating...~n").


-spec handle_call(calls(), pid(), expr:env()) -> call_replies().
handle_call({eval, Expr}, _From, Env) ->
    {reply, expr:eval(Env, Expr), Env}.


-spec handle_cast(casts(), expr:env()) -> cast_replies().
handle_cast({print, Expr}, Env) ->
    Str = expr:print(Expr),
    io:format("~s~n", [Str]),
    {noreply, Env};

handle_cast(stop, Env) ->
    {stop, normal, Env}.
