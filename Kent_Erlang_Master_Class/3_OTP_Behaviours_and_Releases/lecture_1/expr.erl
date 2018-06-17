-module(expr).
%%% @moduledoc
%%% A shortened version of the `expr.erl` file from the first part of
%%% the course. Holds the `print/1` and `eval/1/2` functions, two types
%%% used by them, as well as a `lookup/2` auxiliary function that is used
%%% by `eval/2`.

-export([print/1, eval/1, eval/2]).


%%% Types

-type expr() :: {'num',integer()}
             |  {'var',atom()}
             |  {'add',expr(),expr()}
             |  {'sub', expr(),expr()}
             |  {'mul',expr(),expr()}
             |  {'divi', expr(),expr()}.

-type env() :: [{atom(),integer()}].


%%% API

%%
%% @doc print/1
%%
%% Turn an expression into a string.
%% For example,
%%   print({add,{var,a},{mul,{num,2},{var,b}}). is
%%   "(a+(2*b))"
-spec print(expr()) -> string().
print({num,N}) ->
    integer_to_list(N);
print({var,A}) ->
    atom_to_list(A);
print({add,E1,E2}) ->
    "("++ print(E1) ++ "+" ++ print(E2) ++")";
print({sub,E1,E2}) ->
    "("++ print(E1) ++ "-" ++ print(E2) ++")";
print({mul,E1,E2}) ->
    "("++ print(E1) ++ "*" ++ print(E2) ++")";
print({divi,E1,E2}) ->
    "("++ print(E1) ++ "/" ++ print(E2) ++")".


%%
%% @doc eval/1
%%
%% Evaluate an expression.
%% Turn an expression into a number, by working out its value.
%%
%% For example,
%%   eval({mul,{num,6},{sub,{num,3},{num,1}}}). is
%%   12
-spec eval(expr()) -> integer().
eval({num,N}) ->
    N;
eval({add,E1,E2}) ->
    eval(E1) + eval(E2);
eval({sub,E1,E2}) ->
    eval(E1) - eval(E2);
eval({mul,E1,E2}) ->
    eval(E1) * eval(E2);
eval({divi,E1,E2}) ->
    eval(E1) / eval(E2).


%%
%% @doc eval/2
%%
%% Evaluate an expression that contains variables (fails if they don't exist).
%% Turn an expression into a number, by working out its value.
%%
%% For example,
%%   eval([{a,12},{b,6}],{add,{var,a},{var,b}}). is
%%   18
-spec eval(env(),expr()) -> integer().
eval(_Env,{num,N}) ->
    N;
eval(Env,{var,A}) ->
    lookup(A,Env);
eval(Env,{add,E1,E2}) ->
    eval(Env,E1) + eval(Env,E2);
eval(Env,{sub,E1,E2}) ->
    eval(Env,E1) - eval(Env,E2);
eval(Env,{mul,E1,E2}) ->
    eval(Env,E1) * eval(Env,E2);
eval(Env,{divi,E1,E2}) ->
    eval(Env,E1) / eval(Env,E2).


%%% Auxiliary functions

%% @doc lookup/2
%% Key in a list of key-value pairs.
%% Fails if the key not present.
-spec lookup(atom(),env()) -> integer().
lookup(A, {A,V}) ->
    V;
lookup(A, [{A,V}|_]) ->
    V;
lookup(A, [_|Rest]) ->
    lookup(A,Rest).
