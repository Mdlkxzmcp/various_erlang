-module(macros).
-export([test/0, test_assign/0]).

-define(ANSWER, 42).
-define(DOUBLE, 2*).
-define(TWICE(F,X), F(F(X))).

test() -> ?TWICE(?DOUBLE, ?ANSWER).


-ifdef(debug).
  -define(Assign(Var, Exp), Var=Exp,
    io:format("~s = ~s -> ~p~n", [??Var, ??Exp, Var])).
-else.
  -define(Assign(Var, Exp), Var=Exp).
-endif.

test_assign() -> ?Assign(X, lists:sum([1,2,3])).
