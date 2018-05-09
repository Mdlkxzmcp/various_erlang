% pattern matching
> ex1:factorial(3).
6
> ex1:factorial(-3).
**exception error: no function clause ...

> ex2:print_all([one, two, three]).
one two three
ok


% anonymous functions
> ex3:filter(fun(X) -> X rem 2 == 0 end, [1,2,3,4]).
[2,4]
> ex3:filter(fun ex3:is_even/1, [1,2,3,4]).
[2,4]
> F = fun Filter(_, []) -> [];
  Filter(P, [X|Xs]) -> case P(X) of
    true -> [X|Filter(P,Xs)];
    false -> Filter(P, Xs) end end.
#Fun
> F(fun(X) -> X rem 2 == 0 end, [1,2,3,4]).
[2,4]


% list comprehensions
> [Element || Element <- [1,2,3,4], Element rem 2 == 0].
[2,4]
> [Element || Element <- [1,2,3,4], ex3:is_even(Element)].
[2,4]
> [Element || Element <- lists:seq(1,4), ex3:is_even(Element)].
[2,4]
> [io:format("~p~n",[Element]) || Element <- [one, two, three]].
one
two
three
[ok,ok,ok]

> [ {X, Y} || X <- [1,2,3], X rem 2 /= 0, Y <- [X+3, X+4, X+5], (X+Y) rem 2 /= 0].
[{1,4},{1,6},{3,6},{3,8}]


% message passing
> echo:go().  % echo.erl
hello
stop

% processes and monitors
> Pid = spawn(echo, loop, []).
<0.34.0>
> erlang:monitor(process, Pid).
#Ref<0.0.0.34>
> exit(Pid, kill).
true
> flush().
Shell got {'DOWN',#Ref<0.0.0.34>,process,<0.34.0>,killed}
ok


% records
> addr:type("127.0.0.1"). % addr.erl
inet
> addr:type("::1").
inet6


% maps
> EmptyMap = #{}.
#{}
> erlang:map_size(EmptyMap).
0
> RelDates = #{ "R15B03-1" => {2012, 11, 28} }.
#{"R15B03-1" => {2012,11,28}}
> RelDates2 = RelDates#{ "17.0" => {2014, 4, 9} }.
#{"R15B03-1" => {2012,11,28}, "17.0" => {2014,4,9}}
> RelDates3 = RelDates2#{ "17.0" := {2022, 2, 2} }
#{"R15B03-1" => {2012,11,28}, "17.0" => {2022,2,2}}
> #{"17.0" := Date} = RelDates3.
#{"R15B03-1" => {2012,11,28}, "17.0" => {2022,2,2}}
> Date.
{2022,2,2}


% macros
> c('macros.erl', {d, debug}).
{ok,macros}
> macros:test_assign().
X = lists : sum ( [ 1 , 2 , 3 ] ) -> 6
ok
> c('macros.erl', {u, debug}).
{ok,macros}
> macros:test_assign().
6


% ETS
> TabId = ets:new(tab, [named_table]). % by default a set.
tab
> ets:insert(tab, {haskell, lazy}).
true
> ets:lookup(tab, haskell).
[{haskell,lazy}]
> ets:insert(tab, {haskell, ghci}).
true
> ets:lookup(tab, haskell).
[{haskell, ghci}]
> ets:lookup(tab, racket).
[]

> ets:insert(tab, {racket, strict}).
true
> ets:insert(tab, {ocaml, strict}).
true
> ets:first(tab).
racket
> ets:next(tab, racket).
haskell

> ets:match(tab, {'$1', '$0'}).
[[strict,ocaml],[ghci,haskell],[strict,racket]]
> ets:match(tab, {'$1', '_'}).
[[ocaml],[haskell],[racket]]
> ets:match(tab, {'$1', 'strict'}).
[[ocaml],[racket]]

> hlr:new().  % hlr.erl
ok
> hlr:attach(12345).
true
> hlr:lookup_ms(self()).
{ok,12345}
> hlr:lookup_id(12345).
{ok,<0.32.0>}
> hlr:detach().
true
> hlr:lookup_id(12345).
{error,invalid}


% Distributed Erlang
> monitor_node(Node, true),
  {serve, Node} ! {self(), Msg},
  receive
    {ok, Resp} ->
      monitor_node(Node, false),
      <handle process response>;  % Pseudocode to handle the process response
    {nodedown, Node} ->
      <handle lack of response>   % Pseudocode to handle lack of response
  end.

$ erl -sname foo -setcookie abc
$ erl -sname bar -setcookie abc
bar> net_adm:ping('foo@...').
pong
bar> [Node] = nodes().
['foo@...']
bar> Shell = self().
<0.38.0>
bar> spawn(Node, fun() -> Shell ! self() end).
<5985.46.0>
bar> receive Pid -> Pid end.
<5985.46.0>
bar> node(Pid).
'foo@...'
bar> spawn(Node, fun() -> Shell ! node() end).
<5985.47.0>
bar> flush().
Shell got 'foo@...'
ok
