-module(fizzbuzz).

-export([start/0, calc/2, generate_sequence/2, shutdown/1]).

start() ->
  spawn(fun() -> loop() end).

loop() ->
  receive
    {analyze, Caller, Value} ->
      Caller ! analyze(Value),
      loop();
    {generate, Caller, Value} ->
      Caller ! {ok, analyze(Value)},
      generate_sequence(Caller, Value - 1);
    shutdown ->
      io:format("Shutting down...~n"),
      ok
  end.

shutdown(Pid) ->
  Pid ! shutdown.

calc(Pid, Value) ->
  Pid ! {analyze, self(), Value},
  receive
    Result ->
      Result
  end.

generate_sequence(Pid, N) ->
  case is_number(N) of
    false ->
      exit({bad_arg, N});
    true ->
      Pid ! {generate, self(), N},
      receive
        {ok, Result} ->
          Result
      after 1000 ->
        {error, timeout}
      end
  end.

analyze(N) ->
  if
    N rem 15 == 0 ->
      fizzbuzz;
    N rem 3 == 0 ->
      fizz;
    N rem 5 == 0 ->
      buzz;
    true ->
      N
  end.
