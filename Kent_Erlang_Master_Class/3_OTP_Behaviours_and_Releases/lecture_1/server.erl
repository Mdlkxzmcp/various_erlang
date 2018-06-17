-module(server).
-export([start/2, stop/1, request/2]).
-export([init/2]).

-spec start(module(), any()) -> 'true'.
start(Name, Args) ->
    register(Name, spawn(?MODULE, init, [Name, Args])).

-spec init(module(), any()) -> no_return().
init(Name, Args) ->
    LoopData = Name:init(Args),
    loop(Name, LoopData).

-spec stop(module()) -> 'stop'.
stop(Name) ->
    Name ! stop.

-spec request(module(), any()) -> any().
request(Name, Msg) ->
    Ref = erlang:monitor(process, Name),
    Name ! {request, {self(), Ref}, Msg},
    receive
        {reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, _Pid, _} ->
            error(noproc)
    end.

-spec loop(module(), any()) -> no_return().
loop(Name, LoopData) ->
    receive
        {request, From, Msg} ->
            {Reply, NewLoopData} = Name:handle(Msg, LoopData),
            From ! {reply, Reply},
            loop(Name, NewLoopData);
        stop ->
            Name:terminate(LoopData)
    end.
