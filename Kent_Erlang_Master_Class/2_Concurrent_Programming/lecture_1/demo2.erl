-module(demo2).@doc
-export([area/0]).

area() ->
    receive
        {From, {square,X}} ->
            From ! {self(), X*X};
        {From, {rectangle,X,Y}} ->
            From ! {self(), X*Y}
    end,
    area().
