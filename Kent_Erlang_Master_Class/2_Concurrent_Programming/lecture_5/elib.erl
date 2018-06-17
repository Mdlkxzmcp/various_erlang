-module(elib).
-include_lib("kernel/include/inet.hrl").
-export([my_ip/0, rpc/2]).

%%
%% @doc my_ip/0
%%
%% Returns the IP address of the caller.
-spec my_ip() -> inet:ip4_address().
my_ip() ->
    {ok, L} = inet:getifaddrs(),
    {value,{_,L1}} = lists:keysearch("en0", 1, L),
    get_ip4(L1).

%%
%% @doc get_ip4/1
%%
-spec get_ip4([{'addr', inet:ip4_address()}]) -> inet:ip4_address().
get_ip4([{addr,{A,B,C,D}}|_]) -> {A,B,C,D};
get_ip4([_|T]) -> get_ip4(T);
get_ip4([]) -> [].


%%
%% @doc rpc/2
%%
rpc(Pid, Query) ->
    Self = self(),
    Pid ! {Self, Query},
    receive
        {Pid, Reply} ->
            Reply
    end.
