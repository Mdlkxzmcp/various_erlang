-module(triv_tcp_fac_server).
-include_lib("kernel/include/inet.hrl").
-export([start/0, start/1]).


-type socket() :: port().


%%
%% @doc start/0
%%
%% Start the server on port `6010`.
start() -> start(6010).


%%
%% @doc start/1
%%
%% Start the server on given `Port`.
-spec start(inet:port_number()) -> no_return().
start(Port) ->
    IP = elib:my_ip(),
    io:format("starting factorial server ~p:~p~n",[IP, Port]),
    {ok, Listen} = gen_tcp:listen(Port,
        [
            binary,
            {packet, 4},
            {reuseaddr, true},
            {active, true}
        ]
    ),
    % Now register ourselves with the name server
    triv_tcp_resolver:store("fac", {IP, Port}),
    spawn(fun() -> connect(Listen) end).


%%
%% @doc connect/1
%%
-spec connect(socket()) -> no_return().
connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> connect(Listen) end),
    connection_loop(Socket).

%%
%% @doc connection_loop/1
%%
-spec connection_loop(socket()) -> no_return().
connection_loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Term = binary_to_term(Bin),
            case Term of
                {fac_query, N} ->
                    Reply = {faq_response, fac(N)},
                    gen_tcp:send(Socket,
                    term_to_binary(Reply)),
                    connection_loop(Socket)
            end;
        {tcp_closed, Socket} ->
            exit(normal)
    end.


%% @doc fac/1
-spec fac(integer()) -> integer().
fac(0) -> 1;
fac(N) -> N * fac(N-1).
