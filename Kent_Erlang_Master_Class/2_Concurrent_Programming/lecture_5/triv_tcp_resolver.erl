-module(triv_tcp_resolver).
-include_lib("kernel/include/inet.hrl").
-export([store/2, lookup/1]).


-type ip() :: inet:ip4_address().


%% -define(HOST, "localhost").
-define(HOST, "77.238.55.150").
-define(PORT, 6000).


%%
%% @doc store/2
%%
%% Store the given ip and port number under given `Key` in the `gen_tcp`.
-spec store(string(), {ip(), inet:port_number()}) -> {'ok', 'sent'} | {'error', 'connect'}.
store(Key, Value) ->
	case gen_tcp:connect(?HOST, ?PORT,
		[
			binary,
			{packet, 4},
			{active, true}
		]
	) of
		{ok, Socket} ->
			ok = gen_tcp:send(Socket, term_to_binary({add, Key,Value})),
			gen_tcp:close(Socket),
			{ok, sent};
		{error, _} ->
			{error, connect}
	end.


%%
%% @doc lookup/1
%%
%% Look for the given `Key` in the `gen_tcp`.
-spec lookup(string()) -> {'ok', term()}
						| {'error', 'closed'}
						| {'error', 'connect'}.
lookup(Key) ->
	case gen_tcp:connect(?HOST, ?PORT,
		[
			binary,
			{packet, 4},
			{active, true}
		]
	) of
		{ok, Socket} ->
			ok = gen_tcp:send(Socket, term_to_binary({lookup,Key})),
			receive
				{tcp, Socket, Data} ->
					gen_tcp:close(Socket),
					{ok, binary_to_term(Data)};
				{tcp_closed, Socket} ->
					{error, closed}
			end;
		{error, _} ->
			{error, connect}
	end.
