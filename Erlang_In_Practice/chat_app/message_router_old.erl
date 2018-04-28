-module(message_router_old).

-define(SERVER, message_router).

-compile(export_all).

start() ->
  server_util:start(?SERVER, {message_router, route_messages, [dict:new()]}),
  message_store:start_link().

stop() ->
  server_util:stop(?SERVER),
  message_store:shutdown().

send_chat_message(Addressee, MessageBody) ->
  global:send_message(?SERVER, {send_chat_msg, Addressee, MessageBody}).

register_nick(ClientName, ClientPid) ->
  global:send_message(?SERVER, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
  global:send_message(?SERVER, {unregister_nick, ClientName}).

route_messages(Clients) ->
  receive
    {send_chat_msg, ClientName, MessageBody} ->
      case dict:find(ClientName, Clients) of
        {ok, ClientPid} ->
          ClientPid ! {print_msg, MessageBody};
        error ->
          message_store:save_message(ClientName, MessageBody),
          server_util:display_message({archived, ClientName})
      end,
      route_messages(Clients);
    {register_nick, ClientName, ClientPid} ->
      Messages = message_store:find_messages(ClientName),
      lists:foreach(fun(Msg) -> ClientPid ! {print_msg, Msg} end, Messages),
      route_messages(dict:store(ClientName, ClientPid, Clients));
    {unregister_nick, ClientName} ->
      case dict:find(ClientName, Clients) of
        {ok, ClientPid} ->
          ClientPid ! stop,
          route_messages(dict:erase(ClientName, Clients));
        error ->
          server_util:display_message({unknown_client, ClientName}),
          route_messages(Clients)
      end;
    shutdown ->
      server_util:display_message(shutdown);
    Oops ->
      server_util:display_message(Oops),
      route_messages(Clients)
  end.
