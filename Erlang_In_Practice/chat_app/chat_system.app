{application, chat_system,
  [
    {description, "Chat System"},
    {vsn, "1.0.0"},
    {modules, [chat_client, chat_system_sup, message_router_sup,
      message_router, message_store, mucc, server_util, web_server, web_sup]},
    {registered, [message_router, message_store, mucc]},
    {applications, [kernel, stdlib, mnesia]},
    {env, []},
    {mod, {chat_system, 9090}}
  ]
}.
