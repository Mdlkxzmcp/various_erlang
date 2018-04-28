-module(server_util).

-compile(export_all).

start(ServerName, {Module, Function, Args}) ->
  global:trans({ServerName, ServerName}, fun() ->
    case global:whereis_name(ServerName) of
      undefined ->
        Pid = spawn(Module, Function, Args),
        global:register_name(ServerName, Pid);
      _ ->
        ok
    end
  end).

stop(ServerName) ->
  global:trans({ServerName, ServerName}, fun() ->
    case global:whereis_name(ServerName) of
      undefined ->
        ok;
      _ ->
        global:send_message(ServerName, shutdown)
    end
  end).

display_message(MessageType) ->
  case MessageType of
    {unknown_client, ClientName} ->
      io:format("Unknown client: ~p~n", [ClientName]);
    {archived, ClientName} ->
      io:format("Archived message for ~p~n", [ClientName]);
    shutdown ->
      io:format("Shutting down.~n");
    Other ->
      io:format("Warning! Received: ~p~n", [Other])
  end.
