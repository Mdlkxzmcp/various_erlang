-module(addr).
-export([type/1]).

-include("kernel/include/inet.hrl").

% type(Addr) ->
%   {ok, HostEnt} = inet:gethostbyaddr(Addr),
%   HostEnt#hostent.h_addrtype.

type(Addr) ->
  {ok, #hostent.h_addrtype} = inet:gethostbyaddr(Addr),
  AddrType.

hostent(Host, inet) ->
  #hostent{h_name=Host, h_addrtype=inet, h_length=4,
    h_addr_list=inet:getaddrs(Host, inet)}.
