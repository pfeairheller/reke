%% Copyright
-module(reke).
-author("pfeairheller").

-export([start/0, execute/1]).

start() ->
  reke_connection:start(reke).

execute(Args) ->
  [ModuleName | [MethodName | RestArgs]] = Args,
  Module = list_to_atom("reke_" ++ ModuleName),
  Method = list_to_atom(MethodName),
  reke:start(),
  apply(Module, Method, [RestArgs]).
