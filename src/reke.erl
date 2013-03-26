%% Copyright
-module(reke).
-author("pfeairheller").

%% API
-export([start/0, migrate/0, generate/0]).


start() ->
  io:format("Hello Reke World!~n").

generate() ->
  receive
    {create_table, TableName, Contents} ->
      io:format("Gonna create ~p~p~n", [TableName, Contents]),
      generate();
    {drop, TableName} ->
      io:format("Gonna delete ~p~n", [TableName]),
      generate();
    _ ->
      {error, unknown_command}
  end.

migrate() ->
  Pid = spawn(fun() -> reke:generate() end),
  case file:list_dir("db/migrate") of
    {ok, Files} ->
      process_files(Files, Pid),
      ok;
    _ ->
      {error, "bad dir"}
  end .

process_files([], Pid) ->
  Pid ! {finished},
  ok;
process_files([File|Rest], Pid) ->
  erlang:display(File),
  case file:consult("db/migrate/" ++ File) of
    {ok, Tokens} ->
      Term = proplists:get_value(up, Tokens),
      Pid ! Term,
      process_files(Rest, Pid);
    _ ->
      {error, unknown}
  end.


%% Sort by version
%% For each module, where version is not already in schema_migrations, call ModName:up and pass term as message to process
%% Insert all version numbers in schema_migrations
