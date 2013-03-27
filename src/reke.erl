%% Copyright
-module(reke).
-author("pfeairheller").

%% API
-export([start/0, migrate/0, generate/0]).


start() ->
  %% getopt parsing, call function.

  Result = reke_connection:start(reke),
  io:format("Hello Reke World! ~p ~n", [Result]).

generate() ->
  receive
    {create_table, TableName, Contents} ->
      Stmt = generate_create_statement(TableName, Contents),
      erlang:display(Stmt),
      case reke_connection:squery(reke, Stmt) of
        Result ->
          erlang:display(Result)
      end,
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
  end.

process_files([], Pid) ->
  Pid ! {finished},
  ok;
process_files([File | Rest], Pid) ->
  erlang:display(File),
  case file:consult("db/migrate/" ++ File) of
    {ok, Tokens} ->
      Term = proplists:get_value(up, Tokens),
      Pid ! Term,
      process_files(Rest, Pid);
    _ ->
      {error, unknown}
  end.

generate_create_statement(TableName, Contents) ->
  "CREATE TABLE " ++ TableName ++ "(\n     id integer NOT NULL \n" ++  create_table_columns("", Contents) ++ ")".

create_table_columns(Stmt, []) ->
  Stmt;
create_table_columns(Stmt, [Column|Columns]) ->
  create_table_columns(Stmt ++ ",\n" ++ column_ref(Column), Columns).


column_ref({string, Name}) ->
  Name ++ " character varying(255)";
column_ref({integer, Name}) ->
  Name ++ " integer";
column_ref({timestamps}) ->
  "created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL".

%% Sort by version
%% For each module, where version is not already in schema_migrations, call ModName:up and pass term as message to process
%% Insert all version numbers in schema_migrations
