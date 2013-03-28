%% Copyright
-module(reke).
-author("pfeairheller").

%% API
-export([start/0, migrate/0, rollback/0, generate/0]).


start() ->
  reke_connection:start(reke).

generate() ->
  receive
    {create_table, TableName, Contents} ->
      Stmt = generate_create_statement(TableName, Contents),
      case reke_connection:squery(reke, Stmt) of
        {error, _Error} ->
          ok;
        Result ->
          erlang:display(Result)
      end,
      generate();
    {drop_table, TableName} ->
      Stmt = generate_drop_statement(TableName),
      case reke_connection:squery(reke, Stmt) of
        Result ->
          erlang:display(Result)
      end,
      generate();
    {finished, Pid} ->
      Pid ! done,
      ok;
    _ ->
      {error, unknown_command}
  end.

migrate() ->
  run_for(up).

rollback() ->
  run_for(down).


run_for(Dir) ->
  Pid = spawn(fun() -> reke:generate() end),
  ensure_migration_table(Pid),
  case file:list_dir("db/migrate") of
    {ok, Files} ->
      process_files(lists:sort(Files), Pid, Dir),
      ok;
    _ ->
      {error, "bad dir"}
  end.

process_files([], Pid, _Dir) ->
  Pid ! {finished, self()},
  receive
    done -> ok
  end;
process_files([File | Rest], Pid, Dir) ->
  erlang:display(File),
  case file:consult("db/migrate/" ++ File) of
    {ok, Tokens} ->
      Term = proplists:get_value(Dir, Tokens),
      Pid ! Term,
      process_files(Rest, Pid, Dir);
    _ ->
      {error, unknown}
  end.

generate_drop_statement(TableName) ->
  "DROP TABLE " ++ TableName.

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


ensure_migration_table(Pid) ->
  Pid ! {create_table, "schema_migrations", [{string, "version"}]}.

%% Insert all version numbers in schema_migrations
