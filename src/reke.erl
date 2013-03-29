%% Copyright
-module(reke).
-author("pfeairheller").

%% API
-export([start/0, execute/0]).
-export([migrate/1, rollback/1, generate/1]).


start() ->
  reke_connection:start(reke).

execute() ->
  receive
    {create_table, TableName, Contents} ->
      Stmt = generate_create_statement(TableName, Contents),
      case reke_connection:squery(reke, Stmt) of
        {error, _Error} ->
          ok;
        Result ->
          erlang:display(Result)
      end,
      execute();
    {drop_table, TableName} ->
      Stmt = generate_drop_statement(TableName),
      case reke_connection:squery(reke, Stmt) of
        Result ->
          erlang:display(Result)
      end,
      execute();
    {finished, Pid} ->
      Pid ! done,
      ok;
    _ ->
      {error, unknown_command}
  end.

migrate(_Options) ->
  run_for(up).

rollback(_Options) ->
  run_for(down).

generate(["model" | [ModelName | ColumnSpecs]]) ->
  io:format("Generating model ~s with columns ~p~n", [ModelName, ColumnSpecs]),
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
  FileName = io_lib:format("db/migrate/~p~2..0B~2..0B~2..0B~2..0B~2..0B_create_~s.ebm", [Year, Month, Day, Hour, Min, Sec, ModelName]),
  Data = create_table_term(ModelName, ColumnSpecs),
  file:write_file(FileName,io_lib:fwrite("~p.\n",[Data]));
generate(["migration" | [MigrationName | ColumnSpecs]]) ->
  io:format("Migrating with ~s with columns ~p~n", [MigrationName, ColumnSpecs]).

run_for(Dir) ->
  Pid = spawn(fun() -> reke:execute() end),
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

create_table_term(ModelName, _ColumnSpecs) ->
  {up,
    {create_table, ModelName,
      [
        {string, "name"},
        {string, "git_account_name"},
        {string, "code"},
        {string, "access_token"},
        {timestamps}
      ]
    }
  }.


%% Insert all version numbers in schema_migrations
