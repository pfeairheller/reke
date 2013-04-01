%% Copyright
-module(reke_db).
-author("pfeairheller").

-behavior(gen_server).

%% API
-export([start_link/0, stop/0, terminate/2]).
-export([migrate/1, rollback/1]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(_) ->
  case ensure_migration_table() of
    {ok, Migrations} ->
      {ok, Migrations};
    {error, Error} ->
      {stop, Error}
  end.

terminate(_Reason, _LoopData) ->
  ok.

handle_call({create_table, TableName, Contents}, _Pid, LoopData) ->
  Stmt = generate_create_statement(TableName, Contents),
  Reply = case reke_connection:squery(reke, Stmt) of
    {error, Error} ->
      {error, Error};
    Result ->
      {ok, Result}
  end,
  {reply, Reply, LoopData};
handle_call({drop_table, TableName}, _Pid, LoopData) ->
  Stmt = generate_drop_statement(TableName),
  Reply = case reke_connection:squery(reke, Stmt) of
    {error, Error} ->
      {error, Error};
    Result ->
      {ok, Result}
  end,
  {reply, Reply, LoopData}.

handle_cast({insert_schema_migration, FileName}, LoopData) ->
  Stmt = generate_schema_migration_statement(FileName),
  case reke_connection:squery(reke, Stmt) of
    Result ->
      erlang:display(Result)
  end,
  {noreply, LoopData};
handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

migrate(_Options) ->
  start_link(),
  run_for(up).

rollback(_Options) ->
  start_link(),
  run_for(down).

run_for(Dir) ->
  case file:list_dir("db/migrate") of
    {ok, Files} ->
      process_files(lists:sort(Files), Dir),
      ok;
    _ ->
      {error, "bad dir"}
  end.

process_files([], _Dir) ->
  stop();
process_files([File | Rest], Dir) ->
  erlang:display(File),
  case file:consult("db/migrate/" ++ File) of
    {ok, Tokens} ->
      Term = proplists:get_value(Dir, Tokens),
      case gen_server:call(?MODULE, Term) of
        {ok, _Result} ->
%%           Pid ! {insert_schema_migration, File},
          process_files(Rest, Dir);
        {error, Error} ->
          io:format("Error with ~s: ~p~n", [File, Error])
      end;
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


ensure_migration_table() ->
  Stmt = generate_create_statement("schema_migrations", [{string, "version"}]),
  reke_connection:squery(reke, Stmt),
  case reke_connection:squery(reke, "SELECT version FROM schema_migrations ORDER BY version ASC") of
    {ok, _Columns, Rows} ->
      {ok, Rows};
    {error, Error} ->
      {error, Error}
  end.


generate_schema_migration_statement(FileName) ->
  [NumStr | _] = string:tokens(FileName, "_"),
  io_lib:format("insert into schema_migrations (version) values ('~s')", [NumStr]).

