%% Copyright
-module(reke_db).
-author("pfeairheller").

-behavior(gen_server).

%% API
-export([start_link/0, stop/0, terminate/2]).
-export([migrate/1, rollback/1]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  ets:new(migrations, [named_table, {}]),
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
  {reply, Reply, LoopData};
handle_call({insert_schema_migration, Version}, _Pid, LoopData) ->
  Stmt = io_lib:format("insert into schema_migrations (version) values ('~s')", [Version]),
  case reke_connection:squery(reke, Stmt) of
    Result ->
      erlang:display(Result)
  end,
  {reply, Result, LoopData};
handle_call({should_run, Version}, _Pid, LoopData) ->

  {reply, true, LoopData}.

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
      process_files(lists:sort(Files), Dir, []),
      ok;
    _ ->
      {error, "bad dir"}
  end.

process_files([], _Dir, Processed) ->
  insert_schema_migrations(Processed);
%%   stop();
process_files([File | Rest], Dir, Processed) ->
  [NumStr | _] = string:tokens(File, "_"),
  erlang:display(File),
  case file:consult("db/migrate/" ++ File) of
    {ok, Tokens} ->
      Term = proplists:get_value(Dir, Tokens),
      case gen_server:call(?MODULE, Term) of
        {ok, _Result} ->
          process_files(Rest, Dir, [NumStr | Processed]);
        {error, Error} ->
          io:format("Error with ~s: ~p~n", [File, Error])
      end;
    _ ->
      {error, unknown}
  end.

generate_drop_statement(TableName) ->
  "DROP TABLE " ++ TableName.

generate_create_statement(TableName, Contents) ->
  "CREATE TABLE " ++ TableName ++ "(\n     id serial NOT NULL \n" ++  create_table_columns("", Contents) ++ ")".

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


insert_schema_migrations([]) ->
  ok;
insert_schema_migrations([Version | Versions]) ->
  gen_server:call(?MODULE, {insert_schema_migration, Version}),
  insert_schema_migrations(Versions).

