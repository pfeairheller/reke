%% Copyright
-module(reke_db_migrate).
-author("pfeairheller").

-behavior(gen_server).

%% API
-export([start_link/1, stop/0, terminate/2]).
-export([execute/1]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link(Migrations) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Migrations, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(Existing) ->
  {ok, {[], Existing}}.

terminate(_Reason, _LoopData) ->
  ok.

handle_call({Term, Version}, _Pid, {Created, Existing} = LoopData) ->
  case lists:any(fun (E) -> E == Version end, Existing) of
    true ->
      io:format("      ----->    ~s Already Run   <----- ~n", [Version]),
      {reply, Created, LoopData};
    false ->
      case Term of
        {create_table, TableName, Contents} ->
          Stmt = reke_sql:generate_create_statement(TableName, Contents),
          VersionStmt = "insert into schema_migrations (version) values ('" ++ Version ++ "')",
          NewCreated = [Stmt | [VersionStmt | Created]],
          {reply, NewCreated, {NewCreated, Existing}}
      end
  end.

handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

execute(_Options) ->
  case ensure_migration_table() of
    {ok, Migrations} ->
      start_link(Migrations),
      reke_exec:start_link(reke),
      run_for("db/migrate");
    {error, Error} ->
      io:format("Unable to load migrations ~p~n", [Error])
  end.

run_for(Dir) ->
  case file:list_dir(Dir) of
    {ok, Files} ->
      process_files(lists:sort(Files), []),
      ok;
    _ ->
      {error, "bad dir"}
  end.

process_files([], Processed) ->
  Results = reke_exec:execute(Processed),
  stop();
process_files([File | Rest], _) ->
  [Version | _] = string:tokens(File, "_"),
  case file:consult("db/migrate/" ++ File) of
    {ok, Tokens} ->
      io:format("Running Migration ~s.~n", [Version]),
      Term = proplists:get_value(up, Tokens),
      case gen_server:call(?MODULE, {Term, Version}) of
        {error, Error} ->
          io:format("Error with ~s: ~p~n", [File, Error]);
        Created ->
          process_files(Rest, Created)
      end;
    _ ->
      {error, unknown}
  end.

ensure_migration_table() ->
  Stmt = reke_sql:generate_create_statement("schema_migrations", [{string, "version"}]),
  reke_connection:squery(reke, Stmt),
  case reke_connection:squery(reke, "SELECT version FROM schema_migrations ORDER BY version ASC") of
    {ok, _Columns, Rows} ->
      {ok, lists:map(fun(E) -> binary:bin_to_list(element(1, E)) end, Rows)};
    {error, Error} ->
      {error, Error}
  end.

