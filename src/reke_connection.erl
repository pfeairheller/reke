-module(reke_connection).

-behaviour(supervisor).

-export([start/1, stop/1, squery/2, equery/3]).
-export([init/1]).

start(Name) ->
  supervisor:start_link({local, reke_connection}, ?MODULE, Name).

stop(_State) ->
  ok.

init(Name) ->
  {PoolArgs, WorkerArgs} = database_config(Name),
  PoolSpecs = [poolboy:child_spec(Name, PoolArgs, WorkerArgs)],
  io:format("~p~n", [PoolSpecs]),
  {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

squery(PoolName, Sql) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {squery, Sql})
  end).

equery(PoolName, Stmt, Params) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {equery, Stmt, Params})
  end).




%% Private
database_config(Name) ->
  Env = case os:getenv("REKE_ENV") of
    false -> "development";
    Value -> Value
  end,
  case file:consult("db/database.config") of
    {ok, Terms} ->
      DatabaseConfig = proplists:get_value(list_to_atom(Env), Terms),
      PoolArgs = [
        {name, {local, Name}},
        {worker_module, reke_connection_worker},
        {size, proplists:get_value(pool, DatabaseConfig, 5)},
        {max_overflow, proplists:get_value(pool_overflow, DatabaseConfig, 5)}
      ],
      DatabaseArgs = [
        {driver,   proplists:get_value(adapter,  DatabaseConfig, "postgresql")},
        {hostname, proplists:get_value(host,     DatabaseConfig, "localhost")},
        {database, proplists:get_value(database, DatabaseConfig, "template1")},
        {username, proplists:get_value(username, DatabaseConfig, "")},
        {password, proplists:get_value(password, DatabaseConfig, "")}
      ],
      {PoolArgs, DatabaseArgs};
    {error, Error} ->
      {error, no_database_config, Error}
  end.
