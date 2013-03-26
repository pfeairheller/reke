-module(reke_connection).

-behaviour(supervisor).

-export([start/0, stop/1, squery/2, equery/3]).
-export([init/1]).

start() ->
  supervisor:start_link({local, reke_connection}, ?MODULE, []).

stop(_State) ->
  ok.

init([]) ->
  DatabaseConfig = database_config(),
  erlang:display(DatabaseConfig),
  {ok, Pools} = application:get_env(reke, pools),
  PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
    PoolArgs = [{name, {local, Name}},
      {worker_module, reke_connection_worker}] ++ SizeArgs,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs)
  end, Pools),
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
database_config() ->
  Env = case os:getenv("REKE_ENV") of
    false -> "development";
    Value -> Value
  end,
  case file:consult("db/database.config") of
    {ok, Terms} ->
      proplists:get_value(list_to_atom(Env), Terms);
    {error, Error} ->
      {error, no_database_config, Error}
  end.
