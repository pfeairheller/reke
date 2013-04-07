%% Copyright
-module(reke_exec).
-author("pfeairheller").

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, terminate/2]).
-export([execute/1]).

-export([init/1, handle_call/3]).


start_link(ConnectionPool) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ConnectionPool, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(Existing) ->
  {ok, Existing}.

terminate(_Reason, _LoopData) ->
  ok.

execute(Statements) ->
  gen_server:call(?MODULE, {execute, Statements}).


handle_call({execute, Statements}, _Pid, LoopData) ->
  Results = run_statements([], Statements),
  {reply, Results, LoopData}.

run_statements(Results, []) ->
  Results;
run_statements(Results, [Stmt | Statements]) ->
  case reke_connection:squery(reke, Stmt) of
    {ok, _Columns, Rows} ->
      run_statements([Rows | Results], Statements);
    {ok, Count} ->
      run_statements([Count| Results], Statements);
    {error, Error} ->
      {error, Error, Results}
  end.
