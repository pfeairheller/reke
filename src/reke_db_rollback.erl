%% Copyright
-module(reke_db_rollback).
-author("pfeairheller").

-behavior(gen_server).

%% API
-export([start_link/1, stop/0, terminate/2]).
-export([execute/1]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link(Files) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Files, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(Existing) ->
  {ok, {[], lists:reverse(lists:sort(Existing))}}.

terminate(_Reason, _LoopData) ->
  ok.

handle_call({rollback, Version}, _Pid, {Rolled, Existing} = LoopData) ->
  Remaining = lists:dropwhile(fun(E) -> [Last | _] = string:tokens(E, "_"), Last /= Version end, Existing),
  [LastFile | NewExisting] = Remaining,
  [Last | _] = string:tokens(LastFile, "_"),

  case Last of
    Version ->
      case file:consult("db/migrate/" ++ LastFile) of
        {ok, Tokens} ->
          io:format("Rolling back Migration ~s.~n", [Version]),
          case proplists:get_value(down, Tokens) of
            {drop_table, TableName} ->
              Stmt = reke_sql:generate_drop_statement(TableName),
              VersionStmt = "delete from schema_migrations where version = '" ++ Version ++ "'",
              NewRolled = [Stmt | [ VersionStmt | Rolled]],
              {reply, NewRolled, {NewRolled, NewExisting}}
          end;
        {error, Error} ->
          io:format("   Error, File ~s is not a migration file ~p~n", [LastFile, Error]),
          {reply, Rolled, LoopData}
      end;
    _ ->
      io:format("   Error, Last Migration is not ~s ~n", [Version]),
      {reply, Rolled, LoopData}
  end.


handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.


execute(OptionStr) ->
  Dir = "db/migrate",
  case file:list_dir(Dir) of
    {ok, Files} ->
      start_link(Files),
      reke_exec:start_link(reke),
      Options = process_options(OptionStr),
      case load_migrations(proplists:get_value(step, Options, "1")) of
        {ok, Migrations} ->
          run_for(Migrations)
      end;
    {error, Error} ->
      io:format("Unable to load directory ~p~n  --> ~p~n", [Dir, Error])
  end.


run_for(Migrations) ->
  process_migrations(Migrations, []),
  io:format("~p~n", [Migrations]).

process_migrations([], Processed) ->
  reke_exec:execute(lists:reverse(Processed)),
  stop();
process_migrations([Version | Rest], _) ->
  case gen_server:call(?MODULE, {rollback, Version}) of
    {error, Error} ->
      io:format("Error with ~s: ~p~n", [Version, Error]);
    Created ->
      process_migrations(Rest, Created)
  end.

load_migrations(Step) ->
  Stmt = "SELECT version FROM schema_migrations ORDER BY version DESC LIMIT " ++ Step,
  io:format("~p~n", [Stmt]),
  case reke_connection:squery(reke, Stmt) of
    {ok, _Columns, Rows} ->
      {ok, lists:map(fun(E) -> binary:bin_to_list(element(1, E)) end, Rows)};
    {error, Error} ->
      {error, Error}
  end.

process_options(Input) ->
  process_options(Input, []).

process_options([], Options) ->
  Options;
process_options(Input, Options) ->
  [OptionStr | Rest] = Input,
  [Key, Value] = string:tokens(OptionStr, "="),
  process_options(Rest, [{list_to_atom(string:to_lower(Key)), Value} | Options]).