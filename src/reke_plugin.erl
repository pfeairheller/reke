%% Copyright
-module(reke_plugin).
-author("pfeairheller").

-export([
  'reke-db:migrate'/2,
  'reke-db:rollback'/2
]).

'reke-db:migrate'(Config, _AppFile) ->
  OrigPath = code:get_path(),
  true = code:add_path(rebar_utils:ebin_dir()),

  Result = case rebar_utils:processing_base_dir(Config) of
    true ->
      reke:start(),
      reke:execute(["db", "migrate"]),
      ok;
    false -> ok
  end,

  true = code:set_path(OrigPath),
  Result.


'reke-db:rollback'(Config, _AppFile) ->
  OrigPath = code:get_path(),
  true = code:add_path(rebar_utils:ebin_dir()),
  Step = rebar_config:get_global(Config, step, "1"),
  Result = case rebar_utils:processing_base_dir(Config) of
    true ->
      reke:start(),
      reke:execute(["db", "rollback", "STEP="++Step]),
      ok;
    false -> ok
  end,

  true = code:set_path(OrigPath),
  Result.
