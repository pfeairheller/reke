%% Copyright
-module(reke_plugin).
-author("pfeairheller").

-export([
  'reke-db:migrate'/2,
  'reke-db:rollback'/2,
  'reke-gen:model'/2,
  'reke-gen:migration'/2
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

'reke-gen:model'(Config, _AppFile) ->
  OrigPath = code:get_path(),
  true = code:add_path(rebar_utils:ebin_dir()),
  ModelName = rebar_config:get_global(Config, model_name, undefined),
  Columns   = rebar_config:get_global(Config, column_spec, undefined),

  Result = case rebar_utils:processing_base_dir(Config) of
    true ->
      reke:start(),
      reke:execute(["gen", "model", ModelName, Columns]),
      ok;
    false -> ok
  end,

  true = code:set_path(OrigPath),
  Result.

'reke-gen:migration'(Config, _AppFile) ->
  OrigPath = code:get_path(),
  true = code:add_path(rebar_utils:ebin_dir()),

  Result = case rebar_utils:processing_base_dir(Config) of
    true ->
      reke:start(),
      reke:execute(["generate", "migration"]),
      ok;
    false -> ok
  end,

  true = code:set_path(OrigPath),
  Result.

