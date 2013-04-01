%% Copyright
-module(reke_generate).
-author("pfeairheller").

-export([generate/1]).


generate(["model" | [ModelName | ColumnSpecs]]) ->
  io:format("Generating model ~s with columns ~p~n", [ModelName, ColumnSpecs]),
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
  FileName = io_lib:format("db/migrate/~p~2..0B~2..0B~2..0B~2..0B~2..0B_create_~s.ebm", [Year, Month, Day, Hour, Min, Sec, ModelName]),
  Data = create_table_term(ModelName, ColumnSpecs),
  file:write_file(FileName,io_lib:fwrite("~p.\n",[Data]));
generate(["migration" | [MigrationName | ColumnSpecs]]) ->
  io:format("Migrating with ~s with columns ~p~n", [MigrationName, ColumnSpecs]).



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
