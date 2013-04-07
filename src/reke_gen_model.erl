%% Copyright
-module(reke_gen_model).
-author("pfeairheller").

-export([execute/1]).


execute([ModelName | ColumnSpecs]) ->
  Name = string:to_lower(ModelName),
  io:format("Generating model ~s with columns ~p~n", [Name, ColumnSpecs]),
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
  FileName = io_lib:format("db/migrate/~p~2..0B~2..0B~2..0B~2..0B~2..0B_create_~s.ebm", [Year, Month, Day, Hour, Min, Sec, Name]),
  Create = create_table_term(Name, ColumnSpecs),
  Drop = drop_table_term(Name),
  file:write_file(FileName,io_lib:fwrite("~p.\n~n~p.~n",[Create, Drop])).



create_table_term(ModelName, ColumnSpecs) ->
  {up,
    {create_table, ModelName,
      create_columns(ColumnSpecs)
    }
  }.

create_columns(ColumnSpec) ->
  [ColSpec | []] = ColumnSpec,
  create_columns([], string:tokens(ColSpec, " ")).
create_columns(Columns, []) ->
  lists:reverse([{timestamps} | Columns]);
create_columns(Columns, [ColumnSpec | Rest]) ->
  [Name, Type] = string:tokens(ColumnSpec, ":"),
  Column = {list_to_atom(string:to_lower(Type)), string:to_lower(Name)},
  create_columns([Column | Columns], Rest).

drop_table_term(ModelName) ->
  {down,
    {drop_table, ModelName}
  }.

