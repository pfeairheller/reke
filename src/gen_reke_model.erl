%% Copyright
-module(gen_reke_model).
-author("pfeairheller").

-export([behaviour_info/1]).

%% API
-export([start_model/1, get_result_proplist/2]).

behaviour_info(callbacks) ->
  [{init, 0}];
behaviour_info(_) ->
  undefined.

start_model(ModuleName) ->
  case apply(ModuleName, init, []) of
    {ok, _ModelSpec} ->
      {ok, ModelProperties} = load_model(ModuleName),
      {ok, ModelProperties};
    _ ->
      {error, invalid_model_spec}
  end.

load_model(ModelName) ->
  Sql = lists:concat(["select column_name, data_type, column_default, is_nullable from INFORMATION_SCHEMA.COLUMNS where table_name =  '", ModelName, "s'"]),
  case reke_connection:squery(pool1, Sql) of
    {ok, Columns, Rows} ->
      {ok, get_result_proplist(Columns, Rows)};
    {error, Error} ->
      {error, Error}
  end.

get_result_proplist(Columns, Rows) ->
  get_result_proplist(Columns, Rows, []).
get_result_proplist(_Columns, [], PropList) ->
  lists:nth(1, PropList);
get_result_proplist(Columns, [Row | Rows], PropList) ->
  case convert(Columns, Row) of
    {ok, Prop} ->
      get_result_proplist(Columns, Rows, [Prop | PropList]);
    {error, conversion_error} ->
      get_result_proplist(Columns, Rows, PropList)
  end.

convert(Columns, Values) ->
  {ok, lists:zipwith(fun(Column, Value) ->
    ColumnName = binary_to_atom(element(2, Column), utf8),
    StrValue = convert_value(Value),
    {ColumnName, StrValue}
  end,
    Columns, tuple_to_list(Values))}.

convert_value(Value) when Value == null ->
  null;
convert_value(Value) when is_atom(Value) ->
  atom_to_list(Value);
convert_value(Value) when is_binary(Value) ->
  binary_to_list(Value);
convert_value(Value) when is_list(Value) ->
  Value.







