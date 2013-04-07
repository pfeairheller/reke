%% Copyright
-module(reke_sql).
-author("pfeairheller").

%% API
-export([generate_drop_statement/1, generate_create_statement/2]).

generate_drop_statement(TableName) ->
  "DROP TABLE " ++ TableName.

generate_create_statement(TableName, Contents) ->
  "CREATE TABLE " ++ TableName ++ "(\n     id serial NOT NULL \n" ++  create_table_columns("", Contents) ++ ")".

create_table_columns(Stmt, []) ->
  Stmt;
create_table_columns(Stmt, [Column|Columns]) ->
  create_table_columns(Stmt ++ ",\n" ++ column_ref(Column), Columns).


column_ref({string, Name}) ->
  Name ++ " character varying(255)";
column_ref({integer, Name}) ->
  Name ++ " integer";
column_ref({timestamps}) ->
  "created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL".


