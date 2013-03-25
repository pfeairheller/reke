-module(create_organizations).

-export([version/0, up/0, down/0]).

up() ->
  {create_table, organizations,
    [
      {string, name},
      {string, git_account_name},
      {string, code},
      {string, access_token},
      {timestamps}
    ]
  }.

down() ->
  {drop_table, organizations}.

version() ->
  20130323122819.
