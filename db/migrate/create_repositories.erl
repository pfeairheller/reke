-module(create_repositories).

-export([version/0, up/0, down/0]).

up() ->
  {create_table, repositories,
    [
      {string, name},
      {string, url},
      {timestamps}
    ]
  }.

down() ->
  {drop_table, repositories}.

version() ->
  20130323124334.
