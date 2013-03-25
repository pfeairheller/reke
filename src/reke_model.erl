%% Copyright
-module(reke_model).

-behaviour(gen_server).

%% API
-export([all/0]).

%% Required.
-export([start_link/0, start_link/1, stop/0]).

%% gen_server
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).


start_link() ->
  {ok, FileName} = application:get_env(dets_name),
  start_link(FileName).

start_link(FileName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
  gen_server:cast(?MODULE, stop).


all() ->
  gen_server:call(?MODULE, all).


%Callback Functions
init(Filename) ->
  usr_db:create_tables(Filename),
  usr_db:restore_backup(),
  {ok, null}.

terminate(_Reason, _LoopData) ->
  usr_db:close_tables().

handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

handle_call(all, _From, LoopData) ->
  Reply = {ok, []},
  {reply, Reply, LoopData}.

