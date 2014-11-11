-module(fyltester_app).
-author("ilia").

-behaviour(application).

-include("log.hrl").
-include("fyltester.hrl").

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  ?I("Starting application: fyltester"),
  ulitos_app:load_config(fyltester, "fyltester.config"),
  fyltester_sup:start_link().

stop(_State) ->
  ok.

