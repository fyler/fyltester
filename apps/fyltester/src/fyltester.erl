%% Copyright
-module(fyltester).
-author("ilia").

-export([start/0, stop/0, upgrade/0]).

start() ->
  ulitos_app:ensure_started(fyltester).

stop() ->
  application:stop(fyltester).

upgrade() ->
  ulitos_app:reload(fyltester),
  ok.
