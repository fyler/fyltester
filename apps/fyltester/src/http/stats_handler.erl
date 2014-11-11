-module(stats_handler).
-author("ilia").

-include("fyltester.hrl").
-include("log.hrl").

%% API
-export([
  init/2
]).

init(Req, State) ->
  Stats = jiffy:encode(fyltester_server:stats()),
  {ok, cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Stats, Req), State}.