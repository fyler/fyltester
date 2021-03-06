-module(notfound_handler).
-include("log.hrl").
%% Cowboy_http_handler callbacks
-export([
  init/2
]).

init(Req, Opts) ->
  ?D({route_not_found}),
  Body = <<"<h1>404 Page Not Found</h1>">>,
  {ok, cowboy_req:reply(404, [], Body, Req), Opts}.
