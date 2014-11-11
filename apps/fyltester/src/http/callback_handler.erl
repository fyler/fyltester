-module(callback_handler).
-author("ilia").

-include("fyltester.hrl").
-include("log.hrl").

%% API
-export([
  init/2
]).

init(Req, State) ->
  Id = cowboy_req:binding(id, Req),
  case cowboy_req:body_qs(Req) of
    {ok, X, _} ->
      Status = binary_to_atom(proplists:get_value(<<"status">>, X, <<"failed">>), latin1),
      fyltester_server:task_complete(Id, Status);
    _ ->
      ?D("no data")
  end,
  {ok, cowboy_req:reply(200, Req), State}.