-module(fyltester_server).
-author("ilia").

-behaviour(gen_server).

%% API
-export([start_link/0, stats/0, task_complete/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("fyltester.hrl").
-include("log.hrl").

-record(state, {
  tasks = [] :: [],
  counter = 1 :: non_neg_integer(),
  fyler :: string(),
  dir :: string(),
  aws_dir :: string(),
  loginpass :: binary(),
  callback :: string()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stats() ->
  GetStat = fun
    (#task{file = File, type = Type, status = progress, start_ts = Start}, List) -> [{[{status, progress}, {file, iolist_to_binary(File)}, {type, Type}, {ts, ulitos:timestamp() - Start}]}|List];
    (#task{file = File, type = Type, status = Status, ts = Ts}, List) -> [{[{status, Status}, {file, iolist_to_binary(File)}, {type, Type}, {ts, Ts}]}|List]
  end,
  lists:reverse(ets:foldl(GetStat, [], ?T_TASKS)).

task_complete(Id, Status) ->
  gen_server:cast(fyltester_server, {task_complete, Id, Status}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->

  ets:new(?T_TASKS, [public, named_table, {keypos, #task.id}]),

  Dir = ?Config(dir, ""),
  AwsDir = ?Config(aws_s3_bucket, "tbconvert") ++ "/" ++ ?Config(aws_dir, "fyltester") ++ "/",
  Tasks = ?Config(tasks, []),
  fill_table(Tasks),

  Login = ?Config(login, ""),
  Pass = ?Config(pass, ""),

  Fyler = ?Config(fyler, "http://localhost:8008"),
  Callback = ?Config(fyltester, "http://localhost:8080") ++ "/callback/",

  start_http_server(),

  self() ! start,

  {ok, #state{tasks = Tasks, fyler = Fyler, dir = Dir, aws_dir = AwsDir, loginpass = iolist_to_binary("login="++Login++"&pass="++Pass), callback = Callback}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({task_complete, Id, Status}, State) ->
  case ets:lookup(?T_TASKS, Id) of
    [#task{start_ts = Start} = Task] ->
      ets:insert(?T_TASKS, Task#task{status = Status, ts = ulitos:timestamp() - Start}),
      self() ! start_task;
    _ ->
      ?E("Invalid task id")
  end,
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(start, #state{dir = Dir, aws_dir = AwsDir} = State) ->
  aws_cli:copy_folder(Dir, "s3://" ++ AwsDir),
  self() ! start_task,
  {noreply, State};

handle_info(start_task, #state{tasks = [{File, Type}|Tasks], counter = Counter, fyler = Fyler, aws_dir = AwsDir, loginpass = LoginPass, callback = Callback} = State) ->
  {ok, Token} = login(Fyler, LoginPass),
  URL = Fyler ++ "/api/tasks",
  Body = io_lib:format("url=~s~s~s&type=~p&callback=~s~p&fkey=~s", [?Prefix, AwsDir, File, Type, Callback, Counter, Token]),
  ibrowse:send_req(URL, [], post, Body),
  ets:insert(?T_TASKS, #task{id = Counter, file = File, type = Type, start_ts = ulitos:timestamp(), status = progress}),
  {noreply, State#state{tasks = Tasks, counter = Counter + 1}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_http_server() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", stats_handler, []},
      {"/stats", stats_handler, []},
      {"/callback/:id", [{id, int}], callback_handler, []},
      {'_', notfound_handler, []}
    ]}
  ]),
  Port = ?Config(http_port, 8080),
  cowboy:start_http(http_listener, 100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ).

login(Fyler, LoginPass) ->
  case ibrowse:send_req(Fyler ++ "/api/auth", [], post, LoginPass) of
    {ok, "200", _Headers, Json} ->
      {[{<<"token">>, Fkey}]} = jiffy:decode(Json),
      {ok, binary_to_list(Fkey)};
    {ok, "401", _Headers, _Body} ->
      ?E("Wrong login or password"),
      auth_faled;
    _ ->
      auth_failed
  end.

fill_table(Tasks) ->
  ets:insert(?T_TASKS, fill_table(Tasks, 1, [])).

fill_table([{File, Type}|Tasks], Counter, Acc) ->
  fill_table(Tasks, Counter + 1, [#task{id = Counter, file = File, type = Type}|Acc]);

fill_table([], _Counter, Acc) ->
  Acc.
