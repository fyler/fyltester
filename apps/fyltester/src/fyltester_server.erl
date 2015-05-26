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

  Session = new_session(),

  Dir = ?Config(dir, ""),
  AwsDir = ?Config(aws_s3_bucket, "tbconvert") ++ "/" ++ ?Config(aws_dir, "fyltester") ++ "/",
  Tasks = fill_table(?Config(tasks, [])),

  Login = ?Config(login, ""),
  Pass = ?Config(pass, ""),

  Fyler = iolist_to_binary(?Config(fyler, "http://localhost:8008")),
  Callback = iolist_to_binary(io_lib:format("~s:~p/callback/~s/", [?Config(fyltester, "http://localhost"), ?Config(http_port, 8080), Session])),

  start_http_server(Session),

  hackney:start(),

  self() ! start,

  {ok, #state{tasks = Tasks, fyler = Fyler, dir = Dir, aws_dir = AwsDir, loginpass = iolist_to_binary("login="++Login++"&pass="++Pass), callback = Callback}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({task_complete, Id, Status}, State) ->
  case ets:lookup(?T_TASKS, Id) of
    [#task{start_ts = Start, category = Category} = Task] ->
      ets:insert(?T_TASKS, Task#task{status = Status, ts = ulitos:timestamp() - Start}),
      self() ! {start_task, Category};
    _ ->
      ?E("Invalid task id")
  end,
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(start, #state{tasks = Tasks, dir = Dir, aws_dir = AwsDir} = State) ->
  aws_cli:copy_folder(Dir, "s3://" ++ AwsDir),
  [self() ! {start_task, Category} || Category <- maps:keys(Tasks)],
  {noreply, State};

handle_info({start_task, Category}, #state{tasks = Tasks, fyler = Fyler, aws_dir = AwsDir, loginpass = LoginPass, callback = Callback} = State) ->
  case maps:get(Category, Tasks, []) of
    [Id|Ids] ->
      case ets:lookup(?T_TASKS, Id) of
        [#task{file = File, type = Type} = Task] ->
          case login(Fyler, LoginPass) of
            {ok, Token} ->
              URL = <<Fyler/binary, "/api/tasks">>,
              Body = iolist_to_binary(io_lib:format("url=~s~s~s&type=~p&callback=~s~p&fkey=~s", [?Prefix, AwsDir, File, Type, Callback, Id, Token])),
              ?D({"Starting new task", File, Type}),
              case hackney:post(URL, [], Body, []) of
                {ok, 200, _, _} ->
                  ets:insert(?T_TASKS, Task#task{start_ts = ulitos:timestamp(), status = progress});
                _Smth ->
                  ?E({"Failed to start task", File, Type, _Smth}),
                  self() ! {start_task, Category},
                  ets:insert(?T_TASKS, Task#task{status = failed})
              end;
            auth_failed ->
              self() ! {start_task, Category},
              ets:insert(?T_TASKS, Task#task{status = failed})
          end,
          {noreply, State#state{tasks = maps:put(Category, Ids, Tasks)}};
        _ ->
          {noreply, State}
      end;
    [] ->
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_http_server(Session) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", stats_handler, []},
      {"/stats", stats_handler, []},
      {"/callback/" ++ Session ++ "/:id", [{id, int}], callback_handler, []},
      {'_', notfound_handler, []}
    ]}
  ]),
  Port = ?Config(http_port, 8080),
  cowboy:start_http(http_listener, 20,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ).

login(Fyler, LoginPass) ->
  case hackney:post(<<Fyler/binary, "/api/auth">>, [], LoginPass, []) of
    {ok, 200, _Headers, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, Body} ->
          {[{<<"token">>, Fkey}]} = jiffy:decode(Body),
          {ok, binary_to_list(Fkey)};
        {error, _Reason} ->
          auth_failed
      end;
    {ok, 401, _Headers, _Body} ->
      ?E("Wrong login or password"),
      auth_failed;
    _ ->
      auth_failed
  end.

fill_table(Tasks) ->
  fill_table(Tasks, #{}, 1).

fill_table([{Category, Tasks}|Other], Map, N) ->
  {NewN, Ids, TasksForTable} = fill_table(Category, Tasks, N, [], []),
  ets:insert(?T_TASKS, TasksForTable),
  fill_table(Other, maps:put(Category, Ids, Map), NewN);

fill_table([], Map, _N) ->
  Map.

fill_table(Category, [{File, Type}|Tasks], Counter, Acc1, Acc2) ->
  fill_table(Category, Tasks, Counter + 1, [Counter|Acc1], [#task{id = Counter, file = File, type = Type, category = Category}|Acc2]);

fill_table(_Category, [], Counter, Acc1, Acc2) ->
  {Counter, lists:reverse(Acc1), Acc2}.

new_session() ->
  random:seed(erlang:now()),
  Length = random:uniform(4) + 4,
  random_string(Length).

random_string(0) -> [];

random_string(Length) -> [random_char() | random_string(Length-1)].

random_char() ->
  N = random:uniform(62),
  if
    N < 11 -> N + 47;
    N < 37 -> N + 54;
    true -> N + 60
  end.
