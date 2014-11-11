-define(Config(X,Y), ulitos_app:get_var(fyltester,X,Y)).

-define(Prefix, "https://s3-eu-west-1.amazonaws.com/").

-define (T_TASKS, fyltester_tasks).

-record(task, {
  id :: non_neg_integer(),
  status = queued :: queued | progress | success | failed,
  file :: string(),
  type :: atom(),
  start_ts = 0:: non_neg_integer(),
  ts = 0:: non_neg_integer()
}).