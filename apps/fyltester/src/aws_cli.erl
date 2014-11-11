-module(aws_cli).
-include("log.hrl").

%% API
-export([copy_folder/2]).

-spec copy_folder(string(), string()) -> any().

copy_folder(From, To) ->
  os:cmd(io_lib:format("aws s3 sync --acl public-read ~s ~s",[From, To])).