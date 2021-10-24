-module(utils).
-author("salvoroni").

%% API
-export([create_endless_list/1, endless_list/1]).

create_endless_list(Rule) ->
  spawn(utils, endless_list, [Rule]).

endless_list(Rule) ->
  receive
    {CurrentX, Pid} ->
      Pid ! Rule(CurrentX),
      endless_list(Rule);
    finished -> ok
  end.