-module(utils).
-author("salvoroni").

%% API
-export([create_endless_list/1, endless_list/2]).

create_endless_list(Rule) ->
  spawn(utils, endless_list, [Rule]).

endless_list(Rule, CurrentX) ->
  receive
    {Pid} ->
      Pid ! Rule(CurrentX),
      endless_list(Rule, CurrentX+1);
    finished -> ok
  end.