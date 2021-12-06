-module(utils).
-author("salvoroni").

%% API
-export([create_endless_list/1, endless_list/2, next/1, filter_next/2, delete/1]).

-spec create_endless_list(function()) -> any().

create_endless_list(Rule) ->
  spawn(utils, endless_list, [Rule]).

endless_list(Rule, CurrentX) ->
  receive
    {Pid} ->
      Pid ! Rule(CurrentX),
      endless_list(Rule, CurrentX+1);
    finished -> ok
  end.

next(ListIter) ->
  ListIter ! self(),
  receive
    Next -> Next
  after 10000 -> error
  end.

filter_next(ListIter, FilterFunc) ->
  case next(ListIter) of
    Next when Next == error -> error;
    Next when Next =/= error ->
      case FilterFunc(Next) of
        true -> Next;
        _ -> filter_next(ListIter, FilterFunc)
      end
  end.

delete(ListIter) ->
  ListIter ! finished.