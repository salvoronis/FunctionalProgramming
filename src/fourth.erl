-module(fourth).
-author("salvoroni").

-export([start/0]).
-import(utils, [create_endless_list/1, endless_list/1]).

%% tailrec

palindrome_tailrec(999, 999, Max) ->
  Max;
palindrome_tailrec(X, 999, Max) ->
  palindrome_tailrec(X + 1, 100, Max);
palindrome_tailrec(X, Y, Max) ->
  Reversed = erlang:list_to_integer(lists:reverse(integer_to_list(X * Y))),
  if
    X * Y == Reversed, X * Y > Max ->
      palindrome_tailrec(X, Y + 1, X * Y);
    true ->
      palindrome_tailrec(X, Y + 1, Max)
  end.

%% endless list solution

start_palindrome_endless() ->
  IterX = create_endless_list(fun(X) -> X + 1 end),
  IterY = create_endless_list(fun(X) -> X + 1 end),
  palindrome_endless(100, 100, 0, IterX, IterY).
palindrome_endless(999, 999, Max, IterX, IterY) ->
  IterY ! finished,
  IterX ! finished,
  Max;
palindrome_endless(X, 999, Max, IterX, IterY) ->
  IterY ! finished,
  NewIterY = create_endless_list(fun(X) -> X + 1 end),
  IterX ! {X, self()},
  receive
    NewX -> palindrome_endless(NewX, 100, Max, IterX, NewIterY)
  end;
palindrome_endless(X, Y, Max, IterX, IterY) ->
  IterY ! {Y, self()},
  receive
    NextY ->
      case erlang:list_to_integer(lists:reverse(integer_to_list(X * Y))) of
        Reversed when X * Y == Reversed, X * Y > Max ->
          palindrome_endless(X, NextY, X * Y, IterX, IterY);
        _ ->
          palindrome_endless(X, NextY, Max, IterX, IterY)
      end
  end.

%% usual recurse

palindrome_rec(100, 100) ->
  10000;
palindrome_rec(X, 100) ->
  palindrome_rec(X-1, 999);
palindrome_rec(X, Y) ->
  Reversed = erlang:list_to_integer(lists:reverse(integer_to_list(X * Y))),
  PrevVal = palindrome_rec(X, Y-1),
  if
    X * Y == Reversed, X * Y > PrevVal ->
      X * Y;
    true ->
      PrevVal
  end.

%% using seq generations

seq_gen_palindrome() ->
  lists:foldl(
    fun(X, Acc) ->
      MaxY = lists:foldl(
        fun(Y, AccY) ->
          Reversed = erlang:list_to_integer(lists:reverse(integer_to_list(X * Y))),
          if
            X * Y == Reversed, X * Y > AccY ->
              X * Y;
            true ->
              AccY
          end
        end,
        0,
        lists:seq(100, 999)),
      if
        MaxY > Acc ->
          MaxY;
        true ->
          Acc
      end
    end,
    0,
    lists:seq(100, 999)).

%% using map function
map_palindrome() ->
  lists:mapfoldl(
    fun(X, Acc) ->
      MaxY = lists:foldl(
        fun(Y, AccY) ->
          Reversed = erlang:list_to_integer(lists:reverse(integer_to_list(X * Y))),
          if
            X * Y == Reversed, X * Y > AccY ->
              X * Y;
            true ->
              AccY
          end
        end,
        0,
        lists:seq(100, 999)),
      {MaxY, if MaxY > Acc -> MaxY; true -> Acc end}
    end,
    0,
    lists:seq(100, 999)
  ).


start() ->
  io:format(
    "Max palindrome by multiplying two 3 digital numbers using tail recurcy = ~p~n",
    [palindrome_tailrec(100, 100, 0)]),
  io:format(
    "Max palindrome by multiplying two 3 digital numbers using usual recurcy = ~p~n",
    [palindrome_rec(999, 999)]),
  io:format(
    "Max palindrome by multiplying two 3 digital numbers using seq generation = ~p~n",
    [seq_gen_palindrome()]),
  io:format(
    "Max palindrome by multiplying two 3 digital numbers using map = ~p~n",
    [element(2,map_palindrome())]),
  io:format(
    "Max palindrome by multiplying two 3 digital numbers using endless list = ~p~n",
    [start_palindrome_endless()]).