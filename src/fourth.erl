-module(fourth).
-author("salvoroni").

-export([start/0]).

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


start() ->
  io:format(
    "Максимальный палиндром, полученный перемножением 2-х 3-х значных чисел = ~p~n",
    [palindrome_tailrec(100, 100, 0)]),
  io:format(
    "Максимальный палиндром, полученный перемножением 2-х 3-х значных чисел = ~p~n",
    [palindrome_rec(999, 999)]).