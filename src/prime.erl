-module(prime).
-author("salvoroni").

%% API
-export([start/0, get_primes/2]).
-import(utils, [create_endless_list/1, endless_list/1]).

%% tail recursion
start_tailrec_sol() ->
  tailrec_sol(0, -999, -999, [], 0, 0, 0).
tailrec_sol(_, 1000, 1000, _, MaxA, MaxB, MaxP) ->
  {MaxA, MaxB, MaxP};
tailrec_sol(_, A, 1000, _, MaxA, MaxB, MaxP) ->
  tailrec_sol(0, A+1, -999, [], MaxA, MaxB, MaxP);
tailrec_sol(N, A, B, Primes, MaxA, MaxB, MaxP) ->
  case isPrime(N*N + A*N + B) of
    true ->
      tailrec_sol(N + 1, A, B, [N*N+A*N+B|Primes], MaxA, MaxB, MaxP);
    false ->
      case len(Primes) of
        Length when Length > MaxP -> tailrec_sol(0, A, B + 1, [], A, B, Length);
        _ -> tailrec_sol(0, A, B + 1, [], MaxA, MaxB, MaxP)
      end
  end.

%% endless list solution
start_endless_solution() ->
  IterA = create_endless_list(fun(X) -> X + 1 end),
  IterB = create_endless_list(fun(X) -> X + 1 end),
  endless_solution(0, -999, -999, [], 0, 0, 0, IterA, IterB).
endless_solution(_, 999, _, _, MaxA, MaxB, MaxP, IterA, IterB) ->
  IterA ! finished,
  IterB ! finished,
  {MaxA, MaxB, MaxP};
endless_solution(_, A, 999, Primes, MaxA, MaxB, MaxP, IterA, IterB) ->
  IterB ! finished,
  NextIterB = create_endless_list(fun(X) -> X + 1 end),
  IterA ! {A, self()},
  receive
    NextA ->
      endless_solution(0, NextA, -999, Primes, MaxA, MaxB, MaxP, IterA, NextIterB)
  end;
endless_solution(N, A, B, Primes, MaxA, MaxB, MaxP, IterA, IterB) ->
  case isPrime(N*N + A * N + B) of
  true ->
    endless_solution(N + 1, A, B, [N*N+A*N+B|Primes], MaxA, MaxB, MaxP, IterA, IterB);
  false ->
    IterB ! {B, self()},
    receive
      NextB ->
        case len(Primes) of
          Len when Len > MaxP -> endless_solution(0, A, NextB, [], A, B, Len, IterA, IterB);
          _ -> endless_solution(0, A, NextB, [], MaxA, MaxB, MaxP, IterA, IterB)
        end
    end
  end.

%% usual recursion
start_recurry_sol() ->
  recurry_sol(-999, -999).
recurry_sol(999, _) ->
  {0, 0, -1};
recurry_sol(A, 999) ->
  recurry_sol(A + 1, -999);
recurry_sol(A, B) ->
  This = get_primes(A, B),
  NextVal = recurry_sol(A, B + 1),
  Max = if
    element(3,NextVal) < element(3, This) -> This;
    true -> NextVal
  end,
  Max.

get_primes(A, B) ->
  primes(0, A, B, []).
primes(_, A, 999, _) ->
  primes(0, A + 1, -999, []);
primes(N, A, B, Primes) ->
  case isPrime(N*N + A*N + B) of
    true ->
      primes(N + 1, A, B, [N*N+A*N+B|Primes]);
    false ->
      Length = len(Primes),
      {A, B, Length}
  end.

%% seq generated solution
seq_gen_sol() ->
  lists:foldl(
    fun(A, Acc) ->
      MaxB = lists:foldl(
        fun(B, AccB) ->
          This = get_primes(A, B),
          if
            element(3, AccB) > element(3, This) -> AccB;
            true -> This
          end
        end,
        {0, 0, -1},
        lists:seq(-999, 999)
      ),
      if
        element(3, MaxB) > element(3, Acc)-> MaxB;
        true -> Acc
      end
    end,
    {0, 0, -1},
    lists:seq(-999, 999)
  ).

%% map solution
map_solution() ->
  lists:mapfoldl(
    fun(A, Acc) ->
      MaxB = lists:foldl(
        fun(B, AccB) ->
          This = get_primes(A, B),
          if
            element(3, AccB) > element(3, This) -> AccB;
            true -> This
          end
        end,
        {0, 0, -1},
        lists:seq(-999, 999)
      ),
      if
        element(3, MaxB) > element(3, Acc)-> {A, MaxB};
        true -> {A, Acc}
      end
    end,
    {0, 0, -1},
    lists:seq(-999, 999)
  ).

len(L) ->
  len(L, 0).
len([], Acc) ->
  Acc;
len([_|Tail], Acc) ->
  len(Tail, Acc + 1).

isPrime(N) -> isPrime(N,2).
isPrime(-1,_) -> false;
isPrime(1,_) -> false;
isPrime(N, M) when M * M > abs(N) -> true;
isPrime(N,M)->
  if
    N rem M == 0 -> false;
    true -> isPrime(N,M+1)
  end.

start() ->
  io:format(
    "tailrec-> ~p~n",
    [start_tailrec_sol()]),
  io:format(
    "rec-> ~p~n",
    [start_recurry_sol()]),
  io:format(
    "seq gen-> ~p~n",
    [seq_gen_sol()]),
  io:format(
    "map-> ~p~n",
    [element(2,map_solution())]),
  io:format(
    "endless-> ~p~n",
    [start_endless_solution()]).