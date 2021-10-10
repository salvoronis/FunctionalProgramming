-module(prime).
-author("salvoroni").

%% API
-export([start/0]).

start_tailrec_sol() ->
  tailrec_sol(0, -999, -999, [], 0, 0, 0).
tailrec_sol(_, 1000, 1000, _, MaxA, MaxB, MaxP) ->
  {MaxA, MaxB, MaxP};
tailrec_sol(_, A, 1000, _, MaxA, MaxB, MaxP) ->
  tailrec_sol(0, A+1, -999, [], MaxA, MaxB, MaxP);
tailrec_sol(N, A, B, Primes, MaxA, MaxB, MaxP) ->
  IsPrime = isPrime(N*N + A*N + B),
  if
    IsPrime == true ->
      tailrec_sol(N + 1, A, B, [N*N+A*N+B|Primes], MaxA, MaxB, MaxP);
    IsPrime == false ->
      Length = len(Primes),
      if
        Length > MaxP -> tailrec_sol(0, A, B + 1, [], A, B, Length);
        true -> tailrec_sol(0, A, B + 1, [], MaxA, MaxB, MaxP)
      end
  end.


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
    "-> ~p~n",
    [start_tailrec_sol()]).