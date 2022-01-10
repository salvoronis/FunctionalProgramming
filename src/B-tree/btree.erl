%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2021 3:30 PM
%%%-------------------------------------------------------------------
-module(btree).
-author("salvoroni").

%% API
-export([start/0]).

-record(node, {left = undefined, right = undefined, height = 1, key}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("proper/include/proper.hrl").

init_btree(Key) -> #node{key = Key}.

add(Node, Key) when is_record(Node, node) ->
  balance(
    case Node#node.key >= Key of
      true ->
        case Node#node.left of
          Left when is_record(Left, node) -> Node#node{left = add(Left, Key)};
          undefined -> Node#node{left = #node{left = undefined, right = undefined, key = Key}}
        end;
      false ->
        case Node#node.right of
          Right when is_record(Right, node) -> Node#node{right = add(Right, Key)};
          undefined -> Node#node{right = #node{left = undefined, right = undefined, key = Key}}
        end
    end
  );
add(undefined, Key) -> init_btree(Key).

height(undefined) -> 0;
height(Node) when is_record(Node, node)->
  Node#node.height.

balance_factor(Node) when is_record(Node, node) ->
  height(Node#node.right) - height(Node#node.left).

fix_height(Node) when is_record(Node, node) ->
  case {height(Node#node.left), height(Node#node.right)} of
    {Hl, Hr} when Hl > Hr -> Hl + 1;
    {_, Hr} -> Hr + 1
  end.

rotate_right(Node) when is_record(Node, node) ->
  Q = Node#node.left,
  Node1 = Node#node{left = Q#node.right},
  Node2 = Node1#node{height = fix_height(Node1)},
  Q1 = Q#node{right = Node2},
  Q1#node{height = fix_height(Q1)}.

rotate_left(Node) when is_record(Node, node) ->
  P = Node#node.right,
  Node1 = Node#node{right = P#node.left},
  Node2 = Node1#node{height = fix_height(Node1)},
  P1 = P#node{left = Node2},
  P1#node{height = fix_height(P1)}.

balance(undefined) -> undefined;
balance(Node) when is_record(Node, node) ->
  Node1 = Node#node{height = fix_height(Node)},
  case balance_factor(Node1) of
    BalanceFactor when BalanceFactor == 2 ->
      case balance_factor(Node1#node.right) of
        BFactor when BFactor < 0 ->
          Node2 = Node1#node{right = rotate_right(Node1#node.right)},
          rotate_left(Node2);
        _ -> rotate_left(Node1)
      end;
    BalanceFactor when BalanceFactor == -2 ->
      case balance_factor(Node1#node.left) of
        BFactor when BFactor > 0 ->
          Node2 = Node1#node{left = rotate_left(Node1#node.left)},
          rotate_right(Node2);
        _ -> rotate_right(Node1)
      end;
    _ -> Node1
  end.

print_btree(Node) when is_record(Node, node) ->
  print_btree("", Node, false).
print_btree(Prefix, Node, IsLeft) when is_record(Node, node) ->
  io:format(Prefix),
  case IsLeft of
    false -> io:format("r───");
    true -> io:format("l───")
  end,
  io:format("~w~n", [Node#node.key]),
  print_btree(Prefix ++ case IsLeft of true -> "│    "; false -> "     " end, Node#node.left, true),
  print_btree(Prefix ++ case IsLeft of true -> "│    "; false -> "     " end, Node#node.right, false);
print_btree(_, _, _) -> ok.

find_min(Node) when is_record(Node, node), Node#node.left == undefined -> Node;
find_min(Node) when is_record(Node, node) -> find_min(Node#node.left).

remove_min(Node) when is_record(Node, node), Node#node.left == undefined -> Node#node.right;
remove_min(Node) when is_record(Node, node) -> balance(Node#node{left = remove_min(Node#node.left)}).

remove(Node, _) when Node == undefined ->
  Node;
remove(Node, Key) ->
  balance(case Node of
    Node when Key < Node#node.key -> Node#node{left = remove(Node#node.left, Key)};
    Node when Key > Node#node.key -> Node#node{right = remove(Node#node.right, Key)};
    Node when Node#node.key == Key ->
      Q = Node#node.left,
      R = Node#node.right,
      case R of
        undefined -> Q;
        _ ->
          Min = find_min(R),
          Min1 = Min#node{right = remove_min(R), left = Q},
          balance(Min1)
      end
  end).

get(Node, Key) ->
  get(Node, Key, undefined).
get(Node, _, _) when Node == undefined -> false;
get(Node, Key, _) when Key < Node#node.key -> get(Node#node.left, Key, Node);
get(Node, Key, _) when Key > Node#node.key -> get(Node#node.right, Key, Node);
get(Node, Key, Parent) when Key == Node#node.key -> {Node, Parent}.

foldl(_, Acc, Node) when Node == undefined -> Acc;
foldl(Fun, Acc, Node) ->
  Acc0 = foldl(Fun, Acc, Node#node.left),
  Acc1 = Fun(Node#node.key, Acc0),
  foldl(Fun, Acc1, Node#node.right).

foldr(_, Acc, Node) when Node == undefined -> Acc;
foldr(Fun, Acc, Node) ->
  Acc0 = foldr(Fun, Acc, Node#node.right),
  Acc1 = Fun(Node#node.key, Acc0),
  foldr(Fun, Acc1, Node#node.left).

btree_map(Fun, Node) ->
  map(Fun, Node, undefined).
map(_, Node, Node1) when Node == undefined -> Node1;
map(Fun, Node, Node1)->
  Node2 = map(Fun, Node#node.left, Node1),
  Node3 = add(Node2, Fun(Node#node.key)),
  map(Fun, Node#node.right, Node3).

filter(Fun, Node) -> filter(Fun, Node, undefined).
filter(_, Node, Node1) when Node == undefined -> Node1;
filter(Fun, Node, Node1) ->
  Node2 = filter(Fun, Node#node.left, Node1),
  Node3 = case Fun(Node#node.key) of
    true -> add(Node2, Node#node.key);
    false -> Node2
  end,
  filter(Fun, Node#node.right, Node3).

addTree(undefined, undefined) -> undefined;
addTree(undefined, Tree) -> Tree;
addTree(Tree, undefined) -> Tree;
addTree(MasterTree, SlaveTree) ->
  MasterTree1 = addTree(MasterTree, SlaveTree#node.left),
  MasterTree2 = add(MasterTree1, SlaveTree#node.key),
  addTree(MasterTree2, SlaveTree#node.right).

subTree(undefined, undefined) -> undefined;
subTree(undefined, _) -> undefined;
subTree(Tree, undefined) -> Tree;
subTree(MasterTree, SlaveTree) ->
  MasterTree1 = subTree(MasterTree, SlaveTree#node.left),
  MasterTree2 = remove(MasterTree1, SlaveTree#node.key),
  subTree(MasterTree2, SlaveTree#node.right).

btree_equals(undefined, undefined) -> true;
btree_equals(undefined, _) -> false;
btree_equals(_, undefined) -> false;
btree_equals(FirstTree, SecondTree) when FirstTree#node.key /= SecondTree#node.key -> false;
btree_equals(FirstTree, SecondTree) ->
  btree_equals(FirstTree#node.left, SecondTree#node.left) and btree_equals(FirstTree#node.right, SecondTree#node.right).

from_list(List) ->
  from_list(List, undefined).
from_list([], Tree) ->
  Tree;
from_list([Item|Tail], Tree) ->
  from_list(Tail, add(Tree, Item)).


start() ->
  Tree0 = init_btree(6),
  Tree1 = add(Tree0, 2),
  Tree2 = add(Tree1, 3),
  print_btree(Tree2),
  print_btree(rotate_right(Tree2)),
  print_btree(rotate_left(Tree2)),
  Tree3 = add(Tree2, 4),
  Tree4 = add(Tree3, 5),
  io:format("~w~n", [btree_map(fun(X) -> X*2 end, add(init_btree(1),2))]),
  print_btree(Tree4),
  print_btree(filter(fun(X) -> case X of 3 -> false; _ -> true end end, Tree4)),
  io:format("equals = ~w~n", [btree_equals(Tree4, Tree4)]).

-ifdef(EUNIT).
init_test_() -> [
  ?_assert(init_btree(1) =:= #node{key = 1})
].
add_test_() -> [
  ?_assert(add(undefined, 1) =:= #node{key = 1}),
  ?_assert(add(add(undefined, 1),2) =:= #node{key = 1, height = 2, right = #node{key = 2}})
].
rotate_test_() -> [
  ?_assert(rotate_right(from_list([6,2,3])) =:= #node{key = 2, height = 3, right = #node{key = 3, height = 2, right = #node{key = 6}}}),
  ?_assert(rotate_left(from_list([6,2,3])) =:= #node{key = 6, height = 3, left = #node{key = 3, height = 2, left = #node{key = 2}}})
].
from_list_test_() -> [
  ?_assert(from_list([1]) =:= add(undefined, 1)),
  ?_assert(from_list([1,2]) =:= add(add(undefined, 1),2))
].
height_test_() -> [
  ?_assert(height(from_list([1])) == 1),
  ?_assert(height(undefined) == 0)
].
balance_factor_test_() -> [
  ?_assert(balance_factor(from_list([1])) =:= 0),
  ?_assert(balance_factor(from_list([1,2])) =:= 1)
].
fix_height_test_() -> [
  ?_assert(fix_height(from_list([1,2])) =:= 2),
  ?_assert(fix_height(from_list([1])) =:= 1)
].
balance_test_() -> [
  ?_assert(from_list([6,2,3,4,5]) =:= #node{
    key = 3,
    height = 3,
    left = #node{
      key = 2
    },
    right = #node{
      key = 5,
      height = 2,
      left = #node{
        key = 4
      },
      right = #node{
        key = 6
      }
    }
  })
].
get_test_() -> [
  ?_assert(get(from_list([6,2,3,4,5]), 4) =:= {
    #node{key = 4},
    #node{key = 5, height = 2,
      left = #node{key = 4},
      right = #node{key = 6}
    }
  })
].
foldl_test_() -> [
  ?_assert(
    foldl(
      fun(X, Res) -> X + Res end, 0, from_list([1,2])
    ) =:= 3
  )
].
foldr_test_() -> [
  ?_assert(
    foldr(
      fun(X, Res) -> X - Res*2 end, 0, from_list([1,2])
    ) =:= -3)
].
map_test_() -> [
  ?_assert(
    btree_map(
      fun(X) -> X*2 end, from_list([1,2])
    ) =:= from_list([2,4])
  )
].
filter_test_() -> [
  ?_assert(
    filter(
      fun(X) -> case X of 2 -> false; _ -> true end end, from_list([1,2])
    ) =:= from_list([1])
  )
].
add_tree_test_() -> [
  ?_assert(
    addTree(init_btree(1), init_btree(2)) =:= from_list([1,2])
  )
].
sub_tree_test_() -> [
  ?_assert(
    subTree(from_list([1,2]), from_list([2])) =:= from_list([1])
  )
].
equals_test_() ->
  Tree3 = from_list([6,2,3,4]),
  Tree4 = add(Tree3, 5),
  [
    ?_assert(btree_equals(Tree4, Tree4) =:= true),
    ?_assert(btree_equals(Tree4, Tree3) =:= false)
  ].

%%Prop tests
prop_add_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = from_list(L1),
      Tree2 = from_list(L2),
      btree_equals(addTree(Tree1, Tree2), addTree(Tree2, Tree1))
    end
  ).

prop_sub_not_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = from_list(L1),
      Tree2 = from_list(L2),
      case btree_equals(Tree1, Tree2) of
        true -> true;
        _ -> btree_equals(subTree(Tree1, Tree2), subTree(Tree2, Tree1)) == false
      end
    end
  ).

prop_add_zero_elem_commutativ() ->
  ?FORALL(
    {L1, L2},
    {list(), list(integer())},
    begin
      ZeroTree = from_list(L1),
      Tree = from_list(L2),
      equals(addTree(ZeroTree, Tree), addTree(Tree, ZeroTree))
    end
  ).

%%addition_commutative_test() ->
%%  ?assert(proper:quickcheck(prop_add_commutativity(), [{to_file, user}, {numtests, 1488}]) == true).
sub_not_commutative_test() ->
  ?assert(proper:quickcheck(prop_sub_not_commutativity(), [{to_file, user},{numtests, 666}]) == true).
%%addition_zero_elem_commutative_test() ->
%%  ?assert(proper:quickcheck(prop_add_zero_elem_commutativ(), [{to_file, user},{numtests, 777}]) == true).
-endif.