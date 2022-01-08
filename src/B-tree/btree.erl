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

-record(node, {left = undefined, right = undefined, height = 1, val}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("proper/include/proper.hrl").

init_btree(Val) -> #node{val = Val}.

add(Node, Val) when is_record(Node, node) ->
  balance(
    case Node#node.val >= Val of
      true ->
        case Node#node.left of
          Left when is_record(Left, node) -> Node#node{left = add(Left, Val)};
          undefined -> Node#node{left = #node{left = undefined, right = undefined, val = Val}}
        end;
      false ->
        case Node#node.right of
          Right when is_record(Right, node) -> Node#node{right = add(Right, Val)};
          undefined -> Node#node{right = #node{left = undefined, right = undefined, val = Val}}
        end
    end
  );
add(_, Val) -> init_btree(Val).

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
%%  QTwo.

rotate_left(Node) when is_record(Node, node) ->
  P = Node#node.right,
  Node1 = Node#node{right = P#node.left},
  Node2 = Node1#node{height = fix_height(Node1)},
  P1 = P#node{left = Node2},
  P1#node{height = fix_height(P1)}.
%%  PSuperNew.

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
  io:format("~w~n", [Node#node.val]),
  print_btree(Prefix ++ case IsLeft of true -> "│    "; false -> "     " end, Node#node.left, true),
  print_btree(Prefix ++ case IsLeft of true -> "│    "; false -> "     " end, Node#node.right, false);
print_btree(_, _, _) -> ok.

find_min(Node) when is_record(Node, node) ->
  case Node#node.left of
    undefined -> Node;
    LeftNode -> find_min(LeftNode)
  end.

remove_min(Node) when is_record(Node, node), Node#node.left == undefined ->
  Node#node.right;
remove_min(Node) when is_record(Node, node) ->
  balance(Node#node{left = remove_min(Node#node.left)}).

remove(Node, _) when Node == undefined ->
  Node;
remove(Node, Key) ->
  balance(case Node of
    Node when Key < Node#node.val -> Node#node{left = remove(Node#node.left, Key)};
    Node when Key > Node#node.val -> Node#node{right = remove(Node#node.right, Key)};
    Node when Node#node.val == Key ->
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
%%  balance(New).

get(Node, Key) ->
  get(Node, Key, undefined).
get(Node, Key, Parent) ->
  case Node of
    undefined -> false;
    Node when Key < Node#node.val -> get(Node#node.left, Key, Node);
    Node when Key > Node#node.val -> get(Node#node.right, Key, Node);
    Node when Key == Node#node.val -> {Node, Parent}
  end.



foldl(Fun, Acc, Node) ->
  case Node of
    undefined -> Acc;
    _ ->
      Acc0 = foldl(Fun, Acc, Node#node.left),
      Acc1 = Fun(Node#node.val, Acc0),
      foldl(Fun, Acc1, Node#node.right)
  end.

foldr(Fun, Acc, Node) ->
  case Node of
    undefined -> Acc;
    _ ->
      Acc0 = foldr(Fun, Acc, Node#node.right),
      Acc1 = Fun(Node#node.val, Acc0),
      foldr(Fun, Acc1, Node#node.left)
  end.

btree_map(Fun, Node) ->
  map(Fun, Node, undefined).
map(_, Node, Node1) when Node == undefined -> Node1;
map(Fun, Node, Node1)->
  Node2 = map(Fun, Node#node.left, Node1),
  Node3 = add(Node2, Fun(Node#node.val)),
  map(Fun, Node#node.right, Node3).

filter(Fun, Node) -> filter(Fun, Node, undefined).
filter(_, Node, Node1) when Node == undefined -> Node1;
filter(Fun, Node, Node1) ->
  Node2 = filter(Fun, Node#node.left, Node1),
  Node3 = case Fun(Node#node.val) of
    true -> add(Node2, Node#node.val);
    false -> Node2
  end,
  filter(Fun, Node#node.right, Node3).

addTree(undefined, undefined) -> undefined;
addTree(undefined, Tree) -> Tree;
addTree(Tree, undefined) -> Tree;
addTree(MasterTree, SlaveTree) ->
  MasterTree1 = addTree(MasterTree, SlaveTree#node.left),
  MasterTree2 = add(MasterTree1, SlaveTree#node.val),
  addTree(MasterTree2, SlaveTree#node.right).

subTree(undefined, undefined) -> undefined;
subTree(undefined, _) -> undefined;
subTree(Tree, undefined) -> Tree;
subTree(MasterTree, SlaveTree) ->
  MasterTree1 = subTree(MasterTree, SlaveTree#node.left),
  MasterTree2 = remove(MasterTree1, SlaveTree#node.val),
  subTree(MasterTree2, SlaveTree#node.right).

btree_equals(undefined, undefined) -> true;
btree_equals(undefined, _) -> false;
btree_equals(_, undefined) -> false;
btree_equals(FirstTree, SecondTree) when FirstTree#node.val /= SecondTree#node.val -> false;
btree_equals(FirstTree, SecondTree) ->
  btree_equals(FirstTree#node.left, SecondTree#node.left) and btree_equals(FirstTree#node.right, SecondTree#node.right).

from_list(List) ->
  from_list(List, #node{}).
from_list([], Tree) ->
  Tree;
from_list([Item|Tail], Tree) ->
  add(Tree, Item),
  from_list(Tail, Tree).


start() ->
  Tree0 = init_btree(6),
  Tree1 = add(Tree0, 2),
  Tree2 = add(Tree1, 3),
  Tree3 = add(Tree2, 4),
  Tree4 = add(Tree3, 5),
  io:format("~w~n", [btree_map(fun(X) -> X*2 end, add(init_btree(1),2))]),
  print_btree(Tree4),
  print_btree(filter(fun(X) -> case X of 3 -> false; _ -> true end end, Tree4)),
  io:format("equals = ~w~n", [btree_equals(Tree4, Tree4)]).

-ifdef(EUNIT).
init_test_() -> [
  ?_assert(init_btree(1) =:= #node{left = undefined, right = undefined, val = 1, height = 1})
].
add_test_() -> [
  ?_assert(add(not_a_node, 1) =:= #node{left = undefined, right = undefined, val = 1, height = 1}),
  ?_assert(add(add(not_a_node, 1),2) =:= #node{left = undefined, val = 1, height = 2, right = #node{left = undefined, right = undefined, val = 2, height = 1}})
].
height_test_() -> [
  ?_assert(height(add(not_a_node, 1)) =:= 1),
  ?_assert(height(undefined) =:= 0)
].
balance_factor_test_() -> [
  ?_assert(balance_factor(init_btree(1)) =:= 0),
  ?_assert(balance_factor(add(init_btree(1),2)) =:= 1)
].
fix_height_test_() -> [
  ?_assert(fix_height(add(init_btree(1),2)) =:= 2),
  ?_assert(fix_height(init_btree(1)) =:= 1)
].
balance_test_() -> [
  ?_assert(add(add(add(add(init_btree(6),2),3),4),5) =:= #node{
    val = 3,
    height = 3,
    left = #node{
      left = undefined,
      right = undefined,
      val = 2,
      height = 1
    },
    right = #node{
      val = 5,
      height = 2,
      left = #node{
        val = 4,
        height = 1,
        left = undefined,
        right = undefined
      },
      right = #node{
        val = 6,
        height = 1,
        left = undefined,
        right = undefined
      }
    }
  })
].
get_test_() -> [
  ?_assert(get(add(add(add(add(init_btree(6),2),3),4),5), 4) =:= {
    #node{val = 4, height = 1},
    #node{val = 5, height = 2, left = #node{val = 4, height = 1}, right = #node{val = 6, height = 1}}
  })
].
foldl_test_() -> [
  ?_assert(foldl(fun(X, Res) -> X + Res end,0,add(init_btree(1),2)) =:= 3)
].
foldr_test_() -> [
  ?_assert(foldr(fun(X, Res) -> X - Res*2 end, 0, add(init_btree(1),2)) =:= -3)
].
map_test_() -> [
  ?_assert(btree_map(fun(X) -> X*2 end, add(init_btree(1),2)) =:= #node{val = 2, height = 2, right = #node{val = 4}})
].
filter_test_() -> [
  ?_assert(filter(fun(X) -> case X of 2 -> false; _ -> true end end, add(init_btree(1),2)) =:= #node{val = 1})
].
add_tree_test_() -> [
  ?_assert(addTree(init_btree(1), init_btree(2)) =:= #node{val = 1, height = 2, right = #node{val = 2}})
].
sub_tree_test_() -> [
  ?_assert(subTree(add(init_btree(1),2), init_btree(2)) =:= #node{val = 1})
].
equals_test_() ->
  Tree0 = init_btree(6),
  Tree1 = add(Tree0, 2),
  Tree2 = add(Tree1, 3),
  Tree3 = add(Tree2, 4),
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

addition_commutative_test() ->
  ?assert(proper:quickcheck(prop_add_commutativity(), [{to_file, user}, {numtests, 1488}]) == true).
sub_not_commutative_test() ->
  ?assert(proper:quickcheck(prop_sub_not_commutativity(), [{to_file, user},{numtests, 666}]) == true).
addition_zero_elem_commutative_test() ->
  ?assert(proper:quickcheck(prop_add_zero_elem_commutativ(), [{to_file, user},{numtests, 777}]) == true).
-endif.