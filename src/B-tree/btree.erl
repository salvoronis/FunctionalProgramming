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

init_btree(Val) -> #node{left = undefined, right = undefined, val = Val, height = 1}.

add(Node, Val) when is_record(Node, node) ->
  ResNode = case Node#node.val >= Val of
    true ->
      case Node#node.left of
        Left when is_record(Left, node) -> Node#node{left = add(Left, Val)};
        undefined -> Node#node{left = #node{left = undefined, right = undefined, val = Val}}%%Node#node.height + 1}}
      end;
    false ->
      case Node#node.right of
        Right when is_record(Right, node) -> Node#node{right = add(Right, Val)};
        undefined -> Node#node{right = #node{left = undefined, right = undefined, val = Val}}%%Node#node.height + 1}}
      end
  end,
  balance(ResNode);
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
  NodeNew = Node#node{left = Q#node.right},
  NodeSuperNew = NodeNew#node{height = fix_height(NodeNew)},
  QNew = Q#node{right = NodeSuperNew},
  QSuperNew = QNew#node{height = fix_height(QNew)},
  QSuperNew.

rotate_left(Node) when is_record(Node, node) ->
  P = Node#node.right,
  NodeNew = Node#node{right = P#node.left},
  NodeSuperNew = NodeNew#node{height = fix_height(NodeNew)},
  PNew = P#node{left = NodeSuperNew},
  PSuperNew = PNew#node{height = fix_height(PNew)},
  PSuperNew.

balance(undefined) -> undefined;
balance(Node) when is_record(Node, node) ->
  NodeNew = Node#node{height = fix_height(Node)},
  case balance_factor(NodeNew) of
    BalanceFactor when BalanceFactor == 2 ->
      case balance_factor(NodeNew#node.right) of
        BFactor when BFactor < 0 ->
          NodeSuperNew = NodeNew#node{right = rotate_right(NodeNew#node.right)},
          rotate_left(NodeSuperNew);
        _ -> rotate_left(NodeNew)
      end;
    BalanceFactor when BalanceFactor == -2 ->
      case balance_factor(NodeNew#node.left) of
        BFactor when BFactor > 0 ->
          NodeSuperNew = NodeNew#node{left = rotate_left(NodeNew#node.left)},
          rotate_right(NodeSuperNew);
        _ -> rotate_right(NodeNew)
      end;
    _ -> NodeNew
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
  New = case Node of
    Node when Key < Node#node.val -> Node#node{left = remove(Node#node.left, Key)};
    Node when Key > Node#node.val -> Node#node{right = remove(Node#node.right, Key)};
    Node when Node#node.val == Key ->
      Q = Node#node.left,
      R = Node#node.right,
      case R of
        undefined -> Q;
        _ ->
          Min = find_min(R),
          NewMin = Min#node{right = remove_min(R), left = Q},
          balance(NewMin)
      end
  end,
  balance(New).

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
      NewAcc = Fun(Node#node.val, Acc0),
      foldl(Fun, NewAcc, Node#node.right)
  end.

foldr(Fun, Acc, Node) ->
  case Node of
    undefined -> Acc;
    _ ->
      Acc0 = foldr(Fun, Acc, Node#node.right),
      NewAcc = Fun(Node#node.val, Acc0),
      foldr(Fun, NewAcc, Node#node.left)
  end.

btree_map(Fun, Node) ->
  map(Fun, Node, ok).
map(_, Node, NewNode) when Node == undefined ->NewNode;
map(Fun, Node, NewNode)->
  SuperNewNode = map(Fun, Node#node.left, NewNode),
  NewNewNode = add(SuperNewNode, Fun(Node#node.val)),
  map(Fun, Node#node.right, NewNewNode).

filter(Fun, Node) -> filter(Fun, Node, ok).
filter(_, Node, NewNode) when Node == undefined -> NewNode;
filter(Fun, Node, NewNode) ->
  SuperNewNode = filter(Fun, Node#node.left, NewNode),
  NewNewNode = case Fun(Node#node.val) of
    true -> add(SuperNewNode, Node#node.val);
    false -> SuperNewNode
  end,
  filter(Fun, Node#node.right, NewNewNode).

addTree(undefined, undefined) -> undefined;
addTree(undefined, Tree) -> Tree;
addTree(Tree, undefined) -> Tree;
addTree(MainTree, SlaveTree) ->
  SuperNewMainTree = addTree(MainTree, SlaveTree#node.left),
  SuperPuperNewMainTree = add(SuperNewMainTree, SlaveTree#node.val),
  addTree(SuperPuperNewMainTree, SlaveTree#node.right).

subTree(undefined, undefined) -> undefined;
subTree(undefined, _) -> undefined;
subTree(Tree, undefined) -> Tree;
subTree(MainTree, SlaveTree) ->
  SuperNewMainTree = subTree(MainTree, SlaveTree#node.left),
  SuperPuperNewMainTree = remove(SuperNewMainTree, SlaveTree#node.val),
  subTree(SuperPuperNewMainTree, SlaveTree#node.right).

btree_equals(undefined, undefined) -> true;
btree_equals(undefined, _) -> false;
btree_equals(_, undefined) -> false;
btree_equals(FirstTree, SecondTree) when FirstTree#node.val /= SecondTree#node.val -> false;
btree_equals(FirstTree, SecondTree) when FirstTree#node.val == SecondTree#node.val ->
  true and btree_equals(FirstTree#node.left, SecondTree#node.left) and btree_equals(FirstTree#node.right, SecondTree#node.right).

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