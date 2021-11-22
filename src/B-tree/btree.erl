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

-record(node, {left, right = undefined, height, val}).

init_btree(Val) -> #node{left = undefined, right = undefined, val = Val, height = 1}.

add(Node, Val) when is_record(Node, node) ->
  ResNode = case Node#node.val >= Val of
    true ->
      case Node#node.left of
        Left when is_record(Left, node) -> left = Node#node{left = add(Left, Val)};
        undefined -> Node#node{left = #node{left = undefined, right = undefined, val = Val, height = Node#node.height + 1}}
      end;
    false ->
      case Node#node.right of
        Right when is_record(Right, node) -> Node#node{right = add(Right, Val)};
        undefined -> Node#node{right = #node{left = undefined, right = undefined, val = Val, height = Node#node.height + 1}}
      end
  end,
  balance(ResNode).

height(Node) ->
  case Node of
    undefined -> 0;
    Result -> Result#node.height
  end.

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
  io:format("~w|~w~n", [Node#node.val, Node#node.height]),
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

remove(Node, Key) ->
  New = case Node of
    undefined -> ok;
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

start() ->
  Tree0 = init_btree(2),
  io:format("~w~n",[Tree0]),
  Tree1 = add(Tree0, 1),
  io:format("~w~n",[Tree1]),
  Tree2 = add(Tree1, 3),
  io:format("~w~n",[Tree2]),
  Tree3 = add(Tree2, 4),
  io:format("~w~n",[Tree3]),
  print_btree(Tree3),
  TreeRm = remove(Tree3, 4),
  print_btree(TreeRm).