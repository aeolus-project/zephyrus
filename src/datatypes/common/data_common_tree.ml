(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(* Depends on nothing.
*)

type ('leaf_data, 'node_data) tree =
  | Node of 'node_data * ('leaf_data, 'node_data) tree list
  | Leaf of 'leaf_data

let rec fold_tree f_leaf f_node tree =
  match tree with
  | Leaf (leaf_data)           -> f_leaf leaf_data
  | Node (node_data, children) -> f_node node_data (List.map (fold_tree f_leaf f_node) children)

type tree_index = int list

let tree_level_of_tree_index = List.length

let foldi_tree f_leaf f_node tree =
  let rec foldi_tree' tree index =
    match tree with
    | Node (node_data, children) -> 
        f_node index node_data (Data_common_list.List.mapi (fun i subtree -> foldi_tree' subtree (i::index)) children)
    | Leaf (leaf_data) -> 
        f_leaf index leaf_data
  in
  foldi_tree' tree []

let map_tree f_leaf f_node =
  fold_tree 
    (fun leaf_data          -> Leaf (f_leaf leaf_data))
    (fun node_data children -> Node (f_node node_data, children) )

type ('leaf_data, 'node_data) tree_walk_step =
  | Entered_node of 'node_data
  | Exitted_node of 'node_data
  | Reached_leaf of 'leaf_data

type ('leaf_data, 'node_data) indexed_tree_walk_step = (tree_index * ('leaf_data, 'node_data) tree_walk_step)

let walk tree =
  fold_tree
    (fun leaf_data          -> [Reached_leaf leaf_data])
    (fun node_data children -> [Entered_node node_data] @ (List.flatten children) @ [Exitted_node node_data])
    tree

let walki tree =
  foldi_tree
    (fun index leaf_data          -> [index, Reached_leaf (leaf_data)])
    (fun index node_data children -> [index, Entered_node (node_data)] @ (List.flatten children) @ [index, Exitted_node (node_data)])
    tree

(*
(* Direct implementation: *)

let rec walkk tree = match tree with
  | Node (node_data, children) -> [(Entered_node node_data)] @ (List.flatten (List.map walkk children)) @ [(Exitted_node node_data)]
  | Leaf (leaf_data)           -> [Reached_leaf leaf_data]

let walkii tree = 
  let rec walki' tree (index : int list) =
    match tree with
    | Node (node_data, children) -> 
        [index, Entered_node (node_data)] @ 
        (List.flatten (Data_common_list.List.mapi (fun i subtree -> walki' subtree (i::index)) children)) @ 
        [index, Exitted_node (node_data)]
    | Leaf (leaf_data) -> 
        [index, Reached_leaf (leaf_data)]
  in
  walki' tree []
*)

(*
(* Example trees: *)

let t0 =
  Node ("root", [
    Node ("group 1", [
      Leaf ("a");
      Leaf ("b");
      Leaf ("c")
    ]);
    Node ("group 1", [
      Leaf ("d");
      Leaf ("e");
      Leaf ("f")
    ])
  ])

let t1 =
  Node ("root", [
    Node ("group 1", [
      Leaf ("a");
      Leaf ("b");
      Leaf ("c")
    ]);
    Node ("group 1", [
      Leaf ("d");
      Leaf ("e");
      Leaf ("f");
      Leaf ("g")
    ])
  ])
*)
