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

type ('node_data, 'leaf_data) tree =
  | Node of 'node_data * ('node_data, 'leaf_data) tree list
  | Leaf of 'leaf_data

type ('node_data, 'leaf_data) tree_walk_step =
  | Entered_node of 'node_data
  | Exitted_node of 'node_data
  | Reached_leaf of 'leaf_data

let rec walk tree = match tree with
  | Node (node_data, subtree) -> [(Entered_node node_data)] @ (List.flatten (List.map walk subtree)) @ [(Exitted_node node_data)]
  | Leaf (leaf_data)          -> [Reached_leaf leaf_data]
