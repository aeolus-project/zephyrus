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

(** Operations on polymorphic trees. *)

(* Depends on nothing.
*)

(** A polymorphic tree with data in nodes and leafs. *)
type ('node_data, 'leaf_data) tree =
  | Node of 'node_data * ('node_data, 'leaf_data) tree list
  | Leaf of 'leaf_data

val fold_tree : ('leaf_data -> 'a) -> ('node_data -> 'a list -> 'a) -> ('node_data, 'leaf_data) tree -> 'a

val foldi_tree : (int list -> 'leaf_data -> 'a) -> (int list -> 'node_data -> 'a list -> 'a) -> ('node_data, 'leaf_data) tree -> 'a

val map_tree : ('leaf_data -> 'a) -> ('node_data -> 'b) -> ('node_data, 'leaf_data) tree -> ('b, 'a) tree

(** Abstract representation of tree traversal. *)
type ('node_data, 'leaf_data) tree_walk_step =
  | Entered_node of 'node_data
  | Exitted_node of 'node_data
  | Reached_leaf of 'leaf_data

(** [walk t] traverses the polymorphic tree [t] depth-first recording every step and returns the list of traversal steps. *)
val walk  : ('node_data, 'leaf_data) tree -> ('node_data, 'leaf_data) tree_walk_step list

val walki : ('node_data, 'leaf_data) tree -> (int list * ('node_data, 'leaf_data) tree_walk_step) list
