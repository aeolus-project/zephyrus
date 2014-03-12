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
type ('leaf_data, 'node_data) tree =
  | Node of 'node_data * ('leaf_data, 'node_data) tree list
  | Leaf of 'leaf_data


(** Fold *)

(** [fold_tree leaf_f node_f t] folds the polymorphic tree [t] using the functions [leaf_f leaf_data] and [node_f node_data children]. *)
val fold_tree : ('leaf_data -> 'a) -> ('node_data -> 'a list -> 'a) -> ('leaf_data, 'node_data) tree -> 'a

(** An index describing the position of a given node or leaf in the tree. *)
type tree_index = int list

(** [tree_level_of_tree_index index] returns a level of profondity in the tree of the node or leaf with the given tree index [index]. *)
val tree_level_of_tree_index : tree_index -> int

(** [foldi_tree leaf_f node_f t] folds the polymorphic tree [t] using the functions [leaf_f index leaf_data] and [node_f index node_data children]. *)
val foldi_tree : (tree_index -> 'leaf_data -> 'a) -> (tree_index -> 'node_data -> 'a list -> 'a) -> ('leaf_data, 'node_data) tree -> 'a


(** Map *)

(** [map_tree leaf_f node_f t] folds the polymorphic tree [t] using the functions [leaf_f leaf_data] and [node_f node_data]. *)
val map_tree : ('leaf_data -> 'a) -> ('node_data -> 'b) -> ('leaf_data, 'node_data) tree -> ('a, 'b) tree


(** Walk *)

(** Abstract representation of tree traversal step. *)
type ('leaf_data, 'node_data) tree_walk_step =
  | Entered_node of 'node_data
  | Exitted_node of 'node_data
  | Reached_leaf of 'leaf_data

(** Abstract representation of tree traversal step which includes the tree index of the node or leaf mentioned in the step. *)
type ('leaf_data, 'node_data) indexed_tree_walk_step = (tree_index * ('leaf_data, 'node_data) tree_walk_step)


(** [walk t] traverses the polymorphic tree [t] depth-first recording every step and returns the list of traversal steps. *)
val walk  : ('leaf_data, 'node_data) tree -> ('leaf_data, 'node_data) tree_walk_step list

(** [walki t] traverses the polymorphic tree [t] depth-first recording every step and returns the list of indexed traversal steps. *)
val walki : ('leaf_data, 'node_data) tree -> ('leaf_data, 'node_data) indexed_tree_walk_step list
