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

(** Custom extension of the [List] module from the standard library. *)

(* Depends on
    - List
*)

module List_from_stdlib : module type of List

module List : sig 
  include module type of List_from_stdlib
  (** [filter_map f l] builds a new list, applying the function [f] to each element from the set. If for a given element [el] function [f el] returns [Some x], then the value [x] is included in the returned list. If [f el] returns [None], then the element is discarded. *)
  val filter_map : ('a -> 'b option) -> ('a list) -> ('b list)
  (** Same as [List.map], but the function is applied to the index of the element as first argument (counting from 0), and the element itself as second argument. *)
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  (** [is_empty l] returns [true] if the list [l] is empty and [false] if it has at least one element. *)
  val is_empty : 'a list -> bool
  (** TODO *)
  val fold_combine : ('a -> 'b) -> ('b -> 'b -> 'b) -> ('a list) -> 'b -> 'b
  (** [cartesian_product ll] returns cartesian product of lists [ll] as a list of lists. (If [ll] is a list of lists [[l1, l2, ... , ln]], then [cartesian_product ll] returns a list of lists representing the cartesian product [l1 x l2 x ... ln].) *)
  val cartesian_product : (('a list) list) -> (('a list) list)
  (** [singleton_of_option o] converts an option value [o] to a list: if the option value [o] is of form [Some x], then it returns a list with a single element [x], if it is of form [None], then it returns an empty list. *)
  val singleton_of_option : 'a option -> 'a list
  (** [of_option o] converts an option value [o] to a list: if the option value [o] is of form [Some l], then it returns the list [l], if it is of form [None], then it returns an empty list. *)
  val of_option : 'a list option -> 'a list
end

(** Comment: Extending the [List] module with [mapi] function is not necessary after OCaml 4.00.0 *)