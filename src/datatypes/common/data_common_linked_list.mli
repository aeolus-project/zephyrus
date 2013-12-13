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

module Linked_list : sig
  type 'a t                                               (* type of a generic double linked list *)

  val create : unit -> 'a t                               (* create a new empty generic list *)
  val add_first : 'a -> 'a t -> unit                      (* add a new element at the beginning of the list *)
  val add_last  : 'a -> 'a t -> unit                      (* add a new element at the end of the list *)
  val add       : 'a -> 'a t -> unit                      (* synonym of add_last *)

  val first : 'a t -> 'a                                  (* returns the first element of the list, or raise Not_found *)
  val last  : 'a t -> 'a                                  (* returns the last  element of the list, or raise Not_found *)
  
  val rm_first : 'a t -> unit                             (* removes the first element of the list, or raise Not_found *)
  val rm_last  : 'a t -> unit                             (* removes the last  element of the list, or raise Not_found *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b   (* fold over the list, starting with its first element *)
  val fold_left  : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b   (* fold over the list, starting with its last  element *)
  val fold       : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b   (* synonym of fold_right *)

  val iter_right : ('a -> unit) -> 'a t -> unit           (* iter over the list, starting with its first element *)
  val iter_left  : ('a -> unit) -> 'a t -> unit           (* iter over the list, starting with its last  element *)
  val iter       : ('a -> unit) -> 'a t -> unit           (* synonym of iter_right *)

  val filter : ('a -> bool) -> 'a t -> 'a t               (* returns a new list keeping in the right order all the elements validating the input predicat *)

(*
  val prefix : 'a -> 'a t -> 'a t                         (* returns the smallest sub list that finishes with v in parameter, or raise Not_found *)
  val suffix : 'a -> 'a t -> 'a t                         (* returns the biggest sub list that starts with v in parameter, or raise Not_found *)
  val sub    : 'a -> 'a -> 'a t -> 'a t                   (* equivalent to prefix v2 (suffix v1 l) *)
*)

  val append : 'a t -> 'a t -> 'a t                       (* concat the two lists together *)
  val concat : 'a t list -> 'a t                          (* concat all the lists together *)

  val to_list : 'a t -> 'a list


  type 'a iterator
  
  val forward_iterator : 'a t -> 'a iterator
  val backward_iterator : 'a t -> 'a iterator
  
  val has_next : 'a iterator -> bool
  val next     : 'a iterator -> 'a

end
