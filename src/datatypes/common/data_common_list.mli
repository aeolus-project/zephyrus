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
  val filter_map : ('a -> 'b option) -> ('a list) -> ('b list)
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  (* Comment: Extending the [List] module with [mapi] function is not necessary after OCaml 4.00.0 *)
  val is_empty : 'a list -> bool
  val fold_combine : ('a -> 'b) -> ('b -> 'b -> 'b) -> ('a list) -> 'b -> 'b
  val cartesian_product : (('a list) list) -> (('a list) list)
end