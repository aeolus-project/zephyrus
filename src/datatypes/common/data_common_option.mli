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

(** Custom [Option] module for operation on the options. *)

(* Depends on nothing.
*)

module Option : sig 
  (** [map f o] creates a new option value which is equal to [None] if the given option [o] was also [None] and equal to [Some (f v)] if the given option [o] was equal to [Some v]. *)
  val map         : ('a -> 'b       ) -> 'a option -> 'b option
  (** [map_flatten f o] creates a new option value which is: equal to [None] if the given option [o] was also [None],
      also equal to [None] if the given option [o] was equal to [Some v] and [f v] is equal to [None],
      finally equal to [Some (f v')] if the given option [o] was equal to [Some v] and [f v] is equal to [Some v']. *)
  val map_flatten : ('a -> 'b option) -> 'a option -> 'b option
end