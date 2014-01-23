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

(** One-way mappings operations. *)

(* Depends on
    - Data_common_set
    - Data_common_map
*)

open Data_common_set
open Data_common_map

(** Functor building an implementation of the mapping structure given an implementation for sets of its keys, sets of its values and maps from its keys to its values. 
  @param Key_set The [Set] module implementation for mapping's keys.
  @param Value_set The [Set] module implementation for mapping's values.
  @param Key_map The [Map] module implementation where keys are the mapping's keys. The [Key_map.key] type must be equal to the [Key_set.elt] type.
*)
module Mapping :
  functor (Key_set   : Set.S) ->
  functor (Value_set : Set.S) ->
  functor (Key_map   : Map.S with type key = Key_set.elt) ->
  sig

    type key   = Key_set.elt
    (** The mapping's keys type. Equal to [Key_set.elt]. *)
    type value = Value_set.elt
    (** The mapping's values type. Equal to [Value_set.elt]. *)

    (** The mapping's object interface. *)
    class type mapping_iface = object
      (** Access *)

      method domain           : Key_set.t            (** All the keys. *)
      method codomain         : Value_set.t          (** All the values. *)
      method find             : key -> value         (** Get the value corresponding to the given key. May throw Not_found exception. *)

      (** Modify *)

      method add              : key -> value -> unit (** Update the data structures with the given (key, value) pair. *)
      method remove           : key          -> unit (** Remove the given key and its value from the data structures. *)

      (** Lower level access *)

      method key_to_value_map : value Key_map.t      (** Retrieve directly the key -> value map. *)
    end

    (** [new mapping] returns an empty mapping realizing the interface [mapping_interface]. *)
    class mapping : mapping_iface

    (** [of_key_to_value_map m] returns a new mapping built directly from the given key -> value map [m]. *)
    val of_key_to_value_map : value Key_map.t -> mapping_iface

  end