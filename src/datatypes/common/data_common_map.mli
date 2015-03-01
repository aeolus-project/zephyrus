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

(** Custom extension of the [Map] module from the standard library with construction, conversion and extraction. *)

(* Depends on
    - Map
    - Data_common_int
    - Data_common_set
*)

open Data_common_int
open Data_common_set

module Map_from_stdlib : module type of Map

module Map : sig

  module type OrderedType = Map_from_stdlib.OrderedType

  module type S = sig
    include Map_from_stdlib.S
    
    (** [of_assoc_list l] converts the association list [l] directly into a map. *)
    val of_assoc_list: (key * 'a) list -> 'a t

    (** [of_list f l] converts the list [l] to a map, applying the function [f] to each element of the list to get a (key, value) pair. *)
    val of_list: ('a -> key * 'b) -> 'a list -> 'b t

    (** [to_assoc_list m] converts the map [m] directly to an association list. An alias for the [bindings] function. *)
    val to_assoc_list : 'a t -> (key * 'a)  list
    
    (** [to_list f m] converts the map [m] to a list, applying a given function to each (key, value) pair from the map to get a list element. *)
    val to_list: ( (key * 'a) -> 'b) -> 'a t -> 'b list

    (** [map f m] applies the function [f] to all values from the map [m] and builds a new map where each key [k] which was associated in the map [m] to the value [v] is now associated to the value [f v]. Functional "map" operation on the map's values. *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** [keys m] returns a list containing all the keys of the map [m]. *)
    val keys : 'a t -> key list

    (** [values m] returns a list containing all the values of the map [m]. *)
    val values : 'a t -> 'a list
    
    (** A functor creating a module used to obtain a set containing all the keys of the given map. *)
    module Set_of_keys(Set_target : Set.S with type elt = key) : sig
      (** [set_of_keys m] returns a set containing all the keys of the map [m]. *)
      val set_of_keys : 'a t -> Set_target.t
    end

    (** A functor creating a module used to obtain a set containing all the values of the given map. *)
    module Set_of_values(Set_target : Set.S) : sig
      (** [set_of_values m] returns a set containing all the values of the map [m]. *)
      val set_of_values : Set_target.elt t -> Set_target.t
    end
  end
  
  module Make(Ord : OrderedType) : S with type key = Ord.t

  (** A functor creating a module used to convert one type of maps to another type of maps (perform a functional "map" operation to a map with a different type of keys). *)
  module Convert(Map_origin : S) (Map_target : S) : sig
    (** [convert f m] applies the function [f] to all pairs (key, value) from the map [m] and builds a new map where each pair [(k, v)] from the map [m] is replaced with [f (k, v)]. Functional "map" operation on maps of two different types. *)
    val convert : ((Map_origin.key * 'a) -> (Map_target.key * 'b)) -> 'a Map_origin.t -> 'b Map_target.t
  end
end

(** A map with integers for keys. *)
module Int_map    : Map.S with type key = int
(** A map with strings for keys. *)
module String_map : Map.S with type key = string

(** Module with a single function [set_of_keys], used to extract all the keys of an [Int_map] in form of a [Int_set]. *)
module Keys_of_Int_map    : sig val set_of_keys : 'a Int_map.t    -> Int_set.t    end
(** Module with a single function [set_of_keys], used to extract all the keys of an [String_map] in form of a [String_set]. *)
module Keys_of_String_map : sig val set_of_keys : 'a String_map.t -> String_set.t end


