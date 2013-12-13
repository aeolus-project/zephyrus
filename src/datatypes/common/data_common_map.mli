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

(** Extention of the Map module from the standard library with construction, conversion and extraction. *)

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
    
    (** Convert an association list to a map. *)
    val of_assoc_list: (key * 'a) list -> 'a t

    (** Convert any list to a map. *)
    val map_of_list: ('a -> key * 'b) -> 'a list -> 'b t

    (** Functional "map" operation on map's values. *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Obtain a list containing all the keys of the given map. *)
    val keys : 'a t -> key list

    (** Obtain a list containing all the values of the given map. *)
    val values : 'a t -> 'a list
    
    (** A functor creating a module used to obtain a set containing all the keys of the given map. *)
    module Set_of_keys(Set_target : Set.S with type elt = key) : sig
      val set_of_keys : 'a t -> Set_target.t
    end

    (** A functor creating a module used to obtain a set containing all the values of the given map. *)
    module Set_of_values(Set_target : Set.S) : sig
      val set_of_values : Set_target.elt t -> Set_target.t
    end
  end
  
  module Make(Ord : OrderedType) : S with type key = Ord.t

  (** A functor creating a module used to convert one type of maps to another type of maps (perform a functional "map" operation on maps). *)
  module Convert(Map_origin : S) (Map_target : S) : sig
    val convert : ((Map_origin.key * 'a) -> (Map_target.key * 'b)) -> 'a Map_origin.t -> 'b Map_target.t
  end
end

module Int_map    : Map.S with type key = int
module String_map : Map.S with type key = string

module Keys_of_Int_map    : sig val set_of_keys : 'a Int_map.t    -> Int_set.t    end
module Keys_of_String_map : sig val set_of_keys : 'a String_map.t -> String_set.t end