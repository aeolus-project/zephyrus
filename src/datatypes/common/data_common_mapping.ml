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

(* Depends on
    - Data_common_set
    - Data_common_map
*)

open Data_common_set
open Data_common_map

module Mapping =
  functor (Key_set   : Set.S) ->
  functor (Value_set : Set.S) ->
  functor (Key_map   : Map.S with type key = Key_set.elt) ->
  struct
    
    type key   = Key_set.elt
    type value = Value_set.elt

    class type mapping_iface = object
      (* Access *)
      method domain           : Key_set.t            (* All the values. *)
      method codomain         : Value_set.t          (* All the keys. *)
      method find             : key -> value         (* Get the value corresponding to the given key. May throw Not_found exception. *)
      (* Modify *)
      method add              : key -> value -> unit (* Update the data structures with the given (key, value) pair. *)
      method remove           : key          -> unit (* Remove the given key and its value from the data structures. *)
      (* Lower level access *)
      method key_to_value_map : value Key_map.t      (* Retrieve directly the key -> value map. *)
    end

    (* Implementation of the mapping. *)
    class mapping : mapping_iface = object (self)

      (* Data structures. *)
      val mutable values           : Value_set.t       = Value_set.empty (* All the keys. *)
      val mutable keys             : Key_set.t         = Key_set.empty   (* All the values. *)
      val mutable key_to_value_map : (value Key_map.t) = Key_map.empty   (* Mapping key -> value *)
      
      (* Domain and co-domain. *)
      method domain   : Key_set.t   = keys
      method codomain : Value_set.t = values

      (* Mapping function. May throw Not_found exception. *)
      method private value_of_key (key : key) : value = Key_map.find key key_to_value_map
      method find = self#value_of_key

      (* Add new key and value to appropriate sets 
         and add the relation key -> value to the map. *)
      method private add_new_key_value_pair key value =
        keys             <- Key_set.add   key   keys;
        values           <- Value_set.add value values;
        key_to_value_map <- Key_map.add key value key_to_value_map

      method add = self#add_new_key_value_pair
      
      (* Remove a key and the associated value
         from the map.  *)
      method private remove_key_and_its_value key =
        let value = self#value_of_key key in
        keys             <- Key_set.remove   key   keys;
        values           <- Value_set.remove value values;
        key_to_value_map <- Key_map.remove key key_to_value_map

      method remove = self#remove_key_and_its_value

      (* Retrieve directly the key -> value map. *)
      method key_to_value_map : value Key_map.t = key_to_value_map

    end

    (* Create a new mapping corresponding to a given key -> value map. *)
    let of_key_to_value_map (key_to_value_map : value Key_map.t) : mapping_iface =
      let mapping = new mapping in
      Key_map.iter mapping#add key_to_value_map;
      mapping

  end