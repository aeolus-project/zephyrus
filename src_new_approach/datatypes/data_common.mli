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
    - Map
    - Set
    - String
    - Queue
*)

(*/************************************************************************\*)
(*| 1. Custom sets and maps                                                |*)
(*\************************************************************************/*)

module type OrderedType = Map.OrderedType
module type Map_from_stblib = Map.S
module type Set_from_stblib = Set.S

module Int : sig type t = int val compare : t -> t -> int end
module String : sig type t = string val compare : t -> t -> int end

(** Extension of the Set module from the standard library with Construction and Conversion **)
module Set : sig

  module type S = sig
    include Set_from_stblib
    
    val set_of_direct_list: elt list -> t
    val set_of_list: ('a -> elt) -> 'a list -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t

  module Convert(Set_origin : S) (Set_target : S) : sig
    val convert : (Set_origin.elt -> Set_target.elt) -> Set_origin.t -> Set_target.t
  end

  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) : sig
    val compute : (Set_origin.elt -> Set_origin.elt -> bool) -> Set_origin.t -> Set_target.t
  end

end

module SetInt       : Set.S with type elt = int
module SetSetInt    : Set.S with type elt = SetInt.t
module SetString    : Set.S with type elt = string
module SetSetString : Set.S with type elt = SetString.t

val setstring_of_setint : SetInt.t -> SetString.t

(** Extension of the Map module from the standard library with Construction, Conversion and Extraction **)
module Map : sig

  module type S = sig
    include Map_from_stblib
    
    val map_of_associated_list: (key * 'a) list -> 'a t
    val map_of_list: ('a -> key * 'b) -> 'a list -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t

    module Set_of_keys(Set_target : Set.S with type elt = key) : sig
      val set_of_keys : 'a t -> Set_target.t
    end

    module Set_of_values(Set_target : Set.S) : sig
      val set_of_values : Set_target.elt t -> Set_target.t
    end
  end
  
  module Make(Ord : OrderedType) : S with type key = Ord.t

  module Convert(Map_origin : S) (Map_target : S) : sig
    val convert : ((Map_origin.key * 'a) -> (Map_target.key * 'b)) -> 'a Map_origin.t -> 'b Map_target.t
  end
end

module MapInt : Map.S with type key = int
module MapString : Map.S with type key = string

module Keys_of_MapInt    : sig val set_of_keys : 'a MapInt.t -> SetInt.t end
module Keys_of_MapString : sig val set_of_keys : 'a MapString.t -> SetString.t end


(*/************************************************************************\*)
(*| 2. Unique identifier management                                        |*)
(*\************************************************************************/*)

(* Modules for unique identifier creation *)
module type Fresh =
sig
  type t
  type id
  val create  : unit -> t
  val current : t -> id
  val next    : t -> id
end

module Fresh_integer : Fresh with type id = int


(* Unique identifiers plus special identifier requests *)
module type Fresh_with_special =
sig
  include Fresh
  type special_request
  val special : special_request -> id
end

(* Only one special request: Deprecated (which will typically correspond to id -1) *)
type special_request_deprecated = Deprecated

module Fresh_integer_with_deprecated : Fresh_with_special with 
  type id = int and 
  type special_request = special_request_deprecated


(*/************************************************************************\*)
(*| 3. Catalog                                                             |*)
(*\************************************************************************/*)

module Catalog :
  functor (Fresh_id : Fresh)                              -> (* For generating fresh ids. *)
  functor (Id_set   : Set.S with type elt = Fresh_id.id)  -> (* Set of ids. Must match with the type of ids generated by Fresh_id. *)
  functor (Name_set : Set.S)                              -> (* Set of names. *)
  functor (Id_map   : Map.S with type key = Id_set.elt)   -> (* Map with ids as keys.   Must match with the Id_set  element type. *)
  functor (Name_map : Map.S with type key = Name_set.elt) -> (* Map with names as keys. Must match with the Id_name element type. *)
  sig

    (* Five functors is quite heavy indeed, let's extract the two basic types. *)
    type id   = Id_set.elt
    type name = Name_set.elt

    (* A modifiable catalog with name <-> id mapping. *)
    class type catalog_iface = object
      method ids            : Id_set.t           (* All the ids. *)
      method names          : Name_set.t         (* All the names. *)
      method name_of_id     : id   -> name       (* Mapping name -> id. May throw Not_found exception. *)
      method id_of_name     : name -> id         (* Mapping id -> name. May throw Not_found exception. *)
      method set_id_of_name : name -> id -> unit (* Adds the name to names and makes it correspond to a given id   (only one way, we have name -> id, but not id -> name!). *)
      method set_name_of_id : id -> name -> unit (* Adds the id   to ids   and makes it correspond to a given name (only one way, we have id -> name, but not name -> id!). *)
      method get_or_add     : name -> id         (* Get the id corresponding to a name. If it does not exist, create a new fresh id for this name, update the data structures and the return the id. *)
      method add            : name -> unit       (* As above, but do not return anything. Useful to avoid type warnings (when we discard the returned value). *)
    end

    (* Implementation of the catalog. *)
    class catalog : catalog_iface

    (* A closed catalog (closed means that it cannot be modified. *)
    class type closed_catalog_iface = object
      method ids        : Id_set.t
      method names      : Name_set.t
      method name_of_id : id   -> name
      method id_of_name : name -> id
    end

    (* Implementation of a closed catalog which throws appropriate exceptions. *)
    class closed_catalog_with_exceptions : catalog -> string -> (id -> string)  -> (name -> string) -> closed_catalog_iface
      
  end



(*/************************************************************************\*)
(*| 4. Generic Graph                                                       |*)
(*\************************************************************************/*)

module Graph : sig
  module type Vertice_data = sig type t end
  module type Edge_data   = sig type t end

  module Make(V: Vertice_data)(E: Edge_data) : sig
    module rec Vertice : sig
      type t
      val data   : t -> V.t
      val succs_e: t -> Edge_set.t
      val preds_e: t -> Edge_set.t
      val succs_v: t -> Vertice_set.t
      val preds_v: t -> Vertice_set.t
      
      val parse_tag   : t -> unit
      val is_parsed   : t -> bool
      val parse_untag : t -> unit
      val loop_tag    : Loop.t -> t -> unit
      val loop_untag  : t -> unit
      val is_loop     : t -> bool
      val loop_get    : t -> Loop.t
      
      val compare : t -> t -> int
      val equal   : t -> t -> bool
    end and Vertice_set : (Set.S with type elt = Vertice.t) and Vertice_map : (Map.S with type key = Vertice.t)
    
    and Edge : sig
      type t
      val data   : t -> E.t
      val origin : t -> Vertice.t
      val target : t -> Vertice.t
      
      val loop_tag     : Loop.t -> t -> unit
      val loop_tag_in  : Loop.t -> t -> unit
      val loop_tag_out : Loop.t -> t -> unit
      val loop_untag   : t -> unit
      val is_loop      : t -> bool
      val is_loop_in   : t -> bool
      val is_loop_out  : t -> bool
      val loop_get     : t -> Loop.t
      
      val compare : t -> t -> int
      val equal   : t -> t -> bool
    end and Edge_set : (Set.S with type elt = Edge.t) and Edge_map : (Map.S with type key = Edge.t)

    and Path : sig
      type t
      exception Invalid_path_extension

      val create   : Vertice.t -> t
      val is_empty : t -> bool
      val add      : Edge.t -> t -> t
      val mem      : Vertice.t -> t -> bool
      val vertices : t -> Vertice_set.t
      val edges    : t -> Edge_set.t
      
      val extract_of_vertice : Vertice.t -> t -> t
      val is_loop            : t -> bool
      val center_loop_unsafe : Vertice.t -> t -> t
      val add_loop           : t -> t -> t
    end
    
    and Loop : sig
      type t
      exception Invalid_loop_extension
      
      val create   : Path.t -> t
      val add      : Path.t -> t -> unit
      val vertices : t -> Vertice_set.t
      val edges    : t -> Edge_set.t
      val succs_e  : t -> Edge_set.t
      val preds_e  : t -> Edge_set.t
      val succs_v  : t -> Vertice_set.t
      val preds_v  : t -> Vertice_set.t
      
      val mem    : Vertice.t -> t -> bool
      val center : Vertice.t -> t -> unit
      
      val compare : t -> t -> int
      val equal   : t -> t -> bool
    end and Loop_set : (Set.S with type elt = Loop.t) and Loop_map : (Map.S with type key = Loop.t)

    type t

    val create      : unit -> t
    val add_vertice : V.t -> t -> unit
    val add_edge    : Vertice.t -> E.t -> Vertice.t -> t -> unit
    val get_loops   : t -> Loop_set.t
    
    module Traverse_depth : sig
      val iter : (Path.t -> Vertice.t -> bool -> (unit -> unit) -> unit) -> t -> unit
    end
    
    module Traverse_topology : sig
      val iter_downward : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
      val iter_upward   : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
    end
    
    
  end
end




