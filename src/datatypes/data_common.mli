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
(*| 1. Custom sets, maps and lists                                         |*)
(*\************************************************************************/*)

module type OrderedType = Map.OrderedType
module type Map_from_stblib = Map.S
module type Set_from_stblib = Set.S
module type String_from_stdlib = sig
  type t = string
  val compare: t -> t -> int

  val length : string -> int
  val get : string -> int -> char
  val set : string -> int -> char -> unit
  val create : int -> string
  val make : int -> char -> string
  val copy : string -> string
  val sub : string -> int -> int -> string
  val fill : string -> int -> int -> char -> unit
  val blit : string -> int -> string -> int -> int -> unit
  val concat : string -> string list -> string
  val iter : (char -> unit) -> string -> unit
  val escaped : string -> string
  val index : string -> char -> int
  val rindex : string -> char -> int
  val index_from : string -> int -> char -> int
  val rindex_from : string -> int -> char -> int
  val contains : string -> char -> bool
  val contains_from : string -> int -> char -> bool
  val rcontains_from : string -> int -> char -> bool
  val uppercase : string -> string
  val lowercase : string -> string
  val capitalize : string -> string
  val uncapitalize : string -> string
end

module type List_from_stdlib = sig
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list

  val iter : ('a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
 
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> 'a list -> bool
  val memq : 'a -> 'a list -> bool

  val find : ('a -> bool) -> 'a list -> 'a
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

  val assoc : 'a -> ('a * 'b) list -> 'b
  val assq : 'a -> ('a * 'b) list -> 'b
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list

  val split : ('a * 'b) list -> 'a list * 'b list
  val combine : 'a list -> 'b list -> ('a * 'b) list

  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
end


module Int : sig type t = int val compare : t -> t -> int end
module String : String_from_stdlib
module List : sig include List_from_stdlib val is_empty : 'a list -> bool val fold_combine : ('a -> 'b) -> ('b -> 'b -> 'b) -> ('a list) -> 'b -> 'b end

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
  val to_list : 'a t -> 'a list

end

(** Extension of the Set module from the standard library with Construction and Conversion **)
module Set : sig

  module type S = sig
    include Set_from_stblib
    
    val keep_elements : int -> t -> t
    val set_of_direct_list: elt list -> t
    val set_of_list: ('a -> elt) -> 'a list -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t

  module Convert(Set_origin : S) (Set_target : S) : sig
    val convert : (Set_origin.elt -> Set_target.elt) -> Set_origin.t -> Set_target.t
    val filter_convert : (Set_origin.elt -> Set_target.elt option) -> Set_origin.t -> Set_target.t
    val set_convert : (Set_origin.elt -> Set_target.t) -> Set_origin.t -> Set_target.t
  end

  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) : sig
    val compute : (Set_origin.elt -> Set_origin.elt -> bool) -> Set_origin.t -> Set_target.t
  end

end

module Int_set        : Set.S with type elt = int
module Int_set_set    : Set.S with type elt = Int_set.t
module String_set     : Set.S with type elt = string
module String_set_set : Set.S with type elt = String_set.t

val setstring_of_setint : Int_set.t -> String_set.t

(** Extension of the Map module from the standard library with Construction, Conversion and Extraction **)
module Map : sig

  module type S = sig
    include Map_from_stblib
    
    val of_direct_list: (key * 'a) list -> 'a t
    val map_of_list: ('a -> key * 'b) -> 'a list -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t

    val keys : 'a t -> key list
    val values : 'a t -> 'a list
    
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

module Int_map : Map.S with type key = int
module String_map : Map.S with type key = string

module Keys_of_Int_map    : sig val set_of_keys : 'a Int_map.t -> Int_set.t end
module Keys_of_String_map : sig val set_of_keys : 'a String_map.t -> String_set.t end



(*/************************************************************************\*)
(*| 2. Unique identifier management                                        |*)
(*\************************************************************************/*)

(* Modules for used tokens (names, ids, etc.) management. *)
module type Used_tokens_type =
sig
  type t
  type token
  val empty : unit -> t
  val mem   : token -> t -> bool
  val add   : token -> t -> unit
end

module Used_tokens_string : Used_tokens_type with type token = string
module Used_tokens_int    : Used_tokens_type with type token = int

(* Modules for unique identifier creation *)
module type Fresh =
sig
  type t
  type id
  val create    : unit -> t
  val current   : t -> id
  val next      : t -> id
  val is_used   : t -> id -> bool
  val mark_used : t -> id -> unit
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
(*| 3.1. Mapping                                                           |*)
(*\************************************************************************/*)

module Mapping :
  functor (Key_set   : Set.S) ->
  functor (Value_set : Set.S) ->
  functor (Key_map   : Map.S with type key = Key_set.elt) ->
  sig
    
    type key   = Key_set.elt
    type value = Value_set.elt

    class type mapping_iface = object
      (* Access *)
      method keys              : Key_set.t            (* All the keys. *)
      method values            : Value_set.t          (* All the values. *)
      method find              : key -> value         (* Get the value corresponding to the given key. May throw Not_found exception. *)
      (* Modify *)
      method add               : key -> value -> unit (* Update the data structures with the given (key, value) pair. *)
      method remove            : key          -> unit (* Remove the given key and its value from the data structures. *)
      (* Lower level access *)
      method key_to_value_map  : value Key_map.t      (* Retrieve directly the key -> value map. *)
    end

    (* Implementation of the mapping. *)
    class mapping : mapping_iface

    (* Create a new catalog corresponding to a given id -> object map. *)
    val of_key_to_value_map : value Key_map.t -> mapping_iface

  end

(*/************************************************************************\*)
(*| 3.2. Catalog                                                           |*)
(*\************************************************************************/*)

module Catalog :
  functor (Fresh_id : Fresh)                              -> (* For generating fresh ids. *)
  functor (Id_set   : Set.S with type elt = Fresh_id.id)  -> (* Set of ids. Must match with the type of ids generated by Fresh_id. *)
  functor (Obj_set  : Set.S)                              -> (* Set of names. *)
  functor (Id_map   : Map.S with type key = Id_set.elt)   -> (* Map with ids as keys.   Must match with the Id_set  element type. *)
  functor (Obj_map  : Map.S with type key = Obj_set.elt) -> (* Map with names as keys. Must match with the Id_name element type. *)
  sig

    (* Five functors is quite heavy indeed, let's extract the two basic types. *)
    type id  = Id_set.elt
    type obj = Obj_set.elt

    (* A modifiable catalog with id <-> object mapping. *)
    class type obj_catalog_iface = object
      (* Access *)
      method ids             : Id_set.t           (* All the ids. *)
      method objs            : Obj_set.t          (* All the objects. *)
      method obj_of_id       : id  -> obj         (* Get the object corresponding to the given id. May throw Not_found exception. *)
      method id_of_obj       : obj -> id          (* Get the id corresponding to the given object. May throw Not_found exception. *)
      (* Modify *)
      method get_else_add    : obj -> id          (* Get the id corresponding to the given object. If the object does not exist, create a new fresh id for this object, update the data structures and the return the id. *)
      method add             : obj -> unit        (* If the object does not exist, create a new fresh id for this object and update the data structures. *)
      method add_id_obj_pair : id  -> obj -> unit (* Update the data structures with the given (id, object) pair. *)
      (* Lower level manipulation *)
      method set_id_of_obj   : obj -> id  -> unit (* Adds the object to objs and makes it correspond to the given id     (only one way, we have obj -> id, but not id -> obj!). *)
      method set_obj_of_id   : id  -> obj -> unit (* Adds the id     to ids  and makes it correspond to the given object (only one way, we have id -> obj, but not obj -> id!). *)
      method id_to_obj_map   : obj Id_map.t       (* Retrieve directly the id -> object map. *)
      method obj_to_id_map   : id Obj_map.t       (* Retrieve directly the object -> id map. *)
    end

    (* Implementation of the catalog. *)
    class obj_catalog : obj_catalog_iface

    (* Create a new catalog by taking a set of objects and adding them all. *)
    val of_set_of_objs   : Obj_set.t    -> obj_catalog_iface
    (* Create a new catalog corresponding to a given id -> object map. *)
    val of_id_to_obj_map : obj Id_map.t -> obj_catalog_iface



    (* A catalog variation with just one purely syntactic difference - we have names not objects. *)
    type name = obj

    (* A modifiable catalog with name <-> id mapping. *)
    class type catalog_iface = object
      (* Access *)
      method ids              : Id_set.t           (* All the ids. *)
      method names            : Obj_set.t          (* All the names. *)
      method name_of_id       : id   -> name       (* Get the name corresponding to the given id. May throw Not_found exception. *)
      method id_of_name       : name -> id         (* Get the id corresponding to the given name. May throw Not_found exception. *)
      (* Modify *)
      method get_else_add     : name -> id         (* Get the id corresponding to the given name. If the name does not exist, create a new fresh id for this name, update the data structures and the return the id. *)
      method add              : name -> unit       (* If the name does not exist, create a new fresh id for this name and update the data structures. *)
      method add_id_name_pair : id -> name -> unit (* Update the data structures with the given (id, name) pair. *)
      (* Lower level manipulation *)
      method set_id_of_name   : name -> id -> unit (* Adds the name to names and makes it correspond to the given id     (only one way, we have name -> id, but not id -> name!). *)
      method set_name_of_id   : id -> name -> unit (* Adds the id     to ids  and makes it correspond to the given object (only one way, we have id -> obj, but not obj -> id!). *)
      method id_to_name_map   : name Id_map.t      (* Retrieve directly the id -> object map. *)
      method name_to_id_map   : id Obj_map.t       (* Retrieve directly the object -> id map. *)
    end

    (* Implementation of the catalog. *)
    class catalog : catalog_iface

    (* A closed catalog (closed means that it cannot be modified. *)
    class type closed_catalog_iface = object
      method ids            : Id_set.t
      method names          : Obj_set.t
      method name_of_id     : id   -> name
      method id_of_name     : name -> id
      method id_to_name_map : name Id_map.t
      method name_to_id_map : id Obj_map.t
    end

    val close_catalog : catalog_iface -> closed_catalog_iface

    (* Implementation of a closed catalog which throws appropriate exceptions. *)
    class closed_catalog_with_exceptions : catalog -> string -> (id -> string)  -> (name -> string) -> closed_catalog_iface

    (* Create a new catalog by taking a set of names and adding them all. *)
    val of_set_of_names   : Obj_set.t     -> catalog_iface
    (* Create a new catalog corresponding to a given id -> name map. *)
    val of_id_to_name_map : name Id_map.t -> catalog_iface

  end

(*/************************************************************************\*)
(*| 4. Basic Data Base                                                     |*)
(*\************************************************************************/*)


(* type safe modules and functors for a data base. Support for SQL is not planned *)
module Database : sig 
  exception Table_not_found  (* raised when trying to access a table not present in the data base *)
  exception Column_not_found (* raised with trying to access a column that is not present in a table *)

  (* type safe modules and functors for the definition of a data base table *)
  module Table : sig
    (* type of a table module *)
    module type S = sig
      type t                (* type of the table *)
      type key              (* type of the main key of the table *)
      type 'a column  (* type for columns: we support value conversion, so ['a] is what is inserted, and ['b] is what is stored *)

      val create : int -> t (* create an empty table *)
      
      val mem : t -> key -> bool (* check if an entry was inserted for the given key *)
      val mem_in_column : t -> 'a column -> key -> bool (* check if an entry was inserted on the specific column -- has sense as we all columns to be optional *)
      
      type add_type (* the part of the type of [add] that varies.
           It has the form ['a -> 'b -> 'c -> unit] where ['a] is the type of the last non optional column, ['b] the second last, etc *)
      val add : t -> key -> add_type (* the function to insert an entry in the table. If present, replace the previous entry with the same key *)
      val add_to_column : t -> 'a column -> key -> 'a -> unit (* Used for insertion in optional columns *)
    
      val find : t -> 'a column -> key -> 'a (* get the value stored in a column for a given key (or the default value, if one is given) *)
      val find_key : t -> 'a column -> 'a -> key (* returns the key corresponding to the value *)
    end

    module Empty(K : sig type key type 'a column val compare : key -> key -> int end) 
      : S with type key = K.key and type 'a column = 'a K.column and type add_type = unit

   (* The creation of a table work in several steps:
        1. first, you create an empty table module using the [Empty] functor
        2-n. then, you iteratively add columns of the desired type and the desired functionality.
      The addition of a new column is done in 4 or 5 steps:
        1. first you define a module of type [General_input], with or without the [convert] function
        2. You feed this module to either [WithConversion] or [WithoutConversion] to enable or not value conversion during insertion in the column
        3. You feed the resulting module to either [WithChecking] or [WithoutChecking] to enable or not checking what is inserted in the table.
           This way, insertion can be canceled if an exception is raise by the [check] function
        4. You feed the resulting module to either [WithDefaultValue] or [WithoutDefaultValue] to set a default value for the column
        5. Finally, you finalize the addition by feeding the module you have to one of the following functors, each of then giving you different functionalities:
          - [Mandatory] has the classic column features: when inserting a new entry, you have to specify a value for that column
          - [Optional] do not require a value for a given entry (a value for that column can be inserted separately using [add_to_column])
          - [List] is a find of optional column, that keeps, instead of replacing previous value upon re-insertion of a key.
            The list of everything that was inserted for a key can be accessed with [find_list]
   *)

    module type Input = sig type t type key type 'a column val name : t  column end
    module type First_intermediate = sig include Input val check : (key, t) Hashtbl.t -> key -> t -> unit end
    module type Second_intermediate  = sig include First_intermediate val find : (key, t) Hashtbl.t -> key -> t end
    module type Third_intermediate = sig include Second_intermediate val aggregate : ((key, t) Hashtbl.t) -> ((t, key) Hashtbl.t) -> key -> t -> t end
    
    module WithChecking(C : Input)(P : sig val check : C.key -> C.t -> C.t option -> unit end)
      : First_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column
    module WithoutChecking(C : Input)
      : First_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column

    module WithDefaultValue(C : First_intermediate)(P : sig val default : C.t end)
      : Second_intermediate  with type t = C.t and type key = C.key and type 'a column = 'a C.column
    module WithoutDefaultValue(C : First_intermediate)
      : Second_intermediate  with type t = C.t and type key = C.key and type 'a column = 'a C.column

    module WithAggregate(C : Second_intermediate)(P : sig val aggregate : C.t -> C.t -> C.t end)
      : Third_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column
    module WithoutAggregate(C : Second_intermediate)
      : Third_intermediate with type t = C.t and type key = C.key and type 'a column = 'a C.column


    module AddMandatory(C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column)
      : S with type key = T.key and type 'a column = 'a C.column and type add_type = C.t -> T.add_type

    module AddOptional (C : Third_intermediate)(T : S with type key = C.key and type 'a column = 'a C.column)
      : S with type key = T.key and type 'a column = 'a C.column and type add_type = T.add_type
  end

  (* type of a database module *)
  module type S = sig
    type t               (* type of a database *)
    type 'a key          (* type of keys. The parameter 'a allows different table in the database to use different kind of keys *)
    type ('a, 'b) table  (* type for tables: 'a if the kind of key of the table, and 'b is its type [add_type] *)
    type 'a column       (* generic type for all columns *)

    val create : int -> t
      
    val mem : t -> ('a, 'b) table -> 'a key -> bool
    val mem_in_column : t -> ('a, 'b) table -> 'c column -> 'a key -> bool
      
    val add : t -> ('a, 'b) table -> 'a key -> 'b
    val add_to_column : t -> ('a, 'b) table -> 'c column -> 'a key -> 'c -> unit
    
    val find : t -> ('a, 'b) table -> 'c column -> 'a key -> 'd
    val find_key : t -> ('a, 'b) table -> 'c column -> 'c -> 'a key
  end

  module Empty(K : sig type 'a key type ('a, 'b) table type 'a column end)
    : S with type 'a key = 'a K.key and type ('a, 'b) table = ('a, 'b) K.table and type 'a column = 'a K.column


  module AddTable(T : Table.S)(Id : sig type ('a, 'b) table val name : (T.key, T.add_type) table end)
      (DB : S with type ('a, 'b) table = ('a, 'b) Id.table and type 'a column = 'a T.column)
    : S with type 'a key = 'a DB.key and type ('a, 'b) table = ('a, 'b) DB.table and type 'a column = 'a DB.column

end





(*/************************************************************************\*)
(*| 4. Generic Graph                                                       |*)
(*\************************************************************************/*)

(* Inductive construction of loops inspired from the first pages of "Incremental Cycle Detection, Topological Ordering, and Strong Component Maintenance" *)

module Graph : sig
  module type Vertice_data = sig type t end
  module type Edge_data   = sig type t end

  module type S = sig
    type vertice_data
    type edge_data
    
    module rec Vertice : sig
      type t                           (* type of vertices *)
      val data   : t -> vertice_data   (* returns the data contained in that vertice *)
      val succs_e: t -> Edge_set.t     (* returns the set of edges exiting from that vertice *)
      val preds_e: t -> Edge_set.t     (* returns the set of edges targeting that vertice *)
      val succs_v: t -> Vertice_set.t  (* returns the set of vertices that have an edge from that vertice *)
      val preds_v: t -> Vertice_set.t  (* returns the set of vertices that have an edge to that vertice *)

(*      
      (* Functions used internally. Do not use. TODO: clean this by providing two signatures in the implementation, one internal, one for the .mli *)
      val parse_tag   : t -> unit
      val is_parsed   : t -> bool
      val parse_untag : t -> unit
      val loop_tag    : Loop.t -> t -> unit
      val loop_untag  : t -> unit

      val is_loop     : t -> bool      (* returns if the vertice is part of a loop *)
      val loop_get    : t -> Loop.t    (* returns the loop in which the vertice is, or raise Not_found *)
*)      
      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Vertice_set : (Set.S with type elt = Vertice.t) and Vertice_map : (Map.S with type key = Vertice.t)
    
    and Edge : sig
      type t                           (* type of edges *)
      val data   : t -> edge_data      (* returns the data contained in that edge *)
      val origin : t -> Vertice.t      (* returns the origin vertice of that edge *)
      val target : t -> Vertice.t      (* returns the target vertice of that edge *)
(*      
      (* Functions used internally. Do not use. TODO: clean this by providing two signatures in the implementation, one internal, one for the .mli *)
      val loop_tag     : Loop.t -> t -> unit
      val loop_tag_in  : Loop.t -> t -> unit
      val loop_tag_out : Loop.t -> t -> unit
      val loop_untag   : t -> unit
*)      
      val is_loop      : t -> bool     (* returns if that edge is part of a loop *)
      val is_loop_in   : t -> bool     (* returns if that edge targets a vertice in a loop *)
      val is_loop_out  : t -> bool     (* returns if that edge originates from a vertice in a loop *)
(*      val loop_get     : t -> Loop.t   (* returns the loop linked to that edge, or raise Not_found *) *)
      
      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Edge_set : (Set.S with type elt = Edge.t) and Edge_map : (Map.S with type key = Edge.t)

    and Path : sig
      type t                                      (* type of path *)
      exception Invalid_path_extension            (* exception raised when erroneous operation is done on a path *)

      val create      : Vertice.t -> t            (* create an empty path starting on that vertice *)
      val is_empty    : t -> bool                 (* returns if the path is empty *)
      val add         : Edge.t -> t -> t          (* returns that path extended with that edge, or raise Invalid_path_extension *)
      val mem         : Vertice.t -> t -> bool    (* returns if that vertice is in that path *)
      val vertices    : t -> Vertice_set.t        (* returns the set of vertices in that path *)
      val edges       : t -> Edge_set.t           (* returns the set of edges in that path *)
      val split       : Vertice.t -> t -> t list  (* [split v p] returns the list [p1;...;pn] with [p = pn -> v -> ... -> v -> p1. Does not create empty path in case origin or target is [v] *)
      val concat      : t -> t -> t               (* concat the two paths together, or raise Invalid_path_extension  *)
      val concat_list : t list -> t               (* concat a non-empty list of paths *)
      val is_loop     : t -> bool                 (* tests if that path is a loop *)
      
      val compare : t -> t -> int                 (* classic comparison function *)
      val equal   : t -> t -> bool                (* comparison that returns a bool instead of an int *)
    end and Path_set : (Set.S with type elt = Path.t) and Path_map : (Map.S with type key = Path.t)

    and Loop : sig
      type t                                     (* type of loop *)
      
      val vertices : t -> Vertice_set.t          (* returns the set of vertices that are part of that loop *)
      val edges    : t -> Edge_set.t             (* returns the set of edges that are part of that loop *)
      val succs_e  : t -> Edge_set.t             (* returns the set of edges whose origin is part of the loop (and which are not part of the loop) *)
      val preds_e  : t -> Edge_set.t             (* returns the set of edges whose target is part of the loop (and which are not part of the loop) *)
      val succs_v  : t -> Vertice_set.t          (* returns the targets of the edges in [succs_e] of that loop *)
      val preds_v  : t -> Vertice_set.t          (* returns the origins of the edges in [preds_e] of that loop *)
      
      val mem    : Vertice.t -> t -> bool        (* tests if that vertice is part of that loop *)
      val center : Vertice.t -> t -> unit        (* if that vertice is part of that loop, move the starting point of the main path of the loop on that vertice *)
      val path   : t -> Edge.t list              (* returns the main path of the loop, once it has been centered *)
      
      val compare : t -> t -> int                 (* classic comparison function *)
      val equal   : t -> t -> bool                (* comparison that returns a bool instead of an int *)
    end and Loop_set : (Set.S with type elt = Loop.t) and Loop_map : (Map.S with type key = Loop.t)

    type t

    val create      : unit -> t                                            (* creates a new empty graph *)
    val add_vertice : vertice_data -> t -> Vertice.t                       (* adds a new vertice to the graph *)
    val add_edge    : Vertice.t -> edge_data -> Vertice.t -> t -> Edge.t   (* adds a new edge to the graph *)

    val vertices : t -> Vertice_set.t  (* returns the set of all vertices in the graph *)
    val edges    : t -> Edge_set.t     (* returns the set of all edges in the graph *)
    val loops    : t -> Loop_set.t     (* returns the set of all loops in the graph *) 
    
    val vertice_roots : t -> Vertice_set.t  (* returns the set of vertices that are roots (no ancestors) in the graph *)
    val loop_roots    : t -> Loop_set.t     (* returns the set of loops that are roots in the graph *) 
    val vertice_leafs : t -> Vertice_set.t  (* returns the set of vertices that are leafs (no children) in the graph *)
    val loop_leafs    : t -> Loop_set.t     (* returns the set of loops that are leafs in the graph *) 
    
    module Traverse_depth : sig
      val iter : (Path.t -> Vertice.t -> bool -> (unit -> unit) -> unit) -> t -> unit
    end
    
    module Traverse_topology : sig
      val iter_downward : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
      val iter_upward   : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit 
    end    
  end

  module Make(V: Vertice_data)(E: Edge_data) : S with type vertice_data = V.t and type edge_data = E.t

end




