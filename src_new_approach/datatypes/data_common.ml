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
*)

(*/************************************************************************\*)
(*| 0. Model helpers                                                       |*)
(*\************************************************************************/*)

let get_name model_object = model_object#name

(*/************************************************************************\*)
(*| 1. Custom sets and maps                                                |*)
(*\************************************************************************/*)

module Map_global_from_stdlib = Map
module Set_global_from_stdlib = Set

module type OrderedType = Map_global_from_stdlib.OrderedType
module type Map_from_stblib = Map_global_from_stdlib.S
module type Set_from_stblib = Set_global_from_stdlib.S

module String = String
module Int = struct type t = int let compare = (-) end

module Set = struct

  module type S = sig
    include Set_from_stblib
    
    val keep_elements : int -> t -> t
    val set_of_direct_list: elt list -> t
    val set_of_list: ('a -> elt) -> 'a list -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
    module Set_tmp = Set_global_from_stdlib.Make(Ord)
    include Set_tmp


    let keep_elements n s = if n <= 0 then empty else (let rec f n s = if n <= 0 then s else f (n - 1) (remove (choose s) s) in f ((cardinal s) - n) s)
    let set_of_direct_list l = List.fold_left (fun res v -> add v res) empty l
    let set_of_list f l = List.fold_left (fun res v -> add (f v) res) empty l
  end

  module Convert(Set_origin : S) (Set_target : S) = struct
    let convert f s = Set_origin.fold (fun v res -> Set_target.add (f v) res) s Set_target.empty
    let filter_convert f s = 
      Set_origin.fold (fun el s -> 
        match f el with
        | None   -> s 
        | Some x -> Set_target.add x s
      ) s Set_target.empty
  end

  module EquivalenceClass(Set_origin : S)(Set_target : S with type elt = Set_origin.t) = struct
    let compute f s =
      let res = ref Set_target.empty in
      Set_origin.iter (fun e -> let b = Set_target.fold (fun s b -> 
          if b && (f (Set_origin.choose s) e) then (res := Set_target.add (Set_origin.add e s) (Set_target.remove s !res); false) else true) !res true in
        (if b then res := Set_target.add (Set_origin.singleton e) !res)
      ) s; !res
  end
end

module SetInt       = Set.Make(Int)
module SetSetInt    = Set.Make(SetInt)
module SetString    = Set.Make(String)
module SetSetString = Set.Make(SetString)
module SetInt_to_SetString = Set.Convert(SetInt)(SetString)
let setstring_of_setint s = SetInt_to_SetString.convert string_of_int s

module Map = struct

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
  
  module Make(Ord : OrderedType) : S with type key = Ord.t = struct
    module Map_tmp = Map_global_from_stdlib.Make(Ord)
    include Map_tmp
  
    let map_of_list f l = List.fold_left (fun res el -> let (k,v) = f el in add k v res) empty l
    let map_of_associated_list l = List.fold_left (fun res (k,v) -> add k v res) empty l
    let map f m = fold (fun k v res -> add k (f v) res) m empty    

    module Set_of_keys(Set_target : Set.S with type elt = key) = struct
      let set_of_keys map = fold (fun k _ res -> Set_target.add k res) map Set_target.empty 
    end

    module Set_of_values(Set_target : Set.S) = struct
      let set_of_values map = fold (fun _ v res -> Set_target.add v res) map Set_target.empty
    end
  end
  
  module Convert(Map_origin : S) (Map_target : S) = struct
    let convert f m = Map_origin.fold (fun k v res -> let (k',v') = f (k,v) in Map_target.add k' v' res) m Map_target.empty
  end


end

module MapInt = Map.Make(Int)
module MapString = Map.Make(String)

module Keys_of_MapInt    = MapInt.Set_of_keys(SetInt)
module Keys_of_MapString = MapString.Set_of_keys(SetString)


module List_from_stdlib = List
module List = struct
  include List_from_stdlib
  let is_empty l = (l = [])
  let rec fold_combine conv combine l init = match l with
    | [] -> init
    | [el] -> conv el
    | el::l' -> combine (conv el) (fold_combine conv combine l' init)
end

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

module Fresh_integer : Fresh with type id = int =
struct
  type t = int ref
  type id = int
  let create () = ref 0
  let current t = !t
  let next t =
    let value = !t in
    t := !t + 1;
    value
end

(* Unique identifiers plus special identifier requests *)
module type Fresh_with_special =
sig
  include Fresh
  type special_request (* type used to make requests for special identifiers *)
  val special : special_request -> id (* return an id corresponding to a special request *)
end

(* Only one special request: Deprecated *)
type special_request_deprecated = Deprecated

module Fresh_integer_with_deprecated =
struct
  include Fresh_integer
  type special_request = special_request_deprecated
  let special = function Deprecated -> -1
end


(*/************************************************************************\*)
(*| 3. Catalog                                                             |*)
(*\************************************************************************/*)


(* Module providing a class for storing id and names and creating mappings between names and ids. *)
(* In fact it's general enough to use it for mappings between ids and anything 
   (not only names but also any objects) as long as we can order them. *)
(* A five-functor module is quite heavy, but these functors here are redundant, so it is not as
   complicated as it looks. It could be probably possible to reduce it to three functors, however
   I have no idea if that would type correctly, so I prefer to leave it like this. *)
module Catalog =
  functor (Fresh_id : Fresh)                             -> (* For generating fresh ids. *)
  functor (Id_set   : Set.S with type elt = Fresh_id.id) -> (* Set of ids. Must match with the type of ids generated by Fresh_id. *)
  functor (Obj_set  : Set.S)                             -> (* Set of names. *)
  functor (Id_map   : Map.S with type key = Id_set.elt)  -> (* Map with ids as keys.   Must match with the Id_set  element type. *)
  functor (Obj_map  : Map.S with type key = Obj_set.elt) -> (* Map with names as keys. Must match with the Id_name element type. *)
  struct

    (* Five functors is quite heavy indeed, let's extract the two basic types. *)
    type id  = Id_set.elt
    type obj = Obj_set.elt

    (* A modifiable catalog with id <-> object mapping. *)
    class type obj_catalog_iface = object
      method ids           : Id_set.t           (* All the ids. *)
      method objs          : Obj_set.t          (* All the objs. *)
      method obj_of_id     : id  -> obj         (* Mapping obj -> id. May throw Not_found exception. *)
      method id_of_obj     : obj -> id          (* Mapping id -> obj. May throw Not_found exception. *)
      method set_id_of_obj : obj -> id -> unit  (* Adds the obj to objs and makes it correspond to a given id   (only one way, we have obj -> id, but not id -> obj!). *)
      method set_obj_of_id : id  -> obj -> unit (* Adds the id   to ids   and makes it correspond to a given obj (only one way, we have id -> obj, but not obj -> id!). *)
      method get_or_add    : obj -> id          (* Get the id corresponding to a obj. If it does not exist, create a new fresh id for this obj, update the data structures and the return the id. *)
      method add           : obj -> unit        (* As above, but do not return anything. Useful to avoid type warnings (when we discard the returned value). *)
      method id_to_obj_map : obj Id_map.t
      method obj_to_id_map : id Obj_map.t
    end

    (* Implementation of the catalog. *)
    class obj_catalog : obj_catalog_iface = 

      (* Data structures. *)
      let objs          : Obj_set.t ref      = ref Obj_set.empty in  (* All the ids. *)
      let ids           : Id_set.t  ref      = ref Id_set.empty  in  (* All the objs. *)
      let obj_to_id_map : (id Obj_map.t) ref = ref Obj_map.empty in  (* Mapping obj -> id *)
      let id_to_obj_map : (obj Id_map.t) ref = ref Id_map.empty  in  (* Mapping id -> obj *)
    
      (* Mapping functions. *)
      let id_of_obj (obj : obj) : id  = Obj_map.find obj !obj_to_id_map in
      let obj_of_id (id  : id)  : obj = Id_map .find id  !id_to_obj_map in

      (* Add the obj to objs and make it correspond to a given id (obj -> id). *)
      let set_id_of_obj obj id =
        objs          := Obj_set.add obj    (!objs);
        obj_to_id_map := Obj_map.add obj id (!obj_to_id_map) in

      (* Add the id to ids and make it correspond to a given obj (id -> obj). *)
      let set_obj_of_id id obj =
        ids           := Id_set.add id     (!ids);
        id_to_obj_map := Id_map.add id obj (!id_to_obj_map) in
          
      (* Adds new obj and id to appropriate sets 
         and add the relation id <-> obj to both maps. *)
      let add_new_obj_id_pair obj id =
        set_id_of_obj obj id;
        set_obj_of_id id obj in

      (* For unique identifier creation. *)
      let current_id = Fresh_id.create () in

      (* Look for the obj in the maps, if it does not exist create it and give it a fresh id. *)
      let get_or_add obj = 
        try id_of_obj obj
        with Not_found -> let id = Fresh_id.next current_id in add_new_obj_id_pair obj id; id in

      (* As above, but don't return anything. *)
      let add obj =
        let _ = get_or_add obj in () in

      (* The object catalog: *)
      object
        method ids           = !ids
        method objs          = !objs
        method id_of_obj     = id_of_obj
        method obj_of_id     = obj_of_id
        method set_id_of_obj = set_id_of_obj
        method set_obj_of_id = set_obj_of_id
        method get_or_add    = get_or_add
        method add           = add
        method id_to_obj_map = !id_to_obj_map
        method obj_to_id_map = !obj_to_id_map
      end

    let of_set_of_objs (objs : Obj_set.t) : obj_catalog_iface = 
      let catalog = new obj_catalog in
      Obj_set.iter catalog#add objs;
      catalog

    let of_id_to_obj_map (id_to_obj_map : obj Id_map.t) : obj_catalog_iface =
      let catalog = new obj_catalog in
      Id_map.iter (fun id obj ->
        catalog#set_id_of_obj obj id;
        catalog#set_obj_of_id id obj
      ) id_to_obj_map;
      catalog


    type name = obj

    (* A modifiable catalog with name <-> id mapping. *)
    class type catalog_iface = object
      method ids            : Id_set.t           (* All the ids. *)
      method names          : Obj_set.t          (* All the names. *)
      method name_of_id     : id   -> name       (* Mapping name -> id. May throw Not_found exception. *)
      method id_of_name     : name -> id         (* Mapping id -> name. May throw Not_found exception. *)
      method set_id_of_name : name -> id -> unit (* Adds the name to names and makes it correspond to a given id   (only one way, we have name -> id, but not id -> name!). *)
      method set_name_of_id : id -> name -> unit (* Adds the id   to ids   and makes it correspond to a given name (only one way, we have id -> name, but not name -> id!). *)
      method get_or_add     : name -> id         (* Get the id corresponding to a name. If it does not exist, create a new fresh id for this name, update the data structures and the return the id. *)
      method add            : name -> unit       (* As above, but do not return anything. Useful to avoid type warnings (when we discard the returned value). *)
    end

    let catalog_of_obj_catalog (obj_catalog : obj_catalog) : catalog_iface =
      (* The name catalog: *)
      object
        method ids            = obj_catalog#ids
        method names          = obj_catalog#objs
        method id_of_name     = obj_catalog#id_of_obj
        method name_of_id     = obj_catalog#obj_of_id
        method set_id_of_name = obj_catalog#set_id_of_obj
        method set_name_of_id = obj_catalog#set_obj_of_id
        method get_or_add     = obj_catalog#get_or_add
        method add            = obj_catalog#add
      end

    class catalog : catalog_iface = 
      let obj_catalog = new obj_catalog in 
      (* The name catalog: *)
      object
        method ids            = obj_catalog#ids
        method names          = obj_catalog#objs
        method id_of_name     = obj_catalog#id_of_obj
        method name_of_id     = obj_catalog#obj_of_id
        method set_id_of_name = obj_catalog#set_id_of_obj
        method set_name_of_id = obj_catalog#set_obj_of_id
        method get_or_add     = obj_catalog#get_or_add
        method add            = obj_catalog#add
      end

    (* A closed catalog (closed means that it cannot be modified. *)
    class type closed_catalog_iface = object
      method ids        : Id_set.t
      method names      : Obj_set.t
      method name_of_id : id   -> name
      method id_of_name : name -> id
    end

    (* Implementation of a closed catalog which throws appropriate exceptions. *)
    class closed_catalog_with_exceptions (catalog : catalog) (catalog_name : string) (string_of_id : id -> string) (string_of_name : name -> string) : closed_catalog_iface = object
      method ids             = catalog#ids
      method names           = catalog#names
      method name_of_id id   = try catalog#name_of_id id   with Not_found -> failwith (Printf.sprintf "%s#name_of_id %s" catalog_name (string_of_id id))
      method id_of_name name = try catalog#id_of_name name with Not_found -> failwith (Printf.sprintf "%s#id_of_name %s" catalog_name (string_of_name name))
    end
    
    let of_set_of_names (names : Obj_set.t) : catalog_iface = 
      catalog_of_obj_catalog (of_set_of_objs names)

    let of_id_to_name_map (id_to_name_map : name Id_map.t) : catalog_iface =
      catalog_of_obj_catalog (of_id_to_obj_map id_to_name_map)      

  end

(*/************************************************************************\*)
(*| 4. Generic Graph                                                       |*)
(*\************************************************************************/*)

module Graph = struct
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
      
      val parse_tag   : t -> unit
      val is_parsed   : t -> bool
      val parse_untag : t -> unit
      val loop_tag    : Loop.t -> t -> unit
      val loop_untag  : t -> unit
      val is_loop     : t -> bool      (* returns if the vertice is part of a loop *)
      val loop_get    : t -> Loop.t    (* returns the loop in which the vertice is, or raise Not_found *)
      
      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Vertice_set : (Set.S with type elt = Vertice.t) and Vertice_map : (Map.S with type key = Vertice.t)
    
    and Edge : sig
      type t                           (* type of edges *)
      val data   : t -> edge_data      (* returns the data contained in that edge *)
      val origin : t -> Vertice.t      (* returns the origin vertice of that edge *)
      val target : t -> Vertice.t      (* returns the target vertice of that edge *)
      
      val loop_tag     : Loop.t -> t -> unit
      val loop_tag_in  : Loop.t -> t -> unit
      val loop_tag_out : Loop.t -> t -> unit
      val loop_untag   : t -> unit
      val is_loop      : t -> bool     (* returns if that edge is part of a loop *)
      val is_loop_in   : t -> bool     (* returns if that edge targets a vertice in a loop *)
      val is_loop_out  : t -> bool     (* returns if that edge originates from a vertice in a loop *)
      val loop_get     : t -> Loop.t   (* returns the loop linked to that edge, or raise Not_found *)
      
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
      exception Invalid_loop_extension           (* exception raised when erroneous operation is done on a path *)
      
      val create   : Path.t -> t                 (* create a loop from a looping path, or raise Invalid_loop_extension *)
      val add      : Path.t -> t -> unit         (* extend a lopp with a new sub-loop, or raise Invalid_loop_extension *)
      val vertices : t -> Vertice_set.t          (* returns the set of vertices that are part of that loop *)
      val edges    : t -> Edge_set.t             (* returns the set of edges that are part of that loop *)
      val succs_e  : t -> Edge_set.t             (* returns the set of edges whose origin is part of the loop (and which are not part of the loop) *)
      val preds_e  : t -> Edge_set.t             (* returns the set of edges whose target is part of the loop (and which are not part of the loop) *)
      val succs_v  : t -> Vertice_set.t          (* returns the targets of the edges in [succs_e] of that loop *)
      val preds_v  : t -> Vertice_set.t          (* returns the origins of the edges in [preds_e] of that loop *)
      
      val mem    : Vertice.t -> t -> bool        (* tests if that vertice is part of that loop *)
      val center : Vertice.t -> t -> unit        (* if that vertice is part of that loop, move the starting point of the main path of the loop on that vertice *)
      
      val compare : t -> t -> int                 (* classic comparison function *)
      val equal   : t -> t -> bool                (* comparison that returns a bool instead of an int *)
    end and Loop_set : (Set.S with type elt = Loop.t) and Loop_map : (Map.S with type key = Loop.t)

    type t

    val create      : unit -> t
    val add_vertice : vertice_data -> t -> Vertice.t
    val add_edge    : Vertice.t -> edge_data -> Vertice.t -> t -> Edge.t

    val vertices : t -> Vertice_set.t
    val edges    : t -> Edge_set.t
    val loops    : t -> Loop_set.t
    
    val vertice_roots : t -> Vertice_set.t
    val loop_roots    : t -> Loop_set.t
    val vertice_leafs : t -> Vertice_set.t
    val loop_leafs    : t -> Loop_set.t
    
    module Traverse_depth : sig
      val iter : (Path.t -> Vertice.t -> bool -> (unit -> unit) -> unit) -> t -> unit
    end
    
    module Traverse_topology : sig
      val iter_downward : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
      val iter_upward   : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
    end    
  end


  module Make(V: Vertice_data)(E: Edge_data) = struct
    type vertice_data = V.t
    type edge_data = E.t

    type loop_id = int
    type vertice_id = int
    type edge_id = int
  
    module rec Data_types : sig
      type vertice = {
        v_id : vertice_id; v_data : vertice_data;
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;
        mutable v_parse_tag : bool; mutable v_loop_tag : Loop.t option }
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t
      and edge = { e_id : edge_id; e_data : edge_data; e_origin : Vertice.t; e_target : Vertice.t; mutable e_loop_tag : edge_loop_tag }
      and path = { p_origin : Vertice.t; p_end : Vertice.t; p_vertices : Vertice_set.t; p_path : edge list }
      and loop = { l_id : loop_id; mutable l_main_path : Path.t; mutable l_paths : Path_set.t; mutable l_edges_in : Edge_set.t; mutable l_edges_out : Edge_set.t }
    end = struct
      type vertice = {
        v_id : vertice_id; v_data : vertice_data;
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;
        mutable v_parse_tag : bool; mutable v_loop_tag : Loop.t option }
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t
      and edge = { e_id : edge_id; e_data : edge_data; e_origin : Vertice.t; e_target : Vertice.t; mutable e_loop_tag : edge_loop_tag }
      and path = { p_origin : Vertice.t; p_end : Vertice.t; p_vertices : Vertice_set.t; p_path : edge list }
      and loop = { l_id : loop_id; mutable l_main_path : Path.t; mutable l_paths : Path_set.t; mutable l_edges_in : Edge_set.t; mutable l_edges_out : Edge_set.t }
      
    end and Vertice : sig
      type t = Data_types.vertice
      val create : vertice_data -> t
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
    end = struct
      open Data_types
      type t = vertice
      let id = Fresh_integer.create ()
      let create data = { v_id = Fresh_integer.next id; v_data = data; v_successors = Edge_set.empty; v_predecessors = Edge_set.empty; v_parse_tag = false; v_loop_tag = None }
      let data n  = n.v_data
      let succs_e n = n.v_successors
      let preds_e n = n.v_predecessors
      let succs_v n = Edge_set_to_vertices.convert Edge.target (succs_e n)
      let preds_v n = Edge_set_to_vertices.convert Edge.origin (preds_e n)
      
      let parse_tag   v = v.v_parse_tag <- true
      let is_parsed   v = v.v_parse_tag
      let parse_untag v = v.v_parse_tag <- false
      let loop_tag id v = v.v_loop_tag <- Some id
      let loop_untag  v = v.v_loop_tag <- None
      let is_loop     v = match v.v_loop_tag with | None -> false | _ -> true
      let loop_get    v = match v.v_loop_tag with | None -> raise Not_found | Some l -> l
      
      let compare v1 v2 = v1.v_id - v2.v_id
      let equal v1 v2 = (compare v1 v2) = 0
    end and Vertice_set : Set.S with type elt = Vertice.t = Set.Make(Vertice) and Vertice_map : Map.S with type key = Vertice.t = Map.Make(Vertice)
    
    and Edge : sig
      type t = Data_types.edge
      val create : Vertice.t -> edge_data -> Vertice.t -> t
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
    end = struct
      open Data_types
      type t = edge
      let id = Fresh_integer.create ()
      let create o data t = let res = { e_id = Fresh_integer.next id; e_data = data; e_origin = o; e_target = t; e_loop_tag = Edge_no_loop } in
        o.v_successors <- Edge_set.add res o.v_successors; o.v_predecessors <- Edge_set.add res o.v_predecessors; res
      let data e  = e.e_data
      let origin e = e.e_origin
      let target e = e.e_target
      
      let loop_tag     id e = e.e_loop_tag <- Edge_loop id
      let loop_tag_in  id e = e.e_loop_tag <- Edge_loop_in id
      let loop_tag_out id e = e.e_loop_tag <- Edge_loop_out id
      let loop_untag      e = e.e_loop_tag <- Edge_no_loop
      let is_loop     e = match e.e_loop_tag with | Edge_loop _ -> true | _ -> false
      let is_loop_in  e = match e.e_loop_tag with | Edge_loop_in _ -> true | _ -> false
      let is_loop_out e = match e.e_loop_tag with | Edge_loop_out _ -> true | _ -> false
      let loop_get    e = match e.e_loop_tag with | Edge_no_loop -> raise Not_found | Edge_loop id -> id | Edge_loop_in id -> id | Edge_loop_out id -> id
      
      let compare e1 e2 = e1.e_id - e2.e_id
      let equal e1 e2 = (compare e1 e2) = 0
    end and Edge_set : Set.S with type elt = Edge.t = Set.Make(Edge) and Edge_map : Map.S with type key = Edge.t = Map.Make(Edge)
    and Edge_set_to_vertices : sig val convert : (Edge.t -> Vertice.t) -> Edge_set.t -> Vertice_set.t end = Set.Convert(Edge_set)(Vertice_set)
    
    and Path : sig
      type t = Data_types.path
      exception Invalid_path_extension

      val create      : Vertice.t -> t
      val is_empty    : t -> bool
      val add         : Edge.t -> t -> t
      
      val origin      : t -> Vertice.t
      val target      : t -> Vertice.t
      val mem         : Vertice.t -> t -> bool
      val vertices    : t -> Vertice_set.t
      val edges       : t -> Edge_set.t
      val edges_rev   : t -> Edge.t list
      
      val split       : Vertice.t -> t -> t list
      val concat      : t -> t -> t
      val concat_list : t list -> t
      val is_loop     : t -> bool

      val compare : t -> t -> int
      val equal   : t -> t -> bool
    end = struct
      open Data_types
      type t = path

      exception Invalid_path_extension

      let create o = { p_origin = o; p_end = o; p_vertices = Vertice_set.singleton o; p_path = []}
      let is_empty p = match p.p_path with | [] -> true | _ -> false
      let add e p = if Vertice.equal p.p_end (Edge.origin e) then raise (Invalid_path_extension) else let t = Edge.target e in {
          p_origin = p.p_origin;
          p_end = t;
          p_vertices = Vertice_set.add t p.p_vertices;
          p_path = e::p.p_path
        }
      
      let origin p = p.p_origin
      let target p = p.p_end
      let mem n p = Vertice_set.mem n p.p_vertices
      let vertices p = p.p_vertices
      let edges p = Edge_set.set_of_direct_list p.p_path
      let edges_rev p = p.p_path
      
      let of_edge_list l = (* the list l must not be empty, and consistent with a path structure. Not exported for general use *)
       let rec f l accu = match l with
         | []    -> raise Invalid_path_extension
         | [e]   -> (Edge.origin e, Vertice_set.add (Edge.origin e) accu)
         | e::l' -> f l' (Vertice_set.add (Edge.origin e) accu) in
       let target = try Edge.target (List.hd l) with  Failure _ -> raise Invalid_path_extension in
       let (origin, vs) = f l (Vertice_set.singleton target) in
         { p_origin = origin; p_end = target; p_vertices = vs; p_path = l }
      
      let split v p =
        let rec f l = match l with
          | [] -> ([],[])
          | e::l' -> let (accu,res) = f l' in if Vertice.equal (Edge.target e) v then ([], (e::accu)::res) else (e::accu, res) in
        if is_empty p then [p]
        else (
          let (p', ps) = f p.p_path in
          if List.is_empty p' then List.map of_edge_list ps else List.map of_edge_list (p'::ps)
        )
        
      let concat p1 p2 = if Vertice.equal (origin p2) (target p1) then {
        p_origin = p1.p_origin;
        p_end = p2.p_end;
        p_vertices = Vertice_set.union p1.p_vertices p2.p_vertices;
        p_path = p2.p_path @ p1.p_path
      } else raise Invalid_path_extension
      let rec concat_list l = match l with
        | [] -> raise Invalid_path_extension
        | [p] -> p
        | p::l -> let p' = concat_list l in concat p p'
      
      let is_loop p = (Vertice.equal p.p_origin p.p_end) && (not (is_empty p))
    
      

      let compare p1 p2 =
        let rec f dl = match dl with
          | ([], [])         -> 0
          | (e1::l1, e2::l2) -> let tmp = Edge.compare e1 e2 in if tmp != 0 then tmp else f (l1,l2)
          | ( []   , _     ) -> -1
          | ( _    , []    ) ->  1 in f (p1.p_path, p2.p_path)
      let equal p1 p2 = (compare p1 p2 = 0)
    end and Path_set : Set.S with type elt = Path.t = Set.Make(Path) and Path_map : Map.S with type key = Path.t = Map.Make(Path)
    
    and Loop : sig
      type t = Data_types.loop
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
      
      val main_loop : t -> Path.t
      val sub_loops : t -> Path_set.t
      
      val compare : t -> t -> int
      val equal   : t -> t -> bool
    end = struct
      open Data_types
      type t = loop
      exception Invalid_loop_extension

      let vertices l = Path.vertices l.l_main_path
      let edges    l = Path.edges l.l_main_path
      let succs_e  l = l.l_edges_out
      let succs_v  l = Edge_set_to_vertices.convert Edge.target l.l_edges_out
      let preds_e  l = l.l_edges_in
      let preds_v  l = Edge_set_to_vertices.convert Edge.origin l.l_edges_in
      let mem v l = Path.mem v l.l_main_path
      let center v l = if mem v l then match Path.split v l.l_main_path with
        | p1::l1 -> l.l_main_path <- Path.concat_list (l1 @ [p1])
        | _ ->  raise Invalid_loop_extension (* NOT POSSIBLE CASE *)
      
      let main_loop l = l.l_main_path
      let sub_loops l = l.l_paths
      
      let id = Fresh_integer.create ()
      let create p = if not (Path.is_loop p) then raise Invalid_loop_extension else (
        let vs = Path.vertices p in let es = Path.edges p in
        let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs Edge_set.empty) es in
        let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs Edge_set.empty) es in
        let paths = Path_set.set_of_direct_list (Path.split (Path.origin p) p) in
        let res   = { l_id = Fresh_integer.next id; l_main_path = p; l_paths = paths; l_edges_in = e_in; l_edges_out = e_out } in
        Vertice_set.iter (fun v -> Vertice.loop_tag res v) vs; Edge_set.iter (fun e -> Edge.loop_tag res e) es; res
      )
      
      let add p l = if Path.is_loop p then (
        let vs_common = Vertice_set.inter (Path.vertices p) (vertices l) in
        if Vertice_set.is_empty (vs_common) then raise Invalid_loop_extension
        else (
          let v = Vertice_set.choose vs_common in
          let p_res = match (Path.split v p, Path.split v l.l_main_path) with
            | (p1::l1, p2::l2) -> Path.concat_list ((l1 @ [p1]) @ (l2 @ [p2]))
            | _ ->  raise Invalid_loop_extension (* NOT POSSIBLE CASE *) in
          let vs = Path.vertices p in let es = Path.edges p in
          Vertice_set.iter (fun v -> Vertice.loop_tag l v) vs; Edge_set.iter (fun e -> Edge.loop_tag l e) es;
          let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs l.l_edges_in) es in
          let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs l.l_edges_out) es in
          let paths = Path_set.set_of_direct_list (Path.split (Path.origin p) p) in
          l.l_main_path <- p_res; l.l_paths <- Path_set.union paths l.l_paths; l.l_edges_in  <- e_in; l.l_edges_out <- e_out
        )) else raise Invalid_loop_extension
            
      let compare l1 l2 = l1.l_id - l2.l_id
      let equal l1 l2 = (compare l1 l2) = 0
     end and Loop_set : Set.S with type elt = Loop.t = Set.Make(Loop) and Loop_map : Map.S with type key = Loop.t = Map.Make(Loop)




    module El = struct
      type t = V of Vertice.t | L of Loop.t
      
      let is_vertice el = match el with V _ -> true | L _ -> false
      let is_loop    el = match el with V _ -> false | L _ -> true
      
      let compare el1 el2 = match (el1, el2) with
        | (V(v1), V(v2)) -> Vertice.compare v1 v2
        | (L(l1), L(l2)) -> Loop.compare l1 l2
        | (V(_) , L(_) ) -> -1
        | (L(_) , V(_) ) -> 1
    end module El_set = Set.Make(El)
   
    type t = { mutable g_vertices : Vertice_set.t; mutable g_edges : Edge_set.t;
      mutable g_loops_done : bool; mutable g_loops : Loop_set.t;
      mutable g_roots : El_set.t; mutable g_leafs : El_set.t }
    
    let create () = { g_vertices = Vertice_set.empty; g_edges = Edge_set.empty; g_loops_done = false; g_loops = Loop_set.empty;
       g_roots = El_set.empty; g_leafs = El_set.empty }
    
    let add_vertice data g = let v = Vertice.create data in
      g.g_vertices <- Vertice_set.add v g.g_vertices;
      g.g_roots <- El_set.add (El.V v) g.g_roots;
      g.g_leafs <- El_set.add (El.V v) g.g_leafs; v
      
    let add_edge o data t g = let e = Edge.create o data t in
      g.g_edges <- Edge_set.add e g.g_edges;
      g.g_loops_done <- false;
      g.g_roots <- El_set.remove (El.V t) g.g_roots;
      g.g_leafs <- El_set.remove (El.V o) g.g_leafs; e
    

    (* traversal functions *)
    let graph_parse_untag_all g = Vertice_set.iter (fun v -> Vertice.parse_untag v) g.g_vertices
    
    module Traverse_depth = struct
    
      let iter step g = (* step path v already_parsed next *)
        graph_parse_untag_all g;
        let unseen = ref g.g_vertices in
        let rec traverse p v = (* function for parsing a vertice *)
          let next () = unseen := Vertice_set.remove v !unseen; Edge_set.iter (fun e -> traverse (Path.add e p) (Edge.target e)) (Vertice.succs_e v) in
          let b = Vertice.is_parsed v in Vertice.parse_tag v; step p v b next in
        let rec main () = if not (Vertice_set.is_empty !unseen) then (let v = Vertice_set.choose !unseen in traverse (Path.create v) v; main ()) in main ()
        
    end
    
    module Traverse_topology = struct
    
      let generic_iter init (pred_v, next_v) (pred_l, next_l)  step_v step_l g =
        let q = ref init in (* create the queue *)
        graph_parse_untag_all g;
        let check v = let preds = if Vertice.is_loop v then pred_v v else pred_l (Vertice.loop_get v) in
          Vertice_set.fold (fun v res -> res && (Vertice.is_parsed v)) preds true in
        let add v = if check v then (if Vertice.is_loop v then q := El_set.add (El.L (Vertice.loop_get v)) !q else q := El_set.add (El.V v) !q) in
        while not (El_set.is_empty !q) do
          let el = El_set.choose !q in q := El_set.remove el !q; match el with
          | El.V(v) -> step_v v; Vertice.parse_tag v; Vertice_set.iter add (next_v v)
          | El.L(l) -> step_l l; Vertice_set.iter (fun v -> Vertice.parse_tag v) (Loop.vertices l); Vertice_set.iter add (next_l l)
        done
        
      let iter_downward step_v step_l g = generic_iter g.g_roots (Vertice.preds_v, Vertice.succs_v) (Loop.preds_v, Loop.succs_v) step_v step_l g
      let iter_upward   step_v step_l g = generic_iter g.g_leafs (Vertice.succs_v, Vertice.preds_v) (Loop.succs_v, Loop.preds_v) step_v step_l g
    end
    
    (* Access to data *)
    
    let vertices g = g.g_vertices
    let edges    g = g.g_edges
    let loops    g = if g.g_loops_done then g.g_loops else (
      let res = ref Loop_set.empty in
      (* 1. compute the set of loops *)
      let step p v is_parsed next =
        if Path.mem v p then (
          let p' = List.hd (Path.split v p) in (* the path of the loop *)
          let l = if Vertice.is_loop v then (let l = Vertice.loop_get v in Loop.add p' l; l) else Loop.create p' in
          res := Loop_set.add l !res
        ) else ( if not is_parsed then next ()) in
      Traverse_depth.iter step g;
      (* 2. initialize all data in the graph *)
      g.g_loops_done <- true;
      g.g_loops <- !res;
      let filter_fun el = match el with | El.V _ -> false | El.L _ -> true in
      let (rm_roots, rm_leafs) = (El_set.filter filter_fun g.g_roots, El_set.filter filter_fun g.g_leafs) in
      g.g_roots <- El_set.diff g.g_roots rm_roots;
      g.g_leafs <- El_set.diff g.g_leafs rm_leafs;
      (* 3. add relevant loops to roots and leafs *)
      let sort_loop l =
        if Edge_set.is_empty (Loop.preds_e l) then  g.g_roots <- El_set.add (El.L l) g.g_roots
        else (if Edge_set.is_empty (Loop.succs_e l) then  g.g_leafs <- El_set.add (El.L l) g.g_leafs) in
      Loop_set.iter sort_loop !res; !res)
    
    let vertice_roots g = El_set.fold (fun el res -> match el with El.V v -> Vertice_set.add v res | El.L _ -> res) g.g_roots Vertice_set.empty
    let loop_roots    g = El_set.fold (fun el res -> match el with El.V _ -> res | El.L v -> Loop_set.add v res) g.g_roots Loop_set.empty
    let vertice_leafs g = El_set.fold (fun el res -> match el with El.V v -> Vertice_set.add v res | El.L _ -> res) g.g_leafs Vertice_set.empty
    let loop_leafs    g = El_set.fold (fun el res -> match el with El.V _ -> res | El.L v -> Loop_set.add v res) g.g_leafs Loop_set.empty
  end
end




