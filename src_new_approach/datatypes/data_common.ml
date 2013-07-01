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
    
    val set_of_direct_list: elt list -> t
    val set_of_list: ('a -> elt) -> 'a list -> t
  end
  
  module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
    module Set_tmp = Set_global_from_stdlib.Make(Ord)
    include Set_tmp
    
    let set_of_direct_list l = List.fold_left (fun res v -> add v res) empty l
    let set_of_list f l = List.fold_left (fun res v -> add (f v) res) empty l
  end

  module Convert(Set_origin : S) (Set_target : S) = struct
    let convert f s = Set_origin.fold (fun v res -> Set_target.add (f v) res) s Set_target.empty
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
  functor (Fresh_id : Fresh)                           -> (* For generating fresh ids. *)
  functor (Id_set   : Set.S with type elt = Fresh_id.id)  -> (* Set of ids. Must match with the type of ids generated by Fresh_id. *)
  functor (Name_set : Set.S)                              -> (* Set of names. *)
  functor (Id_map   : Map.S with type key = Id_set.elt)   -> (* Map with ids as keys.   Must match with the Id_set  element type. *)
  functor (Name_map : Map.S with type key = Name_set.elt) -> (* Map with names as keys. Must match with the Id_name element type. *)
  struct

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
    class catalog : catalog_iface = 

      (* Data structures. *)
      let names          : Name_set.t ref      = ref Name_set.empty in  (* All the ids. *)
      let ids            : Id_set.t ref        = ref Id_set.empty   in  (* All the names. *)
      let name_to_id_map : (id Name_map.t) ref = ref Name_map.empty in  (* Mapping name -> id *)
      let id_to_name_map : (name Id_map.t) ref = ref Id_map.empty   in  (* Mapping id -> name *)
    
      (* Mapping functions. *)
      let id_of_name (name : name) : id   = Name_map.find name !name_to_id_map in
      let name_of_id (id   : id)   : name = Id_map  .find id   !id_to_name_map in

      (* Add the name to names and make it correspond to a given id (name -> id). *)
      let set_id_of_name name id =
        names          := Name_set.add name    (!names);
        name_to_id_map := Name_map.add name id (!name_to_id_map) in

      (* Add the id to ids and make it correspond to a given name (id -> name). *)
      let set_name_of_id id name =
        ids            := Id_set  .add id      (!ids);
        id_to_name_map := Id_map  .add id name (!id_to_name_map) in
          
      (* Adds new name and id to appropriate sets 
         and add the relation id <-> name to both maps. *)
      let add_new_name_id_pair name id =
        set_id_of_name name id;
        set_name_of_id id name in

      (* For unique identifier creation. *)
      let current_id = Fresh_id.create () in

      (* Look for the name in the maps, if it does not exist create it and give it a fresh id. *)
      let get_or_add name = 
        try id_of_name name
        with Not_found -> let id = Fresh_id.next current_id in add_new_name_id_pair name id; id in

      (* As above, but don't return anything. *)
      let add name =
        let _ = get_or_add name in () in

      (* The catalog object. *)
      object
        method ids            = !ids
        method names          = !names
        method id_of_name     = id_of_name
        method name_of_id     = name_of_id
        method set_id_of_name = set_id_of_name
        method set_name_of_id = set_name_of_id
        method get_or_add     = get_or_add
        method add            = add
      end

    (* A closed catalog (closed means that it cannot be modified. *)
    class type closed_catalog_iface = object
      method ids        : Id_set.t
      method names      : Name_set.t
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
      
  end


(*/************************************************************************\*)
(*| 4. Generic Graph                                                       |*)
(*\************************************************************************/*)

module Graph = struct
  module type Vertice = sig type t end
  module type Edge   = sig type t end

  module Make(V: Vertice)(E: Edge) = struct
    type loop_id = int
    type vertice_id = int
    type edge_id = int
  
    type vertice = {
      v_id : vertice_id; v_data : V.t;
      mutable v_successors : edge list; mutable v_predecessors : edge list;
      mutable v_parse_tag : bool; mutable v_loop_tag : loop_id option }
    and edge = { e_id : edge_id; e_data : E.t; e_origin : vertice; e_target : vertice; mutable e_loop_tag : loop_id option }

    module Vertice = struct
      type t = vertice
      let id = Fresh_integer.create ()
      let create data = { v_id = Fresh_integer.next id; v_data = data; v_successors = []; v_predecessors = []; v_parse_tag = false; v_loop_tag = None }
      let data n  = n.v_data
      let succs_e n = n.v_successors
      let preds_e n = n.v_predecessors
      let succs_v n = List.map (fun e -> e.e_target) (succs_e n)
      let preds_v n = List.map (fun e -> e.e_origin) (preds_e n)
      let compare v1 v2 = v1.v_id - v2.v_id
      let equal v1 v2 = (compare v1 v2) = 0
    end module Vertice_set = Set.Make(Vertice) module Vertice_map = Map.Make(Vertice)

    module Edge = struct
      type t = edge
      let id = Fresh_integer.create ()
      let create o data t = let res = { e_id = Fresh_integer.next id; e_data = data; e_origin = o; e_target = t; e_loop_tag = None } in
        o.v_successors <- res::o.v_successors; o.v_predecessors <- res::o.v_predecessors; res
      let data e  = e.e_data
      let origin e = e.e_origin
      let target e = e.e_target
      let compare e1 e2 = e1.e_id - e2.e_id
    end module Edge_set = Set.Make(Edge) module Edge_map = Map.Make(Edge)
    
    module Path = struct
      type t = { p_origin : vertice; p_end : vertice; p_vertices : Vertice_set.t; p_path : edge list }

      exception Invalid_path_extension

      let create o = { p_origin = o; p_end = o; p_vertices = Vertice_set.singleton o; p_path = []}
      let is_empty p = match p.p_path with | [] -> true | _ -> false
      let add e p = if Vertice.equal p.p_end (Edge.origin e) then raise (Invalid_path_extension) else let t = Edge.target e in {
          p_origin = p.p_origin;
          p_end = t;
          p_vertices = Vertice_set.add t p.p_vertices;
          p_path = e::p.p_path
        }
      let mem n p = Vertice_set.mem n p.p_vertices
      let vertices p = p.p_vertices
      let edges p = Edge_set.set_of_direct_list p.p_path
      
      let extract_of_vertice v p = (* TODO: switch to a split function *) (* used to extract the loop from a path *)
        let rec f l = match l with
          | [] -> []
          | e::l' -> if Vertice.equal (Edge.origin e) v then [e] else e::(f l') in
        let _path_rev = f p.p_path in
        let _origin = match _path_rev with | [] -> p.p_origin | e::_ -> Edge.origin e in
        let _end = p.p_end in {
          p_origin   = _origin;
          p_end      = _end;
          p_vertices = List.fold_left (fun res e -> Vertice_set.add (Edge.origin e) res) (Vertice_set.singleton _end) _path_rev;
          p_path     = List.rev _path_rev
        }
      let is_loop p = (Vertice.equal p.p_origin p.p_end) && (not (is_empty p))
      
      let center_loop_unsafe v p = 
        let rec f l = match l with (* returns (what stays, what moves in first) *)
          | [] -> ([],[])
          | e::l' -> if Vertice.equal (Edge.origin e) v then (l,[e]) else let (l1, l2) = f l' in (e::l1, l2) in
        let (l1, l2) = f p.p_path in {
          p_origin = v;
          p_end = v;
          p_vertices = p.p_vertices;
          p_path = l2 @ l1
        }
      
      let add_loop pl p = if not (is_loop pl) then raise (Invalid_path_extension) else (
        let s = Vertice_set.inter pl.p_vertices p.p_vertices in
        if Vertice_set.is_empty s then raise (Invalid_path_extension) else (
        let root = Vertice_set.choose s in
          (* 1. recenter pl on root *)
          let pl' = center_loop_unsafe root pl in
          (* 2. insert pl' inside p *)
          let rec f l = match l with
            | [] -> pl.p_path
            | e::l' -> if Vertice.equal (Edge.origin e) root then e::(pl.p_path @ l) else e::(f l') in
          let _path = f p.p_path in {
            p_origin = p.p_origin;
            p_end = p.p_end;
            p_vertices = Vertice_set.union pl.p_vertices p.p_vertices;
            p_path = _path
          }
      ))
    end
    
    module Loop = struct
      type t = { l_id : loop_id; mutable l_path : Path.t; mutable edges_in : Edge_set.t; mutable edges_out : Edge_set.t }
 
     end
(*    
    type t = { mutable g_vertices : Vertice_set.t; mutable g_edges : Edge_set.t; mutable g_done_loops : bool; g_loops : loops;
      mutable g_roots : Vertice_set.t; mutable g_leafs : Vertice_set.t }
    
    let create () = { g_vertices = Vertice_set.empty; g_edges = Edge_set.empty; g_done_loops = false; g_loops = Hashtbl.create 16; (* I don't expect many loops *)
       g_roots = Vertice_set.empty; g_leafs = Vertice_set.empty }
    
    let add_vertice data g = let v = Vertice.create data in
      g.g_vertices <- Vertice_set.add v g.g_vertices;
      g.g_roots <- Vertice_set.add v g.g_roots;
      g.g_leafs <- Vertice_set.add v g.g_leafs
      
    let add_edge o data t g = let e = Edge.create o data t in
      g.g_edges <- Edge_set.add e g.g_edges;
      g.g_done_loops <- false;
      g.g_roots <- Vertice_set.remove t g.g_roots;
      g.g_leafs <- Vertice_set.remove o g.g_leafs
      
    (* parsing functions *)
    
  *)  
  end
end




