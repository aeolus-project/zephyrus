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
    - Data_common_list
    - Data_common_linked_list
    - Data_common_set
    - Data_common_map
    - Data_common_unique_id
*)

open Data_common_list
open Data_common_linked_list
open Data_common_set
open Data_common_map
open Data_common_unique_id

let debug_print s = Zephyrus_log.log_execution s; flush stdout

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

      val compare : t -> t -> int      (* classic comparison function *)
      val equal   : t -> t -> bool     (* comparison that returns a bool instead of an int *)
    end and Vertice_set : (Set.S with type elt = Vertice.t) and Vertice_map : (Map.S with type key = Vertice.t)
    
    and Edge : sig
      type t                           (* type of edges *)
      val data   : t -> edge_data      (* returns the data contained in that edge *)
      val origin : t -> Vertice.t      (* returns the origin vertice of that edge *)
      val target : t -> Vertice.t      (* returns the target vertice of that edge *)

      val is_loop      : t -> bool     (* returns if that edge is part of a loop *)
      val is_loop_in   : t -> bool     (* returns if that edge targets a vertice in a loop *)
      val is_loop_out  : t -> bool     (* returns if that edge originates from a vertice in a loop *)
      
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
      
      val compare : t -> t -> int                (* classic comparison function *)
      val equal   : t -> t -> bool               (* comparison that returns a bool instead of an int *)
    end and Loop_set : (Set.S with type elt = Loop.t) and Loop_map : (Map.S with type key = Loop.t)

    
    type t

    val create      : unit -> t                                           (* create a new empty graph *)
    val add_vertice : vertice_data -> t -> Vertice.t                      (* add a new vertice to the graph *)
    val add_edge    : Vertice.t -> edge_data -> Vertice.t -> t -> Edge.t  (* add a new edge to the graph *)
    
    val vertices : t -> Vertice_set.t                                     (* returns the set of vertices in the graph *)
    val edges    : t -> Edge_set.t                                        (* returns the set of edges in the graph *)
    val loops    : t -> Loop_set.t                                        (* returns the set of loops in the graph *)
    
    val vertice_roots : t -> Vertice_set.t
    val loop_roots    : t -> Loop_set.t 
    val vertice_leafs : t -> Vertice_set.t
    val loop_leafs    : t -> Loop_set.t 
    
(*    val to_dot : (vertice_data -> string * string) -> (edge_data -> string * string) -> t -> string*)
    
    module Traverse_depth : sig
      val iter : (Path.t -> Vertice.t -> bool -> (unit -> unit) -> unit) -> t -> unit
    end
    
    module Traverse_topology : sig
      val iter_downward : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
      val iter_upward   : (Vertice.t -> unit) -> (Loop.t -> unit) -> t -> unit
    end    
  end


  module Make(V: Vertice_data)(E: Edge_data) = struct (* Functor for graphs *)
    type vertice_data = V.t  (* data contained in vertices *)
    type edge_data    = E.t  (* data contained in edges *)

    type loop_id    = int    (* unique identifiers for loops *)
    type vertice_id = int    (* unique identifiers for vertices *)
    type edge_id    = int    (* unique identifiers for edgess *)
  
    module rec Data_types : sig
      type vertice = {                                                            (* a vertice *)
        v_id : vertice_id;                                                        (* has a unique id *)
        v_data : vertice_data;                                                    (* contains some data *)
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;   (* links to its input and output edges (these lists can grow) *)
        mutable v_parse_tag : bool;                                               (* has a tag for basic parsing *)
        mutable v_abst : to_component Lazy.t }                                    (* is part of the abstract graph (its representation can change in case loops are created) *)
        
      and edge = {                                                                (* an edge *)
        e_id : edge_id;                                                           (* has a unique id *)
        e_data : edge_data;                                                       (* contains some data *)
        e_origin : Vertice.t; e_target : Vertice.t;                               (* has an origin and a target vertice *)
        mutable e_loop_tag : edge_loop_tag }                                      (* and some annex informations concerning loops (I don't know if still useful) *)
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t

      and path = {                                                                (* a path *)
        p_origin : Vertice.t; p_end : Vertice.t;                                  (* has an origin and a target vertice (like edges) *)
        p_vertices : Vertice_set.t;                                               (* involves a set of vertices *)
        p_path : edge list }                                                      (* and is an ordered list of edges *)
        
      and loop = {                                                                (* a loop *)
        mutable l_path : Edge.t list;                                             (* is defined by a path going through all the edges of the loop *)
        l_edges : Edge_set.t;                                                     (* the set of all the edges in the loop *)
        l_edges_in : Edge_set.t;                                                  (* contains in annex the set of edges that enter the loop *)
        l_edges_out : Edge_set.t;                                                 (* contains in annex the set of edges that exit the loop *)
        l_abst : to_component Lazy.t }                                            (* is part of the abstract graph (its representation can change in case loops are merged) *)
             
      and to_component = {                                                        (* An element of the abstract graph (called topological order) *)
        mutable to_data     : to_data;                                            (* is either a vertice or a loop *)
        mutable to_position : int;                                                (* has an position in the order *)
        mutable to_status   : to_status }                                         (* is either a root, a leaf or a inner element *)
      and to_data = Loop of Loop.t | Vertice of Vertice.t  
      and to_status
      
      val to_status_inner : to_status                                             (* status of an element with nothing in particular *)
      val to_status_root  : to_status                                             (* status of a root element *)
      val to_status_leaf  : to_status                                             (* status of a leaf element *)
      val to_status_combine : to_status ->  to_status ->  to_status               (* function that combine status *)
      val to_status_is_root : to_status -> bool                                   (* tests if the element is a root *)
      val to_status_is_leaf : to_status -> bool                                   (* tests if the element is a leaf *)
      val to_status_remove : to_status -> to_status -> to_status                  (* remove a status from a status *)

    end = struct   (* Once again, every data types with the corresponding comments *)
      type vertice = {                                                            (* a vertice *)
        v_id : vertice_id;                                                        (* has a unique id *)
        v_data : vertice_data;                                                    (* contains some data *)
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;   (* links to its input and output edges (these lists can grow) *)
        mutable v_parse_tag : bool;                                               (* has a tag for basic parsing *)
        mutable v_abst : to_component Lazy.t }                                    (* is part of the abstract graph (its representation can change in case loops are created) *)
        
      and edge = {                                                                (* an edge *)
        e_id : edge_id;                                                           (* has a unique id *)
        e_data : edge_data;                                                       (* contains some data *)
        e_origin : Vertice.t; e_target : Vertice.t;                               (* has an origin and a target vertice *)
        mutable e_loop_tag : edge_loop_tag }                                      (* and some annex informations concerning loops (I don't know if still useful) *)
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t

      and path = {                                                                (* a path *)
        p_origin : Vertice.t; p_end : Vertice.t;                                  (* has an origin and a target vertice (like edges) *)
        p_vertices : Vertice_set.t;                                               (* involves a set of vertices *)
        p_path : edge list }                                                      (* and is an ordered list of edges *)

      and loop = {                                                                (* a loop *)
        mutable l_path : Edge.t list;                                             (* is defined by a path going through all the edges of the loop *)
        l_edges : Edge_set.t;                                                     (* the set of all the edges in the loop *)
        l_edges_in : Edge_set.t;                                                  (* contains in annex the set of edges that enter the loop *)
        l_edges_out : Edge_set.t;                                                 (* contains in annex the set of edges that exit the loop *)
        l_abst : to_component Lazy.t }                                            (* is part of the abstract graph (its representation can change in case loops are merged) *)

      and to_component = {                                                        (* An element of the abstract graph (called topological order) *)
        mutable to_data     : to_data;                                            (* is either a vertice or a loop *)
        mutable to_position : int;                                                (* has an position in the order *)
        mutable to_status   : to_status }                                         (* is either a root, a leaf or a inner element *)
      and to_data = Loop of Loop.t | Vertice of Vertice.t
      and to_status = int

      let to_status_inner = 0
      let to_status_root = 1
      let to_status_leaf = 2
      let to_status_combine = (lor)

      let to_status_is_root s = (s land to_status_root) != 0
      let to_status_is_leaf s = (s land to_status_leaf) != 0
      let to_status_remove s1 s2 = s1 land (lnot s2)

    end and Vertice : sig
      type t = Data_types.vertice                                  (* type of a vertice *)
      val create : vertice_data -> t                               (* create a vertice with it's abstract representation *)
      
      val data    : t -> V.t                                       (* access the data of a vertice *)
      val succs_e : t -> Edge_set.t                                (* access the edges exiting the vertice *)
      val preds_e : t -> Edge_set.t                                (* access the edges entering the vertice *)
      val succs_v : t -> Vertice_set.t                             (* access the son vertices of that vertice *)
      val preds_v : t -> Vertice_set.t                             (* access the parent vertices of that vertice *)
      
      val parse_tag   : t -> unit                                  (* set the vertice as parsed *)
      val is_parsed   : t -> bool                                  (* returns if the vertice was parsed *)
      val parse_untag : t -> unit                                  (* set the vertice as not parsed *)
      val mod_abst    : Data_types.to_component -> t -> unit       (* change the abstract representation of the vertice *)
      val get_abst    : t -> Data_types.to_component               (* returns the abstract representation of the vertice *)
      
      val compare : t -> t -> int                                  (* compare two vertices *)
      val equal   : t -> t -> bool                                 (* wrapper over compare for simple equality test between two vertices *)
    end = struct
      open Data_types
      type t = vertice

      let new_abst v =                  (* the initial abstract representation of a vertice *)
        let status = Data_types.to_status_combine Data_types.to_status_root Data_types.to_status_leaf in
        { to_position = 0; to_data = Vertice v; to_status = status }
      let id = Fresh_integer.create ()  (* unique identifiers for vertices *)
      let create data = let rec res = {
        v_id = Fresh_integer.next id;
        v_data = data; 
        v_successors = Edge_set.empty; v_predecessors = Edge_set.empty; 
        v_parse_tag = false;
        v_abst = lazy (new_abst res) } in res
      
      let data n  = n.v_data
      let succs_e n = n.v_successors
      let preds_e n = n.v_predecessors
      let succs_v n = Edge_set_to_vertices.convert Edge.target (succs_e n)
      let preds_v n = Edge_set_to_vertices.convert Edge.origin (preds_e n)
      
      let parse_tag   v = v.v_parse_tag <- true
      let is_parsed   v = v.v_parse_tag
      let parse_untag v = v.v_parse_tag <- false
      
      let get_abst      v = Lazy.force v.v_abst
      let mod_abst abst v = v.v_abst <- Lazy.lazy_from_val abst
      
      let compare v1 v2 = v1.v_id - v2.v_id
      let equal v1 v2 = (compare v1 v2) = 0
    end and Vertice_set : Set.S with type elt = Vertice.t = Set.Make(Vertice) and Vertice_map : Map.S with type key = Vertice.t = Map.Make(Vertice)
    
    and Edge : sig
      type t = Data_types.edge                               (* type of an edge *)
      val create : Vertice.t -> edge_data -> Vertice.t -> t  (* create an edge with an origin, a data and a target *)
      val data   : t -> E.t                                  (* access the data of the edge *)
      val origin : t -> Vertice.t                            (* access the origin of the edge *)
      val target : t -> Vertice.t                            (* access the target of the edge *)

      val origin_abst : t -> Data_types.to_component         (* access the abstract component of the origin vertice *)
      val target_abst : t -> Data_types.to_component         (* access the abstract component of the target vertice *)
      
      val loop_tag     : Loop.t -> t -> unit                 (* set the edge to be part of the loop in parameter *)
      val loop_tag_in  : Loop.t -> t -> unit                 (* set the edge to enters the loop in parameter *)
      val loop_tag_out : Loop.t -> t -> unit                 (* set the edge to exit the loop in parameter *)
      val loop_untag   : t -> unit                           (* remove all information concerning loops from the edge *)
      
      val is_loop      : t -> bool                           (* returns if the edge is part of a loop *)
      val is_loop_in   : t -> bool                           (* returns if the edge enters a loop *)
      val is_loop_out  : t -> bool                           (* returns if the edge exits a loop *)
(*      val loop_get     : t -> Loop.t                         (* returns the loop concerned by the formentioned functions (TODO: Bug if an edge links two loops?) *)*)
      
      val compare : t -> t -> int                            (* compare two edges *)
      val equal   : t -> t -> bool                           (* wrapper over compare for simple equality test between two edges *)
    end = struct
      open Data_types
      type t = edge
      let id = Fresh_integer.create ()
      let create o data t = let res = {
        e_id = Fresh_integer.next id; 
        e_data = data; 
        e_origin = o; e_target = t; e_loop_tag = Edge_no_loop } in
        o.v_successors <- Edge_set.add res o.v_successors; t.v_predecessors <- Edge_set.add res t.v_predecessors; res
      
      let data e  = e.e_data
      let origin e = e.e_origin
      let target e = e.e_target
      
      let origin_abst e = Vertice.get_abst (origin e)
      let target_abst e = Vertice.get_abst (target e)
      
      let loop_tag     id e = e.e_loop_tag <- Edge_loop id
      let loop_tag_in  id e = e.e_loop_tag <- Edge_loop_in id
      let loop_tag_out id e = e.e_loop_tag <- Edge_loop_out id
      let loop_untag      e = e.e_loop_tag <- Edge_no_loop
      let is_loop     e = match e.e_loop_tag with | Edge_loop _ -> true | _ -> false (* TODO: replace *)
      let is_loop_in  e = match e.e_loop_tag with | Edge_loop_in _ -> true | _ -> false (* TODO: replace *)
      let is_loop_out e = match e.e_loop_tag with | Edge_loop_out _ -> true | _ -> false (* TODO: replace *)
      
      let compare e1 e2 = e1.e_id - e2.e_id
      let equal e1 e2 = (compare e1 e2) = 0
    end and Edge_set : Set.S with type elt = Edge.t = Set.Make(Edge) and Edge_map : Map.S with type key = Edge.t = Map.Make(Edge)
    and Edge_set_to_vertices : sig val convert : (Edge.t -> Vertice.t) -> Edge_set.t -> Vertice_set.t end = Set.Convert(Edge_set)(Vertice_set)
    and Vertice_set_to_edges : sig val set_convert : (Vertice.t -> Edge_set.t) -> Vertice_set.t -> Edge_set.t end = Set.Convert(Vertice_set)(Edge_set)

    and Path : sig
      type t = Data_types.path                               (* Type of paths *)
      exception Invalid_path_extension                       (* Exception raised when trying to extend a path with an edge that does not originate from the right vertice *)

      val create      : Vertice.t -> t                       (* Create an empty path originating from the vertice in parameter *)
      val is_empty    : t -> bool                            (* tests if the path is empty *)
      val add         : Edge.t -> t -> t                     (* extends the path with a new edge *)
      val of_edges    : Edge.t list -> t                     (* create a path from a non-empty list of edges, or raise Invalid_path_extension *)
      
      val origin      : t -> Vertice.t                       (* access the origin of the path *)
      val target      : t -> Vertice.t                       (* access the target of the path *)
      val mem         : Vertice.t -> t -> bool               (* tests if the vertice in parameter is part of the path *)
      val vertices    : t -> Vertice_set.t                   (* access the set of vertices that are part of the path *)
      val edges       : t -> Edge_set.t                      (* access the set of edges that are part of the path *)
      val edges_rev   : t -> Edge.t list                     (* returns the list of edges defining the path, going from the target until the origin *)
      
      val extract     : Vertice.t -> Vertice.t -> t -> t     (* Extract a sub path that starts and ends with the two vertices in parameter, or raise Not_found *)
      val split       : Vertice.t -> t -> t list             (* returns the maximal decomposition of p such that all sub-path originate or ends on the vertice in parameter (or returns p if the vertice is not part of it) *)
      val concat      : t -> t -> t                          (* returns p1.p2 when such a path is valid (or raise Invalid_path_extension) *)
      val concat_list : t list -> t                          (* wrapper over concat to concat a list of paths. Raise Invalid_path_extension if the input list is empty *)
      val is_loop     : t -> bool                            (* tests if the path is a loop *)

      val compare : t -> t -> int                            (* compare two paths *)
      val equal   : t -> t -> bool                           (* wrapper over compare for simple equality test between two paths *)
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
          p_path = e::p.p_path }
      
      let of_edges l = match l with
        | [] -> raise Invalid_path_extension
        | e::_ -> let p = create (Edge.origin e) in List.fold_right (fun e res -> add e res) l p
      
      let origin p = p.p_origin
      let target p = p.p_end
      let mem n p = Vertice_set.mem n p.p_vertices
      let vertices p = p.p_vertices
      let edges p = Edge_set.of_list_directly p.p_path
      let edges_rev p = p.p_path
      
      let extract v1 v2 p =
        let rec remove_end l = match l with
          | [] -> raise Not_found
          | e::l' -> if Vertice.equal (Edge.target e) v2 then l else remove_end l' in
        let rec keep_path l = match l with
          | [] -> raise Not_found
          | e::l' -> if Vertice.equal (Edge.origin e) v1 then [e] else e::(keep_path l') in
        of_edges (keep_path (remove_end p.p_path))
      
      let split v p =
        let rec f l = match l with
          | [] -> ([],[])
          | e::l' -> let (accu,res) = f l' in if Vertice.equal (Edge.target e) v then ([], (e::accu)::res) else (e::accu, res) in
        if is_empty p then [p]
        else (
          let (p', ps) = f p.p_path in
          if List.is_empty p' then List.map of_edges ps else List.map of_edges (p'::ps) )
        
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

(*    
    and Loop : sig
      type t = Data_types.loop                                      (* type of loops *)
      exception Invalid_loop_extension                              (* exception raised when extending a loop with a unrelated path *)
      
      val create   : Path.t -> t                                    (* create a loop and its abstract representation from a path that must be a loop *)
      val add      : Path.t -> t -> unit                            (* extends the loop with a new adjascent loop *)
      
      val vertices : t -> Vertice_set.t                             (* access the vertices that are part of the loop *)
      val edges    : t -> Edge_set.t                                (* access the edges that are part thof the loop *)
      val succs_e  : t -> Edge_set.t                                (* access the edges that exit the loop *)
      val preds_e  : t -> Edge_set.t                                (* access the edges that enter the loop *)
      val succs_v  : t -> Vertice_set.t                             (* access the vertices that are pointed by the loop *)
      val preds_v  : t -> Vertice_set.t                             (* access the vertices that point to the loop *)

      val mod_abst    : Data_types.to_component -> t -> unit        (* change the abstract representation of the loop *)
      val get_abst    : t -> Data_types.to_component                (* returns the abstract representation of the loop *)
      
      val mem    : Vertice.t -> t -> bool                           (* tests if the vertice is part of the loop *)
      val center : Vertice.t -> t -> unit                           (* set the origin and target of the inner path of the loop to the vertice in parameter *)
      
      val main_loop : t -> Path.t                                   (* returns the main path of the loop *)
      val sub_loops : t -> Path_set.t                               (* returns the minimal loops that are part of the one in parameter *)
      
      val compare : t -> t -> int                                   (* compare two loops *)
      val equal   : t -> t -> bool                                  (* wrapper over compare for simple equality test between two loops *)
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

      let mod_abst abst l = l.l_abst <- Lazy.lazy_from_val abst
      let get_abst      l = Lazy.force l.l_abst

      
      let main_loop l = l.l_main_path
      let sub_loops l = l.l_paths
      
      let new_abst l =
        let root = if Edge_set.is_empty (preds_e l) then Data_types.to_status_root else Data_types.to_status_inner in
        let leaf = if Edge_set.is_empty (succs_e l) then Data_types.to_status_leaf else Data_types.to_status_inner in
       { to_position = 0; to_data = Loop l; to_status = Data_types.to_status_combine root leaf; to_next = None }
      let id = Fresh_integer.create ()
      let create p = if not (Path.is_loop p) then raise Invalid_loop_extension else (
        let vs = Path.vertices p in let es = Path.edges p in
        let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs Edge_set.empty) es in
        let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs Edge_set.empty) es in
        let paths = Path_set.singleton p (* loops are created inductively, and thus, are created minimal *) in
        let rec res = {
          l_id = Fresh_integer.next id;
          l_main_path = p;
          l_paths = paths; l_edges_in = e_in; l_edges_out = e_out;
          l_abst = lazy (new_abst res) } in
        Vertice_set.iter (fun v -> Vertice.mod_abst (get_abst res) v) vs; Edge_set.iter (fun e -> Edge.loop_tag res e) es; res
      )
      
      let add p l = () (* if Path.is_loop p then ( (* TODO: redo this function for inductive extension of loops *)
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
          let paths = Path_set.of_list_directly (Path.split (Path.origin p) p) in
          l.l_main_path <- p_res; l.l_paths <- Path_set.union paths l.l_paths; l.l_edges_in  <- e_in; l.l_edges_out <- e_out
        )) else raise Invalid_loop_extension *)
            
      let compare l1 l2 = l1.l_id - l2.l_id
      let equal l1 l2 = (compare l1 l2) = 0
     end and Loop_set : Set.S with type elt = Loop.t = Set.Make(Loop) and Loop_map : Map.S with type key = Loop.t = Map.Make(Loop)
*)
    and Loop : sig
      type t = Data_types.loop                                      (* type of loops *)
      
      val create : Edge_set.t -> t                                  (* create a loop and its abstract representation from a set of edges that must be a loop (no check performed). *)
      
      val vertices : t -> Vertice_set.t                             (* access the vertices that are part of the loop *)
      val edges    : t -> Edge_set.t                                (* access the edges that are part thof the loop *)
      val succs_e  : t -> Edge_set.t                                (* access the edges that exit the loop *)
      val preds_e  : t -> Edge_set.t                                (* access the edges that enter the loop *)
      val succs_v  : t -> Vertice_set.t                             (* access the vertices that are pointed by the loop *)
      val preds_v  : t -> Vertice_set.t                             (* access the vertices that point to the loop *)
      
      val get_abst    : t -> Data_types.to_component                (* returns the abstract representation of the loop *)
      
      val mem    : Vertice.t -> t -> bool                           (* tests if the vertice is part of the loop *)
      val center : Vertice.t -> t -> unit                           (* set the origin and target of the inner path of the loop to the vertice in parameter *)
      val path : t -> Edge.t list                                   (* returns the list of ordered edges starting at the centered vertice, to parse the loop *)
      
      val compare : t -> t -> int                                   (* compare two loops *)
      val equal   : t -> t -> bool                                  (* wrapper over compare for simple equality test between two loops *)
    end = struct open Data_types
      type t = Data_types.loop                                      (* type of loops *)

      let edges l = l.l_edges
      let vertices l = Edge_set_to_vertices.convert Edge.origin (edges l)
      let succs_e  l = l.l_edges_out
      let succs_v  l = Edge_set_to_vertices.convert Edge.target l.l_edges_out
      let preds_e  l = l.l_edges_in
      let preds_v  l = Edge_set_to_vertices.convert Edge.origin l.l_edges_in
      
      let mem v l = Vertice_set.mem v (vertices l)
      let get_abst l = Lazy.force l.l_abst

      let new_abst l =                  (* the initial abstract representation of a loop *)
        let root = if Edge_set.is_empty l.l_edges_in then Data_types.to_status_root else Data_types.to_status_inner in
        let leaf = if Edge_set.is_empty l.l_edges_out then Data_types.to_status_leaf else Data_types.to_status_inner in
        { to_position = 0; to_data = Data_types.Loop l; to_status = Data_types.to_status_combine root leaf }

      let create es =
        let vs = Edge_set_to_vertices.convert Edge.origin es in
        let in_es = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs Edge_set.empty) es in
        let out_es = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs Edge_set.empty) es in
        let rec res = { l_path = []; l_edges = es; l_edges_in = in_es; l_edges_out = out_es; l_abst = lazy (new_abst res) } in res

      let center v l =
        let rec step vs es = 
          if Edge_set.is_empty es then [] (* we parsed all the edges *)
          else let next_es = Edge_set.inter es (Vertice_set_to_edges.set_convert Vertice.succs_e vs) in
          (Edge_set.elements next_es) @ (step (Edge_set_to_vertices.convert Edge.target next_es) (Edge_set.diff es next_es)) in
        l.l_path <- step (Vertice_set.singleton v) (edges l)

      let path l = (if List.is_empty l.l_path then center (Edge.target (Edge_set.choose (edges l))) l); l.l_path
      
      let compare = Pervasives.compare
      let equal l1 l2 = (compare l1 l2) = 0
    end and Loop_set : Set.S with type elt = Loop.t = Set.Make(Loop) and Loop_map : Map.S with type key = Loop.t = Map.Make(Loop)


    module Topological_order (*: sig
      module rec Component : sig
        type t = Data_types.to_component                   (* An element of the order, abstracting vertices and loops *)
        type data = Data_types.to_data 
        type status                                        (* an element of the order can be a root of the graph, a leaf, both or none *)

        val data : t -> data
        val vertices : t -> Vertice_set.t
        val succs : t -> Component_set.t                   (* returns the components that follow the given one following the edges of the real graph *)
        val preds : t -> Component_set.t                   (* returns the components that preceed the given one following the edges of the real graph *)

        val status_inner : status                          (* status of an element with nothing in particular *)
        val status_root  : status                          (* status of a root element *)
        val status_leaf  : status                          (* status of a leaf element *)
        val status_combine : status ->  status ->  status  (* function that combine status *)

        val is_loop : t -> bool                            (* tests if the element is a loop *)
        val is_root : t -> bool                            (* tests if the element is a root *)
        val is_leaf : t -> bool                            (* tests if the element is a leaf *)

        val compare : t -> t -> int
      end and Component_set : Set.S with type elt = Component.t
      
      type t                                             (* Type of total topological ordering, used for automatic loop detection *)
      val create : unit -> t                             (* create a new, empty order *)

      val add_vertice : Vertice.t -> t -> unit           (* add a new vertice to the order *)
      val add_edge : Edge.t -> t -> unit                 (* add a new edge to the order, which triggers the automatic restructuration of the order *)
            

    end *)= struct
      open Data_types
      
      module rec Component : sig
        type t = Data_types.to_component                   (* An element of the order, abstracting vertices and loops *)
        type data = Data_types.to_data  
        type status = Data_types.to_status                 (* an element of the order can be a root of the graph, a leaf, both or none *)

        val data : t -> data                               (* the data of the component *)
        val status : t -> status                           (* the status of the component *)
        val position : t -> int                            (* the position of the component in the order *)
        val vertices : t -> Vertice_set.t                  (* the vertices contained in the component *)
        val edges    : t -> Edge_set.t
        val succs_v : t -> Vertice_set.t                   (* returns the vertices that follow the given one following the edges of the real graph *)
        val succs_e : t -> Edge_set.t                      (* returns the edges that follow the given one following the edges of the real graph *)
        val succs_c : t -> Component_set.t                 (* returns the components that follow the given one following the edges of the real graph *)
        val preds_v : t -> Vertice_set.t                   (* returns the vertices that preceed the given one following the edges of the real graph *)
        val preds_e : t -> Edge_set.t                      (* returns the edges that preceed the given one following the edges of the real graph *)
        val preds_c : t -> Component_set.t                 (* returns the components that preceed the given one following the edges of the real graph *)
        
        (*val path : Vertice.t -> Vertice.t -> Path.t      (* returns a path between the two vertices, using the edges in their component, or raise Not_found *)*)
        
        val paths_towards : t -> t -> Abstract_path_set.t  (* returns the set of abstract path that goes from c1 until the maximal c such that c <= c2 *)
        
        val merge : Abstract_path_set.t -> Edge.t -> t     (* merge all components together with the edge in a big loop component *)
        val add_edge : Edge.t -> t -> unit                 (* addition of an edge to the component *)
        
        val is_loop : t -> bool                            (* tests if the element is a loop *)
        val is_root : t -> bool                            (* tests if the element is a root *)
        val is_leaf : t -> bool                            (* tests if the element is a leaf *)
        val isnt_anymore : status -> t -> unit             (* disable the status in parameter from the component *)

        val compare : t -> t -> int                        (* compare two components *)
        val equal   : t -> t -> bool                       (* wrapper over compare for simple equality test between two components *)
      end = struct
        type t = Data_types.to_component
        type data = Data_types.to_data  
        type status = Data_types.to_status

        let data c = c.to_data
        let status c = c.to_status
        let position c = c.to_position
        let vertices c = match data c with
          | Vertice v' -> Vertice_set.singleton v'
          | Loop l -> Loop.vertices l
        let edges c = match data c with
          | Vertice _ -> Edge_set.empty
          | Loop l -> Loop.edges l

        let compare c1 c2 = (position c1) - (position c2)
        let equal c1 c2 = (compare c1 c2) = 0

        let generic_add v s = Component_set.add (Vertice.get_abst v) s
        
        let succs_v c = match data c with | Vertice v' -> Vertice.succs_v v' | Loop l -> Loop.succs_v l
        let succs_e c = match data c with | Vertice v' -> Vertice.succs_e v' | Loop l -> Loop.succs_e l
        let succs_c c = match data c with
          | Vertice v' -> Vertice_set.fold generic_add (Vertice.succs_v v') Component_set.empty
          | Loop l -> Vertice_set.fold generic_add (Loop.succs_v l) Component_set.empty
        let preds_v c = match data c with | Vertice v' -> Vertice.preds_v v' | Loop l -> Loop.preds_v l
        let preds_e c = match data c with | Vertice v' -> Vertice.preds_e v' | Loop l -> Loop.preds_e l
        let preds_c c = match data c with
          | Vertice v' -> Vertice_set.fold generic_add (Vertice.preds_v v') Component_set.empty
          | Loop l -> Vertice_set.fold generic_add (Loop.preds_v l) Component_set.empty
        
(*        let path v1 v2 = let c = Vertice.get_abst v1 in if compare c (Vertice.get_abst v2) != 0 then raise Not_found else match data c with
          | Vertice v -> Path.create v
          | Loop l -> Path.extract v1 v2 (Loop.main_loop l)*) (* pb with main_loop. what could I do ?? *)
        
        
       (********************* for debug *)
       let sprint_c c = Printf.sprintf "%i " (Vertice_set.choose (vertices c)).v_id
       (********************* for debug *)

        
        let paths_towards c1 c2 =
          debug_print (Printf.sprintf "stating with the component %s\n" (sprint_c c1));
          let rec step accu = debug_print (Printf.sprintf "sub\n");
            let succs_e = Edge_set.filter (fun e -> ((compare (Edge.target_abst e) c2) <= 0) && ((compare c1 (Edge.target_abst e)) < 0)) (succs_e (Abstract_path.target accu)) in
            if Edge_set.is_empty succs_e 
            then (debug_print (Printf.sprintf "that path is finished\n"); Abstract_path_set.singleton accu)
            else Edge_set.fold (fun e res -> debug_print (Printf.sprintf "going further -> %s\n" (sprint_c (Edge.target_abst e))); Abstract_path_set.union (step (Abstract_path.add e accu)) res) succs_e Abstract_path_set.empty in
          step (Abstract_path.create c1)

        let update_vertices c = Vertice_set.iter (fun v -> debug_print (Printf.sprintf "updating data for vertice %i\n" (v.v_id)); Vertice.mod_abst c v) (vertices c)

        let merge ps e = (* function called when the addition of the edge e from c2 to c1 created a loop between c1 and c2, involving the paths ps from c1 to c2  *)
          (* simply create a new component with all the edges of c1, c2,  ps and e inside *)
          let l = Loop.create (Abstract_path_set.fold (fun p res -> Edge_set.union (Abstract_path.edges p) res) ps (Edge_set.singleton e)) in
          let res = Loop.get_abst l in update_vertices res; res

        let add_edge e c = let res = Loop.get_abst (match data c with
           | Vertice v -> (Loop.create (Edge_set.singleton e))
           | Loop l -> (Loop.create (Edge_set.add e (Loop.edges l)))) in
           c.to_data <- data res;
           c.to_status <- status res;
           Vertice.mod_abst c (Edge.origin e); Vertice.mod_abst c (Edge.target e)
        
        let is_loop c = match data c with Loop _ -> true | _ -> false
        let is_root c = to_status_is_root (status c)
        let is_leaf c = to_status_is_leaf (status c)
        let isnt_anymore s c = c.to_status <- to_status_remove s (status c)
        
        
      end and Component_set : sig
        include Set.S with type elt = Component.t
        val vertices : t -> Vertice_set.t
        val loops    : t -> Loop_set.t
      end = struct
        include Set.Make(Component)
        
        let vertices cs = fold (fun c res -> match (Component.data c) with Vertice v -> Vertice_set.add v res | _ -> res) cs Vertice_set.empty
        let loops cs = fold (fun c res -> match (Component.data c) with Loop l -> Loop_set.add l res | _ -> res) cs Loop_set.empty
      end
      
      and Abstract_path : sig
        type t = { origin : Component.t; target : Component.t; path : Edge.t list } (* the type of an abstract path *)
        exception Invalid_path_extension                                            (* exception when trying to construct an ill-defined path *)
       
       val create : Component.t -> t                                                (* create a new empty path *)
       val add : Edge.t -> t -> t                                                   (* add a new edge to the path, or raise Invalid_path_extension *)
       val of_edges : Edge.t list -> t                                              (* create an abstract path from edges, or raise Invalid_path_extension *)
       val concat : t -> t -> t                                                     (* concat two abstract paths, or raise Invalid_path_extension *)
       
       val origin : t -> Component.t                                                (* the origin component of the path *)
       val target : t -> Component.t                                                (* the target component of the path *)
       val components : t -> Component_set.t                                        (* the components involved in the path *)
       val edges  : t -> Edge_set.t
(*       val path : Vertice.t -> t -> Vertice.t -> Path.t                             (* Compute a real path from an abstract one *) *)
       
       val compare : t -> t -> int                                                  (* compare two paths *)
      end = struct
        type t = { origin : Component.t; target : Component.t; path : Edge.t list }
        exception Invalid_path_extension
        
        let origin p = p.origin
        let target p = p.target
        let components p = List.fold_left (fun res e -> Component_set.add (Edge.target_abst e) res) (Component_set.singleton (origin p)) p.path
        let edges p = Component_set.fold (fun c res -> Edge_set.union (Component.edges c) res) (components p) (Edge_set.of_list_directly p.path)
        
        let create o = {origin = o; target = o; path = [] }
        let add e p =  if Component.equal (target p) (Edge.origin_abst e) then
          { origin = p.origin; target = Edge.target_abst e; path = e::p.path } else raise Invalid_path_extension
          
        let of_edges l = match l with
          | [] -> raise Invalid_path_extension
          | e::_ -> let p = create (Edge.origin_abst e) in List.fold_right (fun e res -> add e res) l p

        let concat p1 p2 = if Component.equal (target p1) (origin p2) then { origin = p1.origin; target = p2.target; path = p2.path @ p1.path } else raise Invalid_path_extension
        
(*        let path v1 p v2 =
          let edges_between v1 v2 = List.rev (Path.edges_rev (Component.path v1 v2)) in
          let rec inner_path l v_target = match l with
            | [] -> []
            | e::l -> (inner_path l (Edge.origin e)) @ (e::(edges_between (Edge.target e) v_target)) in
          match p.path with
            | [] -> Component.path v1 v2
            | e::l  -> Path.of_edges ((edges_between v1 (Edge.origin e)) @ (inner_path p.path v2))*)
        
        let compare = Pervasives.compare
      end and Abstract_path_set : Set.S with type elt = Abstract_path.t = Set.Make(Abstract_path)

      
      type t = Component.t Linked_list.t
      
      let iter_right = Linked_list.iter_right
      let iter_left = Linked_list.iter_left
      let iter = Linked_list.iter
      let fold = Linked_list.fold_right
      
      let prefix c o = Linked_list.filter (fun c' -> (Component.compare c' c) < 0) o
      let suffix c o = Linked_list.filter (fun c' -> (Component.compare c' c) > 0) o
      let sub c1 c2 o = Linked_list.filter (fun c' -> ((Component.compare c' c1) > 0) && ((Component.compare c' c2) < 0)) o
      
      let reset_order o = Pervasives.ignore (Linked_list.fold_right (fun c i -> c.to_position <- i; i + 1) o 0)
      
       (********************* for debug *)
       let string_of_c c = Printf.sprintf "(%i, % i)" ((Vertice_set.choose (Component.vertices c)).v_id) (Component.position c)
       let string_of_o o = String.concat " " ((Linked_list.fold_left (fun res c -> (string_of_c c)::res) [] o))
       (********************* for debug *)
      
      let create  = Linked_list.create
      
      let add_vertice v o = let c = Vertice.get_abst v in let id = try (Component.position (Linked_list.last o)) + 1 with | Not_found -> 0 in
        c.to_position <- id; Linked_list.add_last c o;
        debug_print (Printf.sprintf "new vertice added. Now the order is : %s\n" (string_of_o o))
      
      let add_edge e o = let c1 = Edge.origin_abst e in let c2 = Edge.target_abst e in let value = Component.compare c1 c2 in
        debug_print (Printf.sprintf "Adding a new edge %s -> %s to the graph\n" (string_of_c c1) (string_of_c c2));
          Component.isnt_anymore to_status_leaf c1;
          Component.isnt_anymore to_status_root c2;
        if value < 0 
        then (debug_print (Printf.sprintf "The edge is ok for the order. Nothing to do, yeah\n"); o)
        
        else if value > 0 then ( (* c2 is before c1, which is against the edge. We thus need to update the order *)
          debug_print (Printf.sprintf "The edge is inverted for the order. Computing the paths from c2 toward c1...");
          let ps = Component.paths_towards c2 c1 in
          debug_print (Printf.sprintf " ok\n");
          let (ps_loop, ps_unloop) = Abstract_path_set.partition (fun p -> Component.equal c1 (Abstract_path.target p)) ps in
          debug_print (Printf.sprintf " partition of the set of path done\n");
          let path_set_to_component_set ps = Abstract_path_set.fold (fun p res -> Component_set.union (Abstract_path.components p) res) ps Component_set.empty in
          let cs_in_between = Component_set.of_list_directly (Linked_list.to_list (sub c2 c1 o)) in
          debug_print (Printf.sprintf " computation of the components in between done\n");
          let cs_loop = path_set_to_component_set ps_loop in
          debug_print (Printf.sprintf " computation of the components in loop done\n");
          let cs_unloop = path_set_to_component_set ps_unloop in
          debug_print (Printf.sprintf " computation of the components in unloop done\n");
          let cs_move = Component_set.diff cs_in_between cs_loop in
          debug_print (Printf.sprintf " computation of the components to move done\n");
          let (cs_forward, cs_backward) = Component_set.partition (fun c -> Component_set.mem c cs_unloop) cs_move in
          debug_print (Printf.sprintf " partition of the set of components done\n");
          debug_print (Printf.sprintf "  we need to merge the components          : { %s }\n" (String.concat " " (Component_set.fold (fun c res -> (string_of_c c)::res) cs_loop     [])));
          debug_print (Printf.sprintf "  we need to move backward the components  : { %s }\n" (String.concat " " (Component_set.fold (fun c res -> (string_of_c c)::res) cs_backward [])));
          debug_print (Printf.sprintf "  we need to move forward the components   : { %s }\n" (String.concat " " (Component_set.fold (fun c res -> (string_of_c c)::res) cs_forward  [])));
          (* until here, seems ok *)
          
          (* create the new order *)
          let prefix1 = prefix c2 o in                                              (* the ones that are bigger than c1 *)
          let prefix2 = Linked_list.filter (fun c -> Component_set.mem c cs_backward) o in  (* the ones that must be moved forward *)
          let center = let o' = Linked_list.create () in
            (if Component_set.is_empty cs_loop then (Linked_list.add c1 o'; Linked_list.add c2 o')
             else Linked_list.add (Component.merge ps_loop e) o'); o' in
          let suffix1 = Linked_list.filter (fun c -> Component_set.mem c cs_forward) o in
          let suffix2 = suffix c1 o in
          debug_print (Printf.sprintf "The prefix1 is : %s\n" (string_of_o prefix1));
          debug_print (Printf.sprintf "The prefix2 is : %s\n" (string_of_o prefix2));
          debug_print (Printf.sprintf "The center is  : %s\n" (string_of_o center ));
          debug_print (Printf.sprintf "The suffix1 is : %s\n" (string_of_o suffix1));
          debug_print (Printf.sprintf "The suffix2 is : %s\n" (string_of_o suffix2));
            
          let new_o = Linked_list.concat [prefix1; prefix2; center; suffix1; suffix2] in
          
          reset_order new_o;
          debug_print (Printf.sprintf "Order modification performed. Now the order is : %s\n" (string_of_o new_o));
          new_o

        ) else (  (* the edge is either an auto-reference, or connects two vertices inside a loop *)
          debug_print (Printf.sprintf "The edge is inside a component. Simply adding it to the component\n");
          Component.add_edge e c1; o
        )
        
        
        let roots o = 
          let rec step i =
            if Linked_list.has_next i then
              let c = Linked_list.next i in
              if Data_types.to_status_is_root (Component.status c) then Component_set.add c (step i)
              else Component_set.empty
            else Component_set.empty in step (Linked_list.forward_iterator o)

        let leafs o = 
          let rec step i =
            if Linked_list.has_next i then
              let c = Linked_list.next i in
              if Data_types.to_status_is_leaf (Component.status c) then Component_set.add c (step i)
              else Component_set.empty
            else Component_set.empty in step (Linked_list.backward_iterator o)

        let components o = fold (fun c res -> Component_set.add c res) o Component_set.empty
    end

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
   
    type t = { mutable g_vertices : Vertice_set.t; mutable g_edges : Edge_set.t; mutable g_order : Topological_order.t;
      mutable g_loops_done : bool; mutable g_loops : Loop_set.t;
      mutable g_roots : El_set.t; mutable g_leafs : El_set.t }
    
    let create () = { g_vertices = Vertice_set.empty; g_edges = Edge_set.empty; g_order = Topological_order.create ();
       g_loops_done = false; g_loops = Loop_set.empty;
       g_roots = El_set.empty; g_leafs = El_set.empty }
    
    let add_vertice data g = let v = Vertice.create data in
      g.g_vertices <- Vertice_set.add v g.g_vertices;
      g.g_roots <- El_set.add (El.V v) g.g_roots;
      g.g_leafs <- El_set.add (El.V v) g.g_leafs; Topological_order.add_vertice v g.g_order; v
      
    let add_edge o data t g = let e = Edge.create o data t in
      g.g_edges <- Edge_set.add e g.g_edges;
      g.g_loops_done <- false;
      g.g_roots <- El_set.remove (El.V t) g.g_roots;
      g.g_leafs <- El_set.remove (El.V o) g.g_leafs; g.g_order <- Topological_order.add_edge e g.g_order; e
    

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
      let generic_step step_v step_l c = match (Topological_order.Component.data c) with
        | Data_types.Vertice v -> step_v v
        | Data_types.Loop l -> step_l l

      let iter_downward step_v step_l g = Topological_order.iter_right (generic_step step_v step_l) g.g_order
      let iter_upward   step_v step_l g = Topological_order.iter_left  (generic_step step_v step_l) g.g_order
(*
    
      let generic_iter init (pred_v, next_v) (pred_l, next_l)  step_v step_l g =
        let q = ref init in (* create the queue *)
        graph_parse_untag_all g;
        let check v = let preds = (* if Vertice.is_loop v then Vertice_set.empty (* pred_l (Vertice.loop_get v) *) else*) pred_v v in
          Vertice_set.fold (fun v res -> res && (Vertice.is_parsed v)) preds true in
        let add v = if check v then ((*if Vertice.is_loop v then () (* q := El_set.add (El.L (Vertice.loop_get v)) !q *) else*) q := El_set.add (El.V v) !q) in
        while not (El_set.is_empty !q) do
          let el = El_set.choose !q in q := El_set.remove el !q; match el with
          | El.V(v) -> step_v v; Vertice.parse_tag v; Vertice_set.iter add (next_v v)
          | El.L(l) -> step_l l; Vertice_set.iter (fun v -> Vertice.parse_tag v) (Loop.vertices l); Vertice_set.iter add (next_l l)
        done
        
      let iter_downward step_v step_l g = generic_iter g.g_roots (Vertice.preds_v, Vertice.succs_v) (Loop.preds_v, Loop.succs_v) step_v step_l g
      let iter_upward   step_v step_l g = generic_iter g.g_leafs (Vertice.succs_v, Vertice.preds_v) (Loop.succs_v, Loop.preds_v) step_v step_l g *)
    end
    
    (* Access to data *)
    
    let vertices g = g.g_vertices
    let edges    g = g.g_edges
    (*let loops    g =  if g.g_loops_done then g.g_loops else (
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
    *)
    
    let loops g = Topological_order.Component_set.loops    (Topological_order.components g.g_order)
    
    let vertice_roots g = Topological_order.Component_set.vertices (Topological_order.roots g.g_order)
    let loop_roots    g = Topological_order.Component_set.loops    (Topological_order.roots g.g_order)
    let vertice_leafs g = Topological_order.Component_set.vertices (Topological_order.leafs g.g_order)
    let loop_leafs    g = Topological_order.Component_set.loops    (Topological_order.leafs g.g_order)
    
    
  end
end


(*
(* NOTE: Moved from zephyrus.ml *)

(* test the graph *)
module Test_graph_data = struct type t = int end
module Test_graph = Data_common_graph.Graph.Make(Test_graph_data)(Test_graph_data)
let () =
  let g = Test_graph.create () in
(*                               *)
(*              +----+           *)
(*              v    |           *)
(*            +-O-+  |           *)
(*           5|   |10|           *)
(*            v   v  |           *)
(*        1-->2   3  |           *)
(*          6 |7  |8 |           *)
(*            +>4<+  |9          *)
(*              |    |           *)
(*              +----+           *)
(*                               *)
(*                               *)
  let v1 = (Test_graph.add_vertice 1 g) in (* 1 *)
  let v2 = (Test_graph.add_vertice 2 g) in (* 2 *)
  let v0 = (Test_graph.add_vertice 0 g) in (* 3 *)
  let v3 = (Test_graph.add_vertice 3 g) in (* 4 *)
  let v4 = (Test_graph.add_vertice 4 g) in (* 5 *)
  (* order = 1 2 3 4 5 *)

  Pervasives.ignore (Test_graph.add_edge v0  5 v2 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v1  6 v2 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v2  7 v4 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v3  8 v4 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v4  9 v0 g); (* merge { 2 3 5 } ; order = 1 (2 3 5) 4 *)
  Pervasives.ignore (Test_graph.add_edge v0 10 v3 g)  (* merge { 4 (2 3 5) }; order = 1 (2 3 4 5) *)

(* must add a to_string in dot format in the generic graph *)
*)
