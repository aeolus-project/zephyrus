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
(*| 3. Generic Graph                                                       |*)
(*\************************************************************************/*)

module Graph = struct
  module type Vertice_data = sig type t end
  module type Edge_data   = sig type t end

  module Make(V: Vertice_data)(E: Edge_data) = struct
    type loop_id = int
    type vertice_id = int
    type edge_id = int
  
    module rec Data_types : sig
      type vertice = {
        v_id : vertice_id; v_data : V.t;
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;
        mutable v_parse_tag : bool; mutable v_loop_tag : Loop.t option }
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t
      and edge = { e_id : edge_id; e_data : E.t; e_origin : Vertice.t; e_target : Vertice.t; mutable e_loop_tag : edge_loop_tag }
      and path = { p_origin : Vertice.t; p_end : Vertice.t; p_vertices : Vertice_set.t; p_path : edge list }
      and loop = { l_id : loop_id; mutable l_path : Path.t; mutable l_edges_in : Edge_set.t; mutable l_edges_out : Edge_set.t }
    end = struct
      type vertice = {
        v_id : vertice_id; v_data : V.t;
        mutable v_successors : Edge_set.t; mutable v_predecessors : Edge_set.t;
        mutable v_parse_tag : bool; mutable v_loop_tag : Loop.t option }
      and edge_loop_tag = Edge_no_loop | Edge_loop of Loop.t | Edge_loop_in of Loop.t | Edge_loop_out of Loop.t
      and edge = { e_id : edge_id; e_data : E.t; e_origin : Vertice.t; e_target : Vertice.t; mutable e_loop_tag : edge_loop_tag }
      and path = { p_origin : Vertice.t; p_end : Vertice.t; p_vertices : Vertice_set.t; p_path : edge list }
      and loop = { l_id : loop_id; mutable l_path : Path.t; mutable l_edges_in : Edge_set.t; mutable l_edges_out : Edge_set.t }
      
    end and Vertice : sig
      type t = Data_types.vertice
      val create : V.t -> t
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
      val create : Vertice.t -> E.t -> Vertice.t -> t
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
            | e::l' -> if Vertice.equal (Edge.origin e) root then e::(pl'.p_path @ l) else e::(f l') in
          let _path = f p.p_path in {
            p_origin = p.p_origin;
            p_end = p.p_end;
            p_vertices = Vertice_set.union pl.p_vertices p.p_vertices;
            p_path = _path
          }
      ))
    end
    
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
      
      val compare : t -> t -> int
      val equal   : t -> t -> bool
    end = struct
      open Data_types
      type t = loop
      exception Invalid_loop_extension
      
      let id = Fresh_integer.create ()
      let create p = if not (Path.is_loop p) then raise Invalid_loop_extension else (
        let vs = Path.vertices p in let es = Path.edges p in
        let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs Edge_set.empty) es in
        let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs Edge_set.empty) es in
        let res   = { l_id = Fresh_integer.next id; l_path = p; l_edges_in = e_in; l_edges_out = e_out } in
        Vertice_set.iter (fun v -> Vertice.loop_tag res v) vs; Edge_set.iter (fun e -> Edge.loop_tag res e) es; res
      )
      
      let add p l = let p' = try Path.add_loop p l.l_path with | Path.Invalid_path_extension -> raise Invalid_loop_extension in
        let vs = Path.vertices p in let es = Path.edges p in
        Vertice_set.iter (fun v -> Vertice.loop_tag l v) vs; Edge_set.iter (fun e -> Edge.loop_tag l e) es;
        let e_in  = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.preds_e v) res) vs l.l_edges_in) es in
        let e_out = Edge_set.diff (Vertice_set.fold (fun v res -> Edge_set.union (Vertice.succs_e v) res) vs l.l_edges_out) es in
        l.l_path <- p'; l.l_edges_in  <- e_in; l.l_edges_out <- e_out
      
      let vertices l = Path.vertices l.l_path
      let edges    l = Path.edges l.l_path
      let succs_e  l = l.l_edges_out
      let succs_v  l = Edge_set_to_vertices.convert Edge.target l.l_edges_out
      let preds_e  l = l.l_edges_in
      let preds_v  l = Edge_set_to_vertices.convert Edge.origin l.l_edges_in
      
      let mem v l = Path.mem v l.l_path
      let center v l = if mem v l then l.l_path <- Path.center_loop_unsafe v l.l_path
      
      let compare l1 l2 = l1.l_id - l2.l_id
      let equal l1 l2 = (compare l1 l2) = 0
     end and Loop_set : Set.S with type elt = Loop.t = Set.Make(Loop) and Loop_map : Map.S with type key = Loop.t = Map.Make(Loop)
     

    module El = struct
      type t = V of Vertice.t | L of Loop.t
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
      g.g_leafs <- El_set.add (El.V v) g.g_leafs
      
    let add_edge o data t g = let e = Edge.create o data t in
      g.g_edges <- Edge_set.add e g.g_edges;
      g.g_loops_done <- false;
      g.g_roots <- El_set.remove (El.V t) g.g_roots;
      g.g_leafs <- El_set.remove (El.V o) g.g_leafs
    

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
    
    (* loop computation *)
    
    let get_loops g =
      let res = ref Loop_set.empty in
      (* 1. compute the set of loops *)
      let step p v is_parsed next =
        if Path.mem v p then (
          let p' = Path.extract_of_vertice v p in (* the path of the loop *)
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
      Loop_set.iter sort_loop !res; !res
    
  end
end




