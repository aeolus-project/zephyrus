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


(*/************************************************************************\*)
(*| 1. Module and Type Definition                                          |*)
(*\************************************************************************/*)

open Data_constraint (* To have direct access to the modules Value and Bound *)


let default_bound = Bound.create Value.zero Value.infty

(*
module rec V_data_init : sig
  type port = { p_data : Data_model.port; mutable p_replica : Bound.t; }
  type component_type = Data_model.component_type
  type t_inner = Component_type of component_type | Port of port
  type t = { mutable bounds : Bound.t; mutable conflict : Graph.Vertice_set.t; inner : t_inner}
end = struct
  type port = { p_data : Data_model.port; mutable p_replica : Bound.t; }
  type component_type = Data_model.component_type
  type t_inner = Component_type of component_type | Port of port
  type t = { mutable bounds : Bound.t; mutable conflict : Graph.Vertice_set.t; inner : t_inner}
end and E_data : sig
  type t = Value.t
  
  val of_value : Data_constraint.Value.t -> t
  val of_require_arity : Data_model.require_arity -> t
  val of_provide_arity : Data_model.provide_arity -> t
  val value_of : t -> Data_constraint.Value.t
end = struct
  type t = Value.t
  
  let of_value v = v
  let of_require_arity = Value.of_require_arity
  let of_provide_arity = Value.of_provide_arity
  let value_of v = v
end and Graph : Data_common.Graph.S with type vertice_data = V_data_init.t and type edge_data = E_data.t = Data_common.Graph.Make(V_data_init)(E_data)

module V_data : sig
  type t = V_data_init.t
  exception Invalid_operation
  
  val of_port : Data_model.port -> t
  val of_component_type : Data_model.component_type -> t

  val bound_combine : t -> Bound.t -> unit
  val bound_add_min : t -> Value.t -> unit
  val bound_add_max : t -> Value.t -> unit

  val bound          : t -> Bound.t
  val bound_min      : t -> Value.t
  val bound_max      : t -> Value.t
  val bound_is_empty : t -> bool

  val conflict_add : t -> Graph.Vertice.t -> unit
  val conflict     : t -> Graph.Vertice_set.t

  val port_replica_add_min : t -> Value.t -> unit
  val port_replica_add_max : t -> Value.t -> unit
  val port_replica_min     : t -> Value.t
  val port_replica_max     : t -> Value.t
  
  val is_port            : t -> bool
  val is_component_type  : t -> bool
  val get_port           : t -> Data_model.port
  val get_component_type : t -> Data_model.component_type
end = struct
  type t = V_data_init.t
  exception Invalid_operation
  open V_data_init

  let of_port p = { bounds = default_bound; conflict = Graph.Vertice_set.empty; inner = Port {p_data = p; p_replica = default_bound } }
  let of_component_type t = { bounds = default_bound; conflict = Graph.Vertice_set.empty; inner = Component_type t }
  
  let bound_combine v_data b = v_data.bounds <- Bound.combine v_data.bounds b
  let bound_add_min v_data v = v_data.bounds <- Bound.add_min v_data.bounds v
  let bound_add_max v_data v = v_data.bounds <- Bound.add_max v_data.bounds v

  let bound v_data = v_data.bounds
  let bound_min v_data = Bound.min v_data.bounds
  let bound_max v_data = Bound.max v_data.bounds
  let bound_is_empty v_data = Bound.is_empty v_data.bounds

  let conflict_add v_data v = v_data.conflict <- Graph.Vertice_set.add v v_data.conflict
  let conflict v_data = v_data.conflict
  let port_replica_add_min v_data v = match v_data.inner with | Port d -> d.p_replica <- Bound.add_min d.p_replica v | Component_type _ -> raise Invalid_operation
  let port_replica_add_max v_data v = match v_data.inner with | Port d -> d.p_replica <- Bound.add_max d.p_replica v | Component_type _ -> raise Invalid_operation
  let port_replica_min v_data = match v_data.inner with | Port d -> Bound.min d.p_replica | Component_type _ -> raise Invalid_operation
  let port_replica_max v_data = match v_data.inner with | Port d -> Bound.max d.p_replica | Component_type _ -> raise Invalid_operation
  
  let is_port v_data = match v_data.inner with | Port _ -> true | Component_type _ -> false
  let is_component_type v_data = match v_data.inner with | Port _ -> false | Component_type _ -> true
  let get_port v_data =  match v_data.inner with | Port d -> d.p_data | Component_type _ -> raise Invalid_operation
  let get_component_type v_data =  match v_data.inner with | Port _ -> raise Invalid_operation  | Component_type t -> t
end

type flat_universe = { f_graph : Graph.t; f_p_map : Data_model.port -> Graph.Vertice.t; f_t_map : Data_model.component_type -> Graph.Vertice.t }


(*/************************************************************************\*)
(*| 2. Utility Functions                                                   |*)
(*\************************************************************************/*)

let vertices fu = Graph.vertices fu.f_graph
let edges fu = Graph.edges fu.f_graph

let data_v = Graph.Vertice.data
let handle_vertice f_p f_t v = let d = data_v v in if V_data.is_port d then f_p v d else f_t v d
let data_e = Graph.Edge.data





let finalize_bound_roots fu =
  Graph.Vertice_set.iter (fun v -> let data = data_v v in V_data.bound_add_max data (V_data.bound_min data)) (Graph.vertice_roots fu.f_graph)


let create u = 
  let res = Graph.create () in
  let port_map = ref Data_model.Port_map.empty in
  let component_type_map = ref Data_model.Component_type_map.empty in
  let find_p  p = Data_model.Port_map.find p !port_map in
  let add_p p v = port_map := Data_model.Port_map.add p v !port_map in
  let find_t  t = Data_model.Component_type_map.find t !component_type_map in
  let add_t t v = component_type_map := Data_model.Component_type_map.add t v !component_type_map in
  (* 1. create vertices *)
  Data_model.Port_set.iter (fun p -> add_p p (Graph.add_vertice (V_data.of_port p) res)) u#get_port_ids;
  Data_model.Component_type_set.iter (fun t -> add_t t (Graph.add_vertice (V_data.of_component_type t) res)) u#get_component_types;
  (* 2. create edges and conflicts *)
  let add_conflict p t = let (p', t') = (find_p p, find_t t) in
    V_data.conflict_add (data_v p') t';
    V_data.conflict_add (data_v t') p' in
  let edges_of_component_type t =
    Data_model.Port_set.iter (fun p -> Pervasives.ignore (Graph.add_edge (find_t t) (E_data.of_require_arity (t#require p)) (find_p p) res)) t#require_domain;
    Data_model.Port_set.iter (fun p -> Pervasives.ignore (Graph.add_edge (find_p p) (E_data.of_provide_arity (t#provide p)) (find_t t) res)) t#provide_domain;
    Data_model.Port_set.iter (fun p -> add_conflict p t) t#conflict in
  Data_model.Component_type_set.iter edges_of_component_type u#get_component_types;
  (* 3. Compute loops *)
  let ls = Graph.loops res in
  (* TODO: compute all informations about loops *)
  (* 4. return the full flat universe *)
  { f_graph = res; f_p_map = find_p; f_t_map = find_t }


(*/************************************************************************\*)
(*| 3. Bounds Propagation Algorithms                                       |*)
(*\************************************************************************/*)

let min_quantity_of_require_edge e = Value.prod (data_e e) (V_data.bound_min (data_v (Graph.Edge.origin e)))
let min_replica_of_require_edge  e = if Value.is_sup (V_data.bound_min (data_v (Graph.Edge.origin e))) (Value.zero) then data_e e else Value.zero

let min_quantity_of_provide_edge e = Value.div (V_data.bound_min (data_v (Graph.Edge.origin e))) (data_e e)
let min_replica_of_provide_edge  e = V_data.port_replica_min (data_v (Graph.Edge.origin e))

let propagate_lower_bound fu =
  let step_v_p p_v p_data = let preds = Graph.Vertice.preds_e p_v in        (* 1. deal with port vertice: p_data = data, preds = component types requiring p *)
    V_data.bound_add_min p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (min_quantity_of_require_edge e)::res) preds [])); (* must provide all requirement *)
    Graph.Edge_set.iter (fun e -> V_data.port_replica_add_min p_data (min_replica_of_require_edge e)) preds in       (* must have as many replica as necessary *)
    
  let step_v_t t_v t_data = let preds = Graph.Vertice.preds_e t_v in  (* 2. deal with component type vertice: t_data = data, preds = ports provided by t *)
    let preds' = Graph.Edge_set.filter (fun e -> Graph.Edge_set.cardinal (Graph.Vertice.succs_e (Graph.Edge.origin e)) < 2) preds in (* get the ports with only one provider *)
    let n = Value.sums (Graph.Edge_set.fold (fun e res -> (min_quantity_of_provide_edge e)::res) preds' []) in           (* must provide everything *)
    V_data.bound_add_min t_data (Graph.Edge_set.fold (fun e res -> Value.max res (min_replica_of_provide_edge e)) preds' n) in (* must have as many replica as required *)
    
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement lower bound computation for loops *)
  Graph.Traverse_topology.iter_downward step_v step_l fu.f_graph


let propagate_conflicts fu =
  let step_v_p (p_v, p_data) = if Value.is_sup (V_data.bound_min p_data) Value.zero then
    Graph.Vertice_set.iter (fun v' -> V_data.bound_add_max (data_v v') (if Graph.Vertice_set.mem v' (Graph.Vertice.succs_v p_v) then Value.one else Value.zero))
      (V_data.conflict p_data) in
  let step_v_t (t_v, t_data) = if Value.is_sup (V_data.bound_min t_data) Value.zero then
    Graph.Vertice_set.iter (fun v' -> 
      let bound = Graph.Edge_set.fold (fun e res -> if Graph.Vertice.equal (Graph.Edge.target e) t_v then data_e e else res) (Graph.Vertice.succs_e v') Value.zero in
      V_data.bound_combine (data_v v') (Bound.singleton bound)) (V_data.conflict t_data) in
  let p_list = ref [] in
  let t_list = ref [] in
  Graph.Vertice_set.iter (handle_vertice (fun p_v p_data -> p_list := (p_v, p_data)::!p_list) (fun t_v t_data -> t_list := (t_v, t_data)::!t_list)) (vertices fu);
   List.iter step_v_t !t_list; List.iter step_v_p !p_list


let max_quantity_of_require_edge e = Value.div (V_data.bound_max (data_v (Graph.Edge.target e))) (data_e e)
let max_replica_of_require_edge  e = if Value.is_sup (data_e e) (V_data.bound_max (data_v (Graph.Edge.target e))) then Value.zero else Value.infty

let max_quantity_of_provide_edge e = Value.prod (V_data.bound_max (data_v (Graph.Edge.target e))) (data_e e)
let max_replica_of_provide_edge  e = V_data.bound_max (data_v (Graph.Edge.target e))

let propagate_upper_bound fu = 
  let step_v_p p_v p_data = let succs = Graph.Vertice.succs_e p_v in
    V_data.bound_add_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (max_quantity_of_provide_edge e)::res) succs []));
    V_data.port_replica_add_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (max_replica_of_provide_edge e)::res) succs [])) in
  let step_v_t t_v t_data = let succs = Graph.Vertice.succs_e t_v in
    Graph.Edge_set.iter (fun e -> V_data.bound_add_max t_data (max_quantity_of_require_edge e)) succs;
    Graph.Edge_set.iter (fun e -> V_data.bound_add_max t_data (max_replica_of_require_edge  e)) succs in
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement upper bound computation for loops *)
  Graph.Traverse_topology.iter_upward step_v step_l fu.f_graph


let minimize_quantity_of_require_edge e = Value.prod (data_e e) (V_data.bound_max (data_v (Graph.Edge.origin e)))
let minimize_quantity_of_provide_edge e = Value.div (V_data.bound_max (data_v (Graph.Edge.origin e))) (data_e e)

let minimize_upper_bound fu =
  let step_v_p p_v p_data = let succs = Graph.Vertice.succs_e p_v in
    V_data.bound_add_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (minimize_quantity_of_require_edge e)::res) succs [])) in
  let step_v_t t_v t_data = let succs = Graph.Vertice.succs_e t_v in
    V_data.bound_add_max t_data (Graph.Edge_set.fold (fun e res -> Value.max res (minimize_quantity_of_provide_edge e)) succs Value.zero) in
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement minimize bound computation for loops *)
  Graph.Traverse_topology.iter_downward step_v step_l fu.f_graph


(*/************************************************************************\*)
(*| 4. Initial Bounds Computation                                          |*)
(*\************************************************************************/*)


let get_initial_mins solver universe spec domain =
  let (spv, stv, skv) = Data_model.uv_of_specification spec in
  let main_k = Constraint_of.specification domain spec in
  let annex_k = Constraint_of.location_all_variables spv stv skv domain universe#up universe#get_component in
  let full_k = annex_k @ (List.map (fun (_,k) -> ("",k)) main_k) in
  let vs = variables_of_konstraint (Data_constraint.conj (List.map snd main_k)) in
  let f = Data_constraint.Minimize (Data_constraint.sum (List.map Data_constraint.var2expr (Data_constraint.Variable_set.elements vs))) in
  match solver full_k f with
  | None -> None
  | Some(sol,_) -> Some(sol)

let add_bound_min_p p b fu = V_data.bound_add_min (data_v (fu.f_p_map p)) b
let add_bound_max_p p b fu = V_data.bound_add_max (data_v (fu.f_p_map p)) b
let add_bound_min_t t b fu = V_data.bound_add_min (data_v (fu.f_t_map t)) b
let add_bound_max_t t b fu = V_data.bound_add_max (data_v (fu.f_t_map t)) b

let uv_map_of_sol sol = 
  let inner var ((mp, mt, mk) as accu) = match var with
  | Data_constraint.Global_variable(e) -> begin let v = sol.Data_constraint.variable_values var in match e with
    | Data_constraint.Component_type(t) -> (mp, Data_model.Component_type_id_map.add t v mt, mk)
    | Data_constraint.Port(p)           -> (Data_model.Port_map.add p v mp, mt, mk)
    | Data_constraint.Package(k)        -> (mp, mt, Data_model.Package_id_map.add k v mk) end
  | _ -> accu in
  Data_constraint.Variable_set.fold inner sol.Data_constraint.domain (Data_model.Port_map.empty, Data_model.Component_type_id_map.empty, Data_model.Package_id_map.empty)

let add_bound_min_all sol fu = let (mp, mt, _) = uv_map_of_sol sol in
  Data_model.Port_map.iter (fun id v -> add_bound_min_p id (Data_constraint.Value.of_int v) fu) mp;
  Data_model.Component_type_id_map.iter (fun id v -> add_bound_min_t id (Data_constraint.Value.of_int v) fu) mt

(*/************************************************************************\*)
(*| 5. Bounds Function Definition                                          |*)
(*\************************************************************************/*)

let nb_max_location fu = Value.sums (Graph.Vertice_set.fold (fun v res -> let data = data_v v in if V_data.is_port data then res else (V_data.bound_max data)::res) (vertices fu) [])

let trim_categories categories fu =
  let nb = nb_max_location fu in Location_categories.fold (fun s res -> Location_categories.add
     (Data_model.Location_id_set.keep_elements (Value.int_of nb (Data_model.Location_id_set.cardinal s)) s) res) categories Location_categories.empty

let variable_bounds get_component_type get_location categories fu v =
  let bound_of_element e = match e with 
    | Port p           -> V_data.bound (data_v (fu.f_p_map p))
    | Component_type t -> V_data.bound (data_v (fu.f_t_map (get_component_type t)))
    | Package k        -> Bound.big in match v with
  | Simple_variable               _ -> Bound.big
  | Global_variable               e -> bound_of_element e
  | Local_variable           (l, e) -> bound_of_element e
  | Binding_variable        _       -> Bound.big
  | Local_repository_variable _     -> Bound.small
  | Local_resource_variable   (l,o) -> Bound.singleton (Value.of_resource_provide_arity ((get_location l)#provide_resources o))

*)


module rec V_data_init : sig
  type port = { p_data : Data_model.port; mutable p_replica : Bound.t; }
  type component_type = Data_model.component_type_id
  type t_inner = Component_type of component_type | Port of port
  type t = { mutable bounds : Bound.t; mutable conflict : Graph.Vertice_set.t; inner : t_inner}
end = struct
  type port = { p_data : Data_model.port; mutable p_replica : Bound.t; }
  type component_type = Data_model.component_type_id
  type t_inner = Component_type of component_type | Port of port
  type t = { mutable bounds : Bound.t; mutable conflict : Graph.Vertice_set.t; inner : t_inner}
end and E_data : sig
  type t = Value.t
  
  val of_value : Data_constraint.Value.t -> t
  val of_require_arity : Data_model.require_arity -> t
  val of_provide_arity : Data_model.provide_arity -> t
  val value_of : t -> Data_constraint.Value.t
end = struct
  type t = Value.t
  
  let of_value v = v
  let of_require_arity = Value.of_require_arity
  let of_provide_arity = Value.of_provide_arity
  let value_of v = v
end and Graph : Data_common.Graph.S with type vertice_data = V_data_init.t and type edge_data = E_data.t = Data_common.Graph.Make(V_data_init)(E_data)

module V_data : sig
  type t = V_data_init.t
  exception Invalid_operation
  
  val of_port : Data_model.port -> t
  val of_component_type : Data_model.component_type_id -> t

  val bound_combine : t -> Bound.t -> unit
  val bound_add_min : t -> Value.t -> unit
  val bound_add_max : t -> Value.t -> unit

  val bound          : t -> Bound.t
  val bound_min      : t -> Value.t
  val bound_max      : t -> Value.t
  val bound_is_empty : t -> bool

  val conflict_add : t -> Graph.Vertice.t -> unit
  val conflict     : t -> Graph.Vertice_set.t

  val port_replica_add_min : t -> Value.t -> unit
  val port_replica_add_max : t -> Value.t -> unit
  val port_replica_min     : t -> Value.t
  val port_replica_max     : t -> Value.t
  
  val is_port            : t -> bool
  val is_component_type  : t -> bool
  val get_port           : t -> Data_model.port
  val get_component_type : t -> Data_model.component_type_id
end = struct
  type t = V_data_init.t
  exception Invalid_operation
  open V_data_init

  let of_port p = { bounds = default_bound; conflict = Graph.Vertice_set.empty; inner = Port {p_data = p; p_replica = default_bound } }
  let of_component_type t = { bounds = default_bound; conflict = Graph.Vertice_set.empty; inner = Component_type t }
  
  let bound_combine v_data b = v_data.bounds <- Bound.combine v_data.bounds b
  let bound_add_min v_data v = v_data.bounds <- Bound.add_min v_data.bounds v
  let bound_add_max v_data v = v_data.bounds <- Bound.add_max v_data.bounds v

  let bound v_data = v_data.bounds
  let bound_min v_data = Bound.min v_data.bounds
  let bound_max v_data = Bound.max v_data.bounds
  let bound_is_empty v_data = Bound.is_empty v_data.bounds

  let conflict_add v_data v = v_data.conflict <- Graph.Vertice_set.add v v_data.conflict
  let conflict v_data = v_data.conflict
  let port_replica_add_min v_data v = match v_data.inner with | Port d -> d.p_replica <- Bound.add_min d.p_replica v | Component_type _ -> raise Invalid_operation
  let port_replica_add_max v_data v = match v_data.inner with | Port d -> d.p_replica <- Bound.add_max d.p_replica v | Component_type _ -> raise Invalid_operation
  let port_replica_min v_data = match v_data.inner with | Port d -> Bound.min d.p_replica | Component_type _ -> raise Invalid_operation
  let port_replica_max v_data = match v_data.inner with | Port d -> Bound.max d.p_replica | Component_type _ -> raise Invalid_operation
  
  let is_port v_data = match v_data.inner with | Port _ -> true | Component_type _ -> false
  let is_component_type v_data = match v_data.inner with | Port _ -> false | Component_type _ -> true
  let get_port v_data =  match v_data.inner with | Port d -> d.p_data | Component_type _ -> raise Invalid_operation
  let get_component_type v_data =  match v_data.inner with | Port _ -> raise Invalid_operation  | Component_type t -> t
end

type flat_universe = { f_graph : Graph.t; f_p_map : Data_model.port -> Graph.Vertice.t; f_t_map : Data_model.component_type_id -> Graph.Vertice.t }


(*/************************************************************************\*)
(*| 2. Utility Functions                                                   |*)
(*\************************************************************************/*)

let vertices fu = Graph.vertices fu.f_graph
let edges fu = Graph.edges fu.f_graph

let data_v = Graph.Vertice.data
let handle_vertice f_p f_t v = let d = data_v v in if V_data.is_port d then f_p v d else f_t v d
let data_e = Graph.Edge.data





let finalize_bound_roots fu =
  Graph.Vertice_set.iter (fun v -> let data = data_v v in V_data.bound_add_max data (V_data.bound_min data)) (Graph.vertice_roots fu.f_graph)


let create u = 
  let res = Graph.create () in
  let port_map = ref Data_model.Port_map.empty in
  let component_type_map = ref Data_model.Component_type_id_map.empty in
  let find_p  p = Data_model.Port_map.find p !port_map in
  let add_p p v = port_map := Data_model.Port_map.add p v !port_map in
  let find_t  t = Data_model.Component_type_id_map.find t !component_type_map in
  let add_t t v = component_type_map := Data_model.Component_type_id_map.add t#id v !component_type_map in
  (* 1. create vertices *)
  Data_model.Port_set.iter (fun p -> add_p p (Graph.add_vertice (V_data.of_port p) res)) u#get_port_ids;
  Data_model.Component_type_set.iter (fun t -> add_t t (Graph.add_vertice (V_data.of_component_type t#id) res)) u#get_component_types;
  (* 2. create edges and conflicts *)
  let add_conflict p t = let (p', t') = (find_p p, find_t t#id) in
    V_data.conflict_add (data_v p') t';
    V_data.conflict_add (data_v t') p' in
  let edges_of_component_type t =
    Data_model.Port_set.iter (fun p -> Pervasives.ignore (Graph.add_edge (find_t t#id) (E_data.of_require_arity (t#require p)) (find_p p) res)) t#require_domain;
    Data_model.Port_set.iter (fun p -> Pervasives.ignore (Graph.add_edge (find_p p) (E_data.of_provide_arity (t#provide p)) (find_t t#id) res)) t#provide_domain;
    Data_model.Port_set.iter (fun p -> add_conflict p t) t#conflict in
  Data_model.Component_type_set.iter edges_of_component_type u#get_component_types;
  (* 3. Compute loops *)
 (* let ls = Graph.loops res in *)
  (* TODO: compute all informations about loops *)
  (* 4. return the full flat universe *)
  { f_graph = res; f_p_map = find_p; f_t_map = find_t }


(*/************************************************************************\*)
(*| 3. Bounds Propagation Algorithms                                       |*)
(*\************************************************************************/*)

let min_quantity_of_require_edge e = Value.prod (data_e e) (V_data.bound_min (data_v (Graph.Edge.origin e)))
let min_replica_of_require_edge  e = if Value.is_sup (V_data.bound_min (data_v (Graph.Edge.origin e))) (Value.zero) then data_e e else Value.zero

let min_quantity_of_provide_edge e = Value.div (V_data.bound_min (data_v (Graph.Edge.origin e))) (data_e e)
let min_replica_of_provide_edge  e = V_data.port_replica_min (data_v (Graph.Edge.origin e))

let propagate_lower_bound fu =
  let step_v_p p_v p_data = let preds = Graph.Vertice.preds_e p_v in        (* 1. deal with port vertice: p_data = data, preds = component types requiring p *)
    V_data.bound_add_min p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (min_quantity_of_require_edge e)::res) preds [])); (* must provide all requirement *)
    Graph.Edge_set.iter (fun e -> V_data.port_replica_add_min p_data (min_replica_of_require_edge e)) preds in       (* must have as many replica as necessary *)
    
  let step_v_t t_v t_data = let preds = Graph.Vertice.preds_e t_v in  (* 2. deal with component type vertice: t_data = data, preds = ports provided by t *)
    let preds' = Graph.Edge_set.filter (fun e -> Graph.Edge_set.cardinal (Graph.Vertice.succs_e (Graph.Edge.origin e)) < 2) preds in (* get the ports with only one provider *)
    let n = Value.sums (Graph.Edge_set.fold (fun e res -> (min_quantity_of_provide_edge e)::res) preds' []) in           (* must provide everything *)
    V_data.bound_add_min t_data (Graph.Edge_set.fold (fun e res -> Value.max res (min_replica_of_provide_edge e)) preds' n) in (* must have as many replica as required *)
    
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement lower bound computation for loops *)
  Graph.Traverse_topology.iter_downward step_v step_l fu.f_graph


let propagate_conflicts fu =
  let step_v_p (p_v, p_data) = if Value.is_sup (V_data.bound_min p_data) Value.zero then
    Graph.Vertice_set.iter (fun v' -> V_data.bound_add_max (data_v v') (if Graph.Vertice_set.mem v' (Graph.Vertice.succs_v p_v) then Value.one else Value.zero))
      (V_data.conflict p_data) in
  let step_v_t (t_v, t_data) = if Value.is_sup (V_data.bound_min t_data) Value.zero then
    Graph.Vertice_set.iter (fun v' -> 
      let bound = Graph.Edge_set.fold (fun e res -> if Graph.Vertice.equal (Graph.Edge.target e) t_v then data_e e else res) (Graph.Vertice.succs_e v') Value.zero in
      V_data.bound_combine (data_v v') (Bound.singleton bound)) (V_data.conflict t_data) in
  let p_list = ref [] in
  let t_list = ref [] in
  Graph.Vertice_set.iter (handle_vertice (fun p_v p_data -> p_list := (p_v, p_data)::!p_list) (fun t_v t_data -> t_list := (t_v, t_data)::!t_list)) (vertices fu);
   List.iter step_v_t !t_list; List.iter step_v_p !p_list


let max_quantity_of_require_edge e = Value.div (V_data.bound_max (data_v (Graph.Edge.target e))) (data_e e)
let max_replica_of_require_edge  e = if Value.is_sup (data_e e) (V_data.bound_max (data_v (Graph.Edge.target e))) then Value.zero else Value.infty

let max_quantity_of_provide_edge e = Value.prod (V_data.bound_max (data_v (Graph.Edge.target e))) (data_e e)
let max_replica_of_provide_edge  e = V_data.bound_max (data_v (Graph.Edge.target e))

let propagate_upper_bound fu = 
  let step_v_p p_v p_data = let succs = Graph.Vertice.succs_e p_v in
    V_data.bound_add_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (max_quantity_of_provide_edge e)::res) succs []));
    V_data.port_replica_add_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (max_replica_of_provide_edge e)::res) succs [])) in
  let step_v_t t_v t_data = let succs = Graph.Vertice.succs_e t_v in
    Graph.Edge_set.iter (fun e -> V_data.bound_add_max t_data (max_quantity_of_require_edge e)) succs;
    Graph.Edge_set.iter (fun e -> V_data.bound_add_max t_data (max_replica_of_require_edge  e)) succs in
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement upper bound computation for loops *)
  Graph.Traverse_topology.iter_upward step_v step_l fu.f_graph


let minimize_quantity_of_require_edge e = Value.prod (data_e e) (V_data.bound_max (data_v (Graph.Edge.origin e)))
let minimize_quantity_of_provide_edge e = Value.div (V_data.bound_max (data_v (Graph.Edge.origin e))) (data_e e)

let minimize_upper_bound fu =
  let step_v_p p_v p_data = let succs = Graph.Vertice.succs_e p_v in
    V_data.bound_add_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (minimize_quantity_of_require_edge e)::res) succs [])) in
  let step_v_t t_v t_data = let succs = Graph.Vertice.succs_e t_v in
    V_data.bound_add_max t_data (Graph.Edge_set.fold (fun e res -> Value.max res (minimize_quantity_of_provide_edge e)) succs Value.zero) in
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement minimize bound computation for loops *)
  Graph.Traverse_topology.iter_downward step_v step_l fu.f_graph


(*/************************************************************************\*)
(*| 4. Initial Bounds Computation                                          |*)
(*\************************************************************************/*)


let get_initial_mins solver universe spec domain =
  let (spv, stv, skv) = Data_model.uv_of_specification spec in
  let main_k = Constraint_of.specification domain spec in
  let annex_k = Constraint_of.location_all_variables spv stv skv domain universe#up universe#get_component_type in
  let full_k = annex_k @ (List.map (fun (_,k) -> ("",k)) main_k) in
  let vs = variables_of_konstraint (Data_constraint.conj (List.map snd main_k)) in
  let f = Data_constraint.Minimize (Data_constraint.sum (List.map Data_constraint.var2expr (Data_constraint.Variable_set.elements vs))) in
  match solver full_k f with
  | None -> None
  | Some(sol,_) -> Some(sol)

let add_bound_min_p p b fu = V_data.bound_add_min (data_v (fu.f_p_map p)) b
let add_bound_max_p p b fu = V_data.bound_add_max (data_v (fu.f_p_map p)) b
let add_bound_min_t t b fu = V_data.bound_add_min (data_v (fu.f_t_map t)) b
let add_bound_max_t t b fu = V_data.bound_add_max (data_v (fu.f_t_map t)) b

let uv_map_of_sol sol = 
  let inner var ((mp, mt, mk) as accu) = match var with
  | Data_constraint.Global_variable(e) -> begin let v = sol.Data_constraint.variable_values var in match e with
    | Data_constraint.Component_type(t) -> (mp, Data_model.Component_type_id_map.add t v mt, mk)
    | Data_constraint.Port(p)           -> (Data_model.Port_map.add p v mp, mt, mk)
    | Data_constraint.Package(k)        -> (mp, mt, Data_model.Package_id_map.add k v mk) end
  | _ -> accu in
  Data_constraint.Variable_set.fold inner sol.Data_constraint.domain (Data_model.Port_map.empty, Data_model.Component_type_id_map.empty, Data_model.Package_id_map.empty)

let add_bound_min_all sol fu = let (mp, mt, _) = uv_map_of_sol sol in
  Data_model.Port_map.iter (fun id v -> add_bound_min_p id (Data_constraint.Value.of_int v) fu) mp;
  Data_model.Component_type_id_map.iter (fun id v -> add_bound_min_t id (Data_constraint.Value.of_int v) fu) mt

(*/************************************************************************\*)
(*| 5. Bounds Function Definition                                          |*)
(*\************************************************************************/*)

let nb_max_location fu = Value.sums (Graph.Vertice_set.fold (fun v res -> let data = data_v v in if V_data.is_port data then res else (V_data.bound_max data)::res) (vertices fu) [])

let trim_categories categories fu =
  let nb = nb_max_location fu in Location_categories.fold (fun s res -> Location_categories.add
     (Data_model.Location_id_set.keep_elements (Value.int_of nb (Data_model.Location_id_set.cardinal s)) s) res) categories Location_categories.empty

let variable_bounds get_location fu v =
  let bound_of_element e = match e with 
    | Port p           -> V_data.bound (data_v (fu.f_p_map p))
    | Component_type t -> V_data.bound (data_v (fu.f_t_map t))
    | Package k        -> Bound.big in match v with
  | Simple_variable               _ -> Bound.big
  | Global_variable               e -> bound_of_element e
  | Local_variable           (l, e) -> bound_of_element e
  | Binding_variable        _       -> Bound.big
  | Local_repository_variable _     -> Bound.small
  | Local_resource_variable   (l,o) -> Bound.singleton (Value.of_resource_provide_arity ((get_location l)#provide_resources o))
  | Location_used_variable        _ -> Bound.big


