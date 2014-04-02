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


module rec V_data_init : sig
  type port = { p_data : Data_model.port; mutable p_replica : Bound.t; }
  type component_type = Data_model.component_type_id
  type t_inner = Component_type of component_type | Port of port
  type t = { id : int; mutable bounds : Bound.t; mutable conflict : Graph.Vertice_set.t; inner : t_inner}
end = struct
  type port = { p_data : Data_model.port; mutable p_replica : Bound.t; }
  type component_type = Data_model.component_type_id
  type t_inner = Component_type of component_type | Port of port
  type t = { id : int; mutable bounds : Bound.t; mutable conflict : Graph.Vertice_set.t; inner : t_inner}
end and E_data : sig
  type t = Value.t
  
  val of_value : Data_constraint.Value.t -> t
  val of_require_arity : Data_model.require_arity -> t
  val of_provide_arity : Data_model.provide_arity -> t
  val value_of : t -> Data_constraint.Value.t

  val to_string : t -> string
end = struct
  type t = Value.t
  
  let of_value v = v
  let of_require_arity = Value.of_require_arity
  let of_provide_arity = Value.of_provide_arity
  let value_of v = v

  let to_string = String_of.value
end and Graph : Data_common_graph.Graph.S with type vertice_data = V_data_init.t and type edge_data = E_data.t = Data_common_graph.Graph.Make(V_data_init)(E_data)


module V_data : sig
  type t = V_data_init.t
  exception Invalid_operation
  
  val of_port : Data_model.port -> t
  val of_component_type : Data_model.component_type_id -> t

  val bound_set     : t -> Bound.t -> unit
  val bound_combine : t -> Bound.t -> unit
  val bound_set_min : t -> Value.t -> unit
  val bound_add_min : t -> Value.t -> unit
  val bound_set_max : t -> Value.t -> unit
  val bound_add_max : t -> Value.t -> unit
  val bound_ext_max : t -> Value.t -> unit

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
  
(*  val to_string : (Graph.Vertice.t -> int) -> Graph.Vertice.t -> string*)
  val name : t -> string
  val to_string : t -> string * string
end = struct
  include V_data_init
  exception Invalid_operation

  let local_id = Data_common.Fresh_integer.create ()
  let of_port p = { id = Data_common.Fresh_integer.next local_id; bounds = default_bound; conflict = Graph.Vertice_set.empty; inner = Port {p_data = p; p_replica = default_bound } }
  let of_component_type t = { id = Data_common.Fresh_integer.next local_id; bounds = default_bound; conflict = Graph.Vertice_set.empty; inner = Component_type t }
  
  let bound_set v_data b = v_data.bounds <- b
  let bound_combine v_data b = v_data.bounds <- Bound.combine v_data.bounds b
  let bound_set_min v_data v = v_data.bounds <- Bound.create v (Bound.max v_data.bounds)
  let bound_add_min v_data v = v_data.bounds <- Bound.add_min v_data.bounds v
  let bound_set_max v_data v = v_data.bounds <- Bound.create (Bound.min v_data.bounds) v
  let bound_add_max v_data v = v_data.bounds <- Bound.add_max v_data.bounds v
  let bound_ext_max v_data v = v_data.bounds <- Bound.ext_max v_data.bounds v

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

  (*let to_string map v = let v_data = (Graph.Vertice.data v) in "  " ^ (Printf.sprintf "%3d" (map v)) ^ " => " ^ (match v_data.inner with *)
  let name v = "node" ^ (string_of_int v.id)
  
  let to_string v_data = let my_name = name v_data in let n = "  " ^ my_name  ^ (match v_data.inner with
      | Component_type d -> " [shape=box,label=\"" ^ (String_of.component_type_id d) ^ "\\nbounds = " ^ (String_of.bound v_data.bounds) ^ "\"];\n"
      | Port d -> " [label=\"" ^ (String_of.port_id d.p_data) ^ "\\nbounds = " ^ (String_of.bound v_data.bounds) ^ "\\nreplica = " ^ (String_of.bound d.p_replica) ^ "\"];\n") in
    let e = Graph.Vertice_set.fold (fun v res -> res ^ "  " ^ my_name ^ " -> " ^ (name (Graph.Vertice.data v)) ^ " [color=red,style=dotted]\n") v_data.conflict "" in
    (n,e)
end

type flat_universe = { 
  f_graph : Graph.t;
  f_p_map : Data_model.port -> Graph.Vertice.t; f_t_map : Data_model.component_type_id -> Graph.Vertice.t;
  f_p_map_implem : Graph.Vertice.t Data_model.Port_id_map.t; f_t_map_implem : Graph.Vertice.t Data_model.Component_type_id_map.t 
}


(** Principle of the algorithm
 - first, we construct the dependency graph of for the component types of the universe
 - in parallel, we compute the bounds of the component types in the specification
   - currently, the specification language does not help: it is very difficult (impossible?) to simply extract bounds from it
   - I thus use the solver only on the specification, without dependencies, to get the bounds on the variables in the spec
   - minimilizing the values for the variables to get the lower bound
   - maximizing the values for the variables to get the upper bound (by construction, we abstract 10000 to be infty)
 - Second, we set these bounds in the dependency graph
 - Third, we propagate these bounds to the other component types, using the dependency graph
   - the lower bound is propagated following the dependency relation
   - the upper bound for some component types is set using the conflict relation
   - the upper bound is propagated following the provide relation

 - Fourth, perform a propagation that is sound only when we want to minimize the number of components in the resulting sytem:
   - we set the upper bound of the roots of the graph to the lower bound
   - and we propagate following the dependency relation
*)


(*/************************************************************************\*)
(*| 2. Utility Functions                                                   |*)
(*\************************************************************************/*)

let vertices fu = Graph.vertices fu.f_graph
let edges    fu = Graph.edges fu.f_graph

let data_v = Graph.Vertice.data
let handle_vertice f_p f_t v = let d = data_v v in if V_data.is_port d then f_p v d else f_t v d
let data_e = Graph.Edge.data


let create u = 
  let res = Graph.create () in
  
  let port_map = ref Data_model.Port_id_map.empty in
  let find_p (p_id : Data_model.port_id)    : Graph.Vertice.t  = Data_model.Port_id_map.find p_id !port_map in
  let add_p  (p_id : Data_model.port_id) (v : Graph.Vertice.t) = port_map := Data_model.Port_id_map.add p_id v !port_map in
  
  let component_type_map = ref Data_model.Component_type_id_map.empty in
  let find_t (t_id : Data_model.component_type_id)    : Graph.Vertice.t  = Data_model.Component_type_id_map.find t_id !component_type_map in
  let add_t  (t_id : Data_model.component_type_id) (v : Graph.Vertice.t) = component_type_map := Data_model.Component_type_id_map.add t_id v !component_type_map in
  
  (* 1. create vertices *)
  Data_model.Port_id_set.iter (fun (p_id : Data_model.port_id) ->
    add_p p_id (Graph.add_vertice (V_data.of_port p_id) res)
  ) u#get_port_ids;
  Data_model.Component_type_id_set.iter (fun (t_id : Data_model.component_type_id) ->
    add_t t_id (Graph.add_vertice (V_data.of_component_type t_id) res)
  ) u#get_component_type_ids;

  (* 2. create edges and conflicts *)
  let add_conflict (p : Data_model.port_id) (t_id : Data_model.component_type_id) = 
    let (p', t') = (find_p p, find_t t_id) in
    V_data.conflict_add (data_v p') t';
    V_data.conflict_add (data_v t') p' in

  let edges_of_component_type t_id =
    let t = u#get_component_type t_id in
    Data_model.Port_id_set.iter (fun p -> Pervasives.ignore (Graph.add_edge (find_t t_id) (E_data.of_require_arity (t#require p)) (find_p p) res)) t#require_domain;
    Data_model.Port_id_set.iter (fun p -> Pervasives.ignore (Graph.add_edge (find_p p) (E_data.of_provide_arity (t#provide p)) (find_t t_id) res)) t#provide_domain;
    Data_model.Port_id_set.iter (fun p -> add_conflict p t_id) t#conflict in

  Data_model.Component_type_id_set.iter edges_of_component_type u#get_component_type_ids;
  
  (* 3. Compute loops *)
  (* let ls = Graph.loops res in *)
  (* TODO: compute all informations about loops *)
  
  (* 4. return the full flat universe *)
  { f_graph = res; f_p_map = find_p; f_t_map = find_t; f_p_map_implem = !port_map; f_t_map_implem = !component_type_map }


(*/************************************************************************\*)
(*| 3. Bounds Propagation Algorithms                                       |*)
(*\************************************************************************/*)

  (* 3.1. Lower bound Propagation *)
let min_quantity_of_require_edge e = Value.prod (data_e e) (V_data.bound_min (data_v (Graph.Edge.origin e)))
let min_replica_of_require_edge  e = if Value.is_sup (V_data.bound_min (data_v (Graph.Edge.origin e))) (Value.zero) then data_e e else Value.zero

let min_quantity_of_provide_edge e = Value.div (V_data.bound_min (data_v (Graph.Edge.origin e))) (data_e e)
let min_replica_of_provide_edge  e = V_data.port_replica_min (data_v (Graph.Edge.origin e))

let propagate_lower_bound fu =
  let step_v_p p_v p_data = (* 1. The min of a port is how much it is required from mins of types *) 
    let preds = Graph.Vertice.preds_e p_v in        (* p_data = data, preds = component types requiring p *)
    V_data.bound_add_min p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (min_quantity_of_require_edge e)::res) preds [])); (* must provide all requirement *)
    Graph.Edge_set.iter (fun e -> V_data.port_replica_add_min p_data (min_replica_of_require_edge e)) preds in       (* must have as many replica as necessary *)
    
  let step_v_t t_v t_data = (* 2. The min of a type is how much it must provide for each port, plus how many replica *)
    let preds = Graph.Vertice.preds_e t_v in  (* 2. deal with component type vertice: t_data = data, preds = ports provided by t *)
    let preds' = Graph.Edge_set.filter (fun e -> Graph.Edge_set.cardinal (Graph.Vertice.succs_e (Graph.Edge.origin e)) < 2) preds in (* get the ports that only this type provides *)
    let provide = Graph.Edge_set.fold (fun e res -> (min_quantity_of_provide_edge e)::res) preds' [] in           (* must provide all required ports *)
    let all = Graph.Edge_set.fold (fun e res -> (min_replica_of_provide_edge e)::res) preds' provide in           (* must have as many replica as required *)
    V_data.bound_add_min t_data (Value.maxs all) in 
    
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement lower bound computation for loops *)
  Graph.Traverse_topology.iter_downward step_v step_l fu.f_graph


  (* 3.2. Conflicts Propagation *)
let propagate_conflicts fu =
  let step_v_p (p_v, p_data) = (* if the port must be present, then all the component types in conflict must be equal to 0, except for the providers, which must be equal to 1 *)
    if Value.is_sup (V_data.bound_min p_data) Value.zero then
      Graph.Vertice_set.iter (fun v' -> V_data.bound_add_max (data_v v') (if Graph.Vertice_set.mem v' (Graph.Vertice.succs_v p_v) then Value.one else Value.zero))
        (V_data.conflict p_data) in
  let step_v_t (t_v, t_data) = (* if the component type must be present, then all the ports in conflict must be equal to 0, except for the provided *)
   if Value.is_sup (V_data.bound_min t_data) Value.zero then
    Graph.Vertice_set.iter (fun v' -> 
      let bound = Graph.Edge_set.fold (fun e res -> if Graph.Vertice.equal (Graph.Edge.target e) t_v then data_e e else res) (Graph.Vertice.succs_e v') Value.zero in
      V_data.bound_combine (data_v v') (Bound.singleton bound)) (V_data.conflict t_data) in
  let step_v_t_self (t_v, t_data) = (* if self conflict, then the upper bound is one *)
    if Graph.Vertice_set.mem t_v (Graph.Vertice_set.fold (fun p res -> Graph.Vertice_set.union (Graph.Vertice.succs_v p ) res) (V_data.conflict t_data) Graph.Vertice_set.empty) then
      V_data.bound_add_max t_data Value.one in
  let p_list = ref [] in
  let t_list = ref [] in
  Graph.Vertice_set.iter (handle_vertice (fun p_v p_data -> p_list := (p_v, p_data)::!p_list) (fun t_v t_data -> t_list := (t_v, t_data)::!t_list)) (vertices fu);
   List.iter step_v_t !t_list; List.iter step_v_t_self !t_list; List.iter step_v_p !p_list


  (* 3.3. Upper bound Propagation *)
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


  (* 3.4. Set the upper bounds of the roots of the graph to their lower bound *)
let finalize_bound_roots fu =
  Graph.Vertice_set.iter (fun v -> let data = data_v v in V_data.bound_ext_max data (V_data.bound_min data)) (Graph.vertice_roots fu.f_graph)

let mirror_bounds fu = 
  Graph.Vertice_set.iter (fun v -> let data = data_v v in V_data.bound_ext_max data (V_data.bound_min data)) (Graph.vertices fu.f_graph)

  (* 3.5. Finilizing: minimizing upper bound *)
let minimize_quantity_of_require_edge e = Value.prod (data_e e) (V_data.bound_max (data_v (Graph.Edge.origin e)))
let minimize_quantity_of_provide_edge e = Value.div (V_data.bound_max (data_v (Graph.Edge.origin e))) (data_e e)
let set_quantity_of_port e = Value.prod (data_e e) (V_data.bound_max (data_v (Graph.Edge.target e)))

let minimize_upper_bound fu =
  let step_v_p p_v p_data = (* 1. The max of a port is how much it is provided *)
    let preds = Graph.Vertice.preds_e p_v in if not (Graph.Edge_set.is_empty preds) then
    V_data.bound_ext_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (minimize_quantity_of_require_edge e)::res) preds [])) in
    
  let step_v_t t_v t_data = (* 2. The max of a type is the max of how much it could provide *)
    let preds = Graph.Vertice.preds_e t_v in if not (Graph.Edge_set.is_empty preds) then
    V_data.bound_ext_max t_data (Value.maxs (Graph.Edge_set.fold (fun e res -> (minimize_quantity_of_provide_edge e)::res) preds [])) in
  let step_v = handle_vertice step_v_p step_v_t in
  let step_l l = () in (* TODO: implement minimize bound computation for loops *)
  Graph.Traverse_topology.iter_downward step_v step_l fu.f_graph;
  let step_v_p_reset_port p_v p_data = V_data.bound_set_max p_data (Value.sums (Graph.Edge_set.fold (fun e res -> (set_quantity_of_port e)::res) (Graph.Vertice.succs_e p_v) [])) in
  let step_v_t_reset_port _ _ = () in Graph.Vertice_set.iter (handle_vertice step_v_p_reset_port step_v_t_reset_port) (vertices fu)
  


(*/************************************************************************\*)
(*| 4. Initial Bounds Computation                                          |*)
(*\************************************************************************/*)

(* solve the specification `alone', to see if we have some lower bounds *)
let get_initial_mins (solver : Solvers.t) (bounds : Data_constraint.variable_bounds) (universe : Data_model.universe) (specification : Data_model.specification) (domain : Data_model.Location_id_set.t) : Data_constraint.solution option =
  let (spv, stv, skv) = Data_model.uv_of_specification specification in (* get the port, component type and package variables in the specification *)

  let with_packages = if Settings.find Settings.eliminate_packages then false else true in
  let specification_constraints : Data_state.structured_constraints = Constraint_of.specification          ~with_packages domain specification in      (* generate the constraint corresponding to the specification *)
  let annex_constraints         : Data_state.structured_constraints = Constraint_of.location_all_variables ~with_packages spv stv skv domain universe#get_providers universe#get_component_type in
  
  let all_constraints = annex_constraints @ specification_constraints in
  let vs = Data_state.variables_of_structured_constraints specification_constraints in (* Remark (Kuba) : Why we don't take into account the variables from annex_constraints? *)
  let optimization_function = Multi_objective.Optimize( Multi_objective.Single ( Single_objective.Minimize (Data_constraint.sum (List.map Data_constraint.var2expr (Data_constraint.Variable_set.elements vs))))) in

  match solver bounds all_constraints optimization_function with
  | None        -> None
  | Some(sol,_) -> Some(sol)

(* add some lower bounds for component type and port ids *)
let add_bound_min_p p b fu = V_data.bound_add_min (data_v (fu.f_p_map p)) b
let add_bound_max_p p b fu = V_data.bound_add_max (data_v (fu.f_p_map p)) b
let add_bound_min_t t b fu = V_data.bound_add_min (data_v (fu.f_t_map t)) b
let add_bound_max_t t b fu = V_data.bound_add_max (data_v (fu.f_t_map t)) b

(* extract from a Data_constraint.solution two mappings, one for ports, one for component type ids, to their values  *)
let core_solution sol = 
  let inner var ((mp, mt) as accu) = match var with
  | Data_constraint.Global_variable(e) -> begin let v = sol.Data_constraint.variable_values var in match e with
    | Data_constraint.Component_type(t) -> (mp, Data_model.Component_type_id_map.add t v mt)
    | Data_constraint.Port(p)           -> (Data_model.Port_id_map.add p v mp, mt)
    | Data_constraint.Package _         -> (mp, mt) end
  | _ -> accu in
  Data_constraint.Variable_set.fold inner sol.Data_constraint.domain (Data_model.Port_id_map.empty, Data_model.Component_type_id_map.empty)

let add_bound_min_all sol fu = let (mp, mt) = core_solution sol in
  Data_model.Port_id_map.iter (fun id v -> add_bound_min_p id (Data_constraint.Value.of_int v) fu) mp;
  Data_model.Component_type_id_map.iter (fun id v -> add_bound_min_t id (Data_constraint.Value.of_int v) fu) mt

(*/************************************************************************\*)
(*| 5. Bounds Function Definition                                          |*)
(*\************************************************************************/*)

let nb_max_location fu = Value.sums (Graph.Vertice_set.fold (fun v res -> let data = data_v v in if V_data.is_port data then res else (V_data.bound_max data)::res) (vertices fu) [])

let trim_categories categories fu =
  let nb = nb_max_location fu in Location_categories.fold (fun s res -> Location_categories.add
     (Data_model.Location_id_set.keep_elements (Value.int_of nb (Data_model.Location_id_set.cardinal s)) s) res) categories Location_categories.empty

let variable_bounds get_location fu v =
  let init_p = Data_model.Port_id_map.empty in
  let init_t = Data_model.Component_type_id_map.singleton Data_model.deprecated_component_type_id Bound.null in
  let inner_p p p_v (l_p, g_p) = let b = V_data.bound (Graph.Vertice.data p_v) in
    (Data_model.Port_id_map.add p (Bound.create Value.zero (Bound.max b)) l_p, Data_model.Port_id_map.add p b g_p) in 
  let inner_t t t_v (l_t, g_t) = let b = V_data.bound (Graph.Vertice.data t_v) in
    (Data_model.Component_type_id_map.add t (Bound.create Value.zero (Bound.max b)) l_t, Data_model.Component_type_id_map.add t b g_t) in 
  let (local_p, global_p) = Data_model.Port_id_map.fold inner_p fu.f_p_map_implem (init_p, init_p) in
  let (local_t, global_t) = Data_model.Component_type_id_map.fold inner_t fu.f_t_map_implem (init_t, init_t) in

  let bound_of_global_element e = match e with 
    | Port p           -> Data_model.Port_id_map.find p global_p
    | Component_type t -> Data_model.Component_type_id_map.find t global_t
    | Package k        -> if (Data_model.Package_id.compare k Data_model.deprecated_package_id) = 0 then Bound.null else Bound.big in
  let bound_of_local_element e = match e with 
    | Port p           -> Data_model.Port_id_map.find p local_p
    | Component_type t -> Data_model.Component_type_id_map.find t local_t
    | Package k        -> if (Data_model.Package_id.compare k Data_model.deprecated_package_id) = 0 then Bound.null else Bound.small in match v with
  | Simple_variable               _ -> Bound.big
  | Global_variable               e -> bound_of_global_element e
  | Local_variable           (l, e) -> bound_of_local_element e
  | Binding_variable        _       -> Bound.big
  | Local_repository_variable _     -> Bound.small
  | Local_resource_variable   (l,o) -> Bound.singleton (Value.of_resource_provide_arity ((get_location l)#provide_resources o))
  | Location_used_variable        _ -> Bound.small


(*
  let bound_of_global_element e = match e with 
    | Port p           -> (try V_data.bound (data_v (fu.f_p_map p)) with Not_found -> failwith "engine/preprocess/variable_bounds.ml #558")
    | Component_type t -> if (Data_model.Component_type_id.compare t Data_model.deprecated_component_type_id) = 0 then Bound.null else (try V_data.bound (data_v (fu.f_t_map t)) with Not_found -> failwith "engine/preprocess/variable_bounds.ml #559")
    | Package k        -> if (Data_model.Package_id.compare k Data_model.deprecated_package_id) = 0 then Bound.null else Bound.big in
  let bound_of_local_element e = match e with 
    | Port p           -> (try V_data.bound (data_v (fu.f_p_map p)) with Not_found -> failwith "engine/preprocess/variable_bounds.ml #561")
    | Component_type t -> if (Data_model.Component_type_id.compare t Data_model.deprecated_component_type_id) = 0 then Bound.null else (try V_data.bound (data_v (fu.f_t_map t)) with Not_found -> failwith "engine/preprocess/variable_bounds.ml #562")
    | Package k        -> if (Data_model.Package_id.compare k Data_model.deprecated_package_id) = 0 then Bound.null else Bound.small in match v with
  | Simple_variable               _ -> Bound.big
  | Global_variable               e -> bound_of_global_element e
  | Local_variable           (l, e) -> bound_of_local_element e
  | Binding_variable        _       -> Bound.big
  | Local_repository_variable _     -> Bound.small
  | Local_resource_variable   (l,o) -> Bound.singleton (Value.of_resource_provide_arity ((get_location l)#provide_resources o))
  | Location_used_variable        _ -> Bound.small
*)




(*/************************************************************************\*)
(*| 6. Printing Functions (for debugging...)                               |*)
(*\************************************************************************/*)


let v_name v = V_data.name (Graph.Vertice.data v)

let to_string fu = (* print the nodes of the graph *)
  let (ns,es) = Graph.Vertice_set.fold (fun v (ns,es) -> let (n,e) = V_data.to_string (Graph.Vertice.data v) in (ns ^ n, es ^ e)) (vertices fu) ("","") in
  "digraph flat_universe {\n/* Vertices */\n" ^ ns ^ "\n/* Edges */\n"
    ^ (Graph.Edge_set.fold (fun e res -> "  " ^ (v_name (Graph.Edge.origin e)) ^ " -> "
        ^ (v_name (Graph.Edge.target e)) ^ " [label=\"" ^ (E_data.to_string (Graph.Edge.data e)) ^ "\"];\n" ^ res) (edges fu) "")
    ^ "\n/* Conflicts */\n" ^ es
    ^ "}"

let to_string_full = to_string
    
(*
  " flat universe:\n vertices =\n" ^ (String.concat "\n" (List.map (fun v -> V_data.to_string (find v) find (Graph.Vertice.data v)) (Graph.Vertice_map.keys !map)))
(*  ^ "\n edges =\n" ^ (String.concat "\n" (List.map (fun e -> (find (Graph.Edge.origin e)) ^ " -> " ^ (String_of.value (E_data.value_of (Graph.Edge.data e)))
            ^ " -> " ^ (find (Graph.Edge.target e))) (Graph.Edge_set.elements (edges fu))))
  ^ "\n Successors =\n" ^ (String.concat " ; " (List.map (fun v -> "  " ^ (find v) ^ " => "
            ^ String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.Vertice.succs_v v)))) (Graph.Vertice_map.keys !map)))
  ^ "\n Predecessors =\n" ^ (String.concat " ; " (List.map (fun v -> "  " ^ (find v) ^ " => "
            ^ String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.Vertice.preds_v v)))) (Graph.Vertice_map.keys !map)))
  ^ "\n Roots =\n" ^ (String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.vertice_roots (fu.f_graph)))))
  ^ "\n Leafs =\n" ^ (String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.vertice_leafs (fu.f_graph)))))
  *)


let to_string_full fu = (* print the nodes and the edges of the graph *)
  let r_id =  ref 0 in let map = ref Graph.Vertice_map.empty in
  Graph.Vertice_set.iter (fun v -> let id = !r_id in r_id := id + 1; map := Graph.Vertice_map.add v (Printf.sprintf "%3d" id) !map) (vertices fu);
  let find v = Graph.Vertice_map.find v !map in
  " flat universe:\n vertices =\n" ^ (String.concat "\n" (List.map (fun v -> V_data.to_string (find v) find (Graph.Vertice.data v)) (Graph.Vertice_map.keys !map)))
  ^ "\n edges =\n" ^ (String.concat "\n" (List.map (fun e -> (find (Graph.Edge.origin e)) ^ " -> " ^ (String_of.value (E_data.value_of (Graph.Edge.data e)))
            ^ " -> " ^ (find (Graph.Edge.target e))) (Graph.Edge_set.elements (edges fu))))
(*  ^ "\n Successors =\n" ^ (String.concat " ; " (List.map (fun v -> "  " ^ (find v) ^ " => "
            ^ String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.Vertice.succs_v v)))) (Graph.Vertice_map.keys !map)))
  ^ "\n Predecessors =\n" ^ (String.concat " ; " (List.map (fun v -> "  " ^ (find v) ^ " => "
            ^ String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.Vertice.preds_v v)))) (Graph.Vertice_map.keys !map)))
  ^ "\n Roots =\n" ^ (String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.vertice_roots (fu.f_graph)))))
  ^ "\n Leafs =\n" ^ (String_of.string_list (List.map find (Graph.Vertice_set.elements (Graph.vertice_leafs (fu.f_graph)))))
  *)

*)

