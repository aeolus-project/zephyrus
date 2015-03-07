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
    - datatypes/Data_constraint
    - datatypes/Data_model
    - datatypes/Functions_model
*)

open Data_constraint

open Constraint_generators

let location_all_variables ?(with_packages = true) port_ids component_type_ids package_ids location_ids get_providers get_component_type = 

  let get_component_type_provide_arity component_type_id port_id = 
    let component_type = get_component_type component_type_id in
    if Data_model.Port_id_set.mem port_id component_type#provide_domain
    then component_type#provide port_id
    else Data_model.Finite_provide 0 in

  let location_ports           = ("location ports"          , (location_port           ~port_ids           ~location_ids)) in
  let location_component_types = ("location component types", (location_component_type ~component_type_ids ~location_ids)) in
  let location_packages        = ("location packages"       , (location_package        ~package_ids        ~location_ids)) in
  let definition_ports         = ("definition ports"        , (location_port_equation  ~port_ids           ~location_ids ~get_providers ~get_component_type_provide_arity)) in

  if with_packages
  then [location_ports; location_component_types; location_packages; definition_ports]
  else [location_ports; location_component_types;                    definition_ports]


let universe ?(with_packages = true) (location_ids : Data_model.Location_id_set.t) (universe : Data_model.universe) configuration =

  (* Prepare the data to be used by the simple constraint generators. *)
  let port_ids           = universe#get_port_ids in
  let component_type_ids = universe#get_component_type_ids in
  let repository_ids     = universe#get_repository_ids in
  let package_ids        = universe#get_package_ids in
  let resource_ids       = universe#get_resource_ids in
  
  let get_subports       = universe#get_sub_ports in
  let get_supports       = universe#get_sup_ports in
  let get_requirers      = universe#get_requirers in
  let get_providers      = universe#get_providers in
  let get_conflicters    = universe#get_conflicters in
  let get_provide_domain component_type_id = (universe#get_component_type component_type_id)#provide_domain in

  let get_component_type_require_arity component_type_id = (universe#get_component_type component_type_id)#require in
  let get_component_type_provide_arity component_type_id port_id = 
    let component_type = universe#get_component_type component_type_id in
    if Data_model.Port_id_set.mem port_id component_type#provide_domain
    then component_type#provide port_id
    else Data_model.Finite_provide 0 in
  let get_component_type_consume component_type_id = (universe#get_component_type component_type_id)#consume in

  let get_package_depend   package_id = (universe#get_package package_id)#depend in
  let get_package_conflict package_id = (universe#get_package package_id)#conflict in
  let get_package_consume  package_id = (universe#get_package package_id)#consume in

  let get_repository_packages = (fun r -> (universe#get_repository r)#package_ids) in
  let get_implementation      = universe#get_implementation in
  
  let get_location = configuration#get_location in
  let get_location_provide_resources location_id = (get_location location_id)#provide_resources in

  (* let incompatibilities = Incompatibilities_of.universe universe in *)
  
  if with_packages
  then 
  {
    Data_state.constraint_universe_component_type_require        = require ~port_ids ~get_supports ~get_requirers   ~get_providers ~get_component_type_require_arity;
    Data_state.constraint_universe_component_type_provide        = provide ~port_ids ~get_subports ~get_providers   ~get_requirers ~get_component_type_provide_arity;
    Data_state.constraint_universe_component_type_conflict       = conflict                       ~port_ids ~get_conflicters ~get_providers;
    Data_state.constraint_universe_binding_unicity               = binding                        ~port_ids ~get_supports ~get_requirers   ~get_providers ~get_provide_domain;
    Data_state.constraint_universe_location_component_type       = location_component_type        ~component_type_ids ~location_ids;
    Data_state.constraint_universe_location_package              = location_package               ~package_ids        ~location_ids;
    Data_state.constraint_universe_location_port                 = location_port                  ~port_ids           ~location_ids;
    Data_state.constraint_universe_definition_port               = location_port_equation         ~port_ids           ~location_ids ~get_providers ~get_component_type_provide_arity;
    Data_state.constraint_universe_component_type_implementation = component_type_implementation  ~location_ids ~component_type_ids ~get_implementation;
    Data_state.constraint_universe_repository_unicity            = repository_unique              ~location_ids ~repository_ids;
    Data_state.constraint_universe_repository_package            = repository_package             ~location_ids ~repository_ids ~package_ids ~get_repository_packages;
    Data_state.constraint_universe_package_dependency            = package_dependency             ~location_ids ~package_ids    ~get_package_depend;
    Data_state.constraint_universe_package_conflict              = package_conflict               ~location_ids ~package_ids    ~get_package_conflict;
    Data_state.constraint_universe_resource_consumption          = resource_consumption                     ~with_packages ~location_ids ~resource_ids ~component_type_ids ~package_ids ~get_component_type_consume ~get_package_consume ~get_location_provide_resources;
    Data_state.constraint_universe_deprecated_element            = deprecated_component_types_with_packages ~with_packages ~location_ids;
    Data_state.constraint_universe_used_locations                = used_locations                           ~with_packages ~component_type_ids ~package_ids ~location_ids;
    Data_state.constraint_universe_incompatibilities             = []; (* TODO: Add or not? *)
  }
  else 
  {
    (* Written using a well known programming paradigm invented by Mr. Copy and Dr. Paste. *)
    Data_state.constraint_universe_component_type_require        = require ~port_ids ~get_supports ~get_requirers   ~get_providers ~get_component_type_require_arity;
    Data_state.constraint_universe_component_type_provide        = provide ~port_ids ~get_subports ~get_providers   ~get_requirers ~get_component_type_provide_arity;
    Data_state.constraint_universe_component_type_conflict       = conflict                       ~port_ids ~get_conflicters ~get_providers;
    Data_state.constraint_universe_binding_unicity               = binding                        ~port_ids ~get_supports ~get_requirers ~get_providers ~get_provide_domain;
    Data_state.constraint_universe_location_component_type       = location_component_type        ~component_type_ids ~location_ids;
    Data_state.constraint_universe_location_package              = [];
    Data_state.constraint_universe_location_port                 = location_port                  ~port_ids           ~location_ids;
    Data_state.constraint_universe_definition_port               = location_port_equation         ~port_ids           ~location_ids ~get_providers ~get_component_type_provide_arity;
    Data_state.constraint_universe_component_type_implementation = [];
    Data_state.constraint_universe_repository_unicity            = repository_unique              ~location_ids ~repository_ids;
    Data_state.constraint_universe_repository_package            = [];
    Data_state.constraint_universe_package_dependency            = [];
    Data_state.constraint_universe_package_conflict              = [];
    Data_state.constraint_universe_resource_consumption          = resource_consumption                     ~with_packages ~location_ids ~resource_ids ~component_type_ids ~package_ids:Data_model.Package_id_set.empty ~get_component_type_consume ~get_package_consume ~get_location_provide_resources;
    Data_state.constraint_universe_deprecated_element            = deprecated_component_types_with_packages ~with_packages ~location_ids;
    Data_state.constraint_universe_used_locations                = used_locations                           ~with_packages ~component_type_ids ~package_ids:Data_model.Package_id_set.empty ~location_ids;
    Data_state.constraint_universe_incompatibilities             = direct_incompatibilities                 ~incompatibilities:(Incompatibilities_of.universe universe) ~location_ids;
  }
 

let universe_full ?(with_packages = true) (universe' : Data_model.universe option) (initial_configuration : Data_model.configuration option) =
  let make_constraint_universe u c = universe ~with_packages c#get_location_ids u c in
  match (universe', initial_configuration) with
  | (Some universe', Some configuration) -> make_constraint_universe universe' configuration
  | _                                    -> Data_state.empty_constraint_universe

(*******************************************)
(** 3. Specification Translation           *)
(*******************************************)

let spec_variable_name v = Variable(Simple_variable(v))
let spec_const = constant

let spec_local_element location_id e = match e with
  | Data_model.Spec_local_element_package        (package_id)        -> eNlk location_id package_id
  | Data_model.Spec_local_element_component_type (component_type_id) -> eNlt location_id component_type_id
  | Data_model.Spec_local_element_port           (port_id)           -> eNlp location_id port_id

let rec spec_local_expr location_id e = match e with
  | Data_model.Spec_local_expr_var   (v)      -> spec_variable_name v
  | Data_model.Spec_local_expr_const (c)      -> spec_const c
  | Data_model.Spec_local_expr_arity (e)      -> spec_local_element location_id e
  | Data_model.Spec_local_expr_add   (e1, e2) -> (spec_local_expr location_id e1) +~ (spec_local_expr location_id e2)
  | Data_model.Spec_local_expr_sub   (e1, e2) -> (spec_local_expr location_id e1) -~ (spec_local_expr location_id e2)
  | Data_model.Spec_local_expr_mul   (e1, e2) -> (spec_const e1) *~ (spec_local_expr location_id e2)

let spec_op op = match op with
  | Data_model.Lt  -> ( <~  )
  | Data_model.LEq -> ( <=~ )
  | Data_model.Eq  -> ( =~  )
  | Data_model.GEq -> ( >=~ )
  | Data_model.Gt  -> ( >~  )
  | Data_model.NEq -> ( <>~ )

let rec local_specification location_id s = match s with
  | Data_model.Spec_local_true              -> True
  | Data_model.Spec_local_op   (e1, op, e2) -> (spec_op op) (spec_local_expr location_id e1) (spec_local_expr location_id e2)
  | Data_model.Spec_local_and  (s1, s2)     -> (local_specification location_id s1) &&~~ (local_specification location_id s2)
  | Data_model.Spec_local_or   (s1, s2)     -> (local_specification location_id s1) ||~~ (local_specification location_id s2)
  | Data_model.Spec_local_impl (s1, s2)     -> (local_specification location_id s1) =>~~ (local_specification location_id s2)
  | Data_model.Spec_local_not  (s')         -> !~ (local_specification location_id s')

let spec_resource_constraint location_id co = List.fold_left (fun res (resource_id, op, i) -> ((spec_op op) (eO location_id resource_id) (constant i))::res) [] co
let spec_repository_constraint location_id cr = match cr with 
                                      | [] -> true_konstraint
                                      | _  -> (sum (List.map (fun repository_id -> eR location_id repository_id) cr)) =~ (constant 1)

let spec_element location_ids e = match e with
  | Data_model.Spec_element_package        (package_id)        -> eNk package_id
  | Data_model.Spec_element_component_type (component_type_id) -> eNt component_type_id
  | Data_model.Spec_element_port           (port_id)           -> eNp port_id
  | Data_model.Spec_element_location (co, cr, ls) -> sum (Data_model.Location_id_set.fold
     (fun location_id res -> (reify (conj((local_specification location_id ls)::(spec_repository_constraint location_id cr)::(spec_resource_constraint location_id co))))::res ) location_ids [])

let rec spec_expr location_ids e = match e with
  | Data_model.Spec_expr_var   (v)      -> spec_variable_name v
  | Data_model.Spec_expr_const (c)      -> spec_const c
  | Data_model.Spec_expr_arity (e)      -> spec_element location_ids e
  | Data_model.Spec_expr_add   (e1, e2) -> (spec_expr location_ids e1) +~ (spec_expr location_ids e2)
  | Data_model.Spec_expr_sub   (e1, e2) -> (spec_expr location_ids e1) -~ (spec_expr location_ids e2)
  | Data_model.Spec_expr_mul   (e1, e2) -> (spec_const e1)             *~ (spec_expr location_ids e2)

let rec specification_simple location_ids s = match s with
  | Data_model.Spec_true                     -> True
  | Data_model.Spec_op   (e1, op, e2)        -> (spec_op op) (spec_expr location_ids e1) (spec_expr location_ids e2)
  | Data_model.Spec_and  (s1, s2)            -> (specification_simple location_ids s1) &&~~ (specification_simple location_ids s2)
  | Data_model.Spec_or   (s1, s2)            -> (specification_simple location_ids s1) ||~~ (specification_simple location_ids s2)
  | Data_model.Spec_impl (s1, s2)            -> (specification_simple location_ids s1) =>~~ (specification_simple location_ids s2)
  | Data_model.Spec_not  (s')                -> !~ (specification_simple location_ids s')
  | Data_model.Spec_everywhere          (ls) -> conj (List.map (fun location_id -> local_specification location_id ls) (Data_model.Location_id_set.elements location_ids))
  | Data_model.Spec_at (at_location_ids, ls) -> conj (List.map (fun location_id -> local_specification location_id ls) at_location_ids)


let specification ?(with_packages = true) locations s = 
  Zephyrus_log.log_constraint_execution "Compute Specification\n"; 
  [ "Data_state.constraint_specification_full" , [specification_simple locations s] ]


let specification_full ?(with_packages = true) (specification : Data_model.specification option) (initial_configuration : Data_model.configuration option) : konstraint option = 
  match (specification, initial_configuration) with
  | (Some specification, Some configuration) -> Some(specification_simple configuration#get_location_ids specification)
  | _                                        -> None


(*******************************************)
(** 4. Configuration Translation           *) (* using naming conventions from the paper *)
(*******************************************)


let locations ?(with_packages = true) resource_ids location_ids get_location =
  Zephyrus_log.log_constraint_execution "Compute Resources Provided by Locations\n";
  if Settings.find Settings.modifiable_configuration 
  then [] (* if we can modify the resources of a location, we do not enforce its value in the constraint *)
  else
    [ "Data_state.constraint_configuration_full",
    Data_model.Location_id_set.fold (fun location_id res ->
      let location = get_location location_id in
      Data_model.Resource_id_set.fold (fun o res ->
        ((eO location_id o) =~ (constant (location#provide_resources o)))::res
      ) resource_ids res
    ) location_ids [] ]

(* WTF? Why do we have a second almost exact copy of the same function??? *)
let configuration ?(with_packages = true) resource_ids location_ids get_location = 
  if Settings.find Settings.modifiable_configuration 
  then [] (* if we can modify the resources of a location, we do not enforce its value in the constraint *)
  else
    Data_model.Location_id_set.fold (fun location_id res ->
      let location = get_location location_id in
      Data_model.Resource_id_set.fold (fun o res ->
        ((eO location_id o) =~ (constant (location#provide_resources o)))::res
      ) resource_ids res
    ) location_ids []

let configuration_full ?(with_packages = true) (universe : Data_model.universe option) (initial_configuration : Data_model.configuration option) : konstraint list = 
  match (universe, initial_configuration) with
  | (Some universe, Some initial_configuration) -> configuration universe#get_resource_ids initial_configuration#get_location_ids initial_configuration#get_location
  | _                                           -> []


(*******************************************)
(** 5. Optimisation function Translation   *)
(*******************************************)

(* preliminary definitions for costs *)
let cost_all_components component_type_ids = Data_model.Component_type_id_set.fold (fun t res -> (eNt t)::res) component_type_ids []
let cost_all_packages   package_ids        = Data_model.Package_id_set.fold        (fun k res -> (eNk k)::res) package_ids        []

let cost_local_components location_id component_type_ids = (Data_model.Component_type_id_set.fold (fun t res -> (eNlt location_id t)::res) component_type_ids [])
let cost_local_packages   location_id package_ids        = (Data_model.Package_id_set.fold        (fun k res -> (eNlk location_id k)::res) package_ids        [])

type used_or_free_location = Used_locations | Free_locations

let cost_locations switch location_ids component_type_ids package_ids count_components_only =
  Data_model.Location_id_set.fold (fun location_id res ->
    (reify ((match switch with Used_locations -> (>~) | Free_locations -> (=~) ) (sum (if count_components_only then cost_local_components location_id component_type_ids
              else List.rev_append (cost_local_components location_id component_type_ids) (cost_local_packages location_id package_ids))) (constant 0)))::res
  ) location_ids []

let cost_used_locations = cost_locations Used_locations
let cost_free_locations = cost_locations Free_locations

let cost_difference_components location_ids component_type_ids get_local_component : expression list = 
  Data_model.Location_id_set.fold (fun location_id res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (abs((eNlt location_id t) -~ (constant (Data_model.Component_id_set.cardinal (get_local_component location_id t))) ))::res
    ) component_type_ids res) location_ids []

let cost_difference_packages location_ids package_ids get_local_package : expression list = 
  Data_model.Location_id_set.fold (fun location_id res ->
    Data_model.Package_id_set.fold (fun k res ->
      (if get_local_package location_id k then abs((eNlk location_id k) -~ (constant 1)) else (eNlk location_id k))::res
    ) package_ids res) location_ids []

let cost_locations location_ids (get_location_cost : Data_model.location_id -> Data_model.location_cost) : expression list =
  Data_model.Location_id_set.fold (fun location_id res ->
    ( (eU location_id) *~ constant (get_location_cost location_id) ) :: res
  ) location_ids []

(* translation functions *)
let simple ?(with_packages = true) ~component_type_ids =
  Multi_objective.solve_goal_of_single_solve_goal
    (Single_objective.Optimize (Single_objective.Minimize (sum (cost_all_components component_type_ids)) )) (* Minimize the number of components. *)

let compact_slow ?(with_packages = true) ~location_ids ~component_type_ids ~package_ids ~get_location_cost =
  let minimize_used_location_cost = Single_objective.Minimize (sum (cost_locations location_ids get_location_cost)) in (* First minimize the number of used locations, *) 
  let minimize_component_count    = Single_objective.Minimize (sum (cost_all_components component_type_ids))        in (* then minimize the number of components, *)
  let minimize_package_count      = Single_objective.Minimize (sum (cost_all_packages   package_ids))               in (* finally minimize the number of packages (so we do not have useless packages). *)
  let single_optimizations =
    if with_packages
    then [minimize_used_location_cost; minimize_component_count; minimize_package_count]
    else [minimize_used_location_cost; minimize_component_count] in
  Multi_objective.solve_goal_of_list_of_single_optimizations single_optimizations

let compact_fast ?(with_packages = true) ~location_ids ~component_type_ids ~package_ids ~get_location_cost = 
  let used_location_cost = sum (cost_all_components component_type_ids)        in
  let component_count    = sum (cost_locations location_ids get_location_cost) in
  let package_count      = sum (cost_all_packages package_ids)                 in
  let total_cost = 
    if with_packages
    then sum [used_location_cost; component_count; package_count]
    else sum [used_location_cost; component_count] in
  Multi_objective.Optimize( Multi_objective.Single( Single_objective.Minimize total_cost))

let spread_slow ?(with_packages = true) ~location_ids ~component_type_ids ~package_ids =
  let minimize_component_count = Single_objective.Minimize (sum (cost_all_components component_type_ids))                               in (* First minimize the number of components, *)
  let maximize_location_cost   = Single_objective.Maximize (sum (cost_used_locations location_ids component_type_ids package_ids true)) in (* then maximize the number of used locations (counting only locations with at least one component), *)
  let minimize_package_count   = Single_objective.Minimize (sum (cost_all_packages   package_ids))                                      in (* finally minimize the number of packages. *)
  let single_optimizations =
    if with_packages
    then [minimize_component_count; maximize_location_cost; minimize_package_count]
    else [minimize_component_count; maximize_location_cost] in
  Multi_objective.solve_goal_of_list_of_single_optimizations single_optimizations

let spread_fast ?(with_packages = true) ~location_ids ~component_type_ids ~package_ids = 
  Multi_objective.Optimize( Multi_objective.Single ( Single_objective.Maximize (
    (sum (cost_used_locations location_ids component_type_ids package_ids true)) -~ (sum (cost_all_components component_type_ids)))))

let conservative_slow ?(with_packages = true) ~location_ids ~component_type_ids ~package_ids ~get_local_component ~get_local_package =
  let minimize_component_difference = Single_objective.Minimize (sum (cost_difference_components location_ids component_type_ids get_local_component)) in (* First minimize the number of changed components, *)
  let minimize_location_cost        = Single_objective.Minimize (sum (cost_used_locations        location_ids component_type_ids package_ids false))   in (* then minimize the number of used locations, *)
  let minimize_package_count        = Single_objective.Minimize (sum (cost_difference_packages   location_ids package_ids        get_local_package))   in (* finally minimize the number of changed packages. *)
  let single_optimizations =
    if with_packages
    then [minimize_component_difference; minimize_location_cost; minimize_package_count]
    else [minimize_component_difference; minimize_location_cost] in
  Multi_objective.solve_goal_of_list_of_single_optimizations single_optimizations
    
    
    

(* set the optimization function in Data_state *)

let optimization_function ?(with_packages = true) (universe : Data_model.universe) (configuration : Data_model.configuration) (optimization_function : Data_model.optimization_function) =
  let location_ids        = configuration#get_location_ids in
  let component_type_ids  = universe#get_component_type_ids in
  let package_ids         = universe#get_package_ids in
  let get_local_component = configuration#get_local_component in
  let get_local_package   = configuration#get_local_package in
  let get_location_cost   = (fun location_id -> (configuration#get_location location_id)#cost) in
  match optimization_function with
  | Data_model.Optimization_function_simple       -> simple            ~with_packages ~component_type_ids
  | Data_model.Optimization_function_compact      -> compact_slow      ~with_packages ~location_ids ~component_type_ids ~package_ids ~get_location_cost
  | Data_model.Optimization_function_conservative -> conservative_slow ~with_packages ~location_ids ~component_type_ids ~package_ids ~get_local_component ~get_local_package
  | Data_model.Optimization_function_spread       -> spread_slow       ~with_packages ~location_ids ~component_type_ids ~package_ids
  | Data_model.Optimization_function_none         -> Multi_objective.Satisfy


let optimization_function_full ?(with_packages = true) (universe : Data_model.universe option) (initial_configuration : Data_model.configuration option) (optimization_function' : Data_model.optimization_function option) : Data_constraint.optimization_function option = 
  match optimization_function' with
  | None         -> None
  | Some optimization_function' -> 
    match (universe, initial_configuration) with
    | (Some universe, Some configuration) -> Some(optimization_function ~with_packages universe configuration optimization_function')
    | _                                   -> None


