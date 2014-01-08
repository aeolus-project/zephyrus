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

(*******************************************)
(** 1. Helper functions                    *)
(*******************************************)

let get_provide_arity_safe t p = if Data_model.Port_id_set.mem p t#provide_domain then constant_of_provide_arity (t#provide p) else constant 0
let get_provide_arity t p = constant_of_provide_arity (t#provide p)
let get_require_arity t p = constant_of_require_arity (t#require p)
let get_consume x o = constant (x#consume o)

(* naming convention from the paper *)
let eNt t = Variable(Global_variable(Component_type(t)))
let eNp p = Variable(Global_variable(Port(p)))
let eNk k = Variable(Global_variable(Package(k)))
let eNlt l t = Variable(Local_variable(l, Component_type(t)))
let eNlp l p = Variable(Local_variable(l, Port(p)))
let eNlk l k = Variable(Local_variable(l, Package(k)))

let eB p tp tr = Variable(Binding_variable(p,tp,tr))
let eR l r = Variable(Local_repository_variable(l,r))
let eO l o = Variable(Local_resource_variable(l,o))

let eU l = Variable(Location_used_variable(l))



(*******************************************)
(** 2. Universe Translation                *) (* using naming conventions from the paper *)
(*******************************************)

(* flat model *)
let require u_dp ur up get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute requires\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res -> (
        ((get_require_arity (get_component_type tr) p) *~ (eNt tr))
          <=~ (sum (Data_model.Component_type_id_set.fold (fun tp res -> (eB p tp tr)::res) (up p) [])))::res
    ) (ur p) res
  ) u_dp []

let provide u_dp up ur get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute provides\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tp res -> (
          ((get_provide_arity (get_component_type tp) p) *~ (eNt tp))
          >=~ (sum (Data_model.Component_type_id_set.fold (fun tr res -> (eB p tp tr)::res) (ur p) [])))::res
    ) (up p) res
  ) u_dp []

let binding u_dp ur up = 
  Zephyrus_log.log_constraint_execution "Compute unicitiy\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res ->
      Data_model.Component_type_id_set.fold (fun tp res ->
        ((eB p tp tr) <=~ ((eNt tr) *~ (eNt tp)))::res
      ) (up p) res
    ) (ur p) res
  ) u_dp []


let conflict u_dp uc get_component_type =
  Zephyrus_log.log_constraint_execution "Compute conflicts\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNt t) >=~ (constant 1)) =>~~ ((eNp p) =~ (get_provide_arity_safe (get_component_type t) p)))::res
    ) (uc p) res
  ) u_dp []

(* location val *)
let location_component_type u_dt c_l =
  Zephyrus_log.log_constraint_execution "Compute distribution component types\n";
  Data_model.Component_type_id_set.fold (fun t res ->
    ((eNt t) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlt l t)::res) c_l [])))::res
  ) u_dt []
  
let location_package u_dk c_l =
  Zephyrus_log.log_constraint_execution "Compute distribution packages\n";
  Data_model.Package_id_set.fold (fun k res ->
    ((eNk k) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlk l k)::res) c_l [])))::res
  ) u_dk []

let location_port u_dp c_l =
  Zephyrus_log.log_constraint_execution "Compute distribution ports\n";
  Data_model.Port_id_set.fold (fun p res ->
    ((eNp p) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlp l p)::res) c_l [])))::res
  ) u_dp []

let location_port_equation u_dp c_l up get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute implementation port\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Location_id_set.fold (fun l res ->
      ((eNlp l p) =~ (sum (Data_model.Component_type_id_set.fold
                       (fun t res -> ((get_provide_arity (get_component_type t) p) *~ (eNlt l t))::res) (up p) [])))::res
    ) c_l res
  ) u_dp []


  (* Repositories *)
let repository_unique c_l u_dr =
  Data_model.Location_id_set.fold (fun l res ->
  Zephyrus_log.log_constraint_execution "Compute repository unicity\n";
    ((constant 1) =~ (sum (Data_model.Repository_id_set.fold (fun r res -> (eR l r)::res) u_dr [])))::res
  ) c_l []

let repository_package c_l u_dr u_dk get_packages =
  Zephyrus_log.log_constraint_execution "Compute packages local to repository\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Repository_id_set.fold (fun r res ->
      (((eR l r) =~ (constant 1)) =>~~
        (conj (Data_model.Package_id_set.fold (fun k res -> ((eNlk l k) =~ (constant 0))::res) (Data_model.Package_id_set.diff u_dk (get_packages r)) [])))::res
    ) u_dr res
  ) c_l []

  (* Package dependencies *)
let component_type_implementation c_l u_dt u_i =
  Zephyrus_log.log_constraint_execution "Compute component type implementation\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNlt l t) >=~ (constant 1)) =>~~ ((sum (Data_model.Package_id_set.fold (fun k res -> (eNlk l k)::res) (u_i t) [])) >=~ (constant 1)))::res
    ) u_dt res
  ) c_l []

let package_dependency c_l u_dk get_package =
  Zephyrus_log.log_constraint_execution "Compute package dependencies\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k1 res ->
      Data_model.Package_id_set_set.fold (fun ks res ->
        ((eNlk l k1) <=~ (sum (Data_model.Package_id_set.fold (fun k2 res -> (eNlk l k2)::res) ks [])))::res
      ) ((get_package k1)#depend) res
    ) u_dk res
  ) c_l []

let package_conflict c_l u_dk get_package =
  Zephyrus_log.log_constraint_execution "Compute package conflicts\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k1 res ->
      Data_model.Package_id_set.fold (fun k2 res ->
        (((eNlk l k1) +~ (eNlk l k2)) <=~ (constant 1))::res
      ) ((get_package k1)#conflict) res
    ) u_dk res
  ) c_l []


  (* Resource consumptions *)
let resource_consumption c_l resource_ids u_dt u_dk get_component_type get_package =
  Zephyrus_log.log_constraint_execution "Compute resource consumption\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Resource_id_set.fold (fun o res ->
      ((sum (Data_model.Package_id_set.fold (fun k res -> ((get_consume (get_package k) o) *~ (eNlk l k))::res) u_dk
              (Data_model.Component_type_id_set.fold (fun t res -> ((get_consume (get_component_type t) o) *~ (eNlt l t))::res) u_dt []))) <=~ (eO l o))::res
    ) resource_ids res
  ) c_l []

  (* Deprecated packages and component types *)
let deprecated_component_types_and_packages c_l =
  Data_model.Location_id_set.fold (fun l res ->
  Zephyrus_log.log_constraint_execution "Compute elements to delete\n";
    ((eNlt l Data_model.deprecated_component_type_id) =~ (constant 0))::((eNlk l Data_model.deprecated_package_id) =~ (constant 0))::res
  ) c_l []

  (* Used locations *)
let used_locations u_dt u_dk c_l =
  Zephyrus_log.log_constraint_execution "Compute used locations\n";
  let local_component_exprs l : expression list = Data_model.Component_type_id_set.fold (fun t res -> (eNlt l t)::res) u_dt [] in
  let local_package_exprs   l : expression list = Data_model.Package_id_set.fold        (fun k res -> (eNlk l k)::res) u_dk [] in
  Data_model.Location_id_set.fold (fun l res -> 
     let konstraint = ( eU l =~ reify ((sum (local_component_exprs l @ local_package_exprs l)) >~ (constant 0)) ) in
     konstraint :: res
  ) c_l []



let location_all_variables u_dp u_dt u_dk c_l up get_component_type = [
  ("location ports"          , Data_constraint.conj (location_port u_dp c_l));
  ("location component types", Data_constraint.conj (location_component_type u_dt c_l));
  ("location packages"       , Data_constraint.conj (location_package u_dk c_l));
  ("definition ports"        , Data_constraint.conj (location_port_equation u_dp c_l up get_component_type)) ]

let universe location_ids universe = [ (* TODO: replace the references with description, and let Data_state do the settings *)
    (Data_state.constraint_universe_component_type_require  , require universe#get_port_ids universe#ur universe#up universe#get_component_type) ;
    (Data_state.constraint_universe_component_type_provide  , provide universe#get_port_ids universe#up universe#ur universe#get_component_type) ;
    (Data_state.constraint_universe_component_type_conflict , conflict universe#get_port_ids universe#uc universe#get_component_type) ;
    (Data_state.constraint_universe_component_type_implementation , component_type_implementation location_ids universe#get_component_type_ids universe#get_implementation) ;
    (Data_state.constraint_universe_binding_unicity         , binding universe#get_port_ids universe#ur universe#up) ;
    (Data_state.constraint_universe_location_component_type , location_component_type universe#get_component_type_ids location_ids) ;
    (Data_state.constraint_universe_location_package        , location_package universe#get_package_ids location_ids) ;
    (Data_state.constraint_universe_location_port           , location_port universe#get_port_ids location_ids) ;
    (Data_state.constraint_universe_definition_port         , location_port_equation universe#get_port_ids location_ids universe#up universe#get_component_type) ;
    (Data_state.constraint_universe_repository_unicity      , repository_unique location_ids universe#get_repository_ids) ;
    (Data_state.constraint_universe_repository_package      , repository_package location_ids universe#get_repository_ids universe#get_package_ids (fun r -> (universe#get_repository r)#package_ids)) ;
    (Data_state.constraint_universe_package_dependency      , package_dependency location_ids universe#get_package_ids universe#get_package) ;
    (Data_state.constraint_universe_package_conflict        , package_conflict location_ids universe#get_package_ids universe#get_package) ;
    (Data_state.constraint_universe_resource_consumption    , resource_consumption location_ids universe#get_resource_ids universe#get_component_type_ids universe#get_package_ids universe#get_component_type universe#get_package) ;
    (Data_state.constraint_universe_deprecated_element      , deprecated_component_types_and_packages location_ids);
    (Data_state.constraint_universe_deprecated_element      , used_locations universe#get_component_type_ids universe#get_package_ids location_ids) ]

 

let universe_full () =
  let f (universe: Data_model.universe) configuration =
    Data_state.constraint_universe_component_type_require        := require universe#get_port_ids universe#ur universe#up universe#get_component_type;
    Data_state.constraint_universe_component_type_provide        := provide universe#get_port_ids universe#up universe#ur universe#get_component_type;
    Data_state.constraint_universe_component_type_conflict       := conflict universe#get_port_ids universe#uc universe#get_component_type;
    Data_state.constraint_universe_component_type_implementation := component_type_implementation configuration#get_location_ids universe#get_component_type_ids universe#get_implementation;
    Data_state.constraint_universe_binding_unicity               := binding universe#get_port_ids universe#ur universe#up;
    Data_state.constraint_universe_location_component_type       := location_component_type universe#get_component_type_ids configuration#get_location_ids;
    Data_state.constraint_universe_location_package              := location_package universe#get_package_ids configuration#get_location_ids;
    Data_state.constraint_universe_location_port                 := location_port universe#get_port_ids configuration#get_location_ids;
    Data_state.constraint_universe_definition_port               := location_port_equation universe#get_port_ids configuration#get_location_ids universe#up universe#get_component_type;
    Data_state.constraint_universe_repository_unicity            := repository_unique configuration#get_location_ids universe#get_repository_ids;
    Data_state.constraint_universe_repository_package            := repository_package configuration#get_location_ids universe#get_repository_ids universe#get_package_ids (fun r -> (universe#get_repository r)#package_ids);
    Data_state.constraint_universe_package_dependency            := package_dependency configuration#get_location_ids universe#get_package_ids universe#get_package;
    Data_state.constraint_universe_package_conflict              := package_conflict configuration#get_location_ids universe#get_package_ids universe#get_package;
    Data_state.constraint_universe_resource_consumption          :=
      resource_consumption configuration#get_location_ids universe#get_resource_ids universe#get_component_type_ids universe#get_package_ids universe#get_component_type universe#get_package;
    Data_state.constraint_universe_deprecated_element            := deprecated_component_types_and_packages configuration#get_location_ids;
    Data_state.constraint_universe_used_locations                := used_locations universe#get_component_type_ids universe#get_package_ids configuration#get_location_ids;
  in match (!Data_state.universe_full, !Data_state.initial_configuration_full) with
    | (Some(u), Some(c)) -> f u c
    | _ -> ()




(*******************************************)
(** 3. Specification Translation           *) (* using naming conventions from the paper *)
(*******************************************)

let spec_variable_name v = Variable(Simple_variable(v))
let spec_const = constant

let spec_local_element l e = match e with
  | Data_model.Spec_local_element_package        (package_id)        -> eNlk l package_id
  | Data_model.Spec_local_element_component_type (component_type_id) -> eNlt l component_type_id
  | Data_model.Spec_local_element_port           (port_id)           -> eNlp l port_id

let rec spec_local_expr l e = match e with
  | Data_model.Spec_local_expr_var   (v)      -> spec_variable_name v
  | Data_model.Spec_local_expr_const (c)      -> spec_const c
  | Data_model.Spec_local_expr_arity (e)      -> spec_local_element l e
  | Data_model.Spec_local_expr_add   (e1, e2) -> (spec_local_expr l e1) +~ (spec_local_expr l e2)
  | Data_model.Spec_local_expr_sub   (e1, e2) -> (spec_local_expr l e1) -~ (spec_local_expr l e2)
  | Data_model.Spec_local_expr_mul   (e1, e2) -> (spec_const e1) *~ (spec_local_expr l e2)

let spec_op o = match o with
  | Data_model.Lt  -> ( <~  )
  | Data_model.LEq -> ( <=~ )
  | Data_model.Eq  -> ( =~  )
  | Data_model.GEq -> ( >=~ )
  | Data_model.Gt  -> ( >~  )
  | Data_model.NEq -> ( <>~ )

let rec local_specification l s = match s with
  | Data_model.Spec_local_true              -> True
  | Data_model.Spec_local_op   (e1, op, e2) -> (spec_op op) (spec_local_expr l e1) (spec_local_expr l e2)
  | Data_model.Spec_local_and  (s1, s2)     -> (local_specification l s1) &&~~ (local_specification l s2)
  | Data_model.Spec_local_or   (s1, s2)     -> (local_specification l s1) ||~~ (local_specification l s2)
  | Data_model.Spec_local_impl (s1, s2)     -> (local_specification l s1) =>~~ (local_specification l s2)
  | Data_model.Spec_local_not  (s')         -> !~ (local_specification l s')

let spec_resource_constraint l co = List.fold_left (fun res (o, op, i) -> ((spec_op op) (eO l o) (constant i))::res) [] co
let spec_repository_constraint l cr = match cr with 
                                      | [] -> true_konstraint
                                      | _  -> (sum (List.map (fun r -> eR l r) cr)) =~ (constant 1)

let spec_element location_ids e = match e with
  | Data_model.Spec_element_package        (package_id)        -> eNk package_id
  | Data_model.Spec_element_component_type (component_type_id) -> eNt component_type_id
  | Data_model.Spec_element_port           (port_id)           -> eNp port_id
  | Data_model.Spec_element_location (co, cr, ls) -> sum (Data_model.Location_id_set.fold
     (fun l res -> (reify (conj((local_specification l ls)::(spec_repository_constraint l cr)::(spec_resource_constraint l co))))::res ) location_ids [])

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
  | Data_model.Spec_everywhere          (ls) -> conj (List.map (fun l -> local_specification l ls) (Data_model.Location_id_set.elements location_ids))
  | Data_model.Spec_at (at_location_ids, ls) -> conj (List.map (fun l -> local_specification l ls) at_location_ids)

let specification locations s = Zephyrus_log.log_constraint_execution "Compute Specification\n"; 
  [(Data_state.constraint_specification_full, specification_simple locations s) ]

let specification_full () = match (!Data_state.specification_full, !Data_state.initial_configuration_full) with
  | (Some(s), Some(c)) -> Data_state.constraint_specification_full := Some(specification_simple c#get_location_ids s)
  | _  -> ()


(*******************************************)
(** 4. Configuration Translation           *) (* using naming conventions from the paper *)
(*******************************************)

let locations resource_ids location_ids get_location = Zephyrus_log.log_constraint_execution "Compute Resources Provided by Locations\n";
  if Settings.find Settings.modifiable_configuration then [] (* if we can modify the resources of a location, we do not enforce its value in the constraint *)
  else
    [ Data_state.constraint_configuration_full ,
    Data_model.Location_id_set.fold (fun l_id res ->
      let l = get_location l_id in
      Data_model.Resource_id_set.fold (fun o res ->
        ((eO (l_id) o) =~ (constant (l#provide_resources o)))::res
      ) resource_ids res
    ) location_ids [] ]

let configuration resource_ids c_l get_location = 
  if Settings.find Settings.modifiable_configuration then [] (* if we can modify the resources of a location, we do not enforce its value in the constraint *)
  else
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Resource_id_set.fold (fun o res ->
      ((eO l o) =~ (constant ((get_location l)#provide_resources o)))::res
    ) resource_ids res
  ) c_l []

let configuration_full () = match (!Data_state.universe_full, !Data_state.initial_configuration_full) with
    | (Some(u), Some(c)) -> Data_state.constraint_configuration_full := configuration u#get_resource_ids c#get_location_ids c#get_location
    | _ -> ()


(*******************************************)
(** 5. Optimisation function Translation   *)
(*******************************************)

(* preliminary definitions for costs *)
let cost_all_components u_dt = (Data_model.Component_type_id_set.fold (fun t res -> (eNt t)::res) u_dt [])
let cost_all_packages   u_dk = (Data_model.Package_id_set.fold (fun k res -> (eNk k)::res) u_dk [])

let cost_local_components l u_dt = (Data_model.Component_type_id_set.fold (fun t res -> (eNlt l t)::res) u_dt [])
let cost_local_packages   l u_dk = (Data_model.Package_id_set.fold (fun k res -> (eNlk l k)::res) u_dk [])

type used_or_free_location = Used_locations | Free_locations
let cost_locations switch c_l u_dt u_dk count_components_only =
  Data_model.Location_id_set.fold (fun l res ->
    (reify ((if switch = Used_locations then (>~) else (=~) ) (sum (if count_components_only then cost_local_components l u_dt
              else List.rev_append (cost_local_components l u_dt) (cost_local_packages l u_dk))) (constant 0)))::res
  ) c_l []

let cost_used_locations = cost_locations Used_locations
let cost_free_locations = cost_locations Free_locations

let cost_difference_components c_l u_dt get_local_component = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (abs((eNlt l t) -~ (constant (Data_model.Component_id_set.cardinal (get_local_component l t))) ))::res
    ) u_dt res) c_l []

let cost_difference_packages c_l u_dk get_local_package = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k res ->
      (if get_local_package l k then abs((eNlk l k) -~ (constant 1)) else (eNlk l k))::res
    ) u_dk res) c_l []

let cost_locations c_l (get_location_cost : Data_model.location_id -> Data_model.location_cost) =
  Data_model.Location_id_set.fold (fun l res ->
    ( (eU l) *~ constant (get_location_cost l) ) :: res
  ) c_l []

(* translation functions *)
let compact_slow c_l u_dt u_dk get_location_cost =
  Lexicographic ([
    Minimize (sum (cost_locations c_l get_location_cost));       (* First minimize the number of used locations *)
    Minimize (sum (cost_all_components u_dt)); (* then minimize the number of components *)
    Minimize (sum (cost_all_packages u_dk)) ]) (* finally minimize the number of packages. (so we do not have useless packages) *)
let compact_fast c_l u_dt u_dk get_location_cost = 
  Minimize (sum (List.rev_append (List.rev_append 
    (cost_all_components u_dt) 
    (cost_locations c_l get_location_cost))
    (cost_all_packages u_dk)))
let spread_slow c_l u_dt u_dk =
  Lexicographic ([
    Minimize (sum (cost_all_components u_dt));               (* First minimize the number of components *)
    Maximize (sum (cost_used_locations c_l u_dt u_dk true)); (* then maximize the number of used locations, (counting only locations with at least one component) *)
    Minimize (sum (cost_all_packages u_dk)) ])               (* finally minimize the number of packages. *)
let spread_fast c_l u_dt u_dk = Maximize ((sum (cost_used_locations c_l u_dt u_dk true)) -~ (sum (cost_all_components u_dt)))

let conservative_slow c_l u_dt u_dk get_local_component get_local_package =
  Lexicographic ([
    Minimize (sum (cost_difference_components c_l u_dt get_local_component)); (* First minimize the number of changed components *)
    Minimize (sum (cost_used_locations c_l u_dt u_dk false));                 (* then minimize the number of used locations *)
    Minimize (sum (cost_difference_packages c_l u_dk get_local_package)) ])   (* finally minimize the number of changed packages *)

(* set the optimization function in Data_state *)

let optimization_function u c f =
  let (c_l, u_dt, u_dk, get_local_component, get_local_package, get_location_cost) =
    (c#get_location_ids, u#get_component_type_ids, u#get_package_ids, c#get_local_component, c#get_local_package, (fun location_id -> (c#get_location location_id)#cost) ) in
  match f with
  | Data_model.Optimization_function_simple       -> Minimize (sum (cost_all_components u_dt))
  | Data_model.Optimization_function_compact      -> compact_slow c_l u_dt u_dk get_location_cost
  | Data_model.Optimization_function_conservative -> conservative_slow c_l u_dt u_dk get_local_component get_local_package
  | Data_model.Optimization_function_spread       -> spread_slow c_l u_dt u_dk
  | Data_model.Optimization_function_none         -> Lexicographic([])


let optimization_function_full () = match !Data_state.optimization_function with
  | None -> () | Some(f) -> match (!Data_state.universe_full, !Data_state.initial_configuration_full) with
    | (Some(u), Some(c)) -> Data_state.constraint_optimization_function := Some(optimization_function u c f)
    | _ -> ()
