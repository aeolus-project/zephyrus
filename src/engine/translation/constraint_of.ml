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
let ralfs_redundant_require u_dp ur up get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute Ralf's redundant require constraints\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res -> (
      let tr_arity_equal_zero = ( (eNt tr) =~ (constant 0) ) in
      let tr_require_arity = get_require_arity (get_component_type tr) p in
      let sum_of_tp_global_arities = sum (Data_model.Component_type_id_set.map_to_list (fun tp -> eNt tp) (up p) ) in
      tr_arity_equal_zero ||~~ (tr_require_arity <=~ sum_of_tp_global_arities)
    )::res) (ur p) res
  ) u_dp []

let flat_require u_dp ur up get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute binding requires\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res -> (
        ((get_require_arity (get_component_type tr) p) *~ (eNt tr))
          <=~ (sum (Data_model.Component_type_id_set.fold (fun tp res -> (eB p tp tr)::res) (up p) [])))::res
    ) (ur p) res
  ) u_dp []

let require u_dp ur up get_component_type = 
  let flat_require            = flat_require            u_dp ur up get_component_type in
  let ralfs_redundant_require = ralfs_redundant_require u_dp ur up get_component_type in
  flat_require @ (if Settings.find Settings.ralfs_redundant_constraints then ralfs_redundant_require else [])

let provide_with_fixed_infinity u_dp up ur get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with naive encoding of infinity as a fixed value)\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tp res -> (
          ((get_provide_arity (get_component_type tp) p) *~ (eNt tp))
          >=~ (sum (Data_model.Component_type_id_set.fold (fun tr res -> (eB p tp tr)::res) (ur p) [])))::res
    ) (up p) res
  ) u_dp []

let provide_with_advanced_infinity u_dp up ur get_component_type = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with advanced encoding of infinity)\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tp res ->
      match (get_component_type tp)#provide p with
      | Data_model.Finite_provide provide_arity ->
          (* If the component type [tp] is providing the port [p] with a finite arity: *)

          (* 1. Total number of bindings between components of type [tp] and any components which require port [p] 
                must be equal or smaller than the total arity of port [p] provided by all the components of type [tp] present. *)
          (((constant provide_arity) *~ (eNt tp)) >=~ (sum (Data_model.Component_type_id_set.map_to_list (fun tr -> eB p tp tr) (ur p))))::res
      | Data_model.Infinite_provide ->
          (* If the component type [tp] is providing the port [p] with an infinite arity: *)

          (* 1. If there is no component of type [tp] present then there can be no bidings on port [p] 
                between components of this type and components requiring port [p]. 
                In other words: if at least one component of type [tp] is present, 
                then the number of such bindings is not constrained at all. *)
          let if_there_is_none_there_can_be_no_bindings : konstraint list =
            Data_model.Component_type_id_set.map_to_list (fun tr ->
              ((eNt tp) =~ (constant 0)) =>~~ ((eB p tp tr) =~ (constant 0))
            ) (ur p) in
          if_there_is_none_there_can_be_no_bindings @ res
    ) (up p) res
  ) u_dp []

let binding u_dp ur up = 
  Zephyrus_log.log_constraint_execution "Compute binding unicitiy\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res ->
      Data_model.Component_type_id_set.fold (fun tp res ->
        ((eB p tp tr) <=~ ((eNt tr) *~ (eNt tp)))::res
      ) (up p) res
    ) (ur p) res
  ) u_dp []


let conflict_naive u_dp uc get_component_type =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (naive encoding)\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNt t) >=~ (constant 1)) =>~~ ((eNp p) =~ (get_provide_arity_safe (get_component_type t) p)))::res
    ) (uc p) res
  ) u_dp []

let conflict_advanced u_dp uc up get_component_type =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (advanced encoding)\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    let conflicters = uc p in
    let providers   = up p in
    Data_model.Component_type_id_set.fold (fun t res ->
      if Data_model.Component_type_id_set.mem t providers
      then
        (* If the component type [t] is in conflict with itself on port [p], then: *)

        (* 1. there can be at most one instance of this component type, *)
        let there_can_be_only_one : konstraint = (eNt t) <=~ (constant 1) in

        (* 2. if this single instance of component type [t] is present, then
              no instances of OTHER component types providing port [p] may be present. *)
        let if_there_is_one_there_must_be_no_other_providers : konstraint list =
          Data_model.Component_type_id_set.map_to_list (fun provider ->
            ((eNt t) =~ (constant 1)) =>~~ ((eNt provider) =~ (constant 0))
          ) (Data_model.Component_type_id_set.remove t providers) in

        (there_can_be_only_one :: if_there_is_one_there_must_be_no_other_providers) @ res

      else
        (* The component type [t] is NOT in conflict with itself on port [p], then: *)

        (* 1. if there is at least one instance of component [t], then
              no instances of component types providing port [p] may be present. *)
        let if_there_is_at_least_one_there_must_be_no_providers : konstraint list =
          Data_model.Component_type_id_set.map_to_list (fun provider ->
            ((eNt t) >~ (constant 0)) =>~~ ((eNt provider) =~ (constant 0))
          ) providers in

        if_there_is_at_least_one_there_must_be_no_providers @ res
    ) conflicters res
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
let resource_consumption ?(with_packages = true) c_l resource_ids u_dt u_dk get_component_type get_package get_location =
  Zephyrus_log.log_constraint_execution "Compute resource consumption\n";
  Data_model.Location_id_set.fold (fun l res ->
    let location = get_location l in
    Data_model.Resource_id_set.fold (fun o res ->
      let resources_provided_constraint = ( (eO l o) =~ (constant (location#provide_resources o)) ) in
      let resources_consumed_constraint =
        let resources_consumed_packages   = sum (Data_model.Package_id_set       .map_to_list (fun k -> ((get_consume (get_package        k) o) *~ (eNlk l k)) ) u_dk) in
        let resources_consumed_components = sum (Data_model.Component_type_id_set.map_to_list (fun t -> ((get_consume (get_component_type t) o) *~ (eNlt l t)) ) u_dt) in
        let resources_consumed_total =
          if with_packages
          then resources_consumed_components +~ resources_consumed_packages
          else resources_consumed_components in
        resources_consumed_total <=~ (eO l o) in
      resources_provided_constraint::resources_consumed_constraint::res
    ) resource_ids res
  ) c_l []

  (* Deprecated packages and component types *)
let deprecated_component_types_with_packages ?(with_packages = true) c_l =
  Data_model.Location_id_set.fold (fun l res ->
  Zephyrus_log.log_constraint_execution "Compute elements to delete\n";
    let deprecated_component_constraint = (eNlt l Data_model.deprecated_component_type_id) =~ (constant 0) in
    let deprecated_package_constraint   = (eNlk l Data_model.deprecated_package_id)        =~ (constant 0) in
    if with_packages
    then deprecated_component_constraint::deprecated_package_constraint::res
    else deprecated_component_constraint::res
  ) c_l []

  (* Used locations *)
let used_locations ?(with_packages = true) u_dt u_dk c_l =
  Zephyrus_log.log_constraint_execution "Compute used locations\n";
  let local_component_exprs l : expression list = Data_model.Component_type_id_set.fold (fun t res -> (eNlt l t)::res) u_dt [] in
  let local_package_exprs   l : expression list = Data_model.Package_id_set.fold        (fun k res -> (eNlk l k)::res) u_dk [] in
  let local_objects_exprs   l : expression list = 
    if with_packages
    then local_component_exprs l @ local_package_exprs l
    else local_component_exprs l in
  Data_model.Location_id_set.fold (fun l res -> 
     let konstraint = ( eU l =~ reify ((sum (local_objects_exprs l)) >~ (constant 0)) ) in
     konstraint :: res
  ) c_l []

  (* Incompatibilities *)
let incompatibilities (universe_incompatibilities : Data_model.Component_type_id_set_set.t Data_model.Repository_id_map.t) c_l =
  Zephyrus_log.log_constraint_execution "Compute incompatibilities\n";
  Data_model.Location_id_set.fold (fun location_id res ->
    Data_model.Repository_id_map.fold (fun repository_id repository_incompatibilities res ->
      Data_model.Component_type_id_set_set.fold (fun incompatible_component_ids res ->
        let repository_constraint = (eR location_id repository_id) =~ (constant 1) in
        let component_type_constraints = 
          Data_model.Component_type_id_set.fold (fun component_type_id l ->
            ((eNlt location_id component_type_id) >~ (constant 0)) :: l
          ) incompatible_component_ids [] in
        let konstraint = !~ (conj (repository_constraint :: component_type_constraints)) in
        konstraint :: res
      ) repository_incompatibilities []
    ) universe_incompatibilities []
  ) c_l []

let location_all_variables ?(with_packages = true) u_dp u_dt u_dk c_l up get_component_type = 
  let location_ports           = ("location ports"          , (location_port u_dp c_l)) in
  let location_component_types = ("location component types", (location_component_type u_dt c_l)) in
  let location_packages        = ("location packages"       , (location_package u_dk c_l)) in
  let definition_ports         = ("definition ports"        , (location_port_equation u_dp c_l up get_component_type)) in
  if with_packages
  then [location_ports; location_component_types; location_packages; definition_ports]
  else [location_ports; location_component_types;                    definition_ports]


let universe ?(with_packages = true) location_ids (universe : Data_model.universe) configuration = (* TODO: replace the references with description, and let Data_state do the settings *)
  if with_packages
  then [
    "Data_state.constraint_universe_component_type_require"        , require                        universe#get_port_ids universe#ur universe#up universe#get_component_type;
    "Data_state.constraint_universe_component_type_provide"        , provide_with_advanced_infinity universe#get_port_ids universe#up universe#ur universe#get_component_type;
    "Data_state.constraint_universe_component_type_conflict"       , conflict_advanced              universe#get_port_ids universe#uc universe#up universe#get_component_type;
    "Data_state.constraint_universe_component_type_implementation" , component_type_implementation  location_ids universe#get_component_type_ids universe#get_implementation;
    "Data_state.constraint_universe_binding_unicity"               , binding                        universe#get_port_ids universe#ur universe#up;
    "Data_state.constraint_universe_location_component_type"       , location_component_type        universe#get_component_type_ids location_ids;
    "Data_state.constraint_universe_location_package"              , location_package               universe#get_package_ids        location_ids;
    "Data_state.constraint_universe_location_port"                 , location_port                  universe#get_port_ids           location_ids;
    "Data_state.constraint_universe_definition_port"               , location_port_equation         universe#get_port_ids           location_ids universe#up universe#get_component_type;
    "Data_state.constraint_universe_repository_unicity"            , repository_unique    location_ids universe#get_repository_ids;
    "Data_state.constraint_universe_repository_package"            , repository_package   location_ids universe#get_repository_ids universe#get_package_ids (fun r -> (universe#get_repository r)#package_ids);
    "Data_state.constraint_universe_package_dependency"            , package_dependency   location_ids universe#get_package_ids    universe#get_package;
    "Data_state.constraint_universe_package_conflict"              , package_conflict     location_ids universe#get_package_ids    universe#get_package;
    "Data_state.constraint_universe_resource_consumption"          , resource_consumption location_ids universe#get_resource_ids universe#get_component_type_ids universe#get_package_ids universe#get_component_type universe#get_package configuration#get_location;
    "Data_state.constraint_universe_deprecated_element"            , deprecated_component_types_with_packages location_ids;
    "Data_state.constraint_universe_used_locations"                , used_locations universe#get_component_type_ids universe#get_package_ids location_ids ]
  else [
    (* Written using the well known programming paradigm invented by Mr. Copy and Dr. Paste. *)
    "Data_state.constraint_universe_component_type_require"        , require universe#get_port_ids universe#ur universe#up universe#get_component_type;
    "Data_state.constraint_universe_component_type_provide"        , provide_with_advanced_infinity universe#get_port_ids universe#up universe#ur universe#get_component_type;
    "Data_state.constraint_universe_component_type_conflict"       , conflict_advanced universe#get_port_ids universe#uc universe#up universe#get_component_type;
(*  "Data_state.constraint_universe_component_type_implementation" , component_type_implementation location_ids universe#get_component_type_ids universe#get_implementation; *)
    "Data_state.constraint_universe_binding_unicity"               , binding universe#get_port_ids universe#ur universe#up;
    "Data_state.constraint_universe_location_component_type"       , location_component_type universe#get_component_type_ids location_ids;
(*  "Data_state.constraint_universe_location_package"              , location_package universe#get_package_ids location_ids; *)
    "Data_state.constraint_universe_location_port"                 , location_port universe#get_port_ids location_ids;
    "Data_state.constraint_universe_definition_port"               , location_port_equation universe#get_port_ids location_ids universe#up universe#get_component_type;
    "Data_state.constraint_universe_repository_unicity"            , repository_unique location_ids universe#get_repository_ids;
(*  "Data_state.constraint_universe_repository_package"            , repository_package location_ids universe#get_repository_ids universe#get_package_ids (fun r -> (universe#get_repository r)#package_ids); *)
(*  "Data_state.constraint_universe_package_dependency"            , package_dependency location_ids universe#get_package_ids universe#get_package; *)
(*  "Data_state.constraint_universe_package_conflict"              , package_conflict location_ids universe#get_package_ids universe#get_package; *)
    "Data_state.constraint_universe_resource_consumption"          , resource_consumption ~with_packages:false location_ids universe#get_resource_ids universe#get_component_type_ids (* universe#get_package_ids *) Data_model.Package_id_set.empty universe#get_component_type universe#get_package configuration#get_location;
    "Data_state.constraint_universe_deprecated_element"            , deprecated_component_types_with_packages ~with_packages:false location_ids;
    "Data_state.constraint_universe_used_locations"                , used_locations ~with_packages:false universe#get_component_type_ids (* universe#get_package_ids *) Data_model.Package_id_set.empty location_ids;
    "Data_state.constraint_universe_incompatibilities"             , incompatibilities (Incompatibilities_of.universe universe) location_ids ]

 

let universe_full ?(with_packages = true) (universe : Data_model.universe option) (initial_configuration : Data_model.configuration option) =
  let make_constraint_universe (universe : Data_model.universe) (configuration : Data_model.configuration) =
    if with_packages
    then
    {
      Data_state.constraint_universe_component_type_require        = require universe#get_port_ids universe#ur universe#up universe#get_component_type;
      Data_state.constraint_universe_component_type_provide        = provide_with_advanced_infinity universe#get_port_ids universe#up universe#ur universe#get_component_type;
      Data_state.constraint_universe_component_type_conflict       = conflict_advanced universe#get_port_ids universe#uc universe#up universe#get_component_type;
      Data_state.constraint_universe_component_type_implementation = component_type_implementation configuration#get_location_ids universe#get_component_type_ids universe#get_implementation;
      Data_state.constraint_universe_binding_unicity               = binding universe#get_port_ids universe#ur universe#up;
      Data_state.constraint_universe_location_component_type       = location_component_type universe#get_component_type_ids configuration#get_location_ids;
      Data_state.constraint_universe_location_package              = location_package universe#get_package_ids configuration#get_location_ids;
      Data_state.constraint_universe_location_port                 = location_port universe#get_port_ids configuration#get_location_ids;
      Data_state.constraint_universe_definition_port               = location_port_equation universe#get_port_ids configuration#get_location_ids universe#up universe#get_component_type;
      Data_state.constraint_universe_repository_unicity            = repository_unique configuration#get_location_ids universe#get_repository_ids;
      Data_state.constraint_universe_repository_package            = repository_package configuration#get_location_ids universe#get_repository_ids universe#get_package_ids (fun r -> (universe#get_repository r)#package_ids);
      Data_state.constraint_universe_package_dependency            = package_dependency configuration#get_location_ids universe#get_package_ids universe#get_package;
      Data_state.constraint_universe_package_conflict              = package_conflict configuration#get_location_ids universe#get_package_ids universe#get_package;
      Data_state.constraint_universe_resource_consumption          = resource_consumption configuration#get_location_ids universe#get_resource_ids universe#get_component_type_ids universe#get_package_ids universe#get_component_type universe#get_package configuration#get_location;
      Data_state.constraint_universe_deprecated_element            = deprecated_component_types_with_packages configuration#get_location_ids;
      Data_state.constraint_universe_used_locations                = used_locations universe#get_component_type_ids universe#get_package_ids configuration#get_location_ids;
      Data_state.constraint_universe_incompatibilities             = []; (* TODO: Add or not? *)
    }
    else
    (* Written using the well known programming paradigm invented by Mr. Copy and Dr. Paste. *)
    {
      Data_state.constraint_universe_component_type_require        = require universe#get_port_ids universe#ur universe#up universe#get_component_type;
      Data_state.constraint_universe_component_type_provide        = provide_with_advanced_infinity universe#get_port_ids universe#up universe#ur universe#get_component_type;
      Data_state.constraint_universe_component_type_conflict       = conflict_advanced universe#get_port_ids universe#uc universe#up universe#get_component_type;
      Data_state.constraint_universe_component_type_implementation = [];
      Data_state.constraint_universe_binding_unicity               = binding universe#get_port_ids universe#ur universe#up;
      Data_state.constraint_universe_location_component_type       = location_component_type universe#get_component_type_ids configuration#get_location_ids;
      Data_state.constraint_universe_location_package              = [];
      Data_state.constraint_universe_location_port                 = location_port universe#get_port_ids configuration#get_location_ids;
      Data_state.constraint_universe_definition_port               = location_port_equation universe#get_port_ids configuration#get_location_ids universe#up universe#get_component_type;
      Data_state.constraint_universe_repository_unicity            = repository_unique configuration#get_location_ids universe#get_repository_ids;
      Data_state.constraint_universe_repository_package            = [];
      Data_state.constraint_universe_package_dependency            = [];
      Data_state.constraint_universe_package_conflict              = [];
      Data_state.constraint_universe_resource_consumption          = resource_consumption ~with_packages:false configuration#get_location_ids universe#get_resource_ids universe#get_component_type_ids (* universe#get_package_ids *) Data_model.Package_id_set.empty universe#get_component_type universe#get_package configuration#get_location;
      Data_state.constraint_universe_deprecated_element            = deprecated_component_types_with_packages ~with_packages:false configuration#get_location_ids;
      Data_state.constraint_universe_used_locations                = used_locations ~with_packages:false universe#get_component_type_ids (* universe#get_package_ids *) Data_model.Package_id_set.empty configuration#get_location_ids;
      Data_state.constraint_universe_incompatibilities             = incompatibilities (Incompatibilities_of.universe universe) configuration#get_location_ids;
    } in 
  match (universe, initial_configuration) with
  | (Some universe, Some configuration) -> make_constraint_universe universe configuration
  | _                                   -> Data_state.empty_constraint_universe

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


let specification ?(with_packages = true) locations s = 
  Zephyrus_log.log_constraint_execution "Compute Specification\n"; 
  [ "Data_state.constraint_specification_full" , [specification_simple locations s] ]


let specification_full ?(with_packages = true) (specification : Data_model.specification option) (initial_configuration : Data_model.configuration option) : Data_state.constraint_specification = 
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
    Data_model.Location_id_set.fold (fun l_id res ->
      let l = get_location l_id in
      Data_model.Resource_id_set.fold (fun o res ->
        ((eO (l_id) o) =~ (constant (l#provide_resources o)))::res
      ) resource_ids res
    ) location_ids [] ]


let configuration ?(with_packages = true) resource_ids c_l get_location = 
  if Settings.find Settings.modifiable_configuration then [] (* if we can modify the resources of a location, we do not enforce its value in the constraint *)
  else
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Resource_id_set.fold (fun o res ->
      ((eO l o) =~ (constant ((get_location l)#provide_resources o)))::res
    ) resource_ids res
  ) c_l []

let configuration_full ?(with_packages = true) (universe : Data_model.universe option) (initial_configuration : Data_model.configuration option) : Data_state.constraint_configuration = 
  match (universe, initial_configuration) with
  | (Some universe, Some initial_configuration) -> configuration universe#get_resource_ids initial_configuration#get_location_ids initial_configuration#get_location
  | _                                           -> []


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

let cost_difference_components c_l u_dt get_local_component : expression list = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (abs((eNlt l t) -~ (constant (Data_model.Component_id_set.cardinal (get_local_component l t))) ))::res
    ) u_dt res) c_l []

let cost_difference_packages c_l u_dk get_local_package : expression list = 
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k res ->
      (if get_local_package l k then abs((eNlk l k) -~ (constant 1)) else (eNlk l k))::res
    ) u_dk res) c_l []

let cost_locations c_l (get_location_cost : Data_model.location_id -> Data_model.location_cost) : expression list =
  Data_model.Location_id_set.fold (fun l res ->
    ( (eU l) *~ constant (get_location_cost l) ) :: res
  ) c_l []

(* translation functions *)
let simple ?(with_packages = true) u_dt =
  Multi_objective.Optimize( Multi_objective.Single ( Single_objective.Minimize (sum (cost_all_components u_dt)) ))

let compact_slow ?(with_packages = true) c_l u_dt u_dk get_location_cost =
  let minimize_used_location_cost = Single_objective.Minimize (sum (cost_locations c_l get_location_cost)) in (* First minimize the number of used locations, *) 
  let minimize_component_count    = Single_objective.Minimize (sum (cost_all_components u_dt))             in (* then minimize the number of components, *)
  let minimize_package_count      = Single_objective.Minimize (sum (cost_all_packages u_dk))               in (* finally minimize the number of packages (so we do not have useless packages). *)
  let single_optimizations =
    if with_packages
    then [minimize_used_location_cost; minimize_component_count; minimize_package_count]
    else [minimize_used_location_cost; minimize_component_count] in
  Multi_objective.solve_goal_of_list_of_single_optimizations single_optimizations

let compact_fast ?(with_packages = true) c_l u_dt u_dk get_location_cost = 
  let used_location_cost = sum (cost_all_components u_dt)             in
  let component_count    = sum (cost_locations c_l get_location_cost) in
  let package_count      = sum (cost_all_packages u_dk)               in
  let total_cost = 
    if with_packages
    then sum [used_location_cost; component_count; package_count]
    else sum [used_location_cost; component_count] in
  Multi_objective.Optimize( Multi_objective.Single( Single_objective.Minimize total_cost))

let spread_slow ?(with_packages = true) c_l u_dt u_dk =
  let minimize_component_count = Single_objective.Minimize (sum (cost_all_components u_dt)) in               (* First minimize the number of components, *)
  let maximize_location_cost   = Single_objective.Maximize (sum (cost_used_locations c_l u_dt u_dk true)) in (* then maximize the number of used locations (counting only locations with at least one component), *)
  let minimize_package_count   = Single_objective.Minimize (sum (cost_all_packages u_dk)) in                 (* finally minimize the number of packages. *)
  let single_optimizations =
    if with_packages
    then [minimize_component_count; maximize_location_cost; minimize_package_count]
    else [minimize_component_count; maximize_location_cost] in
  Multi_objective.solve_goal_of_list_of_single_optimizations single_optimizations

let spread_fast ?(with_packages = true) c_l u_dt u_dk = 
  Multi_objective.Optimize( Multi_objective.Single ( Single_objective.Maximize (
    (sum (cost_used_locations c_l u_dt u_dk true)) -~ (sum (cost_all_components u_dt)))))

let conservative_slow ?(with_packages = true) c_l u_dt u_dk get_local_component get_local_package =
  let minimize_component_difference = Single_objective.Minimize (sum (cost_difference_components c_l u_dt get_local_component)) in (* First minimize the number of changed components, *)
  let minimize_location_cost        = Single_objective.Minimize (sum (cost_used_locations        c_l u_dt u_dk false))          in (* then minimize the number of used locations, *)
  let minimize_package_count        = Single_objective.Minimize (sum (cost_difference_packages   c_l u_dk get_local_package))   in (* finally minimize the number of changed packages. *)
  let single_optimizations =
    if with_packages
    then [minimize_component_difference; minimize_location_cost; minimize_package_count]
    else [minimize_component_difference; minimize_location_cost] in
  Multi_objective.solve_goal_of_list_of_single_optimizations single_optimizations
    
    
    

(* set the optimization function in Data_state *)

let optimization_function ?(with_packages = true) u c f =
  let (c_l, u_dt, u_dk, get_local_component, get_local_package, get_location_cost) =
    (c#get_location_ids, u#get_component_type_ids, u#get_package_ids, c#get_local_component, c#get_local_package, (fun location_id -> (c#get_location location_id)#cost) ) in
  match f with
  | Data_model.Optimization_function_simple       -> simple            ~with_packages u_dt
  | Data_model.Optimization_function_compact      -> compact_slow      ~with_packages c_l u_dt u_dk get_location_cost
  | Data_model.Optimization_function_conservative -> conservative_slow ~with_packages c_l u_dt u_dk get_local_component get_local_package
  | Data_model.Optimization_function_spread       -> spread_slow       ~with_packages c_l u_dt u_dk
  | Data_model.Optimization_function_none         -> Multi_objective.Satisfy


let optimization_function_full ?(with_packages = true) (universe : Data_model.universe option) (initial_configuration : Data_model.configuration option) (opt_fun : Data_model.optimization_function option) : Data_state.constraint_optimization_function = 
  match opt_fun with
  | None         -> None
  | Some opt_fun -> 
    match (universe, initial_configuration) with
    | (Some universe, Some configuration) -> Some(optimization_function ~with_packages universe configuration opt_fun)
    | _                                   -> None
