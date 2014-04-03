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
*)

open Data_common
open Data_constraint
open Data_model

(*******************************************)
(** 1. Helper functions                    *)
(*******************************************)

let get_provide_arity_safe t p = if Data_model.Port_id_set.mem p t#provide_domain then constant_of_provide_arity (t#provide p) else constant 0
let get_provide_arity      t p = constant_of_provide_arity (t#provide p)
let get_require_arity      t p = constant_of_require_arity (t#require p)

let get_consume x o = constant (x#consume o)

(* naming convention from the paper *)
let eNt    t   = Variable(Global_variable(Component_type(t)))
let eNp    p   = Variable(Global_variable(Port(p)))
let eNk    k   = Variable(Global_variable(Package(k)))

let eNlt l t   = Variable(Local_variable(l, Component_type(t)))
let eNlp l p   = Variable(Local_variable(l, Port(p)))
let eNlk l k   = Variable(Local_variable(l, Package(k)))

let eB p tp tr = Variable(Binding_variable(p,tp,tr))

let eR l r     = Variable(Local_repository_variable(l,r))
let eO l o     = Variable(Local_resource_variable(l,o))

let eU l       = Variable(Location_used_variable(l))



(*******************************************)
(** 2. Universe Translation                *)
(*******************************************)

(* flat model *)
let ralfs_redundant_require ~port_ids ~get_requirers ~get_providers ~get_component_type_require_arity = 
  Zephyrus_log.log_constraint_execution "Compute Ralf's redundant require constraints\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Component_type_id_set.fold (fun tr acc -> (
      let tr_arity_equal_zero = ( (eNt tr) =~ (constant 0) ) in
      let tr_require_arity = constant_of_require_arity (get_component_type_require_arity tr p) in
      let sum_of_tp_global_arities = sum (Data_model.Component_type_id_set.map_to_list (fun tp -> eNt tp) (get_providers p) ) in
      tr_arity_equal_zero ||~~ (tr_require_arity <=~ sum_of_tp_global_arities)
    )::acc) (get_requirers p) acc
  ) port_ids []

let flat_require ~port_ids ~get_requirers ~get_providers ~get_component_type_require_arity = 
  Zephyrus_log.log_constraint_execution "Compute binding requires\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Component_type_id_set.fold (fun tr acc -> (
        ((constant_of_require_arity (get_component_type_require_arity tr p)) *~ (eNt tr))
          <=~ (sum (Data_model.Component_type_id_set.fold (fun tp acc -> (eB p tp tr)::acc) (get_providers p) [])))::acc
    ) (get_requirers p) acc
  ) port_ids []

let require ~port_ids ~get_requirers ~get_providers ~get_component_type_require_arity = 
  let flat_require            = flat_require            port_ids get_requirers get_providers get_component_type_require_arity in
  let ralfs_redundant_require = ralfs_redundant_require port_ids get_requirers get_providers get_component_type_require_arity in
  flat_require @ (if Settings.find Settings.ralfs_redundant_constraints then ralfs_redundant_require else [])

let provide_with_fixed_infinity ~port_ids ~get_providers ~get_requirers ~get_component_type_provide_arity = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with naive encoding of infinity as a fixed value)\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Component_type_id_set.fold (fun tp acc -> (
          ((constant_of_provide_arity (get_component_type_provide_arity tp p)) *~ (eNt tp))
          >=~ (sum (Data_model.Component_type_id_set.fold (fun tr acc -> (eB p tp tr)::acc) (get_requirers p) [])))::acc
    ) (get_providers p) acc
  ) port_ids []

let provide_with_advanced_infinity ~port_ids ~get_providers ~get_requirers ~get_component_type_provide_arity = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with advanced encoding of infinity)\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Component_type_id_set.fold (fun tp acc ->
      let provide_arity = get_component_type_provide_arity tp p in
      match provide_arity with
      | Data_model.Finite_provide provide_arity ->
          (* If the component type [tp] is providing the port [p] with a finite arity: *)

          (* 1. Total number of bindings between components of type [tp] and any components which require port [p] 
                must be equal or smaller than the total arity of port [p] provided by all the components of type [tp] present. *)
          (((constant provide_arity) *~ (eNt tp)) >=~ (sum (Data_model.Component_type_id_set.map_to_list (fun tr -> eB p tp tr) (get_requirers p))))::acc
      | Data_model.Infinite_provide ->
          (* If the component type [tp] is providing the port [p] with an infinite arity: *)

          (* 1. If there is no component of type [tp] present then there can be no bidings on port [p] 
                between components of this type and components requiring port [p]. 
                In other words: if at least one component of type [tp] is present, 
                then the number of such bindings is not constrained at all. *)
          let if_there_is_none_there_can_be_no_bindings : konstraint list =
            Data_model.Component_type_id_set.map_to_list (fun tr ->
              ((eNt tp) =~ (constant 0)) =>~~ ((eB p tp tr) =~ (constant 0))
            ) (get_requirers p) in
          if_there_is_none_there_can_be_no_bindings @ acc
    ) (get_providers p) acc
  ) port_ids []

let binding ~port_ids ~get_requirers ~get_providers = 
  Zephyrus_log.log_constraint_execution "Compute binding unicitiy\n\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Component_type_id_set.fold (fun tr acc ->
      Data_model.Component_type_id_set.fold (fun tp acc ->
        ((eB p tp tr) <=~ ((eNt tr) *~ (eNt tp)))::acc
      ) (get_providers p) acc
    ) (get_requirers p) acc
  ) port_ids []

let conflict_naive ~port_ids ~get_conflicters ~get_component_type_provide_arity =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (naive encoding)\n\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Component_type_id_set.fold (fun t acc ->
      (((eNt t) >=~ (constant 1)) =>~~ ((eNp p) =~ (constant_of_provide_arity (get_component_type_provide_arity t p))))::acc
    ) (get_conflicters p) acc
  ) port_ids []

let conflict_advanced ~port_ids ~get_conflicters ~get_providers =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (advanced encoding)\n\n";
  Data_model.Port_id_set.fold (fun p acc ->
    let conflicters = get_conflicters p in
    let providers   = get_providers p in
    Data_model.Component_type_id_set.fold (fun t acc ->
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

        (there_can_be_only_one :: if_there_is_one_there_must_be_no_other_providers) @ acc

      else
        (* The component type [t] is NOT in conflict with itself on port [p], then: *)

        (* 1. if there is at least one instance of component [t], then
              no instances of component types providing port [p] may be present. *)
        let if_there_is_at_least_one_there_must_be_no_providers : konstraint list =
          Data_model.Component_type_id_set.map_to_list (fun provider ->
            ((eNt t) >~ (constant 0)) =>~~ ((eNt provider) =~ (constant 0))
          ) providers in

        if_there_is_at_least_one_there_must_be_no_providers @ acc
    ) conflicters acc
  ) port_ids []


(* location val *)
let location_component_type ~component_type_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute distribution component types\n";
  Data_model.Component_type_id_set.fold (fun t acc ->
    ((eNt t) =~ (sum (Data_model.Location_id_set.fold (fun l acc -> (eNlt l t)::acc) location_ids [])))::acc
  ) component_type_ids []
  
let location_package ~package_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute distribution packages\n";
  Data_model.Package_id_set.fold (fun k acc ->
    ((eNk k) =~ (sum (Data_model.Location_id_set.fold (fun l acc -> (eNlk l k)::acc) location_ids [])))::acc
  ) package_ids []

let location_port ~port_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute distribution ports\n";
  Data_model.Port_id_set.fold (fun p acc ->
    ((eNp p) =~ (sum (Data_model.Location_id_set.fold (fun l acc -> (eNlp l p)::acc) location_ids [])))::acc
  ) port_ids []

let location_port_equation ~port_ids ~location_ids ~get_providers ~get_component_type_provide_arity = 
  Zephyrus_log.log_constraint_execution "Compute implementation port\n";
  Data_model.Port_id_set.fold (fun p acc ->
    Data_model.Location_id_set.fold (fun l acc ->
      ((eNlp l p) =~ (sum (Data_model.Component_type_id_set.fold
                       (fun t acc -> ((constant_of_provide_arity (get_component_type_provide_arity t p)) *~ (eNlt l t))::acc) (get_providers p) [])))::acc
    ) location_ids acc
  ) port_ids []


  (* Repositories *)
let repository_unique ~location_ids ~repository_ids =
  Data_model.Location_id_set.fold (fun l acc ->
  Zephyrus_log.log_constraint_execution "Compute repository unicity\n";
    ((constant 1) =~ (sum (Data_model.Repository_id_set.fold (fun r acc -> (eR l r)::acc) repository_ids [])))::acc
  ) location_ids []

let repository_package ~location_ids ~repository_ids ~package_ids ~get_repository_packages =
  Zephyrus_log.log_constraint_execution "Compute packages local to repository\n";
  Data_model.Location_id_set.fold (fun l acc ->
    Data_model.Repository_id_set.fold (fun r acc ->
      (((eR l r) =~ (constant 1)) =>~~
        (conj (Data_model.Package_id_set.fold (fun k acc -> ((eNlk l k) =~ (constant 0))::acc) (Data_model.Package_id_set.diff package_ids (get_repository_packages r)) [])))::acc
    ) repository_ids acc
  ) location_ids []

  (* Package dependencies *)
let component_type_implementation ~location_ids ~component_type_ids ~get_implementation =
  Zephyrus_log.log_constraint_execution "Compute component type implementation\n";
  Data_model.Location_id_set.fold (fun l acc ->
    Data_model.Component_type_id_set.fold (fun t acc ->
      (((eNlt l t) >=~ (constant 1)) =>~~ ((sum (Data_model.Package_id_set.fold (fun k acc -> (eNlk l k)::acc) (get_implementation t) [])) >=~ (constant 1)))::acc
    ) component_type_ids acc
  ) location_ids []

let package_dependency ~location_ids ~package_ids ~get_package_depend =
  Zephyrus_log.log_constraint_execution "Compute package dependencies\n";
  Data_model.Location_id_set.fold (fun l acc ->
    Data_model.Package_id_set.fold (fun k1 acc ->
      Data_model.Package_id_set_set.fold (fun ks acc ->
        ((eNlk l k1) <=~ (sum (Data_model.Package_id_set.fold (fun k2 acc -> (eNlk l k2)::acc) ks [])))::acc
      ) (get_package_depend k1) acc
    ) package_ids acc
  ) location_ids []

let package_conflict ~location_ids ~package_ids ~get_package_conflict =
  Zephyrus_log.log_constraint_execution "Compute package conflicts\n";
  Data_model.Location_id_set.fold (fun l acc ->
    Data_model.Package_id_set.fold (fun k1 acc ->
      Data_model.Package_id_set.fold (fun k2 acc ->
        (((eNlk l k1) +~ (eNlk l k2)) <=~ (constant 1))::acc
      ) (get_package_conflict k1) acc
    ) package_ids acc
  ) location_ids []


  (* Resource consumptions *)
let resource_consumption ?(with_packages = true) ~location_ids ~resource_ids ~component_type_ids ~package_ids ~get_component_type_consume ~get_package_consume ~get_location_provide_resources =
  Zephyrus_log.log_constraint_execution "Compute resource consumption\n";
  Data_model.Location_id_set.fold (fun l acc ->
    Data_model.Resource_id_set.fold (fun o acc ->
      let resources_provided_constraint = ( (eO l o) =~ (constant_of_resource_provide_arity (get_location_provide_resources l o)) ) in
      let resources_consumed_constraint =
        let resources_consumed_packages   = sum (Data_model.Package_id_set       .map_to_list (fun k -> ((constant_of_resource_consume_arity (get_package_consume        k o)) *~ (eNlk l k)) ) package_ids) in
        let resources_consumed_components = sum (Data_model.Component_type_id_set.map_to_list (fun t -> ((constant_of_resource_consume_arity (get_component_type_consume t o)) *~ (eNlt l t)) ) component_type_ids) in
        let resources_consumed_total =
          if with_packages
          then resources_consumed_components +~ resources_consumed_packages
          else resources_consumed_components in
        resources_consumed_total <=~ (eO l o) in
      resources_provided_constraint::resources_consumed_constraint::acc
    ) resource_ids acc
  ) location_ids []

  (* Deprecated packages and component types *)
let deprecated_component_types_with_packages ?(with_packages = true) ~location_ids =
  Data_model.Location_id_set.fold (fun l acc ->
  Zephyrus_log.log_constraint_execution "Compute elements to delete\n";
    let deprecated_component_constraint = (eNlt l Data_model.deprecated_component_type_id) =~ (constant 0) in
    let deprecated_package_constraint   = (eNlk l Data_model.deprecated_package_id)        =~ (constant 0) in
    if with_packages
    then deprecated_component_constraint::deprecated_package_constraint::acc
    else deprecated_component_constraint::acc
  ) location_ids []

  (* Used locations *)
let used_locations ?(with_packages = true) ~component_type_ids ~package_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute used locations\n";
  let local_component_exprs l : expression list = Data_model.Component_type_id_set.fold (fun t acc -> (eNlt l t)::acc) component_type_ids [] in
  let local_package_exprs   l : expression list = Data_model.Package_id_set.fold        (fun k acc -> (eNlk l k)::acc) package_ids [] in
  let local_objects_exprs   l : expression list = 
    if with_packages
    then local_component_exprs l @ local_package_exprs l
    else local_component_exprs l in
  Data_model.Location_id_set.fold (fun l acc -> 
     let konstraint = ( eU l =~ reify ((sum (local_objects_exprs l)) >~ (constant 0)) ) in
     konstraint :: acc
  ) location_ids []

  (* Incompatibilities *)
let direct_incompatibilities ~incompatibilities ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute incompatibilities\n";
  Data_model.Location_id_set.fold (fun location_id acc ->
    Data_model.Repository_id_map.fold (fun repository_id repository_incompatibilities acc ->
      Data_model.Component_type_id_set_set.fold (fun incompatible_component_ids acc ->
        let repository_constraint = (eR location_id repository_id) =~ (constant 1) in
        let component_type_constraints = 
          Data_model.Component_type_id_set.fold (fun component_type_id l ->
            ((eNlt location_id component_type_id) >~ (constant 0)) :: l
          ) incompatible_component_ids [] in
        let konstraint = !~ (conj (repository_constraint :: component_type_constraints)) in
        konstraint :: acc
      ) repository_incompatibilities []
    ) incompatibilities []
  ) location_ids []
