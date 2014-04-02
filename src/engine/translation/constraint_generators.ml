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
let ralfs_redundant_require (u_dp : Port_id_set.t) (ur : port_id -> Component_type_id_set.t) (up : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) = 
  Zephyrus_log.log_constraint_execution "Compute Ralf's redundant require constraints\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res -> (
      let tr_arity_equal_zero = ( (eNt tr) =~ (constant 0) ) in
      let tr_require_arity = get_require_arity (get_component_type tr) p in
      let sum_of_tp_global_arities = sum (Data_model.Component_type_id_set.map_to_list (fun tp -> eNt tp) (up p) ) in
      tr_arity_equal_zero ||~~ (tr_require_arity <=~ sum_of_tp_global_arities)
    )::res) (ur p) res
  ) u_dp []

let flat_require (u_dp : Port_id_set.t) (ur : port_id -> Component_type_id_set.t) (up : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) = 
  Zephyrus_log.log_constraint_execution "Compute binding requires\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res -> (
        ((get_require_arity (get_component_type tr) p) *~ (eNt tr))
          <=~ (sum (Data_model.Component_type_id_set.fold (fun tp res -> (eB p tp tr)::res) (up p) [])))::res
    ) (ur p) res
  ) u_dp []

let require (u_dp : Port_id_set.t) (ur : port_id -> Component_type_id_set.t) (up : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) = 
  let flat_require            = flat_require            u_dp ur up get_component_type in
  let ralfs_redundant_require = ralfs_redundant_require u_dp ur up get_component_type in
  flat_require @ (if Settings.find Settings.ralfs_redundant_constraints then ralfs_redundant_require else [])

let provide_with_fixed_infinity (u_dp : Port_id_set.t) (up : port_id -> Component_type_id_set.t) (ur : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with naive encoding of infinity as a fixed value)\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tp res -> (
          ((get_provide_arity (get_component_type tp) p) *~ (eNt tp))
          >=~ (sum (Data_model.Component_type_id_set.fold (fun tr res -> (eB p tp tr)::res) (ur p) [])))::res
    ) (up p) res
  ) u_dp []

let provide_with_advanced_infinity (u_dp : Port_id_set.t) (up : port_id -> Component_type_id_set.t) (ur : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) = 
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

let binding (u_dp : Port_id_set.t) (ur : port_id -> Component_type_id_set.t) (up : port_id -> Component_type_id_set.t) = 
  Zephyrus_log.log_constraint_execution "Compute binding unicitiy\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun tr res ->
      Data_model.Component_type_id_set.fold (fun tp res ->
        ((eB p tp tr) <=~ ((eNt tr) *~ (eNt tp)))::res
      ) (up p) res
    ) (ur p) res
  ) u_dp []

let conflict_naive (u_dp : Port_id_set.t) (uc : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (naive encoding)\n\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNt t) >=~ (constant 1)) =>~~ ((eNp p) =~ (get_provide_arity_safe (get_component_type t) p)))::res
    ) (uc p) res
  ) u_dp []

let conflict_advanced (u_dp : Port_id_set.t) (uc : port_id -> Component_type_id_set.t) (up : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) =
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
let location_component_type (u_dt : Component_type_id_set.t) (c_l : Location_id_set.t) =
  Zephyrus_log.log_constraint_execution "Compute distribution component types\n";
  Data_model.Component_type_id_set.fold (fun t res ->
    ((eNt t) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlt l t)::res) c_l [])))::res
  ) u_dt []
  
let location_package (u_dk : Package_id_set.t) (c_l : Location_id_set.t) =
  Zephyrus_log.log_constraint_execution "Compute distribution packages\n";
  Data_model.Package_id_set.fold (fun k res ->
    ((eNk k) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlk l k)::res) c_l [])))::res
  ) u_dk []

let location_port (u_dp : Port_id_set.t) (c_l : Location_id_set.t) =
  Zephyrus_log.log_constraint_execution "Compute distribution ports\n";
  Data_model.Port_id_set.fold (fun p res ->
    ((eNp p) =~ (sum (Data_model.Location_id_set.fold (fun l res -> (eNlp l p)::res) c_l [])))::res
  ) u_dp []

let location_port_equation (u_dp : Port_id_set.t) (c_l : Location_id_set.t) (up : port_id -> Component_type_id_set.t) (get_component_type : component_type_id -> component_type) = 
  Zephyrus_log.log_constraint_execution "Compute implementation port\n";
  Data_model.Port_id_set.fold (fun p res ->
    Data_model.Location_id_set.fold (fun l res ->
      ((eNlp l p) =~ (sum (Data_model.Component_type_id_set.fold
                       (fun t res -> ((get_provide_arity (get_component_type t) p) *~ (eNlt l t))::res) (up p) [])))::res
    ) c_l res
  ) u_dp []


  (* Repositories *)
let repository_unique (c_l : Location_id_set.t) (u_dr : Repository_id_set.t) =
  Data_model.Location_id_set.fold (fun l res ->
  Zephyrus_log.log_constraint_execution "Compute repository unicity\n";
    ((constant 1) =~ (sum (Data_model.Repository_id_set.fold (fun r res -> (eR l r)::res) u_dr [])))::res
  ) c_l []

let repository_package (c_l : Location_id_set.t) (u_dr : Repository_id_set.t) (u_dk : Package_id_set.t) (get_packages : repository_id -> Package_id_set.t) =
  Zephyrus_log.log_constraint_execution "Compute packages local to repository\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Repository_id_set.fold (fun r res ->
      (((eR l r) =~ (constant 1)) =>~~
        (conj (Data_model.Package_id_set.fold (fun k res -> ((eNlk l k) =~ (constant 0))::res) (Data_model.Package_id_set.diff u_dk (get_packages r)) [])))::res
    ) u_dr res
  ) c_l []

  (* Package dependencies *)
let component_type_implementation (c_l : Location_id_set.t) (u_dt : Component_type_id_set.t) (u_i : component_type_id -> Package_id_set.t) =
  Zephyrus_log.log_constraint_execution "Compute component type implementation\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Component_type_id_set.fold (fun t res ->
      (((eNlt l t) >=~ (constant 1)) =>~~ ((sum (Data_model.Package_id_set.fold (fun k res -> (eNlk l k)::res) (u_i t) [])) >=~ (constant 1)))::res
    ) u_dt res
  ) c_l []

let package_dependency (c_l : Location_id_set.t) (u_dk : Package_id_set.t) (get_package : package_id -> package) =
  Zephyrus_log.log_constraint_execution "Compute package dependencies\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k1 res ->
      Data_model.Package_id_set_set.fold (fun ks res ->
        ((eNlk l k1) <=~ (sum (Data_model.Package_id_set.fold (fun k2 res -> (eNlk l k2)::res) ks [])))::res
      ) ((get_package k1)#depend) res
    ) u_dk res
  ) c_l []

let package_conflict (c_l : Location_id_set.t) (u_dk : Package_id_set.t) (get_package : package_id -> package) =
  Zephyrus_log.log_constraint_execution "Compute package conflicts\n";
  Data_model.Location_id_set.fold (fun l res ->
    Data_model.Package_id_set.fold (fun k1 res ->
      Data_model.Package_id_set.fold (fun k2 res ->
        (((eNlk l k1) +~ (eNlk l k2)) <=~ (constant 1))::res
      ) ((get_package k1)#conflict) res
    ) u_dk res
  ) c_l []


  (* Resource consumptions *)
let resource_consumption ?(with_packages = true) (c_l : Location_id_set.t) (resource_ids : Resource_id_set.t) (u_dt : Component_type_id_set.t) (u_dk : Package_id_set.t) (get_component_type : component_type_id -> component_type) (get_package : package_id -> package) (get_location : location_id -> location) =
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
let deprecated_component_types_with_packages ?(with_packages = true) (c_l : Location_id_set.t) =
  Data_model.Location_id_set.fold (fun l res ->
  Zephyrus_log.log_constraint_execution "Compute elements to delete\n";
    let deprecated_component_constraint = (eNlt l Data_model.deprecated_component_type_id) =~ (constant 0) in
    let deprecated_package_constraint   = (eNlk l Data_model.deprecated_package_id)        =~ (constant 0) in
    if with_packages
    then deprecated_component_constraint::deprecated_package_constraint::res
    else deprecated_component_constraint::res
  ) c_l []

  (* Used locations *)
let used_locations ?(with_packages = true) (u_dt : Component_type_id_set.t) (u_dk : Package_id_set.t) (c_l : Location_id_set.t) =
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
let incompatibilities (universe_incompatibilities : Data_model.Component_type_id_set_set.t Data_model.Repository_id_map.t) (c_l : Location_id_set.t) =
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
