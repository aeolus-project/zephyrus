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

(*/*****************************************\*)
(*|* 1. Helper functions                    |*)
(*\*****************************************/*)

let get_provide_arity_safe component_type_id port_id = if Port_id_set.mem port_id component_type_id#provide_domain then constant_of_provide_arity (component_type_id#provide port_id) else constant 0
let get_provide_arity      component_type_id port_id = constant_of_provide_arity (component_type_id#provide port_id)
let get_require_arity      component_type_id port_id = constant_of_require_arity (component_type_id#require port_id)

let get_consume x resource_id = constant (x#consume resource_id)

(* naming convention from the paper *)
let eNt    component_type_id = Variable(Global_variable(Component_type(component_type_id)))
let eNp    port_id           = Variable(Global_variable(Port(port_id)))
let eNk    package_id        = Variable(Global_variable(Package(package_id)))

let eNlt location_id component_type_id = Variable(Local_variable(location_id, Component_type(component_type_id)))
let eNlp location_id port_id           = Variable(Local_variable(location_id, Port(port_id)))
let eNlk location_id package_id        = Variable(Local_variable(location_id, Package(package_id)))

let eB portp_id providing_component_type_id portr_id requiring_component_type_id =
  Variable(Binding_variable(portp_id, providing_component_type_id, portr_id, requiring_component_type_id))

let eR location_id repository_id = Variable(Local_repository_variable(location_id, repository_id))
let eO location_id resource_id   = Variable(Local_resource_variable(location_id, resource_id))

let eU location_id = Variable(Location_used_variable(location_id))



(*/*****************************************\*)
(*|* 2. Universe Translation                |*)
(*\*****************************************/*)

(* flat model *)

let require ~port_ids ~get_supports ~get_requirers ~get_providers ~get_component_type_require_arity = 
  Zephyrus_log.log_constraint_execution "Compute binding requires\n";
  Port_id_set.fold (fun port_id acc ->                                                     (* the global land *)
    Component_type_id_set.fold (fun requiring_component_type_id acc -> (                   (* the land on requirers *)
          ((constant_of_require_arity (get_component_type_require_arity requiring_component_type_id port_id)) *~ (eNt requiring_component_type_id))
             <=~
          (sum (Port_id_set.fold (fun support_id acc ->                                    (* the sum on all support of port_id *)
                Component_type_id_set.fold (fun providing_component_type_id acc ->            (* the sum on all proviers of support_id *)
                  (eB support_id providing_component_type_id port_id requiring_component_type_id)::acc) (get_providers support_id) acc) (get_supports port_id) [])
          ))::acc
    ) (get_requirers port_id) acc
  ) port_ids []




let provide ~port_ids ~get_subports ~get_providers ~get_requirers ~get_component_type_provide_arity = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with advanced encoding of infinity)\n";
  Port_id_set.fold (fun port_id acc ->
    Component_type_id_set.fold (fun providing_component_type_id acc ->
      let provide_arity = get_component_type_provide_arity providing_component_type_id port_id in
      match provide_arity with
      | Finite_provide provide_arity -> (* as in the paper *)
          (((constant provide_arity) *~ (eNt providing_component_type_id))
          	>=~ 
          (sum (Port_id_set.fold (fun subport_id acc ->                                    (* the sum on all support of port_id *)
                Component_type_id_set.fold (fun requiring_component_type_id acc ->            (* the sum on all proviers of support_id *)
                  (eB port_id providing_component_type_id subport_id requiring_component_type_id)::acc) (get_requirers subport_id) acc) (get_subports port_id) [])
          ))::acc

      | Infinite_provide -> (* If the component type [providing_component_type] is providing the port [port_id] with an infinite arity: *)
          (* 1. If there is no component of type [providing_component_type] present then there can be no bidings on port [port_id] 
                between components of this type and components requiring port [port_id]. 
                In other words: if at least one component of type [providing_component_type] is present, 
                then the number of such bindings is not constrained at all. *)
            Port_id_set.fold(fun subport_id acc ->
              Component_type_id_set.fold (fun requiring_component_type_id acc ->
                (((eNt providing_component_type_id) =~ (constant 0)) =>~~ ((eB port_id providing_component_type_id subport_id requiring_component_type_id) =~ (constant 0)))::acc
            ) (get_requirers subport_id) acc) (get_subports port_id) acc
    ) (get_providers port_id) acc
  ) port_ids []


let binding ~port_ids ~get_supports ~get_requirers ~get_providers ~get_provide_domain = 
  Zephyrus_log.log_constraint_execution "Compute binding unicitiy\n\n";
  let get_extended_providers port_id = 
    let supports = get_supports port_id in
    Port_id_set.fold (fun support res -> Component_type_id_set.union (get_providers support) res) supports Component_type_id_set.empty in
  Port_id_set.fold (fun port_id acc ->
    Component_type_id_set.fold (fun requiring_component_type_id acc ->
      Component_type_id_set.fold (fun providing_component_type_id acc ->
          ((sum (Port_id_set.map_to_list (fun support_id -> eB support_id providing_component_type_id port_id requiring_component_type_id) (Port_id_set.inter (get_supports port_id) (get_provide_domain providing_component_type_id))))
        <=~
          ((eNt requiring_component_type_id) *~ (eNt providing_component_type_id)))::acc
      ) (get_extended_providers port_id) acc
    ) (get_requirers port_id) acc
  ) port_ids []





let conflict ~port_ids ~get_conflicters ~get_providers =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (advanced encoding)\n\n";
  Port_id_set.fold (fun port_id acc ->
    let conflicters = get_conflicters port_id in
    let providers   = get_providers   port_id in
    Component_type_id_set.fold (fun conflicter_component_type_id acc ->
      if Component_type_id_set.mem conflicter_component_type_id providers
      then
        (* If the component type [conflicter_component_type_id] is in conflict with itself on port [p], then: *)

        (* 1. there can be at most one instance of this component type, *)
        let there_can_be_only_one : konstraint = (eNt conflicter_component_type_id) <=~ (constant 1) in

        (* 2. if this single instance of component type [conflicter_component_type_id] is present, then
              no instances of OTHER component types providing port [p] may be present. *)
        let if_there_is_one_there_must_be_no_other_providers : konstraint list =
          Component_type_id_set.map_to_list (fun providing_component_type_id ->
            ((eNt conflicter_component_type_id) =~ (constant 1)) =>~~ ((eNt providing_component_type_id) =~ (constant 0))
          ) (Component_type_id_set.remove conflicter_component_type_id providers) in

        (there_can_be_only_one :: if_there_is_one_there_must_be_no_other_providers) @ acc

      else
        (* The component type [conflicter_component_type_id] is NOT in conflict with itself on port [p], then: *)

        (* 1. if there is at least one instance of component [conflicter_component_type_id], then
              no instances of component types providing port [p] may be present. *)
        let if_there_is_at_least_one_there_must_be_no_providers : konstraint list =
          Component_type_id_set.map_to_list (fun providing_component_type_id ->
            ((eNt conflicter_component_type_id) >~ (constant 0)) =>~~ ((eNt providing_component_type_id) =~ (constant 0))
          ) providers in

        if_there_is_at_least_one_there_must_be_no_providers @ acc
    ) conflicters acc
  ) port_ids []


(* location val *)
let location_component_type ~component_type_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute distribution component types\n";
  Component_type_id_set.fold (fun component_type_id acc ->
    ((eNt component_type_id) =~ (sum (Location_id_set.fold (fun location_id acc -> (eNlt location_id component_type_id)::acc) location_ids [])))::acc
  ) component_type_ids []
  
let location_package ~package_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute distribution packages\n";
  Package_id_set.fold (fun package_id acc ->
    ((eNk package_id) =~ (sum (Location_id_set.fold (fun location_id acc -> (eNlk location_id package_id)::acc) location_ids [])))::acc
  ) package_ids []

let location_port ~port_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute distribution ports\n";
  Port_id_set.fold (fun port_id acc ->
    ((eNp port_id) =~ (sum (Location_id_set.fold (fun location_id acc -> (eNlp location_id port_id)::acc) location_ids [])))::acc
  ) port_ids []

let location_port_equation ~port_ids ~location_ids ~get_providers ~get_component_type_provide_arity = 
  Zephyrus_log.log_constraint_execution "Compute implementation port\n";
  Port_id_set.fold (fun port_id acc ->
    Location_id_set.fold (fun location_id acc ->
      ((eNlp location_id port_id) =~ (sum (Component_type_id_set.fold
                       (fun providing_component_type_id acc -> ((constant_of_provide_arity (get_component_type_provide_arity providing_component_type_id port_id)) *~ (eNlt location_id providing_component_type_id))::acc) (get_providers port_id) [])))::acc
    ) location_ids acc
  ) port_ids []


  (* Repositories *)
let repository_unique ~location_ids ~repository_ids =
  Location_id_set.fold (fun location_id acc ->
  Zephyrus_log.log_constraint_execution "Compute repository unicity\n";
    ((constant 1) =~ (sum (Repository_id_set.fold (fun repository_id acc -> (eR location_id repository_id)::acc) repository_ids [])))::acc
  ) location_ids []

let repository_package ~location_ids ~repository_ids ~package_ids ~get_repository_packages =
  Zephyrus_log.log_constraint_execution "Compute packages local to repository\n";
  Location_id_set.fold (fun location_id acc ->
    Repository_id_set.fold (fun repository_id acc ->
      (((eR location_id repository_id) =~ (constant 1)) =>~~
        (conj (Package_id_set.fold (fun package_id acc -> ((eNlk location_id package_id) =~ (constant 0))::acc) (Package_id_set.diff package_ids (get_repository_packages repository_id)) [])))::acc
    ) repository_ids acc
  ) location_ids []

  (* Package dependencies *)
let component_type_implementation ~location_ids ~component_type_ids ~get_implementation =
  Zephyrus_log.log_constraint_execution "Compute component type implementation\n";
  Location_id_set.fold (fun location_id acc ->
    Component_type_id_set.fold (fun implemented_component_type acc ->
      (((eNlt location_id implemented_component_type) >=~ (constant 1)) 
      	=>~~ 
      ((sum (Package_id_set.fold (fun package_id acc -> (eNlk location_id package_id)::acc) (get_implementation implemented_component_type) [])) >=~ (constant 1)))::acc
    ) component_type_ids acc
  ) location_ids []

let package_dependency ~location_ids ~package_ids ~get_package_depend =
  Zephyrus_log.log_constraint_execution "Compute package dependencies\n";
  Location_id_set.fold (fun location_id acc ->
    Package_id_set.fold (fun package_id_1 acc ->
      Package_id_set_set.fold (fun depended_on_package_ids acc ->
        ((eNlk location_id package_id_1) <=~ (sum (Package_id_set.fold (fun package_id_2 acc -> (eNlk location_id package_id_2)::acc) depended_on_package_ids [])))::acc
      ) (get_package_depend package_id_1) acc
    ) package_ids acc
  ) location_ids []

let package_conflict ~location_ids ~package_ids ~get_package_conflict =
  Zephyrus_log.log_constraint_execution "Compute package conflicts\n";
  Location_id_set.fold (fun location_id acc ->
    Package_id_set.fold (fun package_id_1 acc ->
      Package_id_set.fold (fun package_id_2 acc ->
        (((eNlk location_id package_id_1) +~ (eNlk location_id package_id_2)) <=~ (constant 1))::acc
      ) (get_package_conflict package_id_1) acc
    ) package_ids acc
  ) location_ids []


  (* Resource consumptions *)
let resource_consumption ?(with_packages = true) ~location_ids ~resource_ids ~component_type_ids ~package_ids ~get_component_type_consume ~get_package_consume ~get_location_provide_resources =
  Zephyrus_log.log_constraint_execution "Compute resource consumption\n";
  Location_id_set.fold (fun location_id acc ->
    Resource_id_set.fold (fun resource_id acc ->
      let resources_provided_constraint = ( (eO location_id resource_id) =~ (constant_of_resource_provide_arity (get_location_provide_resources location_id resource_id)) ) in
      let resources_consumed_constraint =
        let resources_consumed_packages   = sum (Package_id_set       .map_to_list (fun package_id        -> ((constant_of_resource_consume_arity (get_package_consume        package_id        resource_id)) *~ (eNlk location_id package_id       )) ) package_ids       ) in
        let resources_consumed_components = sum (Component_type_id_set.map_to_list (fun component_type_id -> ((constant_of_resource_consume_arity (get_component_type_consume component_type_id resource_id)) *~ (eNlt location_id component_type_id)) ) component_type_ids) in
        let resources_consumed_total =
          if with_packages
          then resources_consumed_components +~ resources_consumed_packages
          else resources_consumed_components in
        resources_consumed_total <=~ (eO location_id resource_id) in
      resources_provided_constraint::resources_consumed_constraint::acc
    ) resource_ids acc
  ) location_ids []

  (* Deprecated packages and component types *)
let deprecated_component_types_with_packages ?(with_packages = true) ~location_ids =
  Location_id_set.fold (fun location_id acc ->
  Zephyrus_log.log_constraint_execution "Compute elements to delete\n";
    let deprecated_component_constraint = (eNlt location_id deprecated_component_type_id) =~ (constant 0) in
    let deprecated_package_constraint   = (eNlk location_id deprecated_package_id)        =~ (constant 0) in
    if with_packages
    then deprecated_component_constraint::deprecated_package_constraint::acc
    else deprecated_component_constraint::acc
  ) location_ids []

  (* Used locations *)
let used_locations ?(with_packages = true) ~component_type_ids ~package_ids ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute used locations\n";
  let local_component_exprs location_id : expression list = Component_type_id_set.fold (fun component_type_id acc -> (eNlt location_id component_type_id)::acc) component_type_ids [] in
  let local_package_exprs   location_id : expression list = Package_id_set.fold        (fun package_id        acc -> (eNlk location_id package_id       )::acc) package_ids        [] in
  let local_objects_exprs   location_id : expression list = 
    if with_packages
    then local_component_exprs location_id @ local_package_exprs location_id
    else local_component_exprs location_id in
  Location_id_set.fold (fun location_id acc -> 
     let konstraint = ( eU location_id =~ reify ((sum (local_objects_exprs location_id)) >~ (constant 0)) ) in
     konstraint :: acc
  ) location_ids []

  (* Incompatibilities *)
let direct_incompatibilities ~incompatibilities ~location_ids =
  Zephyrus_log.log_constraint_execution "Compute incompatibilities\n";
  Location_id_set.fold (fun location_id acc ->
    Repository_id_map.fold (fun repository_id repository_incompatibilities acc ->
      Component_type_id_set_set.fold (fun incompatible_component_ids acc ->
        let repository_constraint = (eR location_id repository_id) =~ (constant 1) in
        let component_type_constraints = 
          Component_type_id_set.fold (fun component_type_id acc ->
            ((eNlt location_id component_type_id) >~ (constant 0)) :: acc
          ) incompatible_component_ids [] in
        let konstraint = !~ (conj (repository_constraint :: component_type_constraints)) in
        konstraint :: acc
      ) repository_incompatibilities []
    ) incompatibilities []
  ) location_ids []


(*/*****************************************\*)
(*|* 3. Annex Constraints (to guide solver) |*)
(*\*****************************************/*)

(*let ralfs_redundant_require ~port_ids ~get_requirers ~get_providers ~get_component_type_require_arity = 
  Zephyrus_log.log_constraint_execution "Compute Ralf's redundant require constraints\n";
  Port_id_set.fold (fun port_id acc ->
    Component_type_id_set.fold (fun requiring_component_type_id acc -> (
      let requiring_component_type_arity_equal_zero : konstraint = ( (eNt requiring_component_type_id) =~ (constant 0) ) in
      let requiring_component_type_require_arity : expression = constant_of_require_arity (get_component_type_require_arity requiring_component_type_id port_id) in
      let sum_of_providing_component_type_global_arities : expression = sum (Component_type_id_set.map_to_list (fun providing_component_type -> eNt providing_component_type) (get_providers port_id) ) in
      requiring_component_type_arity_equal_zero ||~~ (requiring_component_type_require_arity <=~ sum_of_providing_component_type_global_arities)
    )::acc) (get_requirers port_id) acc
  ) port_ids []*)

(*
let require ~port_ids ~get_requirers ~get_providers ~get_component_type_require_arity = 
  let flat_require            = flat_require            port_ids get_requirers get_providers get_component_type_require_arity in
  let ralfs_redundant_require = ralfs_redundant_require port_ids get_requirers get_providers get_component_type_require_arity in
  flat_require @ (if Settings.find Settings.ralfs_redundant_constraints then ralfs_redundant_require else [])
*)



(* (* DEPRECATED, mow using the version with true management of infinity *)
let provide_with_fixed_infinity ~port_ids ~get_providers ~get_requirers ~get_component_type_provide_arity = 
  Zephyrus_log.log_constraint_execution "Compute binding provides (with naive encoding of infinity as a fixed value)\n";
  Port_id_set.fold (fun port_id acc ->
    Component_type_id_set.fold (fun providing_component_type_id acc -> (
          ((constant_of_provide_arity (get_component_type_provide_arity providing_component_type_id port_id)) *~ (eNt providing_component_type_id))
          >=~ (sum (Component_type_id_set.fold (fun requiring_component_type_id acc -> (eB port_id providing_component_type_id requiring_component_type_id)::acc) (get_requirers port_id) [])))::acc
    ) (get_providers port_id) acc
  ) port_ids [] *)


(*let conflict_naive ~port_ids ~get_conflicters ~get_component_type_provide_arity =
  Zephyrus_log.log_constraint_execution "Compute binding conflicts (naive encoding)\n\n";
  Port_id_set.fold (fun port_id acc ->
    Component_type_id_set.fold (fun conflicter_component_type_id acc ->
      (((eNt conflicter_component_type_id) >=~ (constant 1)) 
      	=>~~ 
      ((eNp port_id) =~ (constant_of_provide_arity (get_component_type_provide_arity conflicter_component_type_id port_id))))::acc
    ) (get_conflicters port_id) acc
  ) port_ids []
*)

