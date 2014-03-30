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

(** Different kinds of validation operations which can be performed on the Zephyrus model. *)

(* Depends on
    - datatypes/Data_model
*)

open Data_model

type model_inconsistency_test =
  (* - Universe - *)
  (* Component type *)
  | Component_type_double_id                    of component_type_id                                 (* Multiple component types with the same id exist. *)
  | Provided_port_missing                       of component_type_id * port_id                       (* A component type provides a port which does not exist. *)
  | Required_port_missing                       of component_type_id * port_id                       (* A component type requires a port which does not exist. *)
  | Conflict_port_missing                       of component_type_id * port_id                       (* A component type conflicts with a port which does not exist. *)
  | Consumed_resource_missing                   of component_type_id * resource_id                   (* A component type consumes a resource which does not exist. *)
  (* Repository *)
  | Repository_double_id                        of repository_id                                     (* Multiple repositories with the same id exist. *)
  (* Package *)
  | Package_double_id                           of repository_id * package_id                        (* Multiple component types with the same id exist. *)
  | Package_depend_missing                      of repository_id * package_id * package_id           (* A package depends on another package which does not exist *)
  | Package_conflict_missing                    of repository_id * package_id * package_id           (* A package conflicts with another package which does not exist *)
  | Package_consumed_resource_missing           of repository_id * package_id * resource_id          (* A package consumes a resource which does not exist *)
  (* Implementation *)
  | Implementation_double                       of component_type_id                                 (* Multiple implementations for one component type exist. *)
  | Implementation_for_a_component_type_missing of component_type_id                                 (* A component type has no implementation declared. *)
  | Component_type_in_implementation_missing    of component_type_id                                 (* A component type referenced in the implementation part does not exist. *)
  | Implementation_package_missing              of component_type_id * package_id                    (* A package referenced in the implementation part does not exist. *)
(*| Implementation_repository_missing           of component_type_id * repository_id                 (* A repository referenced in the implementation part does not exist. *)*)
(*| Implementation_package_missing              of component_type_id * repository_id * package_id    (* A repository referenced in the implementation part does not contain a referenced package. *)*)
  (* - Configuration - *)
  (* Location *)
  | Location_double_id                          of location_id                                       (* Multiple locations with the same id exist. *)
  | Location_repository_missing                 of location_id * repository_id                       (* A repository referenced by a location does not exist. *)
  | Location_installed_package_missing          of location_id * repository_id * package_id          (* A package referenced by a location does not exist. *)
  | Location_provided_resource_missing          of location_id * resource_id                         (* A location provides a resource which does not exist. *)
  (* Component *)
  | Component_double_id                         of component_id                                      (* Multiple components with the same id exist. *)
  | Component_type_missing                      of component_id * component_type_id                  (* A component type referenced by a component does not exist. *)
  | Component_location_missing                  of component_id * location_id                        (* A location referenced by a component does not exist. *)
  (* Binding *)
  | Binding_port_missing                        of port_id           * component_id * component_id   (* The port referenced by a binding does not exist. *)
  | Binding_requirer_missing                    of component_type_id * component_id * component_id   (* The requiring component referenced by a binding does not exist. *)
  | Binding_provider_missing                    of component_type_id * component_id * component_id   (* The providing component referenced by a binding does not exist. *)
  (* - Specification - *)
  | Specification_port_missing                  of port_id                                           (* The port referenced in the specification does not exist. *)
  | Specification_component_type_missing        of component_type_id                                 (* The component type referenced in the specification does not exist. *)
  | Specification_package_missing               of package_id                                        (* The package referenced in the specification does not exist. *)

type configuration_validation_test =
  | Require_not_satisfied                       of component_id * port_id                                                      (* There is an unsatisfied require port on a certain component. *)
  | Provide_abused                              of component_id * port_id                                                      (* There is an abused provide port on a certain component. *)
  | Binding_unicity_violated                    of port_id * component_id * component_id                                       (* There are multiple bindings on the same port between two components. *)
  | Conflict_exists                             of component_id * port_id * component_id                                       (* There are two components which are in conflict. *)
  | Package_dependencies_not_satisfied          of location_id * package_id                                                    (* A package has some unsatisfied dependencies. *)
  | Package_in_conflict                         of location_id * package_id * package_id                                       (* A package is in conflict with another package. *)
  | Resource_overconsumed                       of location_id * resource_id * resource_consume_arity * resource_provide_arity (* There is a location where more of certain resource is consumed than provided. *)
  | Component_not_implemented                   of location_id * component_id                                                  (* There is a component which is not implemented by none  *)

type specification_validation_test =
  | Specification_constraints_not_satisfied

type validation_test =
  | Model_inconsistent          of model_inconsistency_test
  | Configuration_invalid       of configuration_validation_test
  | Specification_not_satisfied of specification_validation_test

type validation_result =
  | Ok      of validation_test
  | Failed of validation_test

module String_of = struct

  module String_of = String_of.Quoted_name_string_of

  module Test_failed = struct

    let model_inconsistency_test = function
    (* Component type *)
    | Component_type_double_id  (component_type_id)                                                -> Printf.sprintf "component type %s is defined multiple times"                                        (String_of.component_type_id component_type_id)
    | Provided_port_missing     (component_type_id, port_id)                                       -> Printf.sprintf "component type %s is providing port %s which does not exist in the universe"        (String_of.component_type_id component_type_id) (String_of.port_id port_id)
    | Required_port_missing     (component_type_id, port_id)                                       -> Printf.sprintf "component type %s is requiring port %s which does not exist in the universe"        (String_of.component_type_id component_type_id) (String_of.port_id port_id)
    | Conflict_port_missing     (component_type_id, port_id)                                       -> Printf.sprintf "component type %s is in conflict with port %s which does not exist in the universe" (String_of.component_type_id component_type_id) (String_of.port_id port_id)
    | Consumed_resource_missing (component_type_id, resource_id)                                   -> Printf.sprintf "component type %s is consuming resource %s which does not exist in the universe"    (String_of.component_type_id component_type_id) (String_of.resource_id resource_id)
    (* Repository *)
    | Repository_double_id  (repository_id)                                                        -> Printf.sprintf "repository %s is defined multiple times" (String_of.repository_id repository_id)
    (* Package *)
    | Package_double_id                 (repository_id, package_id)                                -> Printf.sprintf "package %s from repository %s is defined multiple times"                                              (String_of.package_id package_id) (String_of.repository_id repository_id)
    | Package_depend_missing            (repository_id, package_id, depended_on_package_id)        -> Printf.sprintf "package %s from repository %s depends on package %s which does not exist in this repository"          (String_of.package_id package_id) (String_of.repository_id repository_id) (String_of.package_id depended_on_package_id)
    | Package_conflict_missing          (repository_id, package_id, conflicted_package_id)         -> Printf.sprintf "package %s from repository %s is in conflict with package %s which does not exist in this repository" (String_of.package_id package_id) (String_of.repository_id repository_id) (String_of.package_id conflicted_package_id)
    | Package_consumed_resource_missing (repository_id, package_id, resource_id)                   -> Printf.sprintf "package %s from repository %s is consuming resource %s which does not exist in the universe"          (String_of.package_id package_id) (String_of.repository_id repository_id) (String_of.resource_id resource_id)
    (* Implementation *)
    | Implementation_double                       (component_type_id)                              -> Printf.sprintf "implementation for component type %s is provided multiple times"                                                              (String_of.component_type_id component_type_id)
    | Implementation_for_a_component_type_missing (component_type_id)                              -> Printf.sprintf "component type %s has no implementation provided"                                                                             (String_of.component_type_id component_type_id)
    | Component_type_in_implementation_missing    (component_type_id)                              -> Printf.sprintf "there is an implementation provided for component type %s which does not exist in the universe"                               (String_of.component_type_id component_type_id)
    | Implementation_package_missing              (component_type_id, package_id)                  -> Printf.sprintf "the implementation for component type %s mentions the package %s which does not exist in the universe"                        (String_of.component_type_id component_type_id) (String_of.package_id package_id)
  (*| Implementation_repository_missing           (component_type_id, repository_id)               -> Printf.sprintf "the implementation for component type %s mentions the repository %s which does not exist in the universe"                     (String_of.component_type_id component_type_id) (String_of.repository_id repository_id)*)
  (*| Implementation_package_missing              (component_type_id, repository_id, package_id)   -> Printf.sprintf "the implementation for component type %s mentions the repository %s and a package %s which does not exist in this repository" (String_of.component_type_id component_type_id) (String_of.repository_id repository_id) (String_of.package_id package_id)*)
    (* Location *)
    | Location_double_id                 (location_id)                                             -> Printf.sprintf "location %s is defined multiple times"                                                               (String_of.location_id location_id)
    | Location_repository_missing        (location_id, repository_id)                              -> Printf.sprintf "location %s is using the repository %s which does not exist in the universe"                         (String_of.location_id location_id) (String_of.repository_id repository_id)
    | Location_installed_package_missing (location_id, repository_id, package_id)                  -> Printf.sprintf "location %s, using the repository %s, contains a package %s which does not exist in this repository" (String_of.location_id location_id) (String_of.repository_id repository_id) (String_of.package_id package_id)
    | Location_provided_resource_missing (location_id, resource_id)                                -> Printf.sprintf "location %s is providing resource %s which does not exist in the universe"                           (String_of.location_id location_id) (String_of.resource_id resource_id)
    (* Component *)
    | Component_double_id        (component_id)                                                    -> Printf.sprintf "component %s is defined multiple times"                                              (String_of.component_id component_id)
    | Component_type_missing     (component_id, component_type_id)                                 -> Printf.sprintf "component %s is of component type %s which does not exist in the universe"           (String_of.component_id component_id) (String_of.component_type_id component_type_id)
    | Component_location_missing (component_id, location_id)                                       -> Printf.sprintf "component %s is placed on the location %s which does not exist in the configuration" (String_of.component_id component_id) (String_of.location_id location_id)
    (* Binding *)
    | Binding_port_missing     (port_id, requiring_component_id, providing_component_id)           -> Printf.sprintf "in the binding on port %s between requiring component %s and providing component %s the bound port does not exist in the universe" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id providing_component_id)
    | Binding_requirer_missing (port_id, requiring_component_id, providing_component_id)           -> Printf.sprintf "in the binding on port %s between requiring component %s and providing component %s the requiring component does not exist in the configuration" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id providing_component_id)
    | Binding_provider_missing (port_id, requiring_component_id, providing_component_id)           -> Printf.sprintf "in the binding on port %s between requiring component %s and providing component %s the providing component does not exist in the configuration" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id providing_component_id)
    (* Specification *)
    | Specification_port_missing           (port_id)                                               -> Printf.sprintf "the port %s referenced in the specification does not exist in the universe"              (String_of.port_id port_id)
    | Specification_component_type_missing (component_type_id)                                     -> Printf.sprintf "the component_type_id %s referenced in the specification does not exist in the universe" (String_of.component_type_id component_type_id)
    | Specification_package_missing        (package_id)                                            -> Printf.sprintf "the package_id %s referenced in the specification does not exist in the universe"        (String_of.package_id package_id)


    let configuration_validation_test = function
    | Require_not_satisfied              (component_id, port_id)                                   -> Printf.sprintf "component's %s requirements on port %s are not satisfied (not enough bindings)"                                  (String_of.component_id component_id) (String_of.port_id port_id)
    | Provide_abused                     (component_id, port_id)                                   -> Printf.sprintf "component's %s provides on port %s are abused (too much bindings)"                                               (String_of.component_id component_id) (String_of.port_id port_id)
    | Binding_unicity_violated           (port_id, requiring_component_id, prividing_component_id) -> Printf.sprintf "binding unicity on port %s between requiring component %s and providing component %s is violated"                (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id prividing_component_id)
    | Conflict_exists                    (component_id, port_id, conflicted_component_id)          -> Printf.sprintf "a conflict exists between component %s, which has a conflict port %s, and component %s which provides this port" (String_of.component_id component_id) (String_of.port_id port_id) (String_of.component_id conflicted_component_id)
    | Package_dependencies_not_satisfied (location_id, package_id)                                 -> Printf.sprintf "dependencies of package %s on location %s are not satisfied"                                                     (String_of.package_id package_id) (String_of.location_id location_id)
    | Package_in_conflict                (location_id, package_id, conflicted_package_id)          -> Printf.sprintf "package %s on location %s is in conflict with package %s (on the same location)"                                 (String_of.package_id package_id) (String_of.location_id location_id) (String_of.package_id conflicted_package_id)
    | Resource_overconsumed              (location_id, resource_id, consumed, provided)            -> Printf.sprintf "there is more of resource %s consumed (%s) than provided (%s) on location %s"                                    (String_of.resource_id resource_id) (String_of.resource_consume_arity consumed) (String_of.resource_provide_arity provided) (String_of.location_id location_id)
    | Component_not_implemented          (location_id, component_id)                               -> Printf.sprintf "component %s on location %s is not implemented (none of his implementing packages is present on that location)"  (String_of.component_id component_id) (String_of.location_id location_id)

    let specification_validation_test = function
    | Specification_constraints_not_satisfied -> Printf.sprintf "specification constraints are not satisfied"

    let validation_test = function
    | Model_inconsistent          test -> Printf.sprintf "model inconsistent: %s"          (model_inconsistency_test      test)
    | Configuration_invalid       test -> Printf.sprintf "configuration invalid: %s"       (configuration_validation_test test)
    | Specification_not_satisfied test -> Printf.sprintf "specification not satisfied: %s" (specification_validation_test test)

  end

  module Test_ok = struct

    let model_inconsistency_test = function
    (* Component type *)
    | Component_type_double_id  (component_type_id)                                                -> Printf.sprintf "component type %s is defined exactly one time"                                       (String_of.component_type_id component_type_id)
    | Provided_port_missing     (component_type_id, port_id)                                       -> Printf.sprintf "component type %s is providing port %s which is well defined in the universe"        (String_of.component_type_id component_type_id) (String_of.port_id port_id)
    | Required_port_missing     (component_type_id, port_id)                                       -> Printf.sprintf "component type %s is requiring port %s which is well defined in the universe"        (String_of.component_type_id component_type_id) (String_of.port_id port_id)
    | Conflict_port_missing     (component_type_id, port_id)                                       -> Printf.sprintf "component type %s is in conflict with port %s which is well defined in the universe" (String_of.component_type_id component_type_id) (String_of.port_id port_id)
    | Consumed_resource_missing (component_type_id, resource_id)                                   -> Printf.sprintf "component type %s is consuming resource %s which is well defined in the universe"    (String_of.component_type_id component_type_id) (String_of.resource_id resource_id)
    (* Repository *)
    | Repository_double_id  (repository_id)                                                        -> Printf.sprintf "repository %s is defined exactly one time" (String_of.repository_id repository_id)
    (* Package *)
    | Package_double_id                 (repository_id, package_id)                                -> Printf.sprintf "package %s from repository %s is defined exactly one time"                                             (String_of.package_id package_id) (String_of.repository_id repository_id)
    | Package_depend_missing            (repository_id, package_id, depended_on_package_id)        -> Printf.sprintf "package %s from repository %s depends on package %s which is well present in this repository"          (String_of.package_id package_id) (String_of.repository_id repository_id) (String_of.package_id depended_on_package_id)
    | Package_conflict_missing          (repository_id, package_id, conflicted_package_id)         -> Printf.sprintf "package %s from repository %s is in conflict with package %s which is well present in this repository" (String_of.package_id package_id) (String_of.repository_id repository_id) (String_of.package_id conflicted_package_id)
    | Package_consumed_resource_missing (repository_id, package_id, resource_id)                   -> Printf.sprintf "package %s from repository %s is consuming resource %s which is well defined in the universe"          (String_of.package_id package_id) (String_of.repository_id repository_id) (String_of.resource_id resource_id)
    (* Implementation *)
    | Implementation_double                       (component_type_id)                              -> Printf.sprintf "implementation for component type %s is provided exactly one time"                                                             (String_of.component_type_id component_type_id)
    | Implementation_for_a_component_type_missing (component_type_id)                              -> Printf.sprintf "component type %s has an implementation provided"                                                                              (String_of.component_type_id component_type_id)
    | Component_type_in_implementation_missing    (component_type_id)                              -> Printf.sprintf "there is an implementation provided for component type %s which is well defined in the universe"                               (String_of.component_type_id component_type_id)
    | Implementation_package_missing              (component_type_id, package_id)                  -> Printf.sprintf "the implementation for component type %s mentions the package %s which is well defined in the universe"                        (String_of.component_type_id component_type_id) (String_of.package_id package_id)
  (*| Implementation_repository_missing           (component_type_id, repository_id)               -> Printf.sprintf "the implementation for component type %s mentions the repository %s which is well defined in the universe"                     (String_of.component_type_id component_type_id) (String_of.repository_id repository_id)*)
  (*| Implementation_package_missing              (component_type_id, repository_id, package_id)   -> Printf.sprintf "the implementation for component type %s mentions the repository %s and a package %s which is well present in this repository" (String_of.component_type_id component_type_id) (String_of.repository_id repository_id) (String_of.package_id package_id)*)
    (* Location *)
    | Location_double_id                 (location_id)                                             -> Printf.sprintf "location %s is defined exactly one time"                                                              (String_of.location_id location_id)
    | Location_repository_missing        (location_id, repository_id)                              -> Printf.sprintf "location %s is using the repository %s which is well defined in the universe"                         (String_of.location_id location_id) (String_of.repository_id repository_id)
    | Location_installed_package_missing (location_id, repository_id, package_id)                  -> Printf.sprintf "location %s, using the repository %s, contains a package %s which is well present in this repository" (String_of.location_id location_id) (String_of.repository_id repository_id) (String_of.package_id package_id)
    | Location_provided_resource_missing (location_id, resource_id)                                -> Printf.sprintf "location %s is providing resource %s which is well defined in the universe"                           (String_of.location_id location_id) (String_of.resource_id resource_id)
    (* Component *)
    | Component_double_id        (component_id)                                                    -> Printf.sprintf "component %s is defined exactly one time"                                             (String_of.component_id component_id)
    | Component_type_missing     (component_id, component_type_id)                                 -> Printf.sprintf "component %s is of component type %s which is well defined in the universe"           (String_of.component_id component_id) (String_of.component_type_id component_type_id)
    | Component_location_missing (component_id, location_id)                                       -> Printf.sprintf "component %s is placed on the location %s which is well present in the configuration" (String_of.component_id component_id) (String_of.location_id location_id)
    (* Binding *)
    | Binding_port_missing     (port_id, requiring_component_id, providing_component_id)           -> Printf.sprintf "in the binding on port %s between requiring component %s and providing component %s the bound port is well defined in the universe" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id providing_component_id)
    | Binding_requirer_missing (port_id, requiring_component_id, providing_component_id)           -> Printf.sprintf "in the binding on port %s between requiring component %s and providing component %s the requiring component is well present in the configuration" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id providing_component_id)
    | Binding_provider_missing (port_id, requiring_component_id, providing_component_id)           -> Printf.sprintf "in the binding on port %s between requiring component %s and providing component %s the providing component is well present in the configuration" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id providing_component_id)
    (* Specification *)
    | Specification_port_missing           (port_id)                                               -> Printf.sprintf "the port %s referenced in the specification is well defined in the universe"              (String_of.port_id port_id)
    | Specification_component_type_missing (component_type_id)                                     -> Printf.sprintf "the component_type_id %s referenced in the specification is well defined in the universe" (String_of.component_type_id component_type_id)
    | Specification_package_missing        (package_id)                                            -> Printf.sprintf "the package_id %s referenced in the specification is well defined in the universe"        (String_of.package_id package_id)


    let configuration_validation_test = function
    | Require_not_satisfied              (component_id, port_id)                                   -> Printf.sprintf "component's %s requirements on port %s are well satisfied"                                              (String_of.component_id component_id) (String_of.port_id port_id)
    | Provide_abused                     (component_id, port_id)                                   -> Printf.sprintf "component's %s provides on port %s are not abused"                                                      (String_of.component_id component_id) (String_of.port_id port_id)
    | Binding_unicity_violated           (port_id, requiring_component_id, prividing_component_id) -> Printf.sprintf "binding unicity on port %s between requiring component %s and providing component %s is well respected" (String_of.port_id port_id) (String_of.component_id requiring_component_id) (String_of.component_id prividing_component_id)
    | Conflict_exists                    (component_id, port_id, conflicted_component_id)          -> Printf.sprintf "component %s, which has a conflict port %s, and component %s are not in conflict"                       (String_of.component_id component_id) (String_of.port_id port_id) (String_of.component_id conflicted_component_id)
    | Package_dependencies_not_satisfied (location_id, package_id)                                 -> Printf.sprintf "dependencies of package %s on location %s are well satisfied"                                           (String_of.package_id package_id) (String_of.location_id location_id)
    | Package_in_conflict                (location_id, package_id, conflicted_package_id)          -> Printf.sprintf "package %s on location %s is not in conflict with package %s (on the same location)"                    (String_of.package_id package_id) (String_of.location_id location_id) (String_of.package_id conflicted_package_id)
    | Resource_overconsumed              (location_id, resource_id, consumed, provided)            -> Printf.sprintf "there is enough of resource %s provided (%s) on location %s to well satisfy the total consumption (%s)" (String_of.resource_id resource_id) (String_of.resource_provide_arity provided) (String_of.location_id location_id) (String_of.resource_consume_arity consumed)
    | Component_not_implemented          (location_id, component_id)                               -> Printf.sprintf "component %s on location %s is well implemented"                                                        (String_of.component_id component_id) (String_of.location_id location_id)

    let specification_validation_test = function
    | Specification_constraints_not_satisfied -> Printf.sprintf "specification constraints are satisfied"  

    let validation_test = function
    | Model_inconsistent          test -> Printf.sprintf "model consistency: %s"        (model_inconsistency_test      test)
    | Configuration_invalid       test -> Printf.sprintf "configuration validation: %s" (configuration_validation_test test)
    | Specification_not_satisfied test -> Printf.sprintf "specification validation: %s" (specification_validation_test test)

  end

  let validation_result = function
  | Ok     validation_test -> Printf.sprintf "OK   : %s" (Test_ok.validation_test    validation_test)
  | Failed validation_test -> Printf.sprintf "ERROR: %s" (Test_failed.validation_test validation_test)

end


(** Validation handlers *)
type validation_handler = bool -> validation_test -> unit

(** A helper function: convert a condition and a validation_test into a validation_result. *)
let validation_result (condition : bool) (test : validation_test) : validation_result =
  if condition (* if condition is true then the test went well, in other case there was an error *)
  then Ok    (test)
  else Failed (test)

(** Validation handler printing immediately information about each check. *)
let debug_printing_validation_handler condition test =
  Printf.printf "%s\n%!" (String_of.validation_result (validation_result condition test))

(** Validation handler immediately raising an exception when a check fails. *)
exception Validation_test_failed of validation_test

let failing_validation_handler condition test =
  if condition (* if condition is true then the test went well, in other case there was an error *)
  then ()
  else raise (Validation_test_failed test)

type results = validation_result list

(** Validation handler accumulating all the check results in a list. *)
let make_remembering_validation_handler () : validation_handler * (unit -> results) =
  let results = ref [] in
  let handle_validation_function condition test =
    results := (validation_result condition test)::!results
  and return_result_function () = !results
  in (handle_validation_function, return_result_function)

(** Helper function: chain multiple validation handlers together. *)
let chain_validation_handlers (handlers : validation_handler list) =
  fun condition test -> List.iter (fun handler -> handler condition test) handlers



(** Validation functions *)

(** Universe consistency tests: component types consistency. *)
let universe_consistency_component_types (universe : universe) handle_validation : unit =
  Component_type_id_set.iter (fun component_type_id -> 
    let component_type = universe#get_component_type component_type_id in
    let port_ids       = universe#get_port_ids in
    
    (* Component_type_double_id *)
    handle_validation 
      (true) (* Always OK, as component type identifiers are stored in a Set. *)
      (Model_inconsistent (Component_type_double_id (component_type_id)));

    (* Provided_port_missing *)
    Port_id_set.iter (fun port_id -> handle_validation 
      (Port_id_set.mem port_id port_ids) 
      (Model_inconsistent (Provided_port_missing (component_type_id, port_id)))
    ) component_type#provide_domain;

    (* Required_port_missing *)
    Port_id_set.iter (fun port_id -> handle_validation
      (Port_id_set.mem port_id port_ids)
      (Model_inconsistent (Required_port_missing (component_type_id, port_id)))
    ) component_type#require_domain;

    (* Conflict_port_missing *)
    Port_id_set.iter (fun port_id -> handle_validation
      (Port_id_set.mem port_id port_ids)
      (Model_inconsistent (Conflict_port_missing (component_type_id, port_id)))
    ) component_type#conflict;

    (* Consumed_resource_missing *)
    Resource_id_set.iter (fun resource_id ->
      handle_validation
        (Resource_id_set.mem resource_id universe#get_resource_ids)
        (Model_inconsistent (Consumed_resource_missing (component_type_id, resource_id)))
    ) component_type#consume_domain

  ) universe#get_component_type_ids

(** Universe consistency tests: repositories and packages consistency. *)
let universe_consistency_repositories_and_packages (universe : universe) handle_validation : unit =
  Repository_id_set.iter (fun repository_id ->
    let repository  = universe#get_repository repository_id in
    let package_ids = repository#package_ids in

    (* Repository_double_id *)
    handle_validation 
      (true) (* Always OK, as repository identifiers are stored in a Set. *)
      (Model_inconsistent (Repository_double_id (repository_id)));

    Package_id_set.iter (fun package_id ->
      let package = universe#get_package package_id in

      (* Package_double_id *)
      handle_validation 
        (true) (* Always OK, as package identifiers are stored in a Set. *)
        (Model_inconsistent (Package_double_id (repository_id, package_id)));

      (* Package_depend_missing *)
      Package_id_set_set.iter (fun package_set ->
        Package_id_set.iter (fun depended_on_package_id -> 
          handle_validation
            (Package_id_set.mem depended_on_package_id package_ids)
            (Model_inconsistent (Package_depend_missing (repository_id, package_id, depended_on_package_id)))
        ) package_set
      ) package#depend;

      (* Package_conflict_missing *)
      Package_id_set.iter (fun conflicted_package_id -> 
        handle_validation
          (Package_id_set.mem conflicted_package_id package_ids)
          (Model_inconsistent (Package_conflict_missing (repository_id, package_id, conflicted_package_id)))
      ) package#conflict;

      (* Package_consumed_resource_missing *)
      Resource_id_set.iter (fun resource_id ->
        handle_validation
          (Resource_id_set.mem resource_id universe#get_resource_ids)
          (Model_inconsistent (Package_consumed_resource_missing (repository_id, package_id, resource_id)))
      ) package#consume_domain

    ) package_ids;

  ) universe#get_repository_ids

(** Universe consistency tests: component type implementation consistency. *)
let universe_consistency_implementation (universe : universe) handle_validation : unit =
  Component_type_id_set.iter (fun component_type_id -> 

    (* Implementation_for_a_component_type_missing *)
    handle_validation
      (Component_type_id_set.mem component_type_id universe#get_implementation_domain)
      (Model_inconsistent (Implementation_for_a_component_type_missing component_type_id));

  ) universe#get_component_type_ids;

  Component_type_id_set.iter (fun component_type_id -> 

    (* Implementation_double *)
    handle_validation 
      (true) (* Always OK, as implementation domain is a Set. *)
      (Model_inconsistent (Implementation_double (component_type_id)));

    (* Component_type_in_implementation_missing *)
    handle_validation
      (Component_type_id_set.mem component_type_id universe#get_component_type_ids)
      (Model_inconsistent (Component_type_in_implementation_missing component_type_id));

    (* Implementation_package_missing *)
    Package_id_set.iter (fun implementation_package_id ->
      handle_validation
        (Package_id_set.mem implementation_package_id universe#get_package_ids)
        (Model_inconsistent (Implementation_package_missing (component_type_id, implementation_package_id)))
    ) (universe#get_implementation component_type_id)

    (* TODO: The right versions of: *)
    (* - Implementation_package_missing *)
    (* - Implementation_repository_missing *)
    (* Argh... Sometimes we have (reposiory_id, package_id) to reference a package, sometimes
       just a package_id. We have to correct this mess. Until then there is nothing more to do here. *)

  ) universe#get_implementation_domain

(** Full universe consistency tests. *)
let universe_consistency (universe : universe) handle_validation : unit =
  universe_consistency_component_types           universe handle_validation;
  universe_consistency_repositories_and_packages universe handle_validation;
  universe_consistency_implementation            universe handle_validation


(** Configuration consistency tests: internal configuration consistency. *)
let configuration_consistency_internal (configuration : configuration) handle_validation : unit =

  (* -- Configurations consistency tests -- *)

  (* - Locations - *)
  Location_id_set.iter (fun location_id ->
    (* let location = configuration#get_location location_id in *)

    (* Location_double_id *)
    handle_validation 
      (true) (* Always OK, as location identifiers are stored in a Set. *)
      (Model_inconsistent (Location_double_id (location_id)));

  ) configuration#get_location_ids;


  (* - Components - *)
  Component_id_set.iter (fun component_id ->
    let component = configuration#get_component component_id in

    (* Component_double_id *)
    handle_validation 
      (true) (* Always OK, as component identifiers are stored in a Set. *)
      (Model_inconsistent (Component_double_id (component_id)));

    let component_location_id = component#location in

    (* Component_location_missing *)
    handle_validation
      (Location_id_set.mem component_location_id configuration#get_location_ids)
      (Model_inconsistent (Component_location_missing (component_id, component_location_id)));    

  ) configuration#get_component_ids;


  (* - Bindings - *)
  Binding_set.iter (fun binding ->
    let port_id     = binding#port in
    let requirer_id = binding#requirer in
    let provider_id = binding#provider in

    (* Binding_requirer_missing *)
    handle_validation
      (Component_id_set.mem requirer_id configuration#get_component_ids)
      (Model_inconsistent (Binding_requirer_missing (port_id, requirer_id, provider_id)));

    (* Binding_provider_missing *)
    handle_validation
      (Component_id_set.mem provider_id configuration#get_component_ids)
      (Model_inconsistent (Binding_provider_missing (port_id, requirer_id, provider_id)));

  ) configuration#get_bindings

(** Configuration consistency tests: configuration consistency with the universe. *)
let configuration_consistency_with_universe (universe : universe) (configuration : configuration) handle_validation : unit =

  (* -- Configurations consistency tests -- *)

  (* - Locations - *)
  Location_id_set.iter (fun location_id ->
    let location = configuration#get_location location_id in
    let repository_id = location#repository in

    (* Location_repository_missing *)
    handle_validation
      (Repository_id_set.mem repository_id universe#get_repository_ids)
      (Model_inconsistent (Location_repository_missing (location_id, repository_id)));

    let repository = universe#get_repository repository_id in
    let repository_package_ids = repository#package_ids in

    (* Location_installed_package_missing *)
    Package_id_set.iter (fun installed_package_id ->
      handle_validation
        (Package_id_set.mem installed_package_id repository_package_ids)
        (Model_inconsistent (Location_installed_package_missing (location_id, repository_id, installed_package_id)))
    ) location#packages_installed;

    (* Location_provided_resource_missing *)
    Resource_id_set.iter (fun resource_id ->
      handle_validation
       (Resource_id_set.mem resource_id universe#get_resource_ids)
       (Model_inconsistent (Location_provided_resource_missing (location_id, resource_id)))
    ) location#provide_resources_domain

  ) configuration#get_location_ids;


  (* - Components - *)
  Component_id_set.iter (fun component_id ->
    let component = configuration#get_component component_id in
    let component_type_id = component#typ in

    (* Component_type_missing *)
    handle_validation
      (Component_type_id_set.mem component_type_id universe#get_component_type_ids)
      (Model_inconsistent (Component_type_missing (component_id, component_type_id)));

  ) configuration#get_component_ids;


  (* - Bindings - *)
  Binding_set.iter (fun binding ->
    let port_id     = binding#port in
    let requirer_id = binding#requirer in
    let provider_id = binding#provider in

    (* Binding_port_missing *)
    handle_validation
      (Port_id_set.mem port_id universe#get_port_ids)
      (Model_inconsistent (Binding_port_missing (port_id, requirer_id, provider_id)));

  ) configuration#get_bindings

(** Full configuration consistency tests. *)
let configuration_consistency (universe : universe) (configuration : configuration) handle_validation : unit =
  configuration_consistency_internal               configuration handle_validation;
  configuration_consistency_with_universe universe configuration handle_validation

(** Specification consistency tests: specification consistency with the universe. *)
let specification_consistency_with_universe (universe : universe) (specification : specification) handle_validation : unit =
  
  (* Get all the port, component type and package ids present in the specification. *)
  let (port_ids, component_type_ids, package_ids) = uv_of_specification specification in

  let universe_port_ids = universe#get_port_ids in

  (* Specification_port_missing *)
  Port_id_set.iter (fun port_id ->
    handle_validation 
      (Port_id_set.mem port_id universe_port_ids) 
      (Model_inconsistent (Specification_port_missing (port_id)))
  ) port_ids;

  let universe_component_type_ids = universe#get_component_type_ids in

  (* Specification_component_type_missing *)
  Component_type_id_set.iter (fun component_type_id ->
    handle_validation
      (Component_type_id_set.mem component_type_id universe_component_type_ids)
      (Model_inconsistent (Specification_component_type_missing (component_type_id)))
  ) component_type_ids;

  let universe_package_ids = universe#get_package_ids in

  (* Specification_package_missing *)
  Package_id_set.iter (fun package_id ->
    handle_validation
      (Package_id_set.mem package_id universe_package_ids)
      (Model_inconsistent (Specification_package_missing (package_id)))
  ) package_ids
  
(** Full specification consistency tests. *)
let specification_consistency (universe : universe) (specification : specification) handle_validation : unit =
  specification_consistency_with_universe universe specification handle_validation

(** Configuration validation tests: flat binding constraints (provide, require, conflict, binding unicity). *)
let configuration_validation_flat (universe : universe) (configuration : configuration) handle_validation : unit =
  Component_id_set.iter (fun component_id ->
    let component = configuration#get_component component_id in
    let component_type_id = component#typ in
    let component_type = universe#get_component_type component_type_id in

    (* Require_not_satisfied *)
    Port_id_set.iter (fun provided_port_id ->
      let bindings = Binding_set.filter (fun binding -> 
        (binding#port = provided_port_id) && (binding#provider = component_id)
      ) configuration#get_bindings in

      handle_validation
        (match component_type#provide provided_port_id with 
         | Finite_provide provide -> provide >= Binding_set.cardinal bindings
         | Infinite_provide -> true)
        (Configuration_invalid (Provide_abused (component_id, provided_port_id)))
    ) component_type#provide_domain;

    (* Provide_abused *)
    Port_id_set.iter (fun required_port_id ->
      let bindings = Binding_set.filter (fun binding -> (binding#port = required_port_id) && (binding#requirer = component_id)) configuration#get_bindings in

      handle_validation
        (component_type#require required_port_id <= Binding_set.cardinal bindings)
        (Configuration_invalid (Require_not_satisfied (component_id, required_port_id)))
    ) component_type#require_domain;

    (* Binding_unicity_violated *)
    (* TODO: Binding unicity cannot be violated because of how they are stored in the model (as a set). *)

    (* Conflict_exists *)
    Port_id_set.iter (fun conflict_port_id ->
      Component_id_set.iter (fun conflict_component_id ->
        let conflict_component_type = universe#get_component_type (configuration#get_component conflict_component_id)#typ in

        handle_validation
          ( (component_id = conflict_component_id) ||
            (not (Port_id_set.mem conflict_port_id conflict_component_type#provide_domain))
            (* TODO: Check if provide arity > 0 ? *) )
          (Configuration_invalid (Conflict_exists (component_id, conflict_port_id, conflict_component_id)))
      ) configuration#get_component_ids
    ) component_type#conflict;

  ) configuration#get_component_ids

(** Configuration validation tests: repositories and packages constraints (package deendencies and conflicts, implementation of components). *)
let configuration_validation_repositories_and_packages (universe : universe) (configuration : configuration) handle_validation : unit =

  (* - Package dependencies and conflicts - *)
  Location_id_set.iter (fun location_id ->
    let location = configuration#get_location location_id in
    (* let repository_id = location#repository in *)
    (* let repository = universe#get_repository repository_id in *)
    (* let repository_package_ids = repository#package_ids in *)
    let packages_installed = location#packages_installed in

    Package_id_set.iter (fun installed_package_id ->
      let package = universe#get_package installed_package_id in

      (* Package_dependencies_not_satisfied *)
      handle_validation
        (Package_id_set_set.is_empty package#depend ||
         (Package_id_set_set.exists (fun depended_on_package_id_set ->
          Package_id_set.for_all (fun depended_on_package_id ->
            Package_id_set.mem depended_on_package_id packages_installed
          ) depended_on_package_id_set
         ) package#depend))
        (Configuration_invalid (Package_dependencies_not_satisfied (location_id, installed_package_id)));

      (* Package_in_conflict *)
      Package_id_set.iter (fun conflicted_package_id ->
        handle_validation
          (not (Package_id_set.mem conflicted_package_id packages_installed))
          (Configuration_invalid (Package_in_conflict (location_id, installed_package_id, conflicted_package_id)))
      ) package#conflict

    ) packages_installed

  ) configuration#get_location_ids;

  (* - Component implementation - *)
  Component_id_set.iter (fun component_id ->
    let component = configuration#get_component component_id in
    let component_type_id = component#typ in
    
    let component_location_id = component#location in
    let location = configuration#get_location component_location_id in
    let packages_installed = location#packages_installed in
    
    let implementation = universe#get_implementation component_type_id in

    (* Component_not_implemented *)
    handle_validation
      (Package_id_set.exists (fun implementing_package_id ->
        Package_id_set.mem implementing_package_id packages_installed
      ) implementation)
      (Configuration_invalid (Component_not_implemented (component_location_id, component_id)))

  ) configuration#get_component_ids

(** Configuration validation tests: resource constraints (resources provided vs resources consumed). *)
let configuration_validation_resources (universe : universe) (configuration : configuration) handle_validation : unit =
  Resource_id_set.iter (fun resource_id ->
    Location_id_set.iter (fun location_id ->
      let location = configuration#get_location location_id in
      let location_component_ids = Component_id_set.filter (fun component_id -> (configuration#get_component component_id)#location = location_id) configuration#get_component_ids in

      let resource_provided = location#provide_resources resource_id in
      
      let resource_consumed = Component_id_set.fold (fun component_id resource_consumed -> 
        let component_type_id = (configuration#get_component component_id)#typ in
        let component_type = universe#get_component_type component_type_id in
        resource_consumed + component_type#consume resource_id
      ) location_component_ids 0 in

      (* Resource_overconsumed *)
      handle_validation
        (resource_consumed <= resource_provided)
        (Configuration_invalid (Resource_overconsumed (location_id, resource_id, resource_consumed, resource_provided)))

    ) configuration#get_location_ids;
  ) universe#get_resource_ids

let configuration_validation_specification (universe : universe) (configuration : configuration) (specification : specification) handle_validation : unit =

  let rec number_of_packages_on_location location_id package_id : int =
    if Package_id_set.mem package_id (configuration#get_location location_id)#packages_installed then 1 else 0

  and number_of_packages_global package_id : int =
    Location_id_set.fold (fun location_id sum ->
      sum + (number_of_packages_on_location location_id package_id)
    ) configuration#get_location_ids 0

  and number_of_components_on_location location_id component_type_id : int =
    Component_id_set.cardinal (Component_id_set.filter (fun component_id -> 
      let component = configuration#get_component component_id in
      (component#location = location_id) && (component#typ = component_type_id)
    ) configuration#get_component_ids)

  and number_of_components_global component_type_id : int =
    Component_id_set.cardinal (Component_id_set.filter (fun component_id -> 
      let component = configuration#get_component component_id in
      component#typ = component_type_id
    ) configuration#get_component_ids)

  and number_of_ports_provided_on_location location_id port_id : int =
    let int_of_provide_arity = function
      | Infinite_provide -> max_int
      | Finite_provide a' -> a'
    in
    Component_id_set.fold (fun component_id sum -> 
      let component = configuration#get_component component_id in
      let provide_arity =
        if component#location = location_id then 
          let component_type = universe#get_component_type (component#typ) in
          if Port_id_set.mem port_id component_type#provide_domain
          then int_of_provide_arity (component_type#provide port_id)
          else 0
        else 0 in
      (sum + provide_arity)
    ) configuration#get_component_ids 0

  and number_of_ports_provided_global port_id : int =
    Location_id_set.fold (fun location_id sum ->
      sum + (number_of_ports_provided_on_location location_id port_id)
    ) configuration#get_location_ids 0

  and spec_local_element l e = match e with
    | Data_model.Spec_local_element_package        (package_id)        -> number_of_packages_on_location       l package_id
    | Data_model.Spec_local_element_component_type (component_type_id) -> number_of_components_on_location     l component_type_id
    | Data_model.Spec_local_element_port           (port_id)           -> number_of_ports_provided_on_location l port_id
  
  and spec_local_expr l e = match e with
    | Data_model.Spec_local_expr_var   v       -> 0 (* TODO: Treat variables... *)
    | Data_model.Spec_local_expr_const c       -> c
    | Data_model.Spec_local_expr_arity e       -> spec_local_element l e
    | Data_model.Spec_local_expr_add  (e1, e2) -> (spec_local_expr l e1) + (spec_local_expr l e2)
    | Data_model.Spec_local_expr_sub  (e1, e2) -> (spec_local_expr l e1) - (spec_local_expr l e2)
    | Data_model.Spec_local_expr_mul  (e1, e2) ->  e1                    * (spec_local_expr l e2)

  and spec_op o = match o with
    | Data_model.Lt  -> ( <  ) 
    | Data_model.LEq -> ( <= )
    | Data_model.Eq  -> ( =  )
    | Data_model.GEq -> ( >= )
    | Data_model.Gt  -> ( >  )
    | Data_model.NEq -> ( <> )

  and local_specification l s = match s with
    | Data_model.Spec_local_true              -> true
    | Data_model.Spec_local_op   (e1, op, e2) ->     (spec_op op) (spec_local_expr l e1) (spec_local_expr l e2)
    | Data_model.Spec_local_and  (s1, s2)     ->     (local_specification l s1) && (local_specification l s2)
    | Data_model.Spec_local_or   (s1, s2)     ->     (local_specification l s1) || (local_specification l s2)
    | Data_model.Spec_local_impl (s1, s2)     -> not (local_specification l s1) || (local_specification l s2)
    | Data_model.Spec_local_not  (s')         -> not (local_specification l s')

  and spec_resource_constraint l co = 
    let location = configuration#get_location l in
    List.for_all (fun (o, op, i) -> (* Are all these resource constraints satisfied on this location? *)
      (spec_op op) (location#provide_resources o) i
    ) co

  and spec_repository_constraint l cr = match cr with 
    | [] -> true (* Special case: any repository. *)
    | _  -> let location = configuration#get_location l in
            List.exists (fun repository_id -> (* Is one of repositories on the location? *)
              location#repository = repository_id
            ) cr

  and spec_element e = match e with
    | Data_model.Spec_element_package        (package_id)        -> number_of_packages_global       package_id
    | Data_model.Spec_element_component_type (component_type_id) -> number_of_components_global     component_type_id
    | Data_model.Spec_element_port           (port_id)           -> number_of_ports_provided_global port_id
    | Data_model.Spec_element_location       (co, cr, ls)        -> 
        (* For all locations fulfilling the conditions the local specification applies. *)
        let concerned_locations = List.filter (fun l -> 
          (spec_resource_constraint l co) && (spec_repository_constraint l cr)
        ) (Location_id_set.elements configuration#get_location_ids) in
        List.length (List.filter (fun l -> local_specification l ls) concerned_locations)

  and spec_expr e = match e with
    | Data_model.Spec_expr_var    v       -> 0 (* TODO: Treat variables... *)
    | Data_model.Spec_expr_const  c       -> c
    | Data_model.Spec_expr_arity  e       -> spec_element e
    | Data_model.Spec_expr_add   (e1, e2) -> (spec_expr e1) + (spec_expr e2)
    | Data_model.Spec_expr_sub   (e1, e2) -> (spec_expr e1) - (spec_expr e2)
    | Data_model.Spec_expr_mul   (e1, e2) ->  e1            * (spec_expr e2)

  and sspecification s = match s with
    | Data_model.Spec_true              -> true
    | Data_model.Spec_op   (e1, op, e2) ->     (spec_op op) (spec_expr e1) (spec_expr e2)
    | Data_model.Spec_and  (s1, s2)     ->     (sspecification s1) && (sspecification s2)
    | Data_model.Spec_or   (s1, s2)     ->     (sspecification s1) || (sspecification s2)
    | Data_model.Spec_impl (s1, s2)     -> not (sspecification s1) || (sspecification s2)
    | Data_model.Spec_not  (s')         -> not (sspecification s')
    | Data_model.Spec_everywhere (ls) ->
        let concerned_locations = Location_id_set.elements configuration#get_location_ids in
        sspecification (Data_model.Spec_at (concerned_locations, ls))
    | Data_model.Spec_at (location_ids, ls) ->
        List.for_all (fun location_id -> local_specification location_id ls) location_ids

  in

  handle_validation
    (sspecification specification)
    (Specification_not_satisfied (Specification_constraints_not_satisfied))

(** Full configuration validation tests. *)
let configuration_validation ?(with_packages=true) (universe : universe) (configuration : configuration) (specification : specification) handle_validation : unit =
  configuration_validation_flat                      universe configuration               handle_validation;
  (if with_packages then 
  configuration_validation_repositories_and_packages universe configuration               handle_validation);
  configuration_validation_resources                 universe configuration               handle_validation;
  configuration_validation_specification             universe configuration specification handle_validation

(** Full model consistency tests. *)
let model_consistency (universe : universe) (configuration : configuration) (specification : specification) handle_validation : unit =
  universe_consistency      universe               handle_validation;
  configuration_consistency universe configuration handle_validation;
  specification_consistency universe specification handle_validation

(** Full model validation tests. *)
let model_validation ?(with_packages=true) (universe : universe) (configuration : configuration) (specification : specification) handle_validation : unit =
  configuration_validation ~with_packages universe configuration specification handle_validation

(** Full configuration consistency and validation tests. *)
let model (universe : universe) (configuration : configuration) (specification : specification) handle_validation : unit =
  model_consistency universe configuration specification handle_validation;
  model_validation  universe configuration specification handle_validation

let standard_model_check ?(with_packages=true) (universe : universe) (configuration : configuration) (specification : specification) : results =

  (* Prepare a remembering handler to store the results. *)
  let (remembering_validation_handler, result) = make_remembering_validation_handler () in

  (* Check the internal consistency of the model, fail on errors. *)
  (
    try
      model_consistency universe configuration specification (chain_validation_handlers [failing_validation_handler; remembering_validation_handler])
    with Validation_test_failed test ->
           failwith (Printf.sprintf "Error encountered during model validation: %s" (String_of.Test_failed.validation_test test))
  );

  (* Validation: check if configuration is valid w.r.t. the universe and the specification. *)
  model_validation ~with_packages universe configuration specification (remembering_validation_handler);

  (* Return validation results. *)
  result ()


let validation_results_filter_errors (results : results) : results =
  List.filter (fun validation_result ->
    match validation_result with
    | Failed (Configuration_invalid _) 
    | Failed (Specification_not_satisfied _) -> true
    | _ -> false
  ) results

let validation_passed (results : results) : bool =
  (validation_results_filter_errors results) = []
