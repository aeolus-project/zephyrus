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
    - datatypes/Data_model
*)

open Data_model

type model_inconsistency_error =
  (* Component type *)
  | Provided_port_missing                 of component_type_id * port_id     (* A component type provides a port which does not exist *)
  | Required_port_missing                 of component_type_id * port_id     (* A component type requires a port which does not exist *)
  | Conflict_port_missing                 of component_type_id * port_id     (* A component type conflicts with a port which does not exist *)
  | Consumed_resource_missing             of component_type_id * resource_id (* A component type consumes a resource which does not exist *)
  (* Package *)
  | Package_depend_missing                of repository_id * package_id * package_id (* A package depends on another package which does not exist *)
  | Package_conflict_missing              of repository_id * package_id * package_id (* A package conflicts with another package which does not exist *)
  (* (* Unnecessary *) | Consumed_resource_missing of component_type_id * resource_id *)
  (* Implementation *)
  | Implementation_for_a_component_type_missing of component_type_id
  | Implementation_component_type_missing       of component_type_id          (* Component type referenced in the implementation part does not exist *)
  | Implementation_repository_missing           of repository_id              (* Repository referenced in the implementation part does not exist *)
  | Implementation_package_missing              of repository_id * package_id (* A repository referenced in the implementation part does not contain a referenced package *)
  (* Location *)
  | Location_repository_missing           of location_id * repository_id
  | Location_installed_package_missing    of location_id * repository_id * package_id
  | Location_provided_resource_missing    of location_id * resource_id
  (* Component *)
  | Component_type_missing                of component_id * component_type_id
  | Component_location_missing            of component_id * location_id
  (* Binding *)
  | Binding_port_missing                  of port_id           * component_id * component_id
  | Binding_requirer_missing              of component_type_id * component_id * component_id
  | Binding_provider_missing              of component_type_id * component_id * component_id

type configuration_validation_error =
  | Require_not_satisfied              of component_id * port_id
  | Provide_abused                     of component_id * port_id
  | Binding_unicity_violated           of component_id * port_id * component_id
  | Conflict_exists                    of component_id * port_id * component_id
  | Package_dependencies_not_satisfied of location_id * package_id
  | Package_in_conflict                of location_id * package_id * package_id
  | Resource_overconsumed              of location_id * resource_id
  | Component_not_implemented          of component_id

type specification_validation_error =
  | Specification_validation_error

type validation_error =
  | Model_inconsistency of model_inconsistency_error
  | Configuration_error of configuration_validation_error
  | Specification_error of specification_validation_error

type validation_result =
  | Ok    of validation_error
  | Error of validation_error

let make_validation_handler () : (validation_result -> unit) * (unit -> (validation_error list * validation_error list)) =
  let results = ref ([], []) in
  let handle =
    let (oks, errors) = !results in
    function
    | Ok    (validation_error) -> results := (validation_error::oks,                   errors)
    | Error (validation_error) -> results := (                  oks, validation_error::errors)
  and return () = !results
  in (handle, return)

let model (universe : universe) (specification : specification) (configuration : configuration) handle_validation : unit =

  let handle_validation condition error = (* if condition is true then it's not an error *)
    handle_validation (
      if condition
      then Ok    (error)
      else Error (error)
    ) in

  (* -- Model incosistency errors -- *)

  (* - Component Types - *)
  Component_type_id_set.iter (fun component_type_id -> 
    let component_type = universe#get_component_type component_type_id in
    let port_ids       = universe#get_port_ids in
    
    (* A component type provides a port which does not exist *)
    Port_id_set.iter (fun port_id -> handle_validation 
      (Port_id_set.mem port_id port_ids) 
      (Model_inconsistency (Provided_port_missing (component_type_id, port_id)))
    ) component_type#provide_domain;

    (* A component type requires a port which does not exist *)
    Port_id_set.iter (fun port_id -> handle_validation
      (Port_id_set.mem port_id port_ids)
      (Model_inconsistency (Required_port_missing (component_type_id, port_id)))
    ) component_type#require_domain;

    (* A component type conflicts with a port which does not exist *)
    Port_id_set.iter (fun port_id -> handle_validation
      (Port_id_set.mem port_id port_ids)
      (Model_inconsistency (Conflict_port_missing (component_type_id, port_id)))
    ) component_type#conflict;

  ) universe#get_component_type_ids;

  (* - Repositories and Packages - *)
  Repository_id_set.iter (fun repository_id ->
    let repository  = universe#get_repository repository_id in
    let package_ids = repository#package_ids in

    Package_id_set.iter (fun package_id ->
      let package = repository#get_package package_id in

      (* Package_depend_missing *)
      Package_id_set_set.iter (fun package_set ->
        Package_id_set.iter (fun depended_on_package_id -> 
          handle_validation
            (Package_id_set.mem depended_on_package_id package_ids)
            (Model_inconsistency (Package_depend_missing (repository_id, package_id, depended_on_package_id)))
        ) package_set
      ) package#depend;

      (* Package_conflict_missing *)
      Package_id_set.iter (fun conflicted_package_id -> 
        handle_validation
          (Package_id_set.mem conflicted_package_id package_ids)
          (Model_inconsistency (Package_conflict_missing (repository_id, package_id, conflicted_package_id)))
      ) package#conflict;

    ) package_ids;

  ) universe#get_repository_ids;

  (* - Implementation - *)
  Component_type_id_set.iter (fun component_type_id -> 

    (* TODO: Currently the implementation domain is not available, so we cannot check neither
             Implementation_for_a_component_type_missing nor Implementation_component_type_missing. *)

    (*
    let implementation = universe#implementation component_type_id in
    let repository_ids = universe#get_repository_ids in
    *)

    (* TODO: Argh... Sometimes we have (reposiory_id, package_id) to reference a package, sometimes
       just a package_id. We have to correct this mess. Until then there is nothing to do here. *)

    ()

  ) universe#get_component_type_ids;

  (* - Locations - *)
  Location_id_set.iter (fun location_id ->
    let location = configuration#get_location location_id in
    let repository_id = location#repository in

    (* Location_repository_missing *)
    handle_validation
      (Repository_id_set.mem repository_id universe#get_repository_ids)
      (Model_inconsistency (Location_repository_missing (location_id, repository_id)));

    let repository = universe#get_repository repository_id in
    let repository_package_ids = repository#package_ids in

    (* Location_installed_package_missing *)
    Package_id_set.iter (fun installed_package_id ->
      handle_validation
        (Package_id_set.mem installed_package_id repository_package_ids)
        (Model_inconsistency (Location_installed_package_missing (location_id, repository_id, installed_package_id)))
    ) location#packages_installed;

    (* Location_provided_resource_missing *)
    (* TODO: Location_provided_resource_missing
       We cannot check the resource incosistency, because the information is not available in the universe. *)

  ) configuration#get_location_ids;

  (* - Components - *)
  Component_id_set.iter (fun component_id ->
    let component = configuration#get_component component_id in

    let component_type_id = component#typ in

    (* Component_type_missing *)
    handle_validation
      (Component_type_id_set.mem component_type_id universe#get_component_type_ids)
      (Model_inconsistency (Component_type_missing (component_id, component_type_id)));

    let component_location_id = component#location in

    (* Component_location_missing *)
    handle_validation
      (Location_id_set.mem component_location_id configuration#get_location_ids)
      (Model_inconsistency (Component_location_missing (component_id, component_location_id)));    

  ) configuration#get_component_ids;

  (* - Bindings - *)  

  Binding_set.iter (fun binding ->
    let port_id     = binding#port in
    let requirer_id = binding#requirer in
    let provider_id = binding#provider in

    (* Binding_port_missing *)
    handle_validation
      (Port_id_set.mem port_id universe#get_port_ids)
      (Model_inconsistency (Binding_port_missing (port_id, requirer_id, provider_id)));

    (* Binding_requirer_missing *)
    handle_validation
      (Component_type_id_set.mem requirer_id universe#get_component_type_ids)
      (Model_inconsistency (Binding_requirer_missing (port_id, requirer_id, provider_id)));

    (* Binding_provider_missing *)
    handle_validation
      (Component_type_id_set.mem provider_id universe#get_component_type_ids)
      (Model_inconsistency (Binding_provider_missing (port_id, provider_id, provider_id)));

  ) configuration#get_bindings;

  (* -- Configuration validation errors -- *)

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
        (Configuration_error (Provide_abused (component_id, provided_port_id)))
    ) component_type#provide_domain;

    (* Provide_abused *)
    Port_id_set.iter (fun required_port_id ->
      let bindings = Binding_set.filter (fun binding -> (binding#port = required_port_id) && (binding#requirer = component_id)) configuration#get_bindings in

      handle_validation
        (component_type#require required_port_id <= Binding_set.cardinal bindings)
        (Configuration_error (Require_not_satisfied (component_id, required_port_id)))
    ) component_type#require_domain;

    (* Binding_unicity_violated *)
    (* TODO: Binding unicity cannot be violated because of how they are stored in the model (as a set). *)

    (* Conflict_exists *)
    Port_id_set.iter (fun conflict_port_id ->
        Component_id_set.iter (fun conflict_component_id ->
          let component_type = universe#get_component_type (configuration#get_component conflict_component_id)#typ in

          handle_validation
            ((Port_id_set.mem conflict_port_id component_type#provide_domain))
            (Configuration_error (Conflict_exists (component_id, conflict_port_id, conflict_component_id)))
        ) configuration#get_component_ids
    ) component_type#conflict;

  ) configuration#get_component_ids;

  Location_id_set.iter (fun location_id ->
    let location = configuration#get_location location_id in
    let repository_id = location#repository in
    let repository = universe#get_repository repository_id in
    (* let repository_package_ids = repository#package_ids in *)
    let packages_installed = location#packages_installed in

    Package_id_set.iter (fun installed_package_id ->
      let package = repository#get_package installed_package_id in

      (* Package_dependencies_not_satisfied *)
      handle_validation
        (Package_id_set_set.exists (fun depended_on_package_id_set ->
          Package_id_set.for_all (fun depended_on_package_id ->
            Package_id_set.mem depended_on_package_id packages_installed
          ) depended_on_package_id_set
        ) package#depend)
        (Configuration_error (Package_dependencies_not_satisfied (location_id, installed_package_id)));

      (* Package_in_conflict *)
      Package_id_set.iter (fun conflicted_package_id ->
        handle_validation
          (Package_id_set.mem conflicted_package_id packages_installed)
          (Configuration_error (Package_in_conflict (location_id, installed_package_id, conflicted_package_id)))
      ) package#conflict

    ) packages_installed

  ) configuration#get_location_ids;
  
  Location_id_set.iter (fun location_id ->
    (* let location = configuration#get_location location_id in *)
    ()
    (* TODO: How to iterate on resources? We have to finally remove the strange "resources" object
       and put the information where it belongs to... *)
    (*  Resource_overconsumed            of location_id * resource_id    *)
  ) configuration#get_location_ids;

  Component_id_set.iter (fun component_id ->
    let component = configuration#get_component component_id in
    let component_type_id = component#typ in
    
    let component_location_id = component#location in
    let location = configuration#get_location component_location_id in
    (* let repository_id = location#repository in
    let repository = universe#get_repository repository_id in *)
    let packages_installed = location#packages_installed in
    
    let implementation = universe#get_implementation component_type_id in

    handle_validation
      (Package_id_set.exists (fun implementing_package_id ->
        Package_id_set.mem implementing_package_id packages_installed
      ) implementation)
      (Configuration_error (Component_not_implemented component_id))

  ) configuration#get_component_ids
  