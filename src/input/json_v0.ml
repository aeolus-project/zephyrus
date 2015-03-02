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

(** Conversion of the Zephyrus model JSON syntax version 0 to and from the abstract IO representation. *)

module J = Json_v0_j
module T = Json_v0_t


(*/************************************************************************\*)
(*| 1. Translation of JSON to Abstract_io                                  |*)
(*\************************************************************************/*)

module To_abstract_io = struct

  module I = T
  module O = Abstract_io


  (* 1. Translation of simple names and values *)
  let port_name           port_name'           = port_name'
  let resource_name       resource_name'       = resource_name'
  let component_type_name component_type_name' = component_type_name'
  let package_name        package_name'        = package_name'
  let repository_name     repository_name'     = repository_name'
  let location_name       location_name'       = location_name'
  let component_name      component_name'      = component_name'

  let provide_arity provide_arity' = 
    match provide_arity' with
    | I.FiniteProvide (i) -> O.FiniteProvide (i)
    | I.InfiniteProvide   -> O.InfiniteProvide
  
  let require_arity          require_arity'          = require_arity'
  let resource_consumption   resource_consumption'   = resource_consumption'
  let resource_provide_arity resource_provide_arity' = resource_provide_arity'

  let single_provide (port_name',     provide_arity'       ) = (port_name     port_name',     provide_arity        provide_arity'       )
  let single_require (port_name',     require_arity'       ) = (port_name     port_name',     require_arity        require_arity'       )
  let single_consume (resource_name', resource_consumption') = (resource_name resource_name', resource_consumption resource_consumption')


  (* 2. Translation of universe *)
  let component_type component_type' = {
    O.component_type_name     = component_type_name     component_type'.I.component_type_name;
    O.component_type_states   = Without_state({ 
      O.provide  = List.map single_provide component_type'.I.component_type_provide;
      O.require  = List.map single_require component_type'.I.component_type_require;
      O.conflict = List.map port_name      component_type'.I.component_type_conflict;
    }); 
    O.component_type_consume  = List.map single_consume component_type'.I.component_type_consume;
  }

  let single_depend package_name_list = List.map package_name package_name_list

  let package package' = {
    O.package_name     = package_name            package'.I.package_name;
    O.package_depend   = List.map single_depend  package'.I.package_depend;
    O.package_conflict = List.map package_name   package'.I.package_conflict;
    O.package_consume  = List.map single_consume package'.I.package_consume;
  }

  let repository repository' = {
    O.repository_name     = repository_name  repository'.I.repository_name;
    O.repository_packages = List.map package repository'.I.repository_packages;
  }

  let package_path (repository_name', package_name') = (repository_name repository_name', package_name package_name')

  let single_implementation (component_type_name', package_path_list) = 
    (component_type_name component_type_name', List.map package_path package_path_list)

  let universe universe' = {
    O.universe_component_types = List.map component_type        universe'.I.universe_component_types;
    O.universe_implementation  = List.map single_implementation universe'.I.universe_implementation;
    O.universe_repositories    = List.map repository            universe'.I.universe_repositories;
    O.universe_port_hierarchy  = []
  }


  (* 3. translation of configurations *)
  let location_cost location_cost' = location_cost'

  let single_provide_resources (resource_name', resource_provide_arity') =
    (resource_name resource_name', resource_provide_arity resource_provide_arity')

  let location location' = {
    O.location_name               = location_name                     location'.I.location_name;
    O.location_provide_resources  = List.map single_provide_resources location'.I.location_provide_resources;
    O.location_repository         = repository_name                   location'.I.location_repository;
    O.location_packages_installed = List.map package_name             location'.I.location_packages_installed;
    O.location_cost               = location_cost                     location'.I.location_cost;
  }

  let component component' = {
    O.component_name     = component_name      component'.I.component_name;
    O.component_type     = O.Component_type_simple(component_type_name component'.I.component_type);
    O.component_location = location_name       component'.I.component_location;
  }

  let binding binding' = {
    O.binding_port_required = port_name      binding'.I.binding_port;
    O.binding_port_provided = port_name      binding'.I.binding_port;
    O.binding_requirer      = component_name binding'.I.binding_requirer;
    O.binding_provider      = component_name binding'.I.binding_provider;
  }

  let configuration configuration' = {
    O.configuration_locations  = List.map location  configuration'.I.configuration_locations;
    O.configuration_components = List.map component configuration'.I.configuration_components;
    O.configuration_bindings   = List.map binding   configuration'.I.configuration_bindings;
  }

  (* 4. translation of specification is absent, because integrated in the parser *)
  (* TODO: put that translation here ? *)
end


(*/************************************************************************\*)
(*| 2. Translation of Abstract_io to JSON                                  |*)
(*\************************************************************************/*)

module Of_abstract_io = struct
  
  module I = Abstract_io
  module O = T

  (* 1. Translation of simple names and values *)
  let port_name           port_name'           = port_name'
  let resource_name       resource_name'       = resource_name'
  let component_type_name component_type_name' = component_type_name'
  let package_name        package_name'        = package_name'
  let repository_name     repository_name'     = repository_name'
  let location_name       location_name'       = location_name'
  let component_name      component_name'      = component_name'

  let provide_arity provide_arity' = 
    match provide_arity' with
    | I.FiniteProvide (i) -> O.FiniteProvide (i)
    | I.InfiniteProvide   -> O.InfiniteProvide
  
  let require_arity          require_arity'          = require_arity'
  let resource_consumption   resource_consumption'   = resource_consumption'
  let resource_provide_arity resource_provide_arity' = resource_provide_arity'

  let single_provide (port_name',     provide_arity'       ) = (port_name     port_name',     provide_arity        provide_arity'       )
  let single_require (port_name',     require_arity'       ) = (port_name     port_name',     require_arity        require_arity'       )
  let single_consume (resource_name', resource_consumption') = (resource_name resource_name', resource_consumption resource_consumption')



  (* 2. Translation of universe *)
  let component_type component_type' =
    let (provide, require, conflict) = match component_type'.component_type_states with
      | With_state _ -> raise I.Exception_incompatible_output_format
      | Without_state(ce) -> (ce.I.provide, ce.I.require, ce.I.conflict) in {
    O.component_type_name     = component_type_name     component_type'.I.component_type_name;
    O.component_type_provide  = List.map single_provide provide;
    O.component_type_require  = List.map single_require require;
    O.component_type_conflict = List.map port_name      conflict;
    O.component_type_consume  = List.map single_consume component_type'.I.component_type_consume;
  }

  let single_depend package_name_list = List.map package_name package_name_list

  let package package' = {
    O.package_name     = package_name            package'.I.package_name;
    O.package_depend   = List.map single_depend  package'.I.package_depend;
    O.package_conflict = List.map package_name   package'.I.package_conflict;
    O.package_consume  = List.map single_consume package'.I.package_consume;
  }

  let repository repository' = {
    O.repository_name     = repository_name  repository'.I.repository_name;
    O.repository_packages = List.map package repository'.I.repository_packages;
  }

  let package_path (repository_name', package_name') = (repository_name repository_name', package_name package_name')

  let single_implementation (component_type_name', package_path_list) = 
    (component_type_name component_type_name', List.map package_path package_path_list)

  let universe universe' = {
    O.universe_component_types = List.map component_type        universe'.I.universe_component_types;
    O.universe_implementation  = List.map single_implementation universe'.I.universe_implementation;
    O.universe_repositories    = List.map repository            universe'.I.universe_repositories;
  }


  (* 3. translation of configurations *)
  let location_cost location_cost' = location_cost'

  let single_provide_resources (resource_name', resource_provide_arity') = (resource_name resource_name', resource_provide_arity resource_provide_arity')

  let location location' = {
    O.location_name               = location_name                     location'.I.location_name;
    O.location_provide_resources  = List.map single_provide_resources location'.I.location_provide_resources;
    O.location_repository         = repository_name                   location'.I.location_repository;
    O.location_packages_installed = List.map package_name             location'.I.location_packages_installed;
    O.location_cost               = location_cost                     location'.I.location_cost;
  }

  let component component' = 
    let component_type_name = match component'.I.component_type with
      | Component_type_state _  -> raise I.Exception_incompatible_output_format
      | Component_type_simple n -> n in {
    O.component_name     = component_name      component'.I.component_name;
    O.component_type     = component_type_name component_type_name;
    O.component_location = location_name       component'.I.component_location;
  }

  let binding binding' = 
  if binding'.I.binding_port_required = binding'.I.binding_port_provided then {
    O.binding_port     = port_name      binding'.I.binding_port_required;
    O.binding_requirer = component_name binding'.I.binding_requirer;
    O.binding_provider = component_name binding'.I.binding_provider;
  } else raise I.Exception_incompatible_output_format

  let configuration configuration' = {
    O.configuration_locations  = List.map location  configuration'.I.configuration_locations;
    O.configuration_components = List.map component configuration'.I.configuration_components;
    O.configuration_bindings   = List.map binding   configuration'.I.configuration_bindings;
  }

  (* 4. translation of specification is absent, because integrated in the parser *)
  (* TODO: put that translation here ? *)
end


