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


module Plain =
  struct

    open Aeolus_types_j

    (* Here we use custom functions in order to avoid having quotes everywhere for the primitive printing functions. *)
    let string_of_component_type_name component_type_name = component_type_name
    let string_of_port_name port_name = port_name
    let string_of_component_name component_name = component_name
    let string_of_package_name package_name = package_name
    let string_of_repository_name repository_name = repository_name
    let string_of_location_name location_name = location_name
    let string_of_resource_name resource_name = resource_name
    let string_of_provide_arity provide_arity = Printf.sprintf "%d" provide_arity
    let string_of_require_arity require_arity = Printf.sprintf "%d" require_arity
    let string_of_resource_consumption resource_consumption = Printf.sprintf "%d" resource_consumption
    let string_of_resource_provide_arity resource_provide_arity = Printf.sprintf "%d" resource_provide_arity

    let string_of_spec_variable_name spec_variable_name = Printf.sprintf "%s" spec_variable_name
    let string_of_spec_const spec_const = Printf.sprintf "%d" spec_const


    (* For the more complex printing functions on we simply use the ATDgen string_of_* functions. *)
    let string_of_component_type component_type =  string_of_component_type component_type
    let string_of_component_types component_types = string_of_component_types component_types
    let string_of_package package = string_of_package package
    let string_of_packages packages = string_of_packages packages
    let string_of_repository repository = string_of_repository repository
    let string_of_repositories repositories = string_of_repositories repositories
    let string_of_package_names package_names = string_of_package_names package_names
    let string_of_universe universe = string_of_universe universe
    let string_of_resources_provided resources_provided = string_of_resources_provided resources_provided
    let string_of_location location = string_of_location location
    let string_of_component component = string_of_component component
    let string_of_binding binding = string_of_binding binding
    let string_of_configuration configuration = string_of_configuration configuration

  end