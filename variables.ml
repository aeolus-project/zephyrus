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


open Aeolus_types_t
open Aeolus_types_output.Plain

open Helpers


type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

let string_of_element element =
  match element with
  | ComponentType (component_type_name) -> Printf.sprintf "%s" (string_of_component_type_name component_type_name)
  | Port          (port_name)           -> Printf.sprintf "%s" (string_of_port_name           port_name)
  | Package       (package_name)        -> Printf.sprintf "%s" (string_of_package_name        package_name)



type global_element_variable = element

let string_of_global_element_variable = 
  fun element ->
    Printf.sprintf
      "N(%s)"
      (string_of_element element)



type local_element_variable = location_name * element

let string_of_local_element_variable = 
  fun (location_name, element) ->
    Printf.sprintf 
      "N(%s,%s)"
      (string_of_location_name location_name)
      (string_of_element       element)



type binding_variable = port_name * component_type_name * component_type_name

let string_of_binding_variable = 
  fun (port_name, providing_component_type_name, requiring_component_type_name) ->
    Printf.sprintf 
      "B(%s,%s,%s)"
      (string_of_port_name           port_name)
      (string_of_component_type_name providing_component_type_name)
      (string_of_component_type_name requiring_component_type_name)



type local_repository_variable = location_name * repository_name

let string_of_local_repository_variable = 
  fun (location_name, repository_name) ->
    Printf.sprintf 
      "R(%s,%s)"
      (string_of_location_name   location_name)
      (string_of_repository_name repository_name)



type local_resource_variable = location_name * resource_name

let string_of_local_resource_variable = 
  fun (location_name, resource_name) ->
    Printf.sprintf 
      "O(%s,%s)"
      (string_of_location_name location_name)
      (string_of_resource_name resource_name)


type specification_variable = spec_variable_name

let string_of_specification_variable =
  fun specification_variable ->
    Printf.sprintf
      "S(%s)"
      (string_of_spec_variable_name specification_variable)



type variable =
  (* Number of instances of a given component_type / port / package installed globally in the configuration. *)
  | GlobalElementVariable    of element

  (* Number of instances of a given component_type / port / package installed on a given location. *)
  | LocalElementVariable     of location_name * element

  (* Number of bindings on the given port between the instances of the given requiring type and given providing type. *)
  | BindingVariable          of port_name * component_type_name * component_type_name

  (* Is the given repository installed on the given location? (boolean variable) *)
  | LocalRepositoryVariable  of location_name * repository_name

  (* How many resources of the given type are provided by the given location. *)
  | LocalResourceVariable    of location_name * resource_name

  (* Specifiaction variable *)
  | SpecificationVariable    of spec_variable_name




let descr_of_variable variable =
  match variable with
  | GlobalElementVariable   (element)                                                                 -> "global element variable"
  | LocalElementVariable    (location_name, element)                                                  -> "local element variable"
  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) -> "binding variable"
  | LocalRepositoryVariable (location_name, repository_name)                                          -> "local repository variable"
  | LocalResourceVariable   (location_name, resource_name)                                            -> "local resource variable"
  | SpecificationVariable   (spec_variable_name)                                                      -> "specification variable"


let string_of_variable variable =
  match variable with
  | GlobalElementVariable   (element) ->
      string_of_global_element_variable (element)

  | LocalElementVariable    (location_name, element) ->
      string_of_local_element_variable (location_name, element)

  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) ->
      string_of_binding_variable (port_name, providing_component_type_name, requiring_component_type_name)

  | LocalRepositoryVariable (location_name, repository_name) ->
      string_of_local_repository_variable (location_name, repository_name)

  | LocalResourceVariable   (location_name, resource_name) ->
      string_of_local_resource_variable (location_name, resource_name)

  | SpecificationVariable   (spec_variable_name) ->
      string_of_specification_variable (spec_variable_name)


let pred_global_element_variable variable =
  match variable with
  | GlobalElementVariable _   -> true
  | _                         -> false

let pred_local_element_variable variable =
  match variable with
  | LocalElementVariable _    -> true
  | _                         -> false

let pred_binding_variable variable =
  match variable with
  | BindingVariable _         -> true
  | _                         -> false

let pred_local_repository_variable variable =
  match variable with
  | LocalRepositoryVariable _ -> true
  | _                         -> false

let pred_local_resource_variable variable =
  match variable with
  | LocalResourceVariable _   -> true
  | _                         -> false

let pred_specification_variable variable =
  match variable with
  | SpecificationVariable _   -> true
  | _                         -> false