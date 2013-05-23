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

type variable =
  | GlobalElementVariable    of element
  (** Number of instances of a given component_type / port / package installed globally in the configuration. *)

  | LocalElementVariable     of location_name * element
  (** Number of instances of a given component_type / port / package installed on a given location. *)

  | BindingVariable          of port_name * component_type_name * component_type_name
  (** Number of bindings on the given port between the instances of the given requiring type and given providing type. *)

  | LocalRepositoryVariable  of location_name * repository_name
  (** Is the given repository installed on the given location? (boolean variable) *)

  | LocalResourceVariable    of location_name * resource_name
  (** How many resources of the given type are provided by the given location. *)

  | SpecificationVariable    of spec_variable_name
  (** Specifiaction variable *)


let string_of_element element =
  match element with
  | ComponentType (component_type_name) -> Printf.sprintf "%s" (string_of_component_type_name component_type_name)
  | Port          (port_name)           -> Printf.sprintf "%s" (string_of_port_name           port_name)
  | Package       (package_name)        -> Printf.sprintf "%s" (string_of_package_name        package_name)

let string_of_variable variable =
  match variable with
  | GlobalElementVariable   (element) ->
      Printf.sprintf
        "N(%s)"
        (string_of_element element)

  | LocalElementVariable    (location_name, element) ->
      Printf.sprintf 
        "N(%s,%s)"
        (string_of_location_name location_name)
        (string_of_element       element)

  | BindingVariable         (port_name, providing_component_type_name, requiring_component_type_name) ->
      Printf.sprintf 
        "B(%s,%s,%s)"
        (string_of_port_name           port_name)
        (string_of_component_type_name providing_component_type_name)
        (string_of_component_type_name requiring_component_type_name)

  | LocalRepositoryVariable (location_name, repository_name) ->
      Printf.sprintf 
        "R(%s,%s)"
        (string_of_location_name   location_name)
        (string_of_repository_name repository_name)

  | LocalResourceVariable   (location_name, resource_name) ->
      Printf.sprintf 
        "O(%s,%s)"
        (string_of_location_name location_name)
        (string_of_resource_name resource_name)

  | SpecificationVariable   (spec_variable_name) ->
      Printf.sprintf
        "S(%s)"
        (string_of_spec_variable_name spec_variable_name)


let descr_of_variable variable =
  match variable with
  | GlobalElementVariable   _ -> "global element variable"
  | LocalElementVariable    _ -> "local element variable"
  | BindingVariable         _ -> "binding variable"
  | LocalRepositoryVariable _ -> "local repository variable"
  | LocalResourceVariable   _ -> "local resource variable"
  | SpecificationVariable   _ -> "specification variable"


type variable_kind =
  | BooleanVariable
  | NaturalVariable

let variable_kind variable =
  match variable with
  | GlobalElementVariable   _ -> NaturalVariable
  | LocalElementVariable    _ -> NaturalVariable
  | BindingVariable         _ -> NaturalVariable
  | LocalRepositoryVariable _ -> BooleanVariable
  | LocalResourceVariable   _ -> NaturalVariable
  | SpecificationVariable   _ -> NaturalVariable