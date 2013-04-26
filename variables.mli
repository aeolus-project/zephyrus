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

type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

val string_of_element : element -> string

type global_element_variable = element
val string_of_global_element_variable : global_element_variable -> string

type local_element_variable = location_name * element
val string_of_local_element_variable : local_element_variable -> string

type binding_variable = port_name * component_type_name * component_type_name
val string_of_binding_variable : binding_variable -> string

type local_repository_variable = location_name * repository_name
val string_of_local_repository_variable : local_repository_variable -> string

type local_resource_variable = location_name * resource_name
val string_of_local_resource_variable : local_resource_variable -> string

type specification_variable = spec_variable_name
val string_of_specification_variable : specification_variable -> string

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


val descr_of_variable  : variable -> string
val string_of_variable : variable -> string

(* Predicates on variable types *)
(* TODO: This is mostly boilerplate code, but I have no idea how to get rid of it... *)
val pred_global_element_variable   : variable -> bool
val pred_local_element_variable    : variable -> bool
val pred_binding_variable          : variable -> bool
val pred_local_repository_variable : variable -> bool
val pred_local_resource_variable   : variable -> bool
val pred_specification_variable    : variable -> bool