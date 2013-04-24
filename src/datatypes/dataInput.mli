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


(************************************)
(** 1/6 Basic Types                 *)
(************************************)

type component_type_name = string
type port_name           = string
type component_name      = string
type package_name        = string
type repository_name     = string
type location_name       = string
type resource_name       = string
type spec_variable_name  = string

type element =
  | ComponentType of component_type_name
  | Port          of port_name
  | Package       of package_name

type provide_arity = InfiniteProvide | FiniteProvide of int
type require_arity = int
type resource_consumption = int
type resource_provide_arity = int

type input = { univ : universe; init : configuration; spec : specification; opt : optimization_function }

(************************************)
(** 2/6 Universe                    *)
(************************************)

type component_type = {
  name                           : component_type_name;
  provide  : (port_name * provide_arity) list;
  require  : (port_name * require_arity) list;
  conflict : port_name list;
  consume  : (resource_name * resource_consumption) list
}

type package = {
  package_name     : package_name;
  package_depend   : package_name list list;
  package_conflict : package_name list;
  package_consume  : (resource_name * resource_consumption) list
}

type repository = {
  repository_name : repository_name;
  repository_packages : package list
}

type universe = {
  universe_component_types : component_type list;
  universe_implementation  : (component_type_name * package_names) list;
  universe_repositories    : repository list
}

(************************************)
(** 3/6 Configuration               *)
(************************************)

type location = {
  location_name : location_name;
  location_provide_resources : resources_provided;
  location_repository : repository_name;
  location_packages_installed : package_name list
}

type component = {
  component_name: component_name;
  component_type: component_type_name;
  component_location: location_name
}

type binding = {
  binding_port : port_name;
  binding_requirer : component_name;
  binding_provider : component_name
}

type configuration = {
  configuration_locations  : location list;
  configuration_components : component list;
  configuration_bindings   : binding list
}

(************************************)
(** 4/6 Specification               *)
(************************************)

type spec_const = int
type spec_local_element = element

type spec_local_expr =
    SpecLocalExprVar of spec_variable_name
  | SpecLocalExprConst of spec_const
  | SpecLocalExprArity of spec_local_element
  | SpecLocalExprAdd of (spec_local_expr * spec_local_expr)
  | SpecLocalExprSub of (spec_local_expr * spec_local_expr)
  | SpecLocalExprMul of (spec_const * spec_local_expr)


type spec_op =  SpecLt | SpecLEq | SpecEq | SpecGEq | SpecGt | SpecNEq

type local_specification =
    SpecLocalTrue
  | SpecLocalOp of (spec_local_expr * spec_op * spec_local_expr)
  | SpecLocalAnd of (local_specification * local_specification)
  | SpecLocalOr of (local_specification * local_specification)
  | SpecLocalImpl of (local_specification * local_specification)
  | SpecLocalNot of local_specification


type spec_repository_constraints = repository_name list
type spec_resource_constraint = (resource_name * spec_op * spec_const)
type spec_resource_constraints = spec_resource_constraint list

type spec_element = 
    SpecElementPackage of package_name
  | SpecElementComponentType of component_type_name
  | SpecElementPort of port_name
  | SpecElementLocalisation of (spec_resource_constraints * spec_repository_constraints * local_specification)

type spec_expr =
    SpecExprVar of spec_variable_name
  | SpecExprConst of spec_const
  | SpecExprArity of spec_element
  | SpecExprAdd of (spec_expr * spec_expr)
  | SpecExprSub of (spec_expr * spec_expr)
  | SpecExprMul of (spec_const * spec_expr)


type specification = 
  | SpecTrue
  | SpecOp of (spec_expr * spec_op * spec_expr)
  | SpecAnd of (specification * specification)
  | SpecOr of (specification * specification)
  | SpecImpl of (specification * specification)
  | SpecNot of specification


(************************************)
(** 5/6 Optimization function       *)
(************************************)

type optimization_function = 
  | Compact_optimization_function
  | Conservative_optimization_function
  | Spread_optimization_function
  | None_optimization_function


(************************************)
(** 6/6 string_of                   *)
(************************************)

val string_of_element : element -> string



