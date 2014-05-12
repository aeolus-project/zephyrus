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

(** Stateful abstract syntax used as a common intermediate form of the stateful Zephyrus model. *)

(** This intermediate form makes it easier to read and write
    the stateful model and to convert it to and from a stateless model. *)

(** {2 General type definitions for naming.} *)

type resource_name       = string
type port_name           = string
type component_type_name = string
type state_name          = string
type package_name        = string
type repository_name     = string
type location_name       = string
type component_name      = string


(** {2 Type definitions for the universe.} *)

(** Type definitions for provides / requires. *)
type provide_arity = FiniteProvide of int | InfiniteProvide
type require_arity = int

(** Type definitions for resource provide / consume. *)
type resource_consumption   = int
type resource_provide_arity = int

(** Type definitions for component types. *)
type state = {
  state_name       : state_name;
  state_initial    : bool;
  state_provide    : (port_name * provide_arity) list;
  state_require    : (port_name * require_arity) list;
  state_conflict   : port_name list;
  state_successors : state_name list;
}

type component_type = {
  component_type_name     : component_type_name;
  component_type_states   : state list;
  component_type_consume  : (resource_name * resource_consumption) list
}

(** Type definitions for packages. *)
type package = {
  package_name     : package_name;
  package_depend   : package_name list list;
  package_conflict : package_name list;
  package_consume  : (resource_name * resource_consumption) list
}

(** Type definitions for package repositories. *)
type repository = {
  repository_name     : repository_name;
  repository_packages : package list
}

(** Type definitions for universe. *)
type universe = {
  universe_component_types : component_type list;
  universe_implementation  : (component_type_name * (repository_name * package_name) list) list;
  universe_repositories    : repository list
}

let universe_add_repositories (universe : universe) (repositories : repository list) : universe = 
  {
    universe_component_types = universe.universe_component_types;
    universe_implementation  = universe.universe_implementation;
    universe_repositories    = universe.universe_repositories @ repositories;
  }  


(** {2 Type definitions for the configuration.} *)

(** Type definitions for locations. *)
type location_cost = int

type location = {
  location_name               : location_name;
  location_provide_resources  : (resource_name * resource_provide_arity) list;
  location_repository         : repository_name;
  location_packages_installed : package_name list;
  location_cost               : location_cost
}

(** Type definitions for components. *)
type component = {
  component_name     : component_name;
  component_type     : component_type_name;
  component_state    : state_name;
  component_location : location_name
}

(** Type definitions for bindings. *)
type binding = {
  binding_port     : port_name;
  binding_requirer : component_name;
  binding_provider : component_name
}

(** Type definitions for configurations. *)
type configuration = {
  configuration_locations  : location list;
  configuration_components : component list;
  configuration_bindings   : binding list
}


(** {2 Type definitions for the specification.} *)

type spec_variable_name = string

type spec_const = int

type spec_local_element =
  | SpecLocalElementPackage       of repository_name * package_name
  | SpecLocalElementComponentType of component_type_name * state_name option
  | SpecLocalElementPort          of port_name

type spec_local_expr =
  | SpecLocalExprVar   of spec_variable_name
  | SpecLocalExprConst of spec_const
  | SpecLocalExprArity of spec_local_element
  | SpecLocalExprAdd   of spec_local_expr * spec_local_expr
  | SpecLocalExprSub   of spec_local_expr * spec_local_expr
  | SpecLocalExprMul   of spec_const * spec_local_expr

type spec_op = Lt | LEq | Eq | GEq | Gt | NEq

type local_specification =
  | SpecLocalTrue
  | SpecLocalOp   of spec_local_expr * spec_op * spec_local_expr
  | SpecLocalAnd  of local_specification * local_specification
  | SpecLocalOr   of local_specification * local_specification
  | SpecLocalImpl of local_specification * local_specification
  | SpecLocalNot  of local_specification

type spec_repository_constraint = repository_name

type spec_repository_constraints = spec_repository_constraint list

type spec_resource_constraint = resource_name * spec_op * spec_const

type spec_resource_constraints = spec_resource_constraint list

type spec_element =
  | SpecElementPackage       of repository_name * package_name
  | SpecElementComponentType of component_type_name
  | SpecElementPort          of port_name
  | SpecElementLocalisation  of spec_resource_constraints * spec_repository_constraints * local_specification

type spec_expr =
  | SpecExprVar   of spec_variable_name
  | SpecExprConst of spec_const
  | SpecExprArity of spec_element
  | SpecExprAdd   of spec_expr * spec_expr
  | SpecExprSub   of spec_expr * spec_expr
  | SpecExprMul   of spec_const * spec_expr

type spec_location_names = location_name list

type specification =
  | SpecTrue
  | SpecOp         of spec_expr * spec_op * spec_expr
  | SpecAnd        of specification * specification
  | SpecOr         of specification * specification
  | SpecImpl       of specification * specification
  | SpecNot        of specification
  | SpecEverywhere of                       local_specification
  | SpecAt         of spec_location_names * local_specification

type optimization_function = 
  | Optimization_function_simple
  | Optimization_function_compact
  | Optimization_function_conservative
  | Optimization_function_spread
  | Optimization_function_none


type initial_model = {
  universe              : universe              option;
  initial_configuration : configuration         option;
  specification         : specification         option;
  optimization_function : optimization_function option;  
}