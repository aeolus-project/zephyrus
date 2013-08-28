(* Auto-generated from "json.atd" *)


(** Type definitions for naming. *)

type component_type_name = string

type port_name = string

type component_name = string

type package_name = string

type repository_name = string

type location_name = string

(** Type definitions for Component Type. *)
type resource_name = string

type provide_arity = [ `InfiniteProvide | `FiniteProvide of int ]

type require_arity = int

type resource_consumption = int

type resource_provide_arity = int

type component_type = {
  component_type_name (*atd name *): component_type_name;
  component_type_provide (*atd provide *): (port_name * provide_arity) list;
  component_type_require (*atd require *): (port_name * require_arity) list;
  component_type_conflict (*atd conflict *): port_name list;
  component_type_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

(** Type definitions for Universe. *)
type component_types = component_type list

type package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): package_name list list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type packages = package list

type repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = repository list

type package_names = (repository_name * package_name) list

(** Type definitions for Configuration. *)
type universe = {
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * package_names) list;
  universe_repositories (*atd repositories *): repositories
}

type resources_provided = (resource_name * resource_provide_arity) list

type location_cost = int

type location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component = {
  component_name: component_name;
  component_type: component_type_name;
  component_location: location_name
}

type binding = {
  binding_port (*atd port *): port_name;
  binding_requirer (*atd requirer *): component_name;
  binding_provider (*atd provider *): component_name
}

(** Type definitions for Specification. *)
type configuration = {
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}

type spec_variable_name = string

type spec_const = int

type spec_local_element = [
    `SpecLocalElementPackage of (repository_name * package_name)
  | `SpecLocalElementComponentType of component_type_name
  | `SpecLocalElementPort of port_name
]

type spec_local_expr = [
    `SpecLocalExprVar of spec_variable_name
  | `SpecLocalExprConst of spec_const
  | `SpecLocalExprArity of spec_local_element
  | `SpecLocalExprAdd of (spec_local_expr * spec_local_expr)
  | `SpecLocalExprSub of (spec_local_expr * spec_local_expr)
  | `SpecLocalExprMul of (spec_const * spec_local_expr)
]

type spec_op = [ `Lt | `LEq | `Eq | `GEq | `Gt | `NEq ]

type local_specification = [
    `SpecLocalTrue
  | `SpecLocalOp of (spec_local_expr * spec_op * spec_local_expr)
  | `SpecLocalAnd of (local_specification * local_specification)
  | `SpecLocalOr of (local_specification * local_specification)
  | `SpecLocalImpl of (local_specification * local_specification)
  | `SpecLocalNot of local_specification
]

type spec_repository_constraint = repository_name

type spec_repository_constraints = spec_repository_constraint list

type spec_resource_constraint = (resource_name * spec_op * spec_const)

type spec_resource_constraints = spec_resource_constraint list

type spec_element = [
    `SpecElementPackage of (repository_name * package_name)
  | `SpecElementComponentType of component_type_name
  | `SpecElementPort of port_name
  | `SpecElementLocalisation
      of (
          spec_resource_constraints
        * spec_repository_constraints
        * local_specification
      )
]

type spec_expr = [
    `SpecExprVar of spec_variable_name
  | `SpecExprConst of spec_const
  | `SpecExprArity of spec_element
  | `SpecExprAdd of (spec_expr * spec_expr)
  | `SpecExprSub of (spec_expr * spec_expr)
  | `SpecExprMul of (spec_const * spec_expr)
]

type specification = [
    `SpecTrue
  | `SpecOp of (spec_expr * spec_op * spec_expr)
  | `SpecAnd of (specification * specification)
  | `SpecOr of (specification * specification)
  | `SpecImpl of (specification * specification)
  | `SpecNot of specification
]
