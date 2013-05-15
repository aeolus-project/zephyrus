
(** Type definitions for naming. *)

(* Two basic modules, which are reused many times after: 
   a Map with strings as its keys and a Set with strings as its elements. *)
module String_map = Map.Make(String)
module String_set = Set.Make(String)

type component_type_name = string
module ComponentTypeNameOrdering = String
module Component_type_name_set = Set.Make(ComponentTypeNameOrdering)
module Component_type_name_map = Map.Make(ComponentTypeNameOrdering)
type port_name = string
module Port_name_ordering = String
module Port_name_set = Set.Make(Port_name_ordering)
module Port_name_map = Map.Make(Port_name_ordering)

module Port_name_set_set = Set.Make(Port_name_set)

type provide_arity = 
  | InfiniteProvide 
  | FiniteProvide of int

type require_arity = int

type repository_name = string
module Repository_name_ordering = String
module Repository_name_set = Set.Make(Repository_name_ordering)
module Repository_name_set_set = Set.Make(Repository_name_set)
module Repository_name_map = Map.Make(Repository_name_ordering)
type package_name = string
module Package_name_ordering = String
module Package_name_set = Set.Make(Package_name_ordering)
module Package_name_map = Map.Make(Package_name_ordering)

type resource_name = string
module Resource_name_ordering = String
module Resource_name_set = Set.Make(Resource_name_ordering)
module Resource_name_set_set = Set.Make(Resource_name_set)
module Resource_name_map = Map.Make(Resource_name_ordering)

type resource_consumption = int
type resource_provide_arity = int

type location_name = string
module Location_name_ordering = String
module Location_name_set = Set.Make(Location_name_ordering)
module Location_name_map = Map.Make(Location_name_ordering)

type component_name = string
module Component_name_ordering = String
module Component_name_map = Map.Make(Component_name_ordering)


(** Type definitions for Component Type. *)




type component_type = {
  component_type_name     : component_type_name;
  component_type_provide  : provide_arity Port_name_map.t;
  component_type_require  : require_arity Port_name_map.t;
  component_type_conflict : Port_name_set.t;
  component_type_consume  : resource_consumption Resource_name_map.t
}

(** Type definitions for Universe. *)

module Component_type_ordering =
  struct
    type t = component_type
    let compare component_type_1 component_type_2 = 
      String.compare component_type_1.component_type_name component_type_2.component_type_name 
  end

module Component_type_set = Set.Make(Component_type_ordering)

type component_types = Component_type_set.t

module Package_name_set_set = Set.Make(Package_name_set)

type package = {
  package_name     : package_name;
  package_depend   : Package_name_set_set.t;
  package_conflict : Package_name_set.t;
  package_consume  : resource_consumption Resource_name_map.t
}

module Package_ordering =
  struct
    type t = package
    let compare package_1 package_2 = 
      String.compare package_1.package_name package_2.package_name 
  end

module Package_set = Set.Make(Package_ordering)
module Package_set_set = Set.Make(Package_set)

type packages = Package_set.t

type repository = {
  repository_name     : repository_name;
  repository_packages : package Package_name_map.t
}

module Repository_ordering =
  struct
    type t = repository
    let compare repository_1 repository_2 = 
      String.compare repository_1.repository_name repository_2.repository_name 
  end

module Repository_set = Set.Make(Repository_ordering)

type repositories = Repository_set.t

type package_names = Package_name_set.t

(** Type definitions for Configuration. *)
type universe = {
  universe_component_types : component_type Component_type_name_map.t;
  universe_implementation  : Package_name_set.t Component_type_name_map.t;
  universe_repositories    : repository Repository_name_map.t;
}

type resources_provided = resource_provide_arity Resource_name_map.t

type location = {
  location_name               : location_name;
  location_provide_resources  : resources_provided;
  location_repository         : repository_name;
  location_packages_installed : package_names;
}

module Location_ordering =
  struct
    type t = location
    let compare location_1 location_2 = 
      String.compare location_1.location_name location_2.location_name 
  end

module Location_set = Set.Make(Location_ordering)

type component = {
  component_name     : component_name;
  component_type     : component_type_name;
  component_location : location_name;
}

module Component_ordering =
  struct
    type t = component
    let compare component_1 component_2 = 
      String.compare component_1.component_name component_2.component_name 
  end

module Component_set = Set.Make(Component_ordering)

type binding = {
  binding_port     : port_name;
  binding_requirer : component_name;
  binding_provider : component_name
}

let lexicographic_compare (compare : 'a -> 'b -> int) (l : ('a * 'b) list) =
  List.fold_left (fun (a : int) ( (h1, h2) : ('a * 'b) ) -> 
    if a != 0
    then a
    else compare h1 h2
  ) 0 l

module Binding_ordering =
  struct
    type t = binding
    let compare binding_1 binding_2 = 
      lexicographic_compare String.compare [
        (binding_1.binding_port,     binding_2.binding_port);
        (binding_1.binding_requirer, binding_2.binding_requirer);
        (binding_1.binding_provider, binding_2.binding_provider)
      ]
  end

module Binding_set = Set.Make(Binding_ordering)

type configuration = {
  configuration_locations  : location Location_name_map.t;
  configuration_components : component Component_name_map.t;
  configuration_bindings   : Binding_set.t
}

(** Type definitions for Specification. *)

type spec_variable_name = string

type spec_const = int

type spec_local_element = 
  | SpecLocalElementPackage of package_name
  | SpecLocalElementComponentType of component_type_name
  | SpecLocalElementPort of port_name

type spec_local_expr = 
  | SpecLocalExprVar of spec_variable_name
  | SpecLocalExprConst of spec_const
  | SpecLocalExprArity of spec_local_element
  | SpecLocalExprAdd of (spec_local_expr * spec_local_expr)
  | SpecLocalExprSub of (spec_local_expr * spec_local_expr)
  | SpecLocalExprMul of (spec_const * spec_local_expr)

type spec_op = 
  | Lt 
  | LEq 
  | Eq 
  | GEq 
  | Gt 
  | NEq

type local_specification = 
  | SpecLocalTrue
  | SpecLocalOp of (spec_local_expr * spec_op * spec_local_expr)
  | SpecLocalAnd of (local_specification * local_specification)
  | SpecLocalOr of (local_specification * local_specification)
  | SpecLocalImpl of (local_specification * local_specification)
  | SpecLocalNot of local_specification

type spec_repository_constraint = repository_name

type spec_repository_constraints = spec_repository_constraint list

type spec_resource_constraint = (resource_name * spec_op * spec_const)

type spec_resource_constraints = spec_resource_constraint list

type spec_element = 
  | SpecElementPackage of package_name
  | SpecElementComponentType of component_type_name
  | SpecElementPort of port_name
  | SpecElementLocalisation of (spec_resource_constraints * spec_repository_constraints * local_specification)

type spec_expr = 
  | SpecExprVar of spec_variable_name
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
