
(** Type definitions for naming. *)

(* Two basic modules, which are reused many times after: 
   a Map with strings as its keys and a Set with strings as its elements. *)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type component_type_name = string
module ComponentTypeNameMap = StringMap
type port_name = string
module PortNameSet = StringSet
module PortNameMap = StringMap

type provide_arity = 
  | InfiniteProvide 
  | FiniteProvide of int

type require_arity = int

type repository_name = string
module RepositoryNameMap = StringMap
type package_name = string
module PackageNameSet = StringSet
module PackageNameMap = StringMap

type resource_name = string
module ResourceNameMap = StringMap

type resource_consumption = int
type resource_provide_arity = int

type location_name = string
module LocationNameMap = StringMap

type component_name = string
module ComponentNameMap = StringMap

(** Type definitions for Component Type. *)




type component_type = {
  component_type_name     : component_type_name;
  component_type_provide  : provide_arity PortNameMap.t;
  component_type_require  : require_arity PortNameMap.t;
  component_type_conflict : PortNameSet.t;
  component_type_consume  : resource_consumption ResourceNameMap.t
}

(** Type definitions for Universe. *)

module ComponentType =
  struct
    type t = component_type
    let compare component_type_1 component_type_2 = 
      String.compare component_type_1.component_type_name component_type_2.component_type_name 
  end

module ComponentTypeSet = Set.Make(ComponentType)

type component_types = ComponentTypeSet.t

module PackageNameSetSet = Set.Make(PackageNameSet)

type package = {
  package_name     : package_name;
  package_depend   : PackageNameSetSet.t;
  package_conflict : PackageNameSet.t;
  package_consume  : resource_consumption ResourceNameMap.t
}

module Package =
  struct
    type t = package
    let compare package_1 package_2 = 
      String.compare package_1.package_name package_2.package_name 
  end

module PackageSet = Set.Make(Package)

type packages = PackageSet.t

type repository = {
  repository_name     : repository_name;
  repository_packages : package PackageNameMap.t
}

module Repository =
  struct
    type t = repository
    let compare repository_1 repository_2 = 
      String.compare repository_1.repository_name repository_2.repository_name 
  end

module RepositorySet = Set.Make(Repository)

type repositories = RepositorySet.t

type package_names = PackageNameSet.t

(** Type definitions for Configuration. *)
type universe = {
  universe_component_types : component_type ComponentTypeNameMap.t;
  universe_implementation  : PackageNameSet.t ComponentTypeNameMap.t;
  universe_repositories    : repository RepositoryNameMap.t;
}

type resources_provided = resource_provide_arity ResourceNameMap.t

type location = {
  location_name               : location_name;
  location_provide_resources  : resources_provided;
  location_repository         : repository_name;
  location_packages_installed : package_names;
}

module Location =
  struct
    type t = location
    let compare location_1 location_2 = 
      String.compare location_1.location_name location_2.location_name 
  end

module LocationSet = Set.Make(Location)

type component = {
  component_name     : component_name;
  component_type     : component_type_name;
  component_location : location_name;
}

module Component =
  struct
    type t = component
    let compare component_1 component_2 = 
      String.compare component_1.component_name component_2.component_name 
  end

module ComponentSet = Set.Make(Component)

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

module Binding =
  struct
    type t = binding
    let compare binding_1 binding_2 = 
      lexicographic_compare String.compare [
        (binding_1.binding_port,     binding_2.binding_port);
        (binding_1.binding_requirer, binding_2.binding_requirer);
        (binding_1.binding_provider, binding_2.binding_provider)
      ]
  end

module BindingSet = Set.Make(Binding)

type configuration = {
  configuration_locations  : location LocationNameMap.t;
  configuration_components : component ComponentNameMap.t;
  configuration_bindings   : BindingSet.t
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
