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

(* Depends on 
 - datatypes/Data_common (sets and maps)
*)

open Data_common

(* TODO: use (id = int) instead of names in the mappings, for efficiency. This maybe requires temporary mappings to replace all name references by ints *)

(** Types of names and basic values for Zephyrus data. *)

(** Generally all names are implemented as strings, but we keep them as separate types
    in case if we want to change implementation in the future. *)


(** A name of a component type in the universe. *)
type component_type_name = string
let string_of_component_type_name component_type_name = component_type_name
module Component_type_name = String
module Component_type_name_set = SetString
module Component_type_name_map = MapString

(** Assertion: all the component type names in the universe must be different. *)


(** A name of a port in provided or required or conflicted by a component type. *)
type port_name = string
let string_of_port_name port_name = port_name
module Port_name = String
module Port_name_set = SetString
module Port_name_set_set = Set.Make(Port_name_set)
module Port_name_map = MapString

(** Assertion: all the port names in the universe must be different. *)


(** A quantity describing to how many other components this component type can provide a port.
    Note: some component types can provide an infinite amount of a port *)
type provide_arity = 
  | FiniteProvide of int
  | InfiniteProvide 
let string_of_provide_arity provide_arity = match provide_arity with | InfiniteProvide -> "infinite" | FiniteProvide i -> string_of_int i


(** A quantity describing how many bindings with different components providing a port are required by this component type. 
    Note: it is always finite, because an infinite require arity would be simply insatiable. *)
type require_arity = int
let string_of_require_arity require_arity = string_of_int require_arity


(** A name of a location in the configuration. *)
type location_name = string
let string_of_location_name location_name = location_name
module Location_name = String
module Location_name_set = SetString
module Location_name_map = MapString

(** Assertion: all the location names in the configuration must be different. *)


(** A name of a component in the configuration. *)
type component_name = string
let string_of_component_name component_name = component_name
module Component_name = String
module Component_name_set = SetString
module Component_name_map = MapString

(** Assertion: all the component names in the configuration must be different. *)


(** A name of a repository in the universe. *)
type repository_name = string
let string_of_repository_name repository_name = repository_name
module Repository_name = String
module Repository_name_set     = SetString
module Repository_name_set_set = Set.Make(Repository_name_set)
module Repository_name_map     = MapString

(** Assertion: all the repository names in the universe must be different. *)


(** A name of a package in a repository. *)
type package_name = string
let string_of_package_name package_name = package_name
module Package_name = String
module Package_name_set     = SetString
module Package_name_set_set = Set.Make(Package_name_set)
module Package_name_map     = MapString

(** Assertion: all the package names in the universe must be different. *)


(** A name of a resource provided by a location or consumed by a component type or a package. *)
type resource_name = string
let string_of_resource_name resource_name = resource_name
module Resource_name = String
module Resource_name_set     = SetString
module Resource_name_set_set = Set.Make(Resource_name_set)
module Resource_name_map     = MapString

(** Assertion: all the resource names in the universe must be different. *)


(** A quantity describing how much units of a resource is provided by a location. *)
type resource_provide_arity = int
let string_of_resource_provide_arity resource_provide_arity = string_of_int resource_provide_arity


(** A quantity describing how much units of a resource is consumed by a component type or a package. *)
type resource_consumption = int
let string_of_resource_consumption resource_consumption = string_of_int resource_consumption



(** Component type. *)

type component_type = {
  component_type_name     : component_type_name;                     (** The name of this component type. *)
  component_type_provide  : provide_arity Port_name_map.t;           (** Which ports does this component provide and with what arities. *)
  component_type_require  : require_arity Port_name_map.t;           (** Which ports does this component require and with what arities. *)
  component_type_conflict : Port_name_set.t;                         (** With which ports is this component type in conflict. *)
  component_type_consume  : resource_consumption Resource_name_map.t (** Which resources does this component consume and in what amounts. *)
}

module Component_type =
  struct
    type t = component_type
    let compare component_type_1 component_type_2 = 
      String.compare component_type_1.component_type_name component_type_2.component_type_name 
  end

module Component_type_set = Set.Make(Component_type)



(** Package. *)

type package = {
  package_name     : package_name;                            (** The name of this package. *)
  package_depend   : Package_name_set_set.t;                  (** Which packages does this package depend on (a disjunction of conjunctions). *)
  package_conflict : Package_name_set.t;                      (** Which packages is this package is in conflict with. *)
  package_consume  : resource_consumption Resource_name_map.t (** Which resources does this package consume and in what amounts. *)
}

module Package =
  struct
    type t = package
    let compare package_1 package_2 = 
      String.compare package_1.package_name package_2.package_name 
  end

module Package_set = Set.Make(Package)
module Package_set_set = Set.Make(Package_set)



(** Repository. *)

type repository = {
  repository_name     : repository_name;           (** The name of this repository. *)
  repository_packages : package Package_name_map.t (** Which packages does this repository contain. *)
}

module Repository =
  struct
    type t = repository
    let compare repository_1 repository_2 = 
      String.compare repository_1.repository_name repository_2.repository_name 
  end

module Repository_set = Set.Make(Repository)



(** Universe. *)
type universe = {
  universe_component_types : component_type Component_type_name_map.t;     (** Component types available in this universe. *)
  universe_implementation  : Package_name_set.t Component_type_name_map.t; (** Which packages can implement the component types of this universe. *)
  universe_repositories    : repository Repository_name_map.t;             (** Package repositories available in this universe. *)
}

(** Conventions: 
    {ul
    {- If a component type is not mentioned in the implementation at all it means that it cannot be implemented (there is no package implementing it). }
    } *)



(** Location. *)
type location = {
  location_name               : location_name;                              (** The name of this location. *)
  location_repository         : repository_name;                            (** The name of the package repository used by this location. *)
  location_packages_installed : Package_name_set.t;                         (** Names of packages installed at this location. *)
  location_provide_resources  : resource_provide_arity Resource_name_map.t; (** Which resources does this location provide and in what amounts. *)
}

(** Assertions:
    {ul
    {- The repository name mentioned by a location must correspond to a repository from the universe. }
    {- All the package names mentioned by a location must correspond to packages from the universe. }
    {- All the packages installed in a location must be available in the package repository which is used by this location. }
    } *)

module Location =
  struct
    type t = location
    let compare location_1 location_2 = 
      String.compare location_1.location_name location_2.location_name 
  end

module Location_set = Set.Make(Location)



(** Component. *)

type component = {
  component_name     : component_name;
  component_type     : component_type_name;
  component_location : location_name;
}

(** Assertions:
    {ul
    {- The component type name mentioned by a location must correspond to a component type from the universe. }
    {- The location name mentioned by a component must correspond to a location from the configuration. }
    } *)

module Component =
  struct
    type t = component
    let compare component_1 component_2 = 
      String.compare component_1.component_name component_2.component_name 
  end

module Component_set = Set.Make(Component)


(** Binding. *)

type binding = {
  binding_port     : port_name;
  binding_requirer : component_name;
  binding_provider : component_name
}

(** Assertions:
    {ul
    {- The component names mentioned by a binding must correspond to components from the configuration. }
    {- The port mentioned by a binding must be required by the requirer component and provided by the provider component. }
    } *)

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

module Binding_set = Set.Make(Binding)


(** Configuration. *)

type configuration = {
  configuration_locations  : location Location_name_map.t;
  configuration_components : component Component_name_map.t;
  configuration_bindings   : Binding_set.t
}




(** Type definitions for Specification. *)

type spec_variable_name = string
let string_of_spec_variable_name spec_variable_name = spec_variable_name

type spec_const = int
let string_of_spec_const spec_const = string_of_int spec_const

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
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)

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



type optimization_function = 
  | Optimization_function_simple
  | Optimization_function_compact
  | Optimization_function_conservative
  | Optimization_function_spread
  | Optimization_function_none


