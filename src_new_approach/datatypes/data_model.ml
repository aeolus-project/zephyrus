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


(** Types for main values for Zephyrus data. *)
(** For efficiency and to avoid name clash, all names are paired with identifiers *)


(************************************)
(** 1. Resources                    *)
(************************************)

  (** A name of a resource provided by a location or consumed by a component type or a package. *)
type resource_name = string
module Resource_name = String
module Resource_name_set     = SetString
module Resource_name_set_set = SetSetString
module Resource_name_map     = MapString

type resource_id = int
module Resource_id = Int
module Resource_id_set     = SetInt
module Resource_id_set_set = SetSetInt
module Resource_id_map     = MapInt
module Resource_id_map_extract_key = Keys_of_MapInt

  (** A quantity describing how much units of a resource is provided by a location. *)
type resource_provide_arity = int
  (** A quantity describing how much units of a resource is consumed by a component type or a package. *)
type resource_consume_arity = int

  (** Resource set *)
class type resources = object
  method resource_names : Resource_name_set.t
  method resource_ids   : Resource_id_set.t
  method get_name       : resource_id -> resource_name
  method get_id         : resource_name -> resource_id
end


(************************************)
(** 2. Universe                     *)
(************************************)
  (** 2.1. Component types *)

  (** The name of a component type in the universe. *)
type component_type_name = string
module Component_type_name     = String
module Component_type_name_set = SetString
module Component_type_name_map = MapString

type component_type_id = int
let deprecated_component_type_id = -1
module Component_type_id     = Int
module Component_type_id_set = SetInt
module Component_type_id_map = MapInt
module Component_type_id_map_extract_key = Keys_of_MapInt

  (** The name of a port in provided or required or conflicted by a component type. *)
type port_name = string
module Port_name         = String
module Port_name_set     = SetString
module Port_name_set_set = SetSetString
module Port_name_map     = MapString

type port_id = int
module Port_id         = Int
module Port_id_set     = SetInt
module Port_id_set_set = SetSetInt
module Port_id_map     = MapInt
module Port_id_map_extract_key = Keys_of_MapInt

type port = port_id
module Port         = Port_id
module Port_set     = Port_id_set
module Port_set_set = Port_id_set_set
module Port_map     = Port_id_map
module Port_map_extract_key = Port_id_map_extract_key

  (** A quantity describing to how many other components this component type can provide a port.
    Note: some component types can provide an infinite amount of a port *)
type provide_arity = 
  | Finite_provide of int
  | Infinite_provide 

  (** A quantity describing how many bindings with different components providing a port are required by this component type. 
    Note: it is always finite, because an infinite require arity would be simply insatiable. *)
type require_arity = int

  (** Component type. *)

class type component_type = object
  method name           : component_type_name                     (** The name of this component type. *)
  method provide        : port_id -> provide_arity                (** Which ports does this component provide and with what arities. *)
  method provide_domain : Port_id_set.t
  method require        : port_id -> require_arity                (** Which ports does this component require and with what arities. *)
  method require_domain : Port_id_set.t
  method conflict       : Port_id_set.t                           (** With which ports is this component type in conflict. *)
  method consume        : resource_id -> resource_consume_arity   (** Which resources does this component consume and in what amounts. *)
end

module Component_type = struct
  type t = component_type
  let compare t1 t2 = String.compare t1#name t2#name 
end module Component_type_set = Set.Make(Component_type) module Component_type_map = Map.Make(Component_type)


  (** 2.2. Packages *)

  (** A name of a package in a repository. *)
type package_name = string
module Package_name         = String
module Package_name_set     = SetString
module Package_name_set_set = SetSetString
module Package_name_map     = MapString

type package_id = int
let deprecated_package_id = -1
module Package_id         = Int
module Package_id_set     = SetInt
module Package_id_set_set = SetSetInt
module Package_id_map     = MapInt
module Package_id_map_extract_key   = Keys_of_MapInt


  (** Package. *)
class type package = object
  method name     : package_name                              (** The name of this package. *)
  method depend   : Package_id_set_set.t                      (** Which packages does this package depend on (a disjunction of conjunctions). *)
  method conflict : Package_id_set.t                          (** Which packages is this package is in conflict with. *)
  method consume  : resource_id -> resource_consume_arity     (** Which resources does this package consume and in what amounts. *)
end

module Package = struct
  type t = package
  let compare k1 k2 = String.compare k1#name k2#name 
end module Package_set = Set.Make(Package) module Package_set_set = Set.Make(Package_set)


  (** 2.3. Repositories *)

  (** A name of a repository in the universe. *)
type repository_name = string
module Repository_name = String
module Repository_name_set     = SetString
module Repository_name_set_set = SetSetString
module Repository_name_map     = MapString

type repository_id = int
module Repository_id = Int
module Repository_id_set     = SetInt
module Repository_id_set_set = SetSetInt
module Repository_id_map     = MapInt
module Repository_id_map_extract_key = Keys_of_MapInt

  (** Repository. *)
class type repository = object
  method name     : repository_name         (** The name of this repository. *)
  method get_package : package_id -> package   (** Which packages does this repository contain. *)

  method packages    : Package_set.t
  method package_ids : Package_id_set.t
end

module Repository = struct
  type t = repository
  let compare r1 r2 = String.compare r1#name r2#name 
end module Repository_set = Set.Make(Repository)


  (** 3.3. Universes *)
class type universe = object
  (* basic methods *)
  method get_component_type : component_type_id -> component_type      (** Component types available in this universe. *)
  method get_implementation : component_type_id -> Package_id_set.t (** Which packages implement the component types of this universe. *)
  method get_repository     : repository_id -> repository               (** Package repositories available in this universe. *)
  method get_package        : package_id -> package

  method repository_of_package : package_id -> repository_id

  method get_component_types : Component_type_set.t
  method get_repositories    : Repository_set.t
  method get_packages        : Package_set.t

  method get_port_ids           : Port_id_set.t
  method get_component_type_ids : Component_type_id_set.t
  method get_repository_ids     : Repository_id_set.t
  method get_package_ids        : Package_id_set.t

  method get_port_names           : Port_name_set.t
  method get_component_type_names : Component_type_name_set.t
  method get_repository_names     : Repository_name_set.t
  method get_package_names        : Package_name_set.t

  (* methods coming from the paper. Usually, aliases for well-named functions *)
  method u_dt : Component_type_id_set.t
  method u_dp : Port_id_set.t
  method u_dr : Repository_id_set.t
  method u_dk : Package_id_set.t
    
  method u_i : component_type_id -> Package_id_set.t
  method u_w : package_id -> package

  method ur : port_id -> Component_type_id_set.t
  method up : port_id -> Component_type_id_set.t
  method uc : port_id -> Component_type_id_set.t

  (* methods for naming *)
  method get_port_id           : port_name -> port_id
  method get_component_type_id : component_type_name -> component_type_id
  method get_repository_id     : repository_name -> repository_id
  method get_package_id        : repository_id -> package_name -> package_id

  method get_port_name           : port_id -> port_name
  method get_component_type_name : component_type_id -> component_type_name
  method get_repository_name     : repository_id -> repository_name
  method get_package_name        : package_id -> package_name
end




(************************************)
(** 4. Configuration                *)
(************************************)

  (** 4.1. Locations *)
  (** A name of a location in the configuration. *)
type location_name = string
module Location_name = String
module Location_name_set = SetString
module Location_name_map = MapString

type location_id = int
module Location_id = Int
module Location_id_set = SetInt
module Location_id_set_set = SetSetInt
module Location_id_map = MapInt
module Location_id_map_extract_key = Keys_of_MapInt

  (** Location. *)
class type location = object
  method name               : location_name                            (** The name of this location. *)
  method repository         : repository_id                            (** The name of the package repository used by this location. *)
  method packages_installed : Package_id_set.t                         (** Names of packages installed at this location. *)
  method provide_resources  : resource_id -> resource_provide_arity    (** Which resources does this location provide and in what amounts. *)
end

module Location = struct
  type t = location
  let compare l1 l2 = String.compare l1#name l2#name 
end 
module Location_set = Set.Make(Location)
module Location_map = Map.Make(Location)

(** Assertions:
    {ul
    {- The repository name mentioned by a location must correspond to a repository from the universe. }
    {- All the package names mentioned by a location must correspond to packages from the universe. }
    {- All the packages installed in a location must be available in the package repository which is used by this location. }
    } *)


  (** 4.2. Component. *)
  (** A name of a component in the configuration. *)
type component_name = string
module Component_name = String
module Component_name_set = SetString
module Component_name_map = MapString

type component_id = int
module Component_id = Int
module Component_id_set = SetInt
module Component_id_map = MapInt
module Component_id_map_extract_key = Keys_of_MapInt

  (** Components *)
class type component = object
  method name     : component_name
  method typ      : component_type_id
  method location : location_id
end

module Component = struct
  type t = component
  let compare c1 c2 = String.compare c1#name c2#name 
end 
module Component_set = Set.Make(Component)
module Component_map = Map.Make(Component)

(** Assertions:
    {ul
    {- The component type name mentioned by a location must correspond to a component type from the universe. <- NO}
    {- The location name mentioned by a component must correspond to a location from the configuration. }
    } *)



  (** 4.3. Binding. *)
class type binding = object
  method port     : port_id
  method requirer : component_id
  method provider : component_id
end

module Binding = struct
  type t = binding
  let compare b1 b2 =
    let r1 = b1#port - b2#port in if r1 = 0 then
      let r2 = Component_id.compare b1#requirer b2#requirer in if r2 = 0 then
        Component_id.compare b1#provider b2#provider
      else r2
    else r1
end module Binding_set = Set.Make(Binding)

(** Assertions:
    {ul
    {- The component names mentioned by a binding must correspond to components from the configuration. }
    {- The port mentioned by a binding must be required by the requirer component and provided by the provider component. }
    } *)


  (** 4.4. Configuration. *)
class type configuration = object
  (* basic methods *)
  method get_location  : location_id -> location
  method get_component : component_id -> component

  method get_locations  : Location_set.t
  method get_components : Component_set.t
  method get_bindings   : Binding_set.t

  method get_location_ids  : Location_id_set.t
  method get_component_ids : Component_id_set.t

  method get_location_names  : Location_name_set.t
  method get_component_names : Component_name_set.t

  (* methods coming from the paper. Usually, aliases for well-named functions *)
  method c_l : Location_id_set.t
  method c_c : Component_id_set.t
  method c_type : component_id -> component_type_id

  method get_local_component : location_id -> component_type_id -> Component_id_set.t
  method get_local_package : location_id -> package_id -> bool

  (* methods for naming *)
  method get_location_id  : location_name -> location_id
  method get_component_id : component_name -> component_id

  method get_location_name  : location_id -> location_name
  method get_component_name : component_id -> component_name
end



(************************************)
(** 5. Specification.               *)
(************************************)

type spec_variable_name = string
type spec_const = int

type spec_local_element = 
  | Spec_local_element_package        of package_id
  | Spec_local_element_component_type of component_type_id
  | Spec_local_element_port           of port_id

type spec_local_expr = 
  | Spec_local_expr_var   of spec_variable_name
  | Spec_local_expr_const of spec_const
  | Spec_local_expr_arity of spec_local_element
  | Spec_local_expr_add   of (spec_local_expr * spec_local_expr)
  | Spec_local_expr_sub   of (spec_local_expr * spec_local_expr)
  | Spec_local_expr_mul   of (spec_const * spec_local_expr)

type spec_op = 
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Grearter-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)

type local_specification = 
  | Spec_local_true
  | Spec_local_op of (spec_local_expr * spec_op * spec_local_expr)
  | Spec_local_and of (local_specification * local_specification)
  | Spec_local_or of (local_specification * local_specification)
  | Spec_local_impl of (local_specification * local_specification)
  | Spec_local_not of local_specification

type spec_repository_constraint = repository_id list
type spec_resource_constraint = (resource_id * spec_op * spec_const) list

type spec_element = 
  | Spec_element_package of package_id
  | Spec_element_component_type of component_type_id
  | Spec_element_port of port_id
  | Spec_element_location of (spec_resource_constraint * spec_repository_constraint * local_specification)

type spec_expr = 
  | Spec_expr_var of spec_variable_name
  | Spec_expr_const of spec_const
  | Spec_expr_arity of spec_element
  | Spec_expr_add of (spec_expr * spec_expr)
  | Spec_expr_sub of (spec_expr * spec_expr)
  | Spec_expr_mul of (spec_const * spec_expr)

type specification = 
  | Spec_true
  | Spec_op of (spec_expr * spec_op * spec_expr)
  | Spec_and of (specification * specification)
  | Spec_or of (specification * specification)
  | Spec_impl of (specification * specification)
  | Spec_not of specification


(************************************)
(** 6. Optimization function        *)
(************************************)

type optimization_function = 
  | Optimization_function_simple
  | Optimization_function_compact
  | Optimization_function_conservative
  | Optimization_function_spread
  | Optimization_function_none


