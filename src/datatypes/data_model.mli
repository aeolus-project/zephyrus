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

(** Internal representation of the Zephyrus model. *)

(* Depends on 
 - datatypes/Data_common (sets and maps)
*)

open Data_common

(** Types for main values for Zephyrus data. *)
(** For efficiency and to avoid name clash, all names are paired with identifiers *)


(*/************************************************************************\*)
(*| 1. Ids and names                                                       |*)
(*\************************************************************************/*)

(** Ids *)

(** The id of a resource provided by a location or consumed by a component type or a package. *)
type   resource_id = int
module Resource_id         : Set.OrderedType with type t = resource_id
module Resource_id_set     : Set.S with type elt = Resource_id.t
module Resource_id_set_set : Set.S with type elt = Resource_id_set.t
module Resource_id_map     : Map.S with type key = Resource_id.t
module Resource_id_map_extract_key : sig val set_of_keys : 'a Resource_id_map.t -> Resource_id_set.t end

(** The id of a port in provided or required or conflicted by a component type. *)
type   port_id = int
module Port_id         : Set.OrderedType with type t = port_id
module Port_id_set     : Set.S with type elt = Port_id.t
module Port_id_set_set : Set.S with type elt = Port_id_set.t
module Port_id_map     : Map.S with type key = Port_id.t
module Port_id_map_extract_key : sig val set_of_keys : 'a Port_id_map.t -> Port_id_set.t end

(** The id of a component type in the universe. *)
type   component_type_id = int
module Component_type_id         : Set.OrderedType with type t = component_type_id
module Component_type_id_set     : Set.S with type elt = Component_type_id.t
module Component_type_id_set_set : Set.S with type elt = Component_type_id_set.t
module Component_type_id_map     : Map.S with type key = Component_type_id.t
module Component_type_id_map_extract_key : sig val set_of_keys : 'a Component_type_id_map.t -> Component_type_id_set.t end

(** The id of a package in a repository. *)
type   package_id = int
module Package_id         : Set.OrderedType with type t = package_id
module Package_id_set     : Set.S with type elt = Package_id.t
module Package_id_set_set : Set.S with type elt = Package_id_set.t
module Package_id_map     : Map.S with type key = Package_id.t
module Package_id_map_extract_key : sig val set_of_keys : 'a Package_id_map.t -> Package_id_set.t end

(** The id of a repository in the universe. *)
type   repository_id = int
module Repository_id         : Set.OrderedType with type t = repository_id
module Repository_id_set     : Set.S with type elt = Repository_id.t
module Repository_id_set_set : Set.S with type elt = Repository_id_set.t
module Repository_id_map     : Map.S with type key = Repository_id.t
module Repository_id_map_extract_key : sig val set_of_keys : 'a Repository_id_map.t -> Repository_id_set.t end

(** The id of a location in the configuration. *)
type   location_id = int
module Location_id         : Set.OrderedType with type t = location_id
module Location_id_set     : Set.S with type elt = Location_id.t
module Location_id_set_set : Set.S with type elt = Location_id_set.t
module Location_id_map     : Map.S with type key = Location_id.t
module Location_id_map_extract_key : sig val set_of_keys : 'a Location_id_map.t -> Location_id_set.t end

(** The id of a component in the configuration. *)
type   component_id = int
module Component_id         : Set.OrderedType with type t = component_id
module Component_id_set     : Set.S with type elt = Component_id.t
module Component_id_set_set : Set.S with type elt = Component_id_set.t
module Component_id_map     : Map.S with type key = Component_id.t
module Component_id_map_extract_key : sig val set_of_keys : 'a Component_id_map.t -> Component_id_set.t end


(** Names *)

(** The name of a resource provided by a location or consumed by a component type or a package. *)
type   resource_name = string
module Resource_name         : Set.OrderedType with type t = resource_name
module Resource_name_set     : Set.S with type elt = Resource_name.t
module Resource_name_set_set : Set.S with type elt = Resource_name_set.t
module Resource_name_map     : Map.S with type key = Resource_name.t

(** The name of a component type in the universe. *)
type   component_type_name = string
module Component_type_name         : Set.OrderedType with type t = component_type_name
module Component_type_name_set     : Set.S with type elt = Component_type_name.t
module Component_type_name_set_set : Set.S with type elt = Component_type_name_set.t
module Component_type_name_map     : Map.S with type key = Component_type_name.t

(** The name of a port in provided or required or conflicted by a component type. *)
type   port_name = string
module Port_name         : Set.OrderedType with type t = port_name
module Port_name_set     : Set.S with type elt = Port_name.t
module Port_name_set_set : Set.S with type elt = Port_name_set.t
module Port_name_map     : Map.S with type key = Port_name.t

(** The name of a package in a repository. *)
type   package_name = string
module Package_name         : Set.OrderedType with type t = package_name
module Package_name_set     : Set.S with type elt = Package_name.t
module Package_name_set_set : Set.S with type elt = Package_name_set.t
module Package_name_map     : Map.S with type key = Package_name.t

(** The name of a repository in the universe. *)
type   repository_name = string
module Repository_name         : Set.OrderedType with type t = repository_name
module Repository_name_set     : Set.S with type elt = Repository_name.t
module Repository_name_set_set : Set.S with type elt = Repository_name_set.t
module Repository_name_map     : Map.S with type key = Repository_name.t

(** The name of a location in the configuration. *)
type   location_name = string
module Location_name         : Set.OrderedType with type t = location_name
module Location_name_set     : Set.S with type elt = Location_name.t
module Location_name_set_set : Set.S with type elt = Location_name_set.t
module Location_name_map     : Map.S with type key = Location_name.t

(** The name of a component in the configuration. *)
type   component_name = string
module Component_name         : Set.OrderedType with type t = component_name
module Component_name_set     : Set.S with type elt = Component_name.t
module Component_name_set_set : Set.S with type elt = Component_name_set.t
module Component_name_map     : Map.S with type key = Component_name.t


(*/************************************************************************\*)
(*| 2. Universe                                                            |*)
(*\************************************************************************/*)

(** 2.1. Resources. *)

type resource = resource_id
module Resource         : Set.OrderedType with type t = resource
module Resource_set     : Set.S with type elt = Resource.t
module Resource_set_set : Set.S with type elt = Resource_set.t
module Resource_map     : Map.S with type key = Resource.t

(** A quantity describing how many units of a resource are provided by a location. *)
type resource_provide_arity = int
(** A quantity describing how many units of a resource are consumed by a component type or a package. *)
type resource_consume_arity = int

  
(** 2.2 Ports. *)

type port = port_id
module Port         : Set.OrderedType with type t = port
module Port_set     : Set.S with type elt = Port.t
module Port_set_set : Set.S with type elt = Port_set.t
module Port_map     : Map.S with type key = Port.t
module Port_map_extract_key : sig val set_of_keys : 'a Port_map.t -> Port_set.t end


(** 2.3. Component types. *)

val deprecated_component_type_id : component_type_id

(** A quantity describing to how many other components this component type can provide a binding on a given port.
    Note: Some component types can provide a binding to an infinite number of components. *)
type provide_arity = 
  | Finite_provide of int
  | Infinite_provide 

(** A quantity describing how many bindings with different components providing a given port are required by this component type. 
    Note: It is always finite, because an infinite require arity would be simply never satiable. *)
type require_arity = int

exception Component_type_provide_port_not_found of port_id
exception Component_type_require_port_not_found of port_id

class component_type :
  ?provide  : provide_arity Port_id_map.t ->
  ?require  : require_arity Port_id_map.t ->
  ?conflict : Port_id_set.t ->
  ?consume  : resource_consume_arity Resource_id_map.t ->
  unit -> object ('selftype)

  val provide  : provide_arity Port_id_map.t              (** Which ports does this component type provide and with what arities. *)
  val require  : require_arity Port_id_map.t              (** Which ports does this component type require and with what arities. *)
  val conflict : Port_id_set.t                            (** With which ports is this component type in conflict. *)
  val consume  : resource_consume_arity Resource_id_map.t (** Which resources does this component type consume and in what amounts. *)
  
  method provide        : port_id -> provide_arity         
  method provide_domain : Port_id_set.t         
  method require        : port_id -> require_arity         
  method require_domain : Port_id_set.t         
  method conflict       : Port_id_set.t         
  method consume        : resource_id -> resource_consume_arity
  method consume_domain : Resource_id_set.t     
end

module Component_type : Set.OrderedType with type t = component_type
module Component_type_set : Set.S with type elt = Component_type.t
module Component_type_map : Map.S with type key = Component_type.t


(** 2.4. Packages. *)

val deprecated_package_id : package_id

class package :
  ?depend   : Package_id_set_set.t ->
  ?conflict : Package_id_set.t ->
  ?consume  : resource_consume_arity Resource_id_map.t ->
  unit -> object ('selftype)

  val depend   : Package_id_set_set.t                     (** Which packages does this package depend on (a disjunction of conjunctions). *)
  val conflict : Package_id_set.t                         (** Which packages is this package is in conflict with. *)
  val consume  : resource_consume_arity Resource_id_map.t (** Which resources does this package consume and in what amounts. *)

  method depend         : Package_id_set_set.t  
  method conflict       : Package_id_set.t      
  method consume        : resource_id -> resource_consume_arity
  method consume_domain : Resource_id_set.t     

  method trim_by_package_ids : Package_id_set.t -> 'selftype

end

module Package         : Set.OrderedType with type t = package
module Package_set     : Set.S with type elt = Package.t
module Package_set_set : Set.S with type elt = Package_set.t
module Package_map     : Map.S with type key = Package.t


(** 2.5. Repositories. *)

exception Repository_package_not_found of package_id

class repository :
  ?packages : Package_id_set.t ->
  unit -> object ('selftype)

  val packages : Package_id_set.t (** Which packages does this repository contain. *)
  
  method package_ids : Package_id_set.t

  method trim_by_package_ids : Package_id_set.t -> 'selftype

end

module Repository     : Set.OrderedType with type t = repository
module Repository_set : Set.S with type elt = Repository.t
module Repository_map : Map.S with type key = Repository.t


(** 2.6. Universes. *)

exception Universe_component_type_not_found of component_type_id
exception Universe_repository_not_found     of repository_id
exception Universe_package_not_found        of package_id
exception Package_repository_not_found      of package_id

class universe :
  ?ports           : Port_id_set.t ->
  ?packages        : package Package_id_map.t ->
  ?resources       : Resource_id_set.t ->
  ?component_types : component_type Component_type_id_map.t ->
  ?implementation  : Package_id_set.t Component_type_id_map.t ->
  ?repositories    : repository Repository_id_map.t ->
  unit -> object ('selftype)

  val ports           : Port_id_set.t                            
  val packages        : package Package_id_map.t                 
  val resources       : Resource_id_set.t                        
  val component_types : component_type Component_type_id_map.t   (** Component types available in this universe. *)
  val implementation  : Package_id_set.t Component_type_id_map.t (** Which packages implement the component types of this universe. *)
  val repositories    : repository Repository_id_map.t           (** Package repositories available in this universe. *)
  
  (* Methods *)
  method get_port_ids              : Port_id_set.t          
  method get_package_ids           : Package_id_set.t       
  method get_resource_ids          : Resource_id_set.t      
  method get_component_type_ids    : Component_type_id_set.t
  method get_repository_ids        : Repository_id_set.t    
  method get_implementation_domain : Component_type_id_set.t

  method get_component_type    : component_type_id -> component_type
  method get_implementation    : component_type_id -> Package_id_set.t
  method get_repository        : repository_id -> repository
  method get_package           : package_id -> package
  method repository_of_package : package_id -> repository_id

  method trim_packages_by_ids : Package_id_set.t -> 'selftype

  (* Methods coming from the paper. *)

  method get_requirers   : port_id -> Component_type_id_set.t
  method get_providers   : port_id -> Component_type_id_set.t
  method get_conflicters : port_id -> Component_type_id_set.t

  (* This method is almost like a constructor, but based on a existing object:
     it will replace only the given fields of the existing object, leaving the rest as it was. *)
  method copy :
    ?ports           : Port_id_set.t ->
    ?packages        : package Package_id_map.t ->
    ?resources       : Resource_id_set.t ->
    ?component_types : component_type Component_type_id_map.t ->
    ?implementation  : Package_id_set.t Component_type_id_map.t ->
    ?repositories    : repository Repository_id_map.t ->
    unit -> 'selftype

end


(*/************************************************************************\*)
(*| 3. Configuration                                                       |*)
(*\************************************************************************/*)

(** 3.1. Locations. *)
type location_cost = int
module Location_cost : Set.OrderedType with type t = location_cost

class location :
   repository         : repository_id ->
  ?packages_installed : Package_id_set.t ->
  ?provide_resources  : resource_provide_arity Resource_id_map.t ->
  ?cost               : location_cost ->
  unit -> object ('selftype)

  val repository         : repository_id                            (** The id of the package repository used by this location. *)
  val packages_installed : Package_id_set.t                         (** Ids of packages installed at this location. *)
  val provide_resources  : resource_provide_arity Resource_id_map.t (** Which resources does this location provide and in what amounts. *)
  val cost               : location_cost                            (** The cost of using this location *)
  
  method repository               : repository_id         
  method packages_installed       : Package_id_set.t      
  method provide_resources        : resource_id -> resource_provide_arity
  method provide_resources_domain : Resource_id_set.t     
  method cost                     : location_cost         

  (* This method is almost like a constructor, but based on a existing object:
     it will replace only the given fields of the existing object, leaving the rest as it was. *)
  method copy :
    ?repository         : repository_id ->
    ?packages_installed : Package_id_set.t ->
    ?provide_resources  : resource_provide_arity Resource_id_map.t ->
    ?cost               : location_cost ->
    unit -> 'selftype

end

module Location     : Set.OrderedType with type t = location
module Location_set : Set.S with type elt = Location.t
module Location_map : Map.S with type key = Location.t
module Location_set_of_location_ids : module type of Set.Convert(Location_id_set)(Location_set)

(** Assertions:
    {ul
    {- The repository name mentioned by a location must correspond to a repository from the universe. }
    {- All the package names mentioned by a location must correspond to packages from the universe. }
    {- All the packages installed in a location must be available in the package repository which is used by this location. }
    } *)


(** 3.2. Components. *)

class component : 
  typ      : component_type_id ->
  location : location_id ->
  unit -> object ('selftype)

  val typ      : component_type_id (** The type of this component. *)
  val location : location_id       (** The location where this component is installed. *)

  method typ      : component_type_id
  method location : location_id      
end

module Component     : Set.OrderedType with type t = component
module Component_set : Set.S with type elt = Component.t
module Component_map : Map.S with type key = Component.t

(** Assertions:
    {ul
    {- The component type name mentioned by a location must correspond to a component type from the universe. <- NO}
    {- The location name mentioned by a component must correspond to a location from the configuration. }
    } *)



(** 3.3. Bindings. *)

class binding :
  port     : port_id ->
  requirer : component_id ->
  provider : component_id -> object ('selftype)

  val port     : port_id      (** The port of this binding. *)
  val requirer : component_id (** The id of the requiring component. *)
  val provider : component_id (** The id of the providing component. *)

  method port     : port_id     
  method requirer : component_id
  method provider : component_id
end

module Binding     : Set.OrderedType with type t = binding
module Binding_set : Set.S with type elt = Binding.t

(** Assertions:
    {ul
    {- The component names mentioned by a binding must correspond to components from the configuration. }
    {- The port mentioned by a binding must be required by the requirer component and provided by the provider component. }
    } *)


(** 3.4. Configurations. *)

exception Configuration_location_not_found  of location_id
exception Configuration_component_not_found of component_id

class configuration :
  ?locations  : location Location_id_map.t ->
  ?components : component Component_id_map.t ->
  ?bindings   : Binding_set.t ->
  unit -> object ('selftype)

  val locations  : location Location_id_map.t   (** Locations in this configuration. *)
  val components : component Component_id_map.t (** Components in this configuration. *)
  val bindings   : Binding_set.t                (** Bindings in this configuration. *)
  
  (* methods *)
  method get_location      : location_id -> location
  method get_component     : component_id -> component
  method get_location_ids  : Location_id_set.t
  method get_component_ids : Component_id_set.t
  method get_bindings      : Binding_set.t

  method get_local_component : location_id -> component_type_id -> Component_id_set.t
  method get_local_package   : location_id -> package_id -> bool

  method trim : Location_id_set.t -> 'selftype

  (* This method is almost like a constructor, but based on a existing object:
     it will replace only the given fields of the existing object, leaving the rest as it was. *)
  method copy :
    ?locations  : location Location_id_map.t ->
    ?components : component Component_id_map.t ->
    ?bindings   : Binding_set.t ->
    unit -> 'selftype

end

(** Merge two configurations. 
    First configuration locations and components are prioritary: 
    if both c1 and c2 have a location/component with the same id, the c1's version will be used. *)
val merge_configurations : configuration -> configuration -> configuration



(*/************************************************************************\*)
(*| 4. Specification.                                                      |*)
(*\************************************************************************/*)

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
  | Spec_local_expr_add   of spec_local_expr * spec_local_expr
  | Spec_local_expr_sub   of spec_local_expr * spec_local_expr
  | Spec_local_expr_mul   of spec_const * spec_local_expr

type spec_op = 
  | Lt  (** Less-than operator *)
  | LEq (** Less-than-or-equal-to operator *)
  | Eq  (** Equal-to operator *)
  | GEq (** Greater-than-or-equal-to operator *)
  | Gt  (** Greater-than operator *)
  | NEq (** Not-equal-to operator *)

type local_specification = 
  | Spec_local_true
  | Spec_local_op   of spec_local_expr * spec_op * spec_local_expr
  | Spec_local_and  of local_specification * local_specification
  | Spec_local_or   of local_specification * local_specification
  | Spec_local_impl of local_specification * local_specification
  | Spec_local_not  of local_specification

type spec_repository_constraint = repository_id list
type spec_resource_constraint   = (resource_id * spec_op * spec_const) list

type spec_element = 
  | Spec_element_package        of package_id
  | Spec_element_component_type of component_type_id
  | Spec_element_port           of port_id
  | Spec_element_location       of spec_resource_constraint * spec_repository_constraint * local_specification

type spec_expr = 
  | Spec_expr_var   of spec_variable_name
  | Spec_expr_const of spec_const
  | Spec_expr_arity of spec_element
  | Spec_expr_add   of spec_expr * spec_expr
  | Spec_expr_sub   of spec_expr * spec_expr
  | Spec_expr_mul   of spec_const * spec_expr

type specification = 
  | Spec_true
  | Spec_op         of spec_expr * spec_op * spec_expr
  | Spec_and        of specification * specification
  | Spec_or         of specification * specification
  | Spec_impl       of specification * specification
  | Spec_not        of specification
  | Spec_everywhere of                    local_specification
  | Spec_at         of location_id list * local_specification


(**  *)

val uv_of_specification : specification -> Port_id_set.t * Component_type_id_set.t * Package_id_set.t



(*/************************************************************************\*)
(*| 5. Optimization function                                               |*)
(*\************************************************************************/*)

type optimization_function = 
  | Optimization_function_simple
  | Optimization_function_compact
  | Optimization_function_conservative
  | Optimization_function_spread
  | Optimization_function_none


(*/************************************************************************\*)
(*| 6. Putting all together                                                |*)
(*\************************************************************************/*)

(* TODO: Remove these two types...*)
type model      = universe * configuration * specification
type model_full = universe * configuration * specification * optimization_function

type initial_model = {
  universe              : universe              option;
  initial_configuration : configuration         option;
  specification         : specification         option;
  optimization_function : optimization_function option;
}
