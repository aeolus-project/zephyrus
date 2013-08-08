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
    - datatypes/Data_model
    - datatypes/Data_constraint
*)

(*  Constraints *)

val parse_nary_op : 'a -> ('b -> 'a) -> ('a -> 'a -> 'a) -> ('b list) -> 'a
val int_of_value : Data_constraint.value -> int




(****************************************************)
(******************** DEPRECATED ********************)
(****************************************************)

(* Depends on
    - datatypes/Data_model (obviously)
    - datatypes/Data_state (for storing. Maybe a better aternative can be found)
*)


(*
val get_provide_arity : component_type -> port_id -> provide_arity
val get_require_arity : component_type -> port_id -> require_arity
val is_in_conflict : component_type -> port_id -> bool

val requirers   : component_type Component_type_id_map.t -> port_id -> Component_type_id_set.t
val providers   : component_type Component_type_id_map.t -> port_id -> Component_type_id_set.t
val conflicters : component_type Component_type_id_map.t -> port_id -> Component_type_id_set.t

*)


(* Functions and data defined in the Zephyrus paper
    - U_dt : set of component type names                     => implemented by "get_component_type_names"
    - U_dp : set of ports                                    => implemented by "get_port_names"
    - U_dr : set of repository names                         => implemented by "get_repository_names"
    - U_dk : set of package names                            => implemented by "get_package_names"
    - U_i  : U_dt |-> P(U_dk) implementation of components   => implemented by "get_component_type_implementation"
    - U_w  : U_dk |-> Pi packages of all package names       => NOT IMPLEMENTED
    - UR   : P |-> P(U_dt) components names requiring a port => implemented by "requirers"
    - UP   : P |-> P(U_dt) components names providing a port => implemented by "providers"
    - UC   : P |-> P(U_dt) components names conflict a port  => implemented by "conflicters"
    - U(t)                                                   => implemented by "get_component_type"
    - U(r)                                                   => implemented by "get_repository"
    - U(k)                                                   => NOT IMPLEMENTED

    - C_l       : set of location                       => implemented by "get_location_names"
    - C_c       : set of component                      => implemented by "get_component_names"
    - C.type(c) : the type of the component name c in C => NOT IMPLEMENTED
    - C(l,t)    : the set of component of type t in l   => NOT IMPLEMENTED
    - C(l,k)    : true if k is installed on l           => NOT IMPLEMENTED

 The other functions are there, what for?
*)

(*
module Core : sig
  (** 1. universe *)
  (** 1.1. component_type *)

  val get_component_type_names : component_type Component_type_name_map.t -> Component_type_name_set.t
  val get_component_types : component_type Component_type_name_map.t -> Component_type_set.t

  val get_component_type : component_type Component_type_name_map.t -> component_type_name -> component_type


  (** 1.2. port *)

  val port_is_provide_strict : provide_arity -> bool

  val get_port_names : Component_type_set.t -> Port_name_set.t

  val get_provide_arity : component_type -> port_name -> provide_arity
  val get_require_arity : component_type -> port_name -> require_arity
  val is_in_conflict : component_type -> port_name -> bool

  val requirers : Component_type_id_map.t -> port_name -> Component_type_id_set.t
  val providers : Component_type_id_map.t -> port_name -> Component_type_id_set.t
  val conflicters : Component_type_id_map.t -> port_name -> Component_type_id_set.t


  (** 1.3. repository *)

  val get_repository_names : repository Repository_name_map.t -> Repository_name_set.t
  val get_repositories : repository Repository_name_map.t -> Repository_set.t

  val get_repository : repository Repository_name_map.t -> repository_name -> repository


  (** 1.4. package *)

  val get_repository_package_names : repository -> Package_name_set.t
  val get_repository_packages : repository -> Package_set.t

  val get_package_names : Repository_set.t -> Package_name_set.t
  val get_packages : Repository_set.t -> Package_set.t

  val is_package_in_repository : repository -> package -> bool
  val get_repository_package : repository -> package_name -> package

  val get_component_type_implementation : Package_name_set.t Component_type_name_map.t -> component_type_name -> Package_name_set.t


  (** 1.5. resource *)

  val get_resource_names : Component_type_set.t -> Package_set.t -> Resource_name_set.t

  val get_component_type_resource_consumption : component_type -> resource_name -> resource_consumption
  val get_package_resource_consumption : package -> resource_name -> resource_provide_arity


  (** 2. configuration *)
  (** 2.1. component *)

  val get_component_names : component Component_name_map.t -> Component_name_set.t
  val get_components : component Component_name_map.t -> Component_set.t

  val get_local_component : location_name -> component_type_name -> component Component_name_map.t -> Component_name_set.t

  (** 2.2. location *)

  val get_location_names : location Location_name_map.t -> Location_name_set.t
  val get_locations : location Location_name_map.t -> Location_set.t

  val get_location : location Location_name_map.t -> location_name -> location
  val get_location_components : Component_set.t -> location_name -> Component_set.t
  val get_location_packages_installed : location -> Package_name_set.t
  val get_local_package : location_name -> package_name -> location Location_name_map.t -> bool

  val get_location_resource_provide_arity : location -> resource_name -> resource_provide_arity
end
*)

(*
module Plain : sig
(** Fetching universe data. *)


(** Fetching component type data. *)

(** Returns the names of all component types available in the universe. *)
val get_component_type_names                : universe -> Component_type_name_set.t

(** Returns all the component types available in the universe. *)
val get_component_types                     : universe -> Component_type_set.t

(** Given a universe and a component type name returns the corresponding component type from the universe. 
    Fails if there is no component type with this name. *)
val get_component_type                      : universe -> component_type_name -> component_type



(** Fetching port data. *)

(** Returns the names of all ports mentioned in the universe. *)
val get_port_names                          : universe -> Port_name_set.t

(** Given a component type and a port name returns the provide arity of that port for that component type.
    If that port is not provided by this component type at all, returns zero. *)
val get_provide_arity                       : component_type -> port_name -> provide_arity

(** Given a component type and a port name returns the require arity of that port for that component type.
    If that port is not required by this component type at all, returns zero. *)
val get_require_arity                       : component_type -> port_name -> require_arity

(** Given a component type and a port name returns true if this component type is in conflict with this port and false otherwise. *)
val is_in_conflict                          : component_type -> port_name -> bool


(** Given a universe and a port name returns all the component types available in this universe which require this port (require arity is greater than zero). *)
val requirers                               : universe -> port_name -> Component_type_set.t

(** Given a universe and a port name returns all the component types available in this universe which provide this port (require arity is greater than zero). *)
val providers                               : universe -> port_name -> Component_type_set.t

(** Given a universe and a port name returns all the component types available in this universe which are in conflict with this port. *)
val conflicters                             : universe -> port_name -> Component_type_set.t



(** Fetching repository data. *)

(** Returns the names of all repositories available in the universe. *)
val get_repository_names                    : universe -> Repository_name_set.t

(** Returns all the repositories available in the universe. *)
val get_repositories                        : universe -> Repository_set.t

(** Given a universe and a repository name returns the corresponding repository from the universe. 
    Fails if there is no repository with this name. *)
val get_repository                          : universe -> repository_name -> repository



(** Fetching package data. *)

(** Returns the names of all packages available in the universe. *)
val get_package_names                       : universe -> Package_name_set.t

(** Returns all the packages available in the universe. *)
val get_packages                            : universe -> Package_set.t

(** Given a repository returns the names of all packages available in this repository. *)
val get_repository_package_names            : repository -> Package_name_set.t

(** Given a repository returns all the packages available in this repository. *)
val get_repository_packages                 : repository -> Package_set.t

(** Given a repository and a package returns true 
    if this package is available in this repository and false otherwise. *)
val is_package_in_repository            : repository -> package -> bool

(** Given a repository and a package name returns the corresponding package from this repository. 
    Fails if there is no package with this name in this repository. *)
val get_repository_package                  : repository -> package_name -> package

(** Given a universe and a component type name returns the names of all packages which can be used to implement that component type. *)
val get_component_type_implementation       : universe -> component_type_name -> Package_name_set.t



(** Fetching resource data. *)

(** Returns the names of all resources mentioned in the universe. *)
val get_resource_names                      : universe -> Resource_name_set.t

(** Given a component type and a resource name returns how much of that resource this component type consumes.
    If that resource is not consumed by this component type at all, returns zero. *)
val get_component_type_resource_consumption : component_type -> resource_name -> resource_consumption

(** Given a package and a resource name returns how much of that resource this package consumes.
    If that resource is not consumed by this package, returns zero. *)
val get_package_resource_consumption        : package        -> resource_name -> resource_consumption





(** Fetching configuration data. *)


(** Fetching location data. *)

(** Returns the names of all locations which are available in the configuration. *)
val get_location_names                      : configuration -> Location_name_set.t

(** Returns all the locations which are available in the configuration. *)
val get_locations                           : configuration -> Location_set.t

(** Given a configuration and a location name returns the corresponding location from the configuration. 
    Fails if there is no location with this name. *)
val get_location                            : configuration -> location_name -> location


(** Given a configuration and a location name returns all the components installed at this location. *)
val get_location_components                 : configuration -> location_name -> Component_set.t

(** Given a configuration and a location name returns all the packages installed at this location. *)
val get_location_packages_installed         : configuration -> location_name -> Package_name_set.t


(** Given a location and a resource name returns how much of that resource this location provides. *)
val get_location_resource_provide_arity     : location -> resource_name -> resource_provide_arity

end
*)

