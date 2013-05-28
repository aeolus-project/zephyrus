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


open Aeolus_types



(** Fetching universe data. *)


(** Fetching component type data. *)

(** Returns the names of all component types available in the universe. *)
val get_component_type_names                : universe -> Set.Make(Component_type_name).t

(** Returns all the component types available in the universe. *)
val get_component_types                     : universe -> Set.Make(Component_type).t

(** Given a universe and a component type name returns the corresponding component type from the universe. 
    Fails if there is no component type with this name. *)
val get_component_type                      : universe -> component_type_name -> component_type



(** Fetching port data. *)

(** Returns the names of all ports mentioned in the universe. *)
val get_port_names                          : universe -> Set.Make(Port_name).t

(** Given a component type and a port name returns the provide arity of that port for that component type.
    If that port is not provided by this component type at all, returns zero. *)
val get_provide_arity                       : component_type -> port_name -> provide_arity

(** Given a component type and a port name returns the require arity of that port for that component type.
    If that port is not required by this component type at all, returns zero. *)
val get_require_arity                       : component_type -> port_name -> require_arity

(** Given a component type and a port name returns true if this component type is in conflict with this port and false otherwise. *)
val is_in_conflict                          : component_type -> port_name -> bool


(** Given a universe and a port name returns all the component types available in this universe which require this port (require arity is greater than zero). *)
val requirers                               : universe -> port_name -> Set.Make(Component_type).t

(** Given a universe and a port name returns all the component types available in this universe which provide this port (require arity is greater than zero). *)
val providers                               : universe -> port_name -> Set.Make(Component_type).t

(** Given a universe and a port name returns all the component types available in this universe which are in conflict with this port. *)
val conflicters                             : universe -> port_name -> Set.Make(Component_type).t



(** Fetching repository data. *)

(** Returns the names of all repositories available in the universe. *)
val get_repository_names                    : universe -> Set.Make(Repository_name).t

(** Returns all the repositories available in the universe. *)
val get_repositories                        : universe -> Set.Make(Repository).t

(** Given a universe and a repository name returns the corresponding repository from the universe. 
    Fails if there is no repository with this name. *)
val get_repository                          : universe -> repository_name -> repository



(** Fetching package data. *)

(** Returns the names of all packages available in the universe. *)
val get_package_names                       : universe -> Set.Make(Package_name).t

(** Returns all the packages available in the universe. *)
val get_packages                            : universe -> Set.Make(Package).t

(** Given a repository returns the names of all packages available in this repository. *)
val get_repository_package_names            : repository -> Set.Make(Package_name).t

(** Given a repository returns all the packages available in this repository. *)
val get_repository_packages                 : repository -> Set.Make(Package).t

(** Given a repository and a package returns true 
    if this package is available in this repository and false otherwise. *)
val is_package_in_the_repository            : repository -> package -> bool

(** Given a repository and a package name returns the corresponding package from this repository. 
    Fails if there is no package with this name in this repository. *)
val get_repository_package                  : repository -> package_name -> package

(** Given a universe and a component type name returns the names of all packages which can be used to implement that component type. *)
val get_component_type_implementation       : universe -> component_type_name -> Set.Make(Package_name).t



(** Fetching resource data. *)

(** Returns the names of all resources mentioned in the universe. *)
val get_resource_names                      : universe -> Set.Make(Resource_name).t

(** Given a component type and a resource name returns how much of that resource this component type consumes.
    If that resource is not consumed by this component type at all, returns zero. *)
val get_component_type_resource_consumption : component_type -> resource_name -> resource_consumption

(** Given a package and a resource name returns how much of that resource this package consumes.
    If that resource is not consumed by this package, returns zero. *)
val get_package_resource_consumption        : package        -> resource_name -> resource_consumption





(** Fetching configuration data. *)


(** Fetching location data. *)

(** Returns the names of all locations which are available in the configuration. *)
val get_location_names                      : configuration -> Set.Make(Location_name).t

(** Returns all the locations which are available in the configuration. *)
val get_locations                           : configuration -> Set.Make(Location).t

(** Given a configuration and a location name returns the corresponding location from the configuration. 
    Fails if there is no location with this name. *)
val get_location                            : configuration -> location_name -> location


(** Given a configuration and a location name returns all the components installed at this location. *)
val get_location_components                 : configuration -> location_name -> Set.Make(Component).t

(** Given a configuration and a location name returns all the packages installed at this location. *)
val get_location_packages_installed         : configuration -> location_name -> Set.Make(Package_name).t

(** Given a location and a resource name returns how much of that resource this location provides. *)
val get_location_resource_provide_arity     : location -> resource_name -> resource_provide_arity
