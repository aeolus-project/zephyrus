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

(** universe *)

(** component_type *)
val get_component_type_names                : universe -> Component_type_name_set.t
val get_component_types                     : universe -> Component_type_set.t
val get_component_type                      : universe -> component_type_name -> component_type

(** port *)
val get_port_names                          : universe -> Port_name_set.t
val get_provide_arity                       : component_type -> port_name -> provide_arity
val get_require_arity                       : component_type -> port_name -> require_arity
val is_in_conflict                          : component_type -> port_name -> bool

val requirers                               : universe -> port_name -> Component_type_name_set.t
val providers                               : universe -> port_name -> Component_type_name_set.t
val conflicters                             : universe -> port_name -> Component_type_name_set.t

(** repository *)
val get_repository_names                    : universe -> Repository_name_set.t
val get_repositories                        : universe -> Repository_set.t
val get_repository                          : universe -> repository_name -> repository

(** package *)
val get_package_names                       : universe -> Package_name_set.t
val get_packages                            : universe -> Package_set.t
val get_repository_package_names            : repository -> Package_name_set.t
val get_repository_packages                 : repository -> Package_set.t
val get_repository_package                  : repository -> package_name -> package

val get_component_type_implementation       : universe -> component_type_name -> Package_name_set.t

(** resource *)
val get_resource_names                      : universe -> Resource_name_set.t
val get_component_type_resource_consumption : component_type -> resource_name -> resource_consumption
val get_package_resource_consumption        : package        -> resource_name -> resource_consumption


(** configuration *)

(** location *)
val get_location_names                      : configuration -> Location_name_set.t
val get_locations                           : configuration -> Location_set.t
val get_location                            : configuration -> location_name -> location

val get_location_components                 : configuration -> location_name -> Component_set.t
val get_location_packages_installed         : configuration -> location_name -> Package_name_set.t
val get_location_resource_provide_arity     : location -> resource_name -> resource_provide_arity
