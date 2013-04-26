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


open Aeolus_types_t
open Variables

val get_component_type                : universe -> component_type_name -> component_type
val get_component_type_implementation : universe -> component_type_name -> package_name list
val get_repository                    : universe -> repository_name -> repository
val get_package                       : repository -> package_name -> package

val get_component_types               : universe -> component_type list
val get_packages                      : universe -> package list

val get_repository_package_names      : universe -> repository_name -> package_name list

val get_component_type_names          : universe -> component_type_name list
val get_port_names                    : universe -> port_name list
val get_package_names                 : universe -> package_name list
val get_repository_names              : universe -> repository_name list
val get_resource_names                : universe -> resource_name list

val get_elements                      : universe -> element list

val get_location_names                : configuration -> location_name list
val get_locations                     : configuration -> location list
val get_location                      : configuration -> location_name -> location
val get_location_components           : configuration -> location_name -> component list
val get_location_packages_installed   : configuration -> location_name -> package_name list

val get_resource_provide_arity        : location -> resource_name -> resource_provide_arity

val get_provide_arity                 : component_type -> port_name -> provide_arity
val get_require_arity                 : component_type -> port_name -> require_arity

val get_component_type_resource_consumption : component_type -> resource_name -> resource_consumption
val get_package_resource_consumption        : package        -> resource_name -> resource_consumption

val requirers   : universe -> port_name -> component_type_name list
val providers   : universe -> port_name -> component_type_name list
val conflicters : universe -> port_name -> component_type_name list
