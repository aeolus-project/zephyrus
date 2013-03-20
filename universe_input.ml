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


open Helpers

module type UNIVERSE =
  sig
    type component_type_name
    type port_name
    type component_name
    type package_name
    type repository_name
    type location_name
    type resource_name
    type provide_arity
    type require_arity
    type resource_consumption
    type resource_provide_arity
    type component_type
    type component_types
    type package
    type packages
    type repository
    type repositories
    type package_names
    type universe
  end

module U = Aeolus_types_j

module type UNIVERSE_INPUT =
  sig
    val component_type_name_of_string    : string -> U.component_type_name
    val port_name_of_string              : string -> U.port_name
    val component_name_of_string         : string -> U.component_name
    val package_name_of_string           : string -> U.package_name
    val repository_name_of_string        : string -> U.repository_name
    val location_name_of_string          : string -> U.location_name
    val resource_name_of_string          : string -> U.resource_name
    val provide_arity_of_string          : string -> U.provide_arity
    val require_arity_of_string          : string -> U.require_arity
    val resource_consumption_of_string   : string -> U.resource_consumption
    val resource_provide_arity_of_string : string -> U.resource_provide_arity
    val component_type_of_string         : string -> U.component_type
    val component_types_of_string        : string -> U.component_types
    val package_of_string                : string -> U.package
    val packages_of_string               : string -> U.packages
    val repository_of_string             : string -> U.repository
    val repositories_of_string           : string -> U.repositories
    val package_names_of_string          : string -> U.package_names
    val universe_of_string               : string -> U.universe
 end

module JSON : UNIVERSE_INPUT = Aeolus_types_j
