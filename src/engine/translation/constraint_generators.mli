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

(** All the basic constraint-building functions. *)

(* Depends on
*)

open Data_model
open Data_constraint

val get_provide_arity_safe : component_type              -> port_id -> expression
val get_provide_arity      : component_type              -> port_id -> expression
val get_require_arity      : component_type              -> port_id -> expression
val get_consume            : < consume : 'a -> int; .. > -> 'a      -> expression

val eNt  : component_type_id                                 -> expression
val eNp  : port_id                                           -> expression
val eNk  : package_id                                        -> expression
val eNlt : location_id -> component_type_id                  -> expression
val eNlp : location_id -> port_id                            -> expression
val eNlk : location_id -> package_id                         -> expression
val eB   : port_id -> component_type_id -> component_type_id -> expression
val eR   : location_id -> repository_id                      -> expression
val eO   : location_id -> resource_id                        -> expression
val eU   : location_id                                       -> expression

val ralfs_redundant_require        : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list
val flat_require                   : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list
val require                        : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list
val provide_with_fixed_infinity    : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list
val provide_with_advanced_infinity : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list

val binding                        : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> konstraint list

val conflict_naive                 : Port_id_set.t -> (port_id -> Component_type_id_set.t)                                         -> (component_type_id -> component_type) -> konstraint list
val conflict_advanced              : Port_id_set.t -> (port_id -> Component_type_id_set.t) -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list

val location_component_type : Component_type_id_set.t -> Location_id_set.t -> konstraint list
val location_package        : Package_id_set.t        -> Location_id_set.t -> konstraint list
val location_port           : Port_id_set.t           -> Location_id_set.t -> konstraint list
val location_port_equation  : Port_id_set.t           -> Location_id_set.t -> (port_id -> Component_type_id_set.t) -> (component_type_id -> component_type) -> konstraint list

val repository_unique  : Location_id_set.t -> Repository_id_set.t -> konstraint list
val repository_package : Location_id_set.t -> Repository_id_set.t -> Package_id_set.t -> (repository_id -> Package_id_set.t) -> konstraint list

val component_type_implementation : Location_id_set.t -> Component_type_id_set.t -> (component_type_id -> Package_id_set.t) -> konstraint list

val package_dependency : Location_id_set.t -> Package_id_set.t -> (package_id -> package) -> konstraint list
val package_conflict   : Location_id_set.t -> Package_id_set.t -> (package_id -> package) -> konstraint list

val resource_consumption : ?with_packages : bool -> Location_id_set.t -> Resource_id_set.t -> Component_type_id_set.t -> Package_id_set.t -> (component_type_id -> component_type) -> (package_id -> package) -> (location_id -> location) -> konstraint list

val deprecated_component_types_with_packages : ?with_packages : bool -> Location_id_set.t -> konstraint list

val used_locations : ?with_packages : bool -> Component_type_id_set.t -> Package_id_set.t -> Location_id_set.t -> konstraint list

val incompatibilities : Data_model.Component_type_id_set_set.t Data_model.Repository_id_map.t -> Location_id_set.t -> konstraint list