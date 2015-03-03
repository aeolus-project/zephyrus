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
val eB   : port_id -> component_type_id -> port_id -> component_type_id -> expression
val eR   : location_id -> repository_id                      -> expression
val eO   : location_id -> resource_id                        -> expression
val eU   : location_id                                       -> expression

(* val ralfs_redundant_require :
    port_ids:Port_id_set.t ->
    get_requirers:(port_id -> Component_type_id_set.t) -> get_providers:(port_id -> Component_type_id_set.t) -> get_component_type_require_arity:(component_type_id -> port_id -> require_arity) ->
    konstraint list
val flat_require               :
   port_ids:Port_id_set.t
    -> get_requirers:(port_id -> Component_type_id_set.t)
    -> get_providers:(port_id -> Component_type_id_set.t)
    -> get_component_type_require_arity:(component_type_id -> port_id -> require_arity)
    -> konstraint list *)
val require                    :
    port_ids:Port_id_set.t
     -> get_supports: (port_id -> Port_id_set.t)
     -> get_requirers:(port_id -> Component_type_id_set.t)
     -> get_providers:(port_id -> Component_type_id_set.t)
     -> get_component_type_require_arity:(component_type_id -> port_id -> require_arity)
     -> konstraint list

(*(* DEPRECATED *) val provide_with_fixed_infinity:
    port_ids:Port_id_set.t
     -> get_providers:(port_id -> Component_type_id_set.t)
     -> get_requirers:(port_id -> Component_type_id_set.t)
     -> get_component_type_provide_arity:(component_type_id -> port_id -> provide_arity)
     -> konstraint list *)
val provide :
    port_ids:Port_id_set.t
     -> get_subports: (port_id -> Port_id_set.t)
     -> get_providers:(port_id -> Component_type_id_set.t)
     -> get_requirers:(port_id -> Component_type_id_set.t)
     -> get_component_type_provide_arity:(component_type_id -> port_id -> provide_arity)
     -> konstraint list

val binding :
    port_ids:Port_id_set.t
     -> get_supports: (port_id -> Port_id_set.t)
     -> get_requirers:(port_id -> Component_type_id_set.t)
     -> get_providers:(port_id -> Component_type_id_set.t)
     -> konstraint list

(*
val conflict_naive                 : port_ids:Port_id_set.t -> get_conflicters:(port_id -> Component_type_id_set.t) -> get_component_type_provide_arity:(component_type_id -> port_id -> provide_arity) -> konstraint list *)
val conflict :
    port_ids:Port_id_set.t
     -> get_conflicters:(port_id -> Component_type_id_set.t)
     -> get_providers:(port_id -> Component_type_id_set.t)
     -> konstraint list

val location_component_type : component_type_ids:Component_type_id_set.t -> location_ids:Location_id_set.t -> konstraint list
val location_package        : package_ids:Package_id_set.t               -> location_ids:Location_id_set.t -> konstraint list
val location_port           : port_ids:Port_id_set.t                     -> location_ids:Location_id_set.t -> konstraint list
val location_port_equation  : port_ids:Port_id_set.t                     -> location_ids:Location_id_set.t -> get_providers:(port_id -> Component_type_id_set.t) -> get_component_type_provide_arity:(component_type_id -> port_id -> provide_arity) -> konstraint list

val repository_unique  : location_ids:Location_id_set.t -> repository_ids:Repository_id_set.t -> konstraint list
val repository_package : location_ids:Location_id_set.t -> repository_ids:Repository_id_set.t -> package_ids:Package_id_set.t -> get_repository_packages:(repository_id -> Package_id_set.t) -> konstraint list

val component_type_implementation : location_ids:Location_id_set.t -> component_type_ids:Component_type_id_set.t -> get_implementation:(component_type_id -> Package_id_set.t) -> konstraint list

val package_dependency : location_ids:Location_id_set.t -> package_ids:Package_id_set.t -> get_package_depend:(package_id -> Package_id_set_set.t) -> konstraint list
val package_conflict   : location_ids:Location_id_set.t -> package_ids:Package_id_set.t -> get_package_conflict:(package_id -> Package_id_set.t)   -> konstraint list

val resource_consumption : ?with_packages : bool -> location_ids:Location_id_set.t -> resource_ids:Resource_id_set.t -> component_type_ids:Component_type_id_set.t -> package_ids:Package_id_set.t -> get_component_type_consume:(component_type_id -> resource_id -> resource_consume_arity) -> get_package_consume:(package_id -> resource_id -> resource_consume_arity) -> get_location_provide_resources:(location_id -> resource_id -> resource_provide_arity) -> konstraint list

val deprecated_component_types_with_packages : ?with_packages : bool -> location_ids:Location_id_set.t -> konstraint list

val used_locations : ?with_packages : bool -> component_type_ids:Component_type_id_set.t -> package_ids:Package_id_set.t -> location_ids:Location_id_set.t -> konstraint list

val direct_incompatibilities : incompatibilities:(Data_model.Component_type_id_set_set.t Data_model.Repository_id_map.t) -> location_ids:Location_id_set.t -> konstraint list




