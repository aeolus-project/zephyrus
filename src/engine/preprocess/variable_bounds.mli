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
    - datatypes/Data_constraint
    - datatypes/Data_model
    - datatypes/Data_common
*)

(*/************************************************************************\*)
(*| 1. Modules and types for the flat universe                             |*)
(*\************************************************************************/*)

module rec V_data_init : sig
  type t
end and E_data : sig
  type t
  val value_of : t -> Data_constraint.Value.t
end and Graph : (Data_common.Graph.S with type vertice_data = V_data_init.t and type edge_data = E_data.t)

module V_data : sig
  type t = V_data_init.t
  exception Invalid_operation
  
  val of_port : Data_model.port -> t
  val of_component_type : Data_model.component_type_id -> t

  val bound_combine : t -> Data_constraint.Bound.t -> unit
  val bound_add_min : t -> Data_constraint.Value.t -> unit
  val bound_add_max : t -> Data_constraint.Value.t -> unit

  val bound          : t -> Data_constraint.Bound.t
  val bound_min      : t -> Data_constraint.Value.t
  val bound_max      : t -> Data_constraint.Value.t
  val bound_is_empty : t -> bool

  val conflict_add : t -> Graph.Vertice.t -> unit
  val conflict     : t -> Graph.Vertice_set.t

  val port_replica_add_min : t -> Data_constraint.Value.t -> unit
  val port_replica_add_max : t -> Data_constraint.Value.t -> unit
  val port_replica_min     : t -> Data_constraint.Value.t
  val port_replica_max     : t -> Data_constraint.Value.t
  
  val is_port            : t -> bool
  val is_component_type  : t -> bool
  val get_port           : t -> Data_model.port
  val get_component_type : t -> Data_model.component_type_id
end

type flat_universe


(*/************************************************************************\*)
(*| 2. Utility Functions                                                   |*)
(*\************************************************************************/*)

val create   : Data_model.universe -> flat_universe

val vertices : flat_universe -> Graph.Vertice_set.t
val edges    : flat_universe -> Graph.Edge_set.t

val to_string : flat_universe -> string
val to_string_full : flat_universe -> string

val get_initial_mins : Solvers.t -> Data_model.universe -> Data_model.specification
  -> Data_model.Location_id_set.t -> Data_constraint.solution option (* Solves the specification alone to get minimal bounds on required components and ports *)
val core_solution : Data_constraint.solution -> (int Data_model.Port_map.t) * (int Data_model.Component_type_id_map.t)

(*/************************************************************************\*)
(*| 3. Bounds Propagation Algorithms                                       |*)
(*\************************************************************************/*)

val add_bound_min_all : Data_constraint.solution -> flat_universe -> unit

val add_bound_min_p : Data_model.port -> Data_constraint.Value.t -> flat_universe -> unit
val add_bound_max_p : Data_model.port -> Data_constraint.Value.t -> flat_universe -> unit
val add_bound_min_t : Data_model.component_type_id -> Data_constraint.Value.t -> flat_universe -> unit
val add_bound_max_t : Data_model.component_type_id -> Data_constraint.Value.t -> flat_universe -> unit

val propagate_lower_bound : flat_universe -> unit
val propagate_conflicts   : flat_universe -> unit
val propagate_upper_bound : flat_universe -> unit
val finalize_bound_roots  : flat_universe -> unit
val minimize_upper_bound  : flat_universe -> unit


(*/************************************************************************\*)
(*| 5. Bounds Function Definition                                          |*)
(*\************************************************************************/*)

val nb_max_location : flat_universe -> Data_constraint.Value.t
val trim_categories : Location_categories.t -> flat_universe -> Location_categories.t

val variable_bounds : (Data_model.location_id -> Data_model.location) -> flat_universe -> (Data_constraint.variable -> Data_constraint.Bound.t)


