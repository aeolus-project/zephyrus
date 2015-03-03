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

(** Conversion of various elements of Zephyrus model to strings. *)

(* Depends on
    - datatypes/Data_model
    - datatypes/Data_constraint
    - datatypes/Data_helper
*)

module type S = sig
  val string_list : string list -> string
  val int_list    : int list -> string

  (*/********************************\*)
  (*  Model                           *)
  (*\********************************/*)

  (** 1. Resources *)
  val resource_name     : Data_model.resource_name -> string
  val resource_name_set : Data_model.Resource_name_set.t -> string
  val resource_id       : Data_model.resource_id -> string
  val resource_id_set   : Data_model.Resource_id_set.t -> string

  val resource_provide_arity : Data_model.resource_provide_arity -> string
  val resource_consume_arity : Data_model.resource_consume_arity -> string

  (** 2. Component types *)
  val port_name     : Data_model.port_name -> string
  val port_name_set : Data_model.Port_name_set.t -> string
  val port_id       : Data_model.port_id -> string
  val port_id_set   : Data_model.Port_id_set.t -> string
  val port_id_map   : ('a -> string) -> 'a Data_model.Port_id_map.t -> string
  val port          : Data_model.port -> string

  val component_type_name     : Data_model.component_type_name       -> string
  val component_type_name_set : Data_model.Component_type_name_set.t -> string
  val component_type_id       : Data_model.component_type_id         -> string
  val component_type_id_set   : Data_model.Component_type_id_set.t   -> string
  val component_type_id_map   : ('a -> string) -> 'a Data_model.Component_type_id_map.t -> string

  val provide_arity : Data_model.provide_arity -> string
  val require_arity : Data_model.require_arity -> string

  (** 3. Packages *)
  val package_name      : Data_model.package_name      -> string
  val package_name_set : Data_model.Package_name_set.t -> string
  val package_id       : Data_model.package_id         -> string
  val package_id_set   : Data_model.Package_id_set.t   -> string

  (** 4. Repositories *)
  val repository_name     : Data_model.repository_name       -> string
  val repository_name_set : Data_model.Repository_name_set.t -> string
  val repository_id       : Data_model.repository_id         -> string
  val repository_id_set   : Data_model.Repository_id_set.t   -> string

  (** 5. Location *)
  val location_name     : Data_model.location_name       -> string
  val location_name_set : Data_model.Location_name_set.t -> string
  val location_id       : Data_model.location_id         -> string
  val location_id_set   : Data_model.Location_id_set.t   -> string

  val location_categories : Location_categories.t -> string

  val component_name     : Data_model.component_name       -> string
  val component_name_set : Data_model.Component_name_set.t -> string
  val component_id       : Data_model.component_id         -> string
  val component_id_set   : Data_model.Component_id_set.t   -> string


  (************************************)
  (** Specification                   *)
  (************************************)

  val spec_variable_name : Data_model.spec_variable_name -> string
  val spec_const         : Data_model.spec_const -> string

  val specification : Data_model.specification -> string

  val model_optimization_function : Data_model.optimization_function -> string

  (************************************)
  (** Constraints                     *)
  (************************************)


  val element       : Data_constraint.element -> string
  val local_element : Data_model.location_id -> Data_constraint.element -> string

  val variable   : Data_constraint.variable   -> string
  val value      : Data_constraint.value      -> string
  val expression : Data_constraint.expression -> string
  val konstraint : Data_constraint.konstraint -> string

  val structured_constraints : Data_state.structured_constraints -> string

  val constraint_single_optimization : ('a -> string) -> 'a Data_constraint.Single_objective.optimization -> string
  val constraint_single_solve_goal   : ('a -> string) -> 'a Data_constraint.Single_objective.solve_goal   -> string
  val constraint_multi_optimization  : ('a -> string) -> 'a Data_constraint.Multi_objective.optimization  -> string
  val constraint_multi_solve_goal    : ('a -> string) -> 'a Data_constraint.Multi_objective.solve_goal    -> string

  val constraint_optimization_function          : Data_constraint.optimization_function                            -> string
  val measured_constraint_optimization_function : Data_constraint.value Data_constraint.Multi_objective.solve_goal -> string

  val bound : Data_constraint.Bound.t -> string

  (************************************)
  (** Solution                        *)
  (************************************)

  val solution : Data_constraint.solution -> string


  (************************************)
  (** Configuration                   *)
  (************************************)

  val configuration : Data_model.universe -> Data_model.configuration -> string

  (* val model_catalog : Data_model_catalog.closed_model_catalog -> string *)
end

(** Three printing modules to choose. *)
module Id_string_of          : S (** Print identifiers directly as numbers. *)
module Quoted_id_string_of   : S (** Print identifiers as quoted numbers. *)
module Name_string_of        : S (** Translate identifiers to names. *)
module Quoted_name_string_of : S (** Translate identifiers to names and add quotes around. *)

(** By default we are using the Id_string_of. *)
include S
