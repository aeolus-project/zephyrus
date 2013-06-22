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
    - datatypes/Data_helper
*)

(*/********************************\*)
(*  Model                           *)
(*\********************************/*)

(** 1. Resources *)
val resource_name : Data_model.resource_name -> string
val resource_id   : Data_model.resource_id -> string

val resource_provide_arity : Data_model.resource_provide_arity -> string
val resource_consume_arity : Data_model.resource_consume_arity -> string

(** 2. Component types *)
val port_name : Data_model.port_name -> string
val port_id   : Data_model.port_id -> string

val component_type_name : Data_model.component_type_name -> string
val component_type_id   : Data_model.component_type_id -> string

val provide_arity : Data_model.provide_arity -> string
val require_arity : Data_model.require_arity -> string

(** 3. Packages *)
val package_name : Data_model.package_name -> string
val package_id   : Data_model.package_id -> string

(** 4. Repositories *)
val repository_name : Data_model.repository_name -> string
val repository_id   : Data_model.repository_id -> string

(** 5. Location *)
val location_name : Data_model.location_name -> string
val location_id   : Data_model.location_id  -> string

val component_name : Data_model.component_name -> string
val component_id   : Data_model.component_id -> string

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

val variable   : Data_constraint.variable -> string
val op         : Data_constraint.op -> string
val value      : Data_constraint.value -> string
val expression : Data_constraint.expression -> string
val konstraint : Data_constraint.konstraint -> string



