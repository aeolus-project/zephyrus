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
*)

open Data_constraint

val location_all_variables : Data_model.Port_set.t -> Data_model.Component_type_id_set.t -> Data_model.Package_id_set.t ->
    Data_model.Location_id_set.t -> (Data_model.port -> Data_model.Component_type_id_set.t) -> (Data_model.component_type_id -> Data_model.component_type) ->
    (string * (konstraint)) list (* generate the constraints linking all the global variables to their local variant *)

val universe      : Data_model.Resource_set.t -> Data_model.Location_id_set.t -> Data_model.universe -> ((konstraint list ref) * (konstraint list)) list
val specification : Data_model.Location_id_set.t -> Data_model.specification -> ((konstraint option ref) * (konstraint)) list
val locations     : Data_model.Resource_set.t -> Data_model.Location_set.t -> ((konstraint list ref) * (konstraint list)) list

val universe_full : unit -> unit      (* set the universe-related constraints in Data_state using what is provided in Data_state *)
val specification_full : unit -> unit (* set the specification-related constraints in Data_state using what is provided in Data_state *)
val configuration_full : unit -> unit (* set the configuration-related constraints in Data_state using what is provided in Data_state *)

val optimization_function : Data_model.universe -> Data_model.configuration -> Data_model.optimization_function -> Data_constraint.optimization_function
val optimization_function_full : unit -> unit (* conversion of the optimization function, and storage in Data_state *)
(* val basic_bounds : unit -> unit          (* set the bounds in Data_state to their most basic values *)*)






(*
module Plain : sig
  val universe : Data_model.Location_name_set.t -> Data_model.Resource_name_set.t -> Data_model.universe -> Data_constraint.t
  val configuration : Data_model.Resource_name_set.t -> Data_model.configuration -> Data_constraint.t
  val specification : Data_model.specification -> Data_constraint.t
  val optimization_function :

  val universe_flat : Data_model.universe -> Data_constraint.t
end
*)

(*
module Improved : sig
  val universe : Data_model.Location_name_set.t -> Data_model.Resource_name_set.t -> Data_model_improved.universe -> Data_constraint.t
  val configuration : Data_model.Resource_name_set.t -> Data_model_improved.configuration -> Data_constraint.t
  val specification : Data_model.specification -> Data_constraint.t
  val optimization_function :

  val universe_flat : Data_model_improved.universe -> Data_constraint.t
end

*)
