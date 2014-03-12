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

(** Classify locations present in a configuration into homogenous categories. *)

(* Depends on
    - datatypes/Data_model
    - datatypes/Data_constraint
*)

include module type of Data_model.Location_id_set_set

val domain : t -> Data_model.Location_id_set.t
val resource_categories : Data_model.universe -> Data_model.configuration -> Data_model.Location_id_set_set.t
val full_categories : Data_model.universe -> Data_model.configuration -> t
val generate_categories : unit -> unit
val generate_constraint : bool -> unit -> Data_constraint.konstraint