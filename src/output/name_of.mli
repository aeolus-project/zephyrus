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

(** Retrieving a readeable (string) name corresponding to identifiers from our Zephyrus model internal representation. *)

(* Depends on
    - datatypes/Data_model
    - datatypes/Data_constraint
    - datatypes/Data_helper
*)

(*val set_catalog : Data_model_catalog.closed_model_catalog option -> unit*)

val resource_id       : Data_model.resource_id       -> string
val port_id           : Data_model.port_id           -> string
val component_type_id : Data_model.component_type_id -> string
val package_id        : Data_model.package_id        -> string
val repository_id     : Data_model.repository_id     -> string
val location_id       : Data_model.location_id       -> string
val component_id      : Data_model.component_id      -> string
