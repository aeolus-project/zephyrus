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
    - input/Settings (to know what to log)
    - Printf (standard library, for logging of course)
*)

val log_stage_new : string -> unit
val log_stage_end : unit -> unit


(* core logging *)
val log_panic : string -> 'a
val log_missing_data : string -> string -> string -> 'a

val log_execution : string -> unit

(* setting logging *)
val log_setting_not_set : string -> unit
val log_settings : string -> unit

(* loading logging *)
val log_input_file_error : string -> string -> unit
val log_data : string -> string Lazy.t -> unit

(* translation into constraint logging *)

val log_constraint_execution : string -> unit
val log_constraint_data : string -> string Lazy.t -> unit (* desc -> data -> print *)

(* solver logging *)
val log_solver_execution : string -> unit
val log_solver_data : string -> string Lazy.t -> unit (* desc -> data -> print *)

