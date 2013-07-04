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
    - output/Output_helper
*)

(* TODO: replace printfs by functions in output_helper.ml *)

let out_channel = stdout

(* core logging *)
let log_panic str = Output_helper.print_capo out_channel ("Zephyrus panic: " ^ str ^ "\nExiting"); flush out_channel; exit(-1)
let log_missing_data kind what where = log_panic ("the " ^ kind ^ " \"" ^ what ^ "\" is missing from the " ^ where)

let log_execution str = if Settings.get_bool_basic Settings.verbose_activities then Output_helper.print out_channel str


(* stage logging *)
let current_stages : (string list) ref = ref []

let log_stage_new str = if Settings.get_bool_basic Settings.verbose_stage then
    Output_helper.print out_channel ("+++++++++++++\nNew Stage: \"" ^ str ^ "\"\n+++++++++++++");
    Output_helper.extend_indent_stage ()

let log_stage_end () =  if Settings.get_bool_basic Settings.verbose_stage then
    let str = List.hd (!current_stages) in current_stages := List.tl (!current_stages);
    Output_helper.print out_channel ("-------------\nEnd Stage: \"" ^ str ^ "\"\n-------------");
    Output_helper.shorten_indent_stage ()


(* setting logging *)
let log_input_settings_unknown_setting str = Printf.printf "Error in settings: the setting \"%s\" is unknown. Skiping its definition\n" str
let log_input_settings_wrong_value str = Printf.printf "Error in settings: the key \"%s\" has an unexpected value. Skipping its definition\n" str
let log_settings str = if Settings.get_bool_basic Settings.verbose_settings_data then Printf.printf "\nSETTINGS:\n\n%s\n" str


(* loading logging *)
let log_input_file_error filename str = Printf.printf "Error: file \"%s\" => %s" filename str
let log_setting_not_set str = if Settings.get_bool_basic Settings.verbose_settings_non_set then Printf.printf "Warning: the %s is not set" str

let log_universe_full desc str = if Settings.get_bool_basic Settings.verbose_data_full then Output_helper.print out_channel (desc ^ (Lazy.force str))
let log_data desc str = if Settings.get_bool_basic Settings.verbose_data then Output_helper.print out_channel (desc ^ (Lazy.force str))

(* translation into constraint logging *)
let log_constraint_execution str = if Settings.get_bool_basic Settings.verbose_constraint then Output_helper.print out_channel str
let log_constraint_data desc str = if Settings.get_bool_basic Settings.verbose_constraint then (
  Output_helper.print out_channel desc; Output_helper.print out_channel (Lazy.force str))

(* solver logging *)
let log_solver_execution str = if Settings.get_bool_basic Settings.verbose_constraint_solver_activities then Output_helper.print out_channel str
let log_solver_data desc str =  if Settings.get_bool_basic Settings.verbose_constraint_solver_activities then (
  Output_helper.print out_channel desc; Output_helper.print out_channel (Lazy.force str))



