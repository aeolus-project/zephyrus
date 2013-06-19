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

(* 1. extracting informations from input/Settings *)

(* TODO: replace printfs by functions in output_helper.ml *)

let out_channel = stdout

let log_panic str = Output_helper.print_capo out_channel ("Zephyrus panic: " ^ str ^ "\nExiting"); exit(-1)
let log_missing_data kind what where = log_panic ("the " ^ kind ^ " \"" ^ what ^ "\" is missing from the " ^ where)

(* 2. logging functions *)

(*    2.1. Logging stage *)

let current_stages : (string list) ref = ref []

let log_stage_new str = if Settings.get_bool_basic Settings.verbose_stage then
    Output_helper.print out_channel ("+++++++++++++\nNew Stage: \"" ^ str ^ "\"\n+++++++++++++");
    Output_helper.extend_indent_stage ()

let log_stage_end () =  if Settings.get_bool_basic Settings.verbose_stage then
    let str = List.hd (!current_stages) in current_stages := List.tl (!current_stages);
    Output_helper.print out_channel ("-------------\nEnd Stage: \"" ^ str ^ "\"\n-------------");
    Output_helper.shorten_indent_stage ()


(*    2.2. parse settings *)
let log_input_settings_unknown_setting str = Printf.printf "Error in settings: the setting \"%s\" is unknown. Skiping its definition\n" str
let log_input_settings_wrong_value str = Printf.printf "Error in settings: the key \"%s\" has an unexpected value. Skipping its definition\n" str


(*    2.3. input *)
let log_input_file_error filename str = Printf.printf "Error: file \"%s\" => %s" filename str

let log_setting_not_set str = if Settings.get_bool_basic Settings.verbose_settings_non_set then Printf.printf "Warning: the %s is not set" str else ()

let log_common str  = Printf.printf "Normal:  %s"  str




