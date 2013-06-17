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

(* 1. extracting informations from input/Settings *)

let get_bool_of_setting r = match !r with
  | None    -> false
  | Some(b) -> b


(* 2. logging functions *)

(*   2.1. parse settings *)
let log_input_settings_unknown_setting str = Printf.printf "Error in settings: the setting \"%s\" is unknown. Skiping its definition" str
let log_input_settings_wrong_value str = Printf.printf "Error in settings: the key \"%s\" has an unexpected value. Skipping its definition" str

let log_input_file_error filename str = Printf.printf "Error: file \"%s\" => %s" filename str

let log_setting_not_set str = if get_bool_of_setting Settings.verbose_settings_non_set then Printf.printf "Warning: the %s is not set" str else ()
let log_missing_data kind what where = Printf.printf "Error: the %s \"%s\" is missing from the %s" kind what where

let log_common str  = Printf.printf "Normal:  %s"  str
