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

let log_input_file_error filename str = Printf.printf "Error: file \"%s\" => %s" filename str
let log_setting_not_set str = if !Settings.verbose_warning_setting_not_set then Printf.printf "Warning: the %s is not set" str else ()
let log_missing_data kind what where = Printf.printf "Error: the %s \"%s\" is missing from the %s" kind what where

let log_common str  = Printf.printf "Normal:  %s"  str
