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
(*    along with Zephyrus.  If not, see <http=//www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(* Depends on
    - Pervasives (standard library: files, opening and closing)
    - String     (standard library)
    - input/Settings (for configuration)
    - output/Zephyrus_log (for logging)
*)

type t_file = | Input_file of in_channel | Input_file_error

val input_open : string -> t_file

val input_and_set : string -> string option ref -> 'a option ref -> (string -> 'a) -> unit
(* parameters 
  - description of what to load (for the warning message)
  - the pointer to the file name, coming from input/Settings
  - the pointer to the object to set, coming from datatypes/All_data
  - the parsing function
*)

