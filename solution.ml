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


open Variable_keys
open Helpers

type solution = (variable_key * int) list 
type solution_with_cost  = solution * int
type solution_with_costs = solution * int list

let string_of_solution solution =
  let strings_of_solution_assoc_list solution_assoc_list string_of_key =
    List.map (fun ( key, i ) -> 
          
          Printf.sprintf 
            "%s = %d" 
            (string_of_key key)
            i
  
        ) solution_assoc_list
  in
  let strings = 
    strings_of_solution_assoc_list solution string_of_variable_key
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)