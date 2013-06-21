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
*)

open Data_constraint

type minizinc = string

(* 
  Protocol to produce some minizinc:
   1. us [variables] to get the variables from the constraints and optimization functions, necessary for the variable declaration part of the result
   2. use [core] to get the variable declaration part, the constraint part, and the output part of the result
   3. Iteratively (for lexicographic optimization functions),
     3.1. apply [optimization_goal] to the result, to feed the solver with and then get an optimum for that goal
     3.2. apply [extra_constraint] to the result, to add informations about the minimal cost, so we can iterate again
 *)

type named_variables = string Variable_map.t
type structured_minizinc = { mzn_variables : named_variables; mzn_declaration : string; mzn_main_constraint : string; mzn_extra_constraint : string; mzn_output : string}

exception Wrong_optimization_function


let variables : (string * konstraint) list -> optimization_function -> named_variables
let core : named_variables -> variable_bound -> (string * konstraint) list -> structured_minizinc

let optimization_goal : structured_minizinc -> optimization_function -> string
let extra_constraint : structured_minizinc -> expression -> int -> structured_minizinc







