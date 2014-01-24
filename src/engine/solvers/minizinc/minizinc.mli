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

(** Produce MiniZinc files. *)

(* Depends on
    - datatypes/Data_common
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

type name = string
module Name_set : Data_common.Set.S with type elt = name
module Name_map : Data_common.Map.S with type key = name

class type named_variables = object
  method variables : Variable_set.t
  method names     : Name_set.t
  method get_variable : name -> variable
  method get_name : variable -> name
end

type structured_minizinc = { mzn_variables : named_variables; mzn_declaration : string; mzn_main_constraint : string; mzn_extra_constraint : string; mzn_output : string}

exception Wrong_optimization_function
val cost_variable_name : string

val get_named_variables : Variable_set.t -> named_variables
val core_translation : named_variables -> variable_bounds -> (string * konstraint) list -> structured_minizinc

val add_optimization_goal : structured_minizinc -> optimization_function -> minizinc
val add_extra_constraint : structured_minizinc -> expression -> int -> structured_minizinc







