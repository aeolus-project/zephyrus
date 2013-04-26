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

open Aeolus_types_t
open Variables
open Generic_constraints

type minizinc_variables

val create_minizinc_variables : variable list -> minizinc_variables
val string_of_minizinc_variables : minizinc_variables -> string

val translate_constraints : 
  minizinc_variables ->                           (* The minizinc variables to use in the translation. *)
  Constraints.generated_constraints ->            (* The generated constraints which will be translated. *)
  Optimization_functions.optimization_function -> (* The optimization expression. *)
  string

type bound_minizinc_variables = (string * int) list

val solution_of_bound_minizinc_variables : minizinc_variables -> bound_minizinc_variables -> Solution.solution_with_cost
