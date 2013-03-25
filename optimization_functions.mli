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
open Typing_context
open Variable_keys
open Generic_constraints

type optimization_function =
  | Satisfy          (* Output the first solution found that satisfies the constraints. *)
  | Maximize of expr (* Search for the solution that maximizes the given expression. *)
  | Minimize of expr (* Search for the solution that minimizes the given expression. *)

(* Ingredients of optimization function expressions. *)
val cost_expr_number_of_all_components          :                  universe         -> expr
val cost_expr_number_of_all_packages            : configuration -> universe         -> expr
val cost_expr_number_of_used_locations          : configuration -> universe -> bool -> expr
val cost_expr_number_of_free_locations          : configuration -> universe -> bool -> expr
val cost_expr_difference_of_components          : configuration -> universe         -> expr
val cost_expr_difference_of_packages            : configuration -> universe         -> expr

(* Full optimization function expressions. *)
val compact      : configuration -> universe -> optimization_function list
val conservative : configuration -> universe -> optimization_function list
val spread       : configuration -> universe -> optimization_function list