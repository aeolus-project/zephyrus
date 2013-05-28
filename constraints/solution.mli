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


open Variables

(** Constraint problem solution: variables bound to values. *)
type solution = (variable * int) list 

(** Solution of a constraint problem with a single minimize/maximize criterium,
    with additional information about the value of optimization function for this solution. *)
type solution_with_cost  = solution * int

(** Solution of a constraint problem with multiple lexicographic minimize/maximize criteris,
    with addutional information about the value of all the optimization functions for this solution. *)
type solution_with_costs = solution * int list

(** Printing *)
val string_of_solution : solution -> string
