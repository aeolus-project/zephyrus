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

type solver_settings = {
  print_solver_vars            : bool;
  print_solver_cstrs           : bool;
  print_solver_exe             : bool;
  print_intermediate_solutions : bool;
}

module type SOLVER =
  sig
    
    val solve : 
      Variable_keys.variable_key list ->
      Constraints.generated_constraints ->
      Generic_constraints.expr -> (* A single optimization expression. *)
      solver_settings ->
      Solution.solution_with_cost (* It returns the solution and its cost. *)
      
  end

module type SOLVER_LEX =
  sig 
    include SOLVER

    val solve_lex : 
      Variable_keys.variable_key list ->
      Constraints.generated_constraints ->
      Generic_constraints.expr list -> (* List of optimization expressions. *)
      solver_settings ->
      Solution.solution_with_costs (* It returns the solution and a list of costs (one for each optimization expression). *)

  end

module G12    : SOLVER_LEX
module GeCode : SOLVER_LEX
module FaCiLe : SOLVER