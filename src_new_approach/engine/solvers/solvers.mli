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

type solver_settings = {
  print_solver_variables       : bool; (* these prints should be handled by Zephyrus log directly, no? Maybe we should use the Lazy library, in case the string will never be printed *)
  print_solver_constraints     : bool;
  print_solver_execution       : bool;
  print_intermediate_solutions : bool;
  input_file_prefix            : string;
  output_file_prefix           : string;
  keep_input_file              : bool;
  keep_output_file             : bool;  
}

module type SOLVER = sig
  val solve : solver_settings ->
      (string * Data_constraint.konstraint) list ->
      Data_constraint.optimization_function ->
      (Data_constraint.solution * (int list)) (* It returns the solution and its cost. *)
end

module G12    : SOLVER
module GeCode : SOLVER

