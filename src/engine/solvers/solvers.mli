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

(** Handle solving of a (possibly multiobjective) constraint problem. *)

(* Depends on
    - datatypes/Data_constraint
*)

(* the prints should be handled by Zephyrus log directly. Moreover maybe we should use the Lazy library, in case the string will never be printed *)
type solver_settings = {
  input_file            : string;
  output_file           : string;
  keep_input_file       : bool;
  keep_output_file      : bool;  
}

type t = Data_constraint.variable_bounds -> (string * Data_constraint.konstraint) list -> Data_constraint.optimization_function -> (Data_constraint.solution * (int list)) option
type t_full = solver_settings -> t

type settings_kind = Preprocess | Main
val settings_of_settings : settings_kind -> solver_settings

val full_of_settings     : settings_kind -> t_full
val of_settings          : settings_kind -> t



module type SOLVER = sig
  val solve : t_full (* It returns the solution and its cost. *)
end

module G12     : SOLVER
module G12_cpx : SOLVER
module GeCode  : SOLVER
val make_custom_solver_module : Engine_helper.program -> (module SOLVER)


