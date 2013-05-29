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

(* I wonder if all these configuration options for the input should not be abstracted away with few modules in the input.mli file *)
type input_configuration =
 | Input_configuration_none
 | Input_configuration_file_json of string

type input_specification =
 | Input_specification_none
 | Input_specification_file_json of string
 | Input_specification_file_text of string
 | Input_specification_cmd_text of string

type input_optimization =
 | Input_optimization_none
 | Input_optimization_file_text of string
 | Input_optimization_cmd_text of string

type input_universe =
 | Input_universe_none
 | Input_universe_file_json of string

type input_repositories =
 | Input_repositories_files of string list


type model =
 | Model_flat
 | Model_location
 | Model_hierarchy

type constraints = 
 | Constraints

type solver =
 | Solver_facile
 | Solver_minizinc
 | Solver_gecode

...


