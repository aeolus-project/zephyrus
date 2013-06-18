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
  MUST DEPEND ON NOTHING !!
*)


(* 00. DataTypes *)
type mode = 
 | Mode_classic
 | Mode_flat
 | Mode_bin_packing
 

type optim =     (* the different kind of optimization in zephyrus *)
  | Optim_none
  | Optim_simple
  | Optim_compact
  | Optim_spread
  | Optim_conservative

type solver =    (* the different kind of solver available in zephyrus *)
  | Solver_none
  | Solver_gcode
  | Solver_g12
  | Solver_facile

type solver_bin_packing = 
  | Solver_bin_packing_unknown

type conf_gen_bindings =  (* the different mean we have to generate the final configuration *)
  | Conf_gen_bindings_none
  | Conf_gen_bindings_candy
  | Conf_gen_bindings_constraint

type conf_gen_packages = (* the different mean we have to generate the packages we need to install *)
  | Conf_gen_packages_none
  | Conf_gen_packages_universe

type out_file = 
  | Out_file_plain
  | Out_file_json
  | Out_file_graph_deployment
  | Out_file_graph_simplified
  | Out_file_graph_components
  | Out_file_graph_packages

(* No Need For A Number: Essencial *)

let zephyrus_mode : mode option ref = ref None

(* 01. Input Files *)

let input_file_universe              : string option ref          = ref None (* the main file of the universe *)
let input_file_repositories          : (string * string) list ref = ref []   (* the file for the repositories, with their names *)
let input_file_initial_configuration : string option ref          = ref None (* initial configuration *)
let input_file_specification         : string option ref          = ref None (* specification file *)
let input_optimization_function      : optim option ref           = ref None (* Optimization function *)

let input_file_settings              : string option ref          = ref None 

(* TODO: Add options for the syntax of files ? *)


(* 02. Which initial Data to Generate *)

let data_generation_universe              : bool option ref = ref None
let data_generation_repositories          : bool option ref = ref None
let data_generation_initial_configuration : bool option ref = ref None
let data_generation_specification         : bool option ref = ref None
let data_generation_optimization_function : bool option ref = ref None

let data_package_name_extended : bool option ref = ref None

let data_check_universe              : bool option ref = ref None
let data_check_repositories          : bool option ref = ref None
let data_check_initial_configuration : bool option ref = ref None
let data_check_universe_full         : bool option ref = ref None
let data_check_specification         : bool option ref = ref None

let data_check_settings : bool option ref = ref None


(* 03. Pre Processing *)

let pre_process_spec_wf_detection           : bool option ref = ref None
let pre_process_spec_empty_detection        : bool option ref = ref None
let pre_process_spec_empty_detection_solver : solver option ref = ref None

let pre_process_universe_loop_detection     : bool option ref = ref None
let pre_process_universe_bound_computation  : bool option ref = ref None

let pre_process_package_coinst              : bool option ref = ref None
let pre_process_package_trim_package        : bool option ref = ref None
let pre_process_component_types_trim        : bool option ref = ref None


(* 04. Constraint Solver *)

let constraint_solver_classic_linear   : bool option ref = ref None
let constraint_solver_classic_kind     : solver option ref = ref None

let constraint_solver_bin_packing_kind : solver_bin_packing option ref = ref None


(* 05. Temporary Files *)

let pre_process_spec_empty_detection_input_file       : string option ref = ref None
let pre_process_spec_empty_detection_output_file      : string option ref = ref None
let pre_process_spec_empty_detection_input_file_keep  : bool option ref = ref None
let pre_process_spec_empty_detection_output_file_keep : bool option ref = ref None

let constraint_solver_flat_input_file       : string option ref = ref None
let constraint_solver_flat_output_file      : string option ref = ref None
let constraint_solver_flat_input_file_keep  : bool option ref = ref None
let constraint_solver_flat_output_file_keep : bool option ref = ref None

let constraint_solver_classic_input_file       : string option ref = ref None
let constraint_solver_classic_output_file      : string option ref = ref None
let constraint_solver_classic_input_file_keep  : bool option ref = ref None
let constraint_solver_classic_output_file_keep : bool option ref = ref None


(* 06. Configuration Generation *)

let configuration_generation_bindings : conf_gen_bindings option ref = ref None
let configuration_generation_packages : conf_gen_packages option ref = ref None


(* 07. Output Configuration *)

let output_file                  : (out_file * string) list ref = ref []


(* 08. Verbose Options *)

let verbose_stage : bool option ref = ref None

(* settings *)

let verbose_settings_check_warning : bool option ref = ref None
let verbose_settings_check_error   : bool option ref = ref None
let verbose_settings_non_set       : bool option ref = ref None

(* 08.1. inputs *)

let verbose_input_warning : bool option ref = ref None
let verbose_input_error   : bool option ref = ref None

let verbose_input_universe              : bool option ref = ref None
let verbose_input_repositories          : bool option ref = ref None
let verbose_input_universe_full         : bool option ref = ref None
let verbose_input_initial_configuration : bool option ref = ref None
let verbose_input_specification         : bool option ref = ref None
let verbose_input_optimization_function : bool option ref = ref None

let verbose_input_universe_check              : bool option ref = ref None
let verbose_input_repositories_check          : bool option ref = ref None
let verbose_input_initial_configuration_check : bool option ref = ref None
let verbose_input_specification_check         : bool option ref = ref None

(* 08.2. pre-processs *)

let verbose_spec_fw_detection                        : bool option ref = ref None
let verbose_spec_empty_detection                     : bool option ref = ref None
let verbose_spec_fw_detection_activities             : bool option ref = ref None
let verbose_spec_empty_detection_activities          : bool option ref = ref None

let verbose_universe_graph                           : bool option ref = ref None
let verbose_universe_loop_detection                  : bool option ref = ref None
let verbose_universe_bound_detection                 : bool option ref = ref None
let verbose_universe_loop_detection_activities       : bool option ref = ref None
let verbose_universe_bound_detection_activities      : bool option ref = ref None

let verbose_universe_package_trim_package            : bool option ref = ref None
let verbose_universe_component_types_trim            : bool option ref = ref None
let verbose_universe_package_trim_package_activities : bool option ref = ref None
let verbose_universe_component_types_trim_activities : bool option ref = ref None

(* 08.3. constraints *)

let verbose_constraint                   : bool option ref = ref None
let verbose_constraint_solver_activities : bool option ref = ref None
let verbose_solution                     : bool option ref = ref None

(* 08.4. configuration generation *)

let verbose_gen_configuration_init    : bool option ref = ref None
let verbose_gen_configuration_package : bool option ref = ref None
let verbose_gen_configuration_full    : bool option ref = ref None



(* 09. Post Processing *)

let generate_plan : bool option ref = ref None (* for instance. TODO: must be expanded when we know more. *)




(* 10. Very Simple functions *)


let get_bool_basic r = match !r with
  | None    -> false
  | Some(b) -> b


