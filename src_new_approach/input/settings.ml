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

type const_gen = (* the different kind of constraint zephyrus can generate *)
  | Const_gen_none
  | Const_gen_paper
  | Const_gen_linear
  | Const_gen_bin_packing

type conf_gen =  (* the different mean we have to generate the final configuration *)
  | Conf_gen_none
  | Conf_gen_candy
  | Conf_gen_constraint

type out_file = 
  | Out_file_plain
  | Out_file_json
  | Out_file_graph_deployment
  | Out_file_graph_simplified
  | Out_file_graph_components
  | Out_file_graph_packages


(* 01. Input Files *)

let input_file_universe              : string option ref          = ref None (* the main file of the universe *)
let input_file_repositories          : (string * string) list ref = ref []   (* the file for the repositories, with their names *)
let input_file_initial_configuration : string option ref          = ref None (* initial configuration *)
let input_file_specification         : string option ref          = ref None (* specification file *)
let input_optimization_function      : optim option ref           = ref None (* Optimization function *)

let input_file_settings              : string option ref          = ref None 

(* Add options for the syntax of files ? *)


(* 02. Data Generation *)

let data_generation_full : bool option ref = ref None
let data_generation_flat : bool option ref = ref None

let data_generation_universe_check      : bool option ref = ref None
let data_generation_configuration_check : bool option ref = ref None
let data_generation_specification_check : bool option ref = ref None

(* 03. Pre Processing *)

let pre_process_with_wf_spec_detection           : bool option ref = ref None
let pre_process_with_empty_spec_detection        : bool option ref = ref None
let pre_process_empty_spec_detection_solver      : solver option ref = ref None
let pre_process_empty_spec_detection_input       : string option ref = ref None
let pre_process_empty_spec_detection_output      : string option ref = ref None
let pre_process_empty_spec_detection_input_keep  : bool option ref = ref None
let pre_process_empty_spec_detection_output_keep : bool option ref = ref None

let pre_process_with_loop_detection         : bool option ref = ref None
let pre_process_with_bound_computation      : bool option ref = ref None
let pre_process_with_flat_model_solving     : bool option ref = ref None
let pre_process_flat_model_solver           : solver option ref = ref None
let pre_process_flat_model_input            : string option ref = ref None
let pre_process_flat_model_output           : string option ref = ref None
let pre_process_flat_model_input_keep       : bool option ref = ref None
let pre_process_flat_model_output_keep      : bool option ref = ref None

let pre_process_using_coinst           : bool option ref = ref None
let pre_process_with_trim_package      : bool option ref = ref None
let pre_process_with_trim_package_full : bool option ref = ref None (* second trimming pass, deleting implementation dependencies *)


(* 04. Constrains Generation *)

let constraint_generation_linear      : bool option ref = ref None
let constraint_generation_bin_packing : bool option ref = ref None


(* 05. Constraint Solver *)

let constraint_solver_classic          : bool option ref = ref None
let constraint_solver_classic_kind     : solver option ref = ref None

let constraint_solver_input_file       : string option ref = ref None
let constraint_solver_output_file      : string option ref = ref None
let constraint_solver_input_file_keep  : bool option ref = ref None
let constraint_solver_output_file_keep : bool option ref = ref None


(* 06. Post Processing *)

  (* => Empty for now *)


(* 07. Configuration Generation *)

let configuration_generation : conf_gen option ref = ref None


(* 08. Output Configuration *)

let output_file                  : (out_file * string) list ref = ref []
let output_package_name_extended : bool option ref = ref None


(* 09. Verbose Options *)

let verbose_warning_setting_not_set               :  bool ref = ref false
let verbose_warning_not_possible                  :  bool ref = ref false

let verbose_input_error_full                      :  bool ref = ref false (* full description of error in an input data *)
let verbose_output_error_full                     :  bool ref = ref false (* full description of why we don't have a solution *)


let verbose_universe_init                         :  bool ref = ref false
let verbose_universe_component_trimmed            :  bool ref = ref false
let verbose_universe_component_graph              :  bool ref = ref false
let verbose_universe_component_bounds             :  bool ref = ref false
let verbose_universe_flat                         :  bool ref = ref false
let verbose_universe_flat_constraint              :  bool ref = ref false
let verbose_universe_flat_solver_internal         :  bool ref = ref false
let verbose_universe_flat_solver_execution        :  bool ref = ref false
let verbose_universe_flat_solution                :  bool ref = ref false

let verbose_universe_package_trimmed              :  bool ref = ref false
let verbose_universe_package_trimmed_full         :  bool ref = ref false
let verbose_universe_constraint                   :  bool ref = ref false


let verbose_configuration_init                    :  bool ref = ref false
let verbose_configuration_final_no_binding        :  bool ref = ref false
let verbose_configuration_final_full              :  bool ref = ref false


let verbose_specification                         :  bool ref = ref false
let verbose_specification_global                  :  bool ref = ref false
let verbose_specification_global_constraint       :  bool ref = ref false
let verbose_specification_global_solver_internal  :  bool ref = ref false
let verbose_specification_global_solver_execution :  bool ref = ref false
let verbose_specification_global_solution         :  bool ref = ref false
let verbose_specification_local                   :  bool ref = ref false


let verbose_constraint                            :  bool ref = ref false
let verbose_constraint_variables                  :  bool ref = ref false
let verbose_constraint_solver_internal            :  bool ref = ref false
let verbose_constraint_solver_execution           :  bool ref = ref false
let verbose_constraint_solver_solutions           :  bool ref = ref false


let verbose_solution                              :  bool ref = ref false

let verbose_binding_generation_constraint         :  bool ref = ref false
let verbose_binding_generation_solver_internal    :  bool ref = ref false
let verbose_binding_generation_solver_execution   :  bool ref = ref false


(* 10. Annex function *)

let optimization_function_of_string str = match str with
  | "Simple"       -> Optim_simple
  | "compact"      -> Optim_compact
  | "spread"       -> Optim_spread
  | "conservative" -> Optim_conservative
  | _ -> Optim_none

let solver_kind_of_string str =  match str with
  | "facile" -> Solver_facile
  | "g12"    -> Solver_g12
  | _        -> Solver_gcode

let output_file_kind_of_string kind = match kind with
    | "plain"                       -> Out_file_plain
    | "json"                        -> Out_file_json
    | "simplified-deployment-graph" -> Out_file_graph_simplified
    | "components-graph"            -> Out_file_graph_components
    | "packages-graph"              -> Out_file_graph_packages
    | _                             -> Out_file_graph_deployment


