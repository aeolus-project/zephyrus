%{ (****************************************************************************)
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
    - input/Settings (obviously)
    - datatypes/Data_common (for the map)
*)

type value =
  | BoolValue  of bool
  | IntValue   of int
  | IdentValue of string
  | PairValue  of value * value
  | ListValue  of value list

let rec string_of_value value = 
  match value with
  | BoolValue  b        -> if b then "true" else "false"
  | IntValue   i        -> Printf.sprintf "%d" i
  | IdentValue s        -> Printf.sprintf "\"%s\"" s
  | PairValue  (v1, v2) -> Printf.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | ListValue  l        -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_value l))

exception Wrong_value

let get_bool  v = match v with | BoolValue(b)     -> b       | _ -> raise Wrong_value
let get_int   v = match v with | IntValue(i)      -> i       | _ -> raise Wrong_value
let get_ident v = match v with | IdentValue(s)    -> s       | _ -> raise Wrong_value
let get_pair  v = match v with | PairValue(v1,v2) -> (v1,v2) | _ -> raise Wrong_value
let get_list  v = match v with | ListValue(l)     -> l       | _ -> raise Wrong_value

(* Here are where you define your functions, and put them in the [functions] list *)


let mode_of_string s = match s with
  | "classic"     -> Settings.Mode_classic
  | "flat"        -> Settings.Mode_flat
  | "bin-packing" -> Settings.Mode_bin_packing
  | _             -> raise Wrong_value
 
let optim_of_string s = match s with
  | "simple"       -> Settings.Optim_simple
  | "compact"      -> Settings.Optim_compact
  | "conservative" -> Settings.Optim_conservative
  | "spread"       -> Settings.Optim_spread
  | "none"         -> Settings.Optim_none
  | _              -> raise Wrong_value

let solver_kind_of_string str =  match str with
  | "facile" -> Settings.Solver_facile
  | "g12"    -> Settings.Solver_g12
  | "gcode"  -> Settings.Solver_gcode
  | "none"   -> Settings.Solver_none
  | _        -> raise Wrong_value

let solver_bin_packing_of_string s = match s with 
  | "unknown" -> Settings.Solver_bin_packing_unknown
  | _         -> raise Wrong_value

let conf_gen_bindings_of_string s = match s with
  | "none"       -> Settings.Conf_gen_bindings_none
  | "candy"      -> Settings.Conf_gen_bindings_candy
  | "constraint" -> Settings.Conf_gen_bindings_constraint
  | _            -> raise Wrong_value

let conf_gen_packages_of_string s = match s with 
  | "none"     -> Settings.Conf_gen_packages_none
  | "universe" -> Settings.Conf_gen_packages_universe
  | _          -> raise Wrong_value

let out_file_of_string kind = match kind with
  | "plain"                       -> Settings.Out_file_plain
  | "json"                        -> Settings.Out_file_json
  | "simplified-deployment-graph" -> Settings.Out_file_graph_simplified
  | "components-graph"            -> Settings.Out_file_graph_components
  | "packages-graph"              -> Settings.Out_file_graph_packages
  | "graph-deployment"            -> Settings.Out_file_graph_deployment
  | _                             -> raise Wrong_value



let assign_values_to_settings_functions : (string * (value -> unit)) list = [
  ( "zephyrus-mode" , fun v -> Settings.zephyrus_mode := Some(mode_of_string (get_ident v)));

(* 01. Input Files *)
  ("input-file-universe"         , fun v -> Settings.input_file_universe              := Some(get_ident v));
  ("input-file-configuration"    , fun v -> Settings.input_file_initial_configuration := Some(get_ident v));
  ("input-file-specification"    , fun v -> Settings.input_file_specification         := Some(get_ident v));
  ("input-file-repositories"     , fun v -> List.iter 
    (fun v' -> let (v1,v2) = get_pair v' in Settings.input_file_repositories := (get_ident v1, get_ident v2)::!Settings.input_file_repositories) (get_list v));
  ("input-optimization-function" , fun v -> Settings.input_optimization_function := Some(optim_of_string (get_ident v)));

(* 02. Which initial Data to Generate *)
  ("import-universe"              , fun v -> Settings.data_generation_universe              := Some(get_bool v));
  ("import-repositories"          , fun v -> Settings.data_generation_repositories          := Some(get_bool v));
  ("import-initial-configuration" , fun v -> Settings.data_generation_initial_configuration := Some(get_bool v));
  ("import-specification"         , fun v -> Settings.data_generation_specification         := Some(get_bool v));
  ("import-optimization-function" , fun v -> Settings.data_generation_optimization_function := Some(get_bool v));

  ("check-universe"              , fun v -> Settings.data_check_universe              := Some(get_bool v));
  ("check-repositories"          , fun v -> Settings.data_check_repositories          := Some(get_bool v));
  ("check-initial-configuration" , fun v -> Settings.data_check_initial_configuration := Some(get_bool v));
  ("check-universe_full"         , fun v -> Settings.data_check_universe_full         := Some(get_bool v));
  ("check-specification"         , fun v -> Settings.data_check_specification         := Some(get_bool v));

  ("check-settings" , fun v -> Settings.data_check_settings := Some(get_bool v));

(* 03. Pre Processing *)

  ( "detect-spec-well-formedness"      , fun v -> Settings.pre_process_spec_wf_detection           := Some(get_bool v));
  ( "detect-spec-is-empty"             , fun v -> Settings.pre_process_spec_empty_detection        := Some(get_bool v));
  ( "detect-spec-solver"               , fun v -> Settings.pre_process_spec_empty_detection_solver := Some(solver_kind_of_string (get_ident v)));

  ( "detect-component-types-have-loop" , fun v -> Settings.pre_process_universe_loop_detection    := Some(get_bool v));
  ( "detect-component-types-bounds"    , fun v -> Settings.pre_process_universe_bound_computation := Some(get_bool v));

  ( "package-coinst"                   , fun v -> Settings.pre_process_package_coinst       := Some(get_bool v));
  ( "package-trim"                     , fun v -> Settings.pre_process_package_trim_package := Some(get_bool v));
  ( "component-types-trim"             , fun v -> Settings.pre_process_component_types_trim := Some(get_bool v));


(* 04. Constraint Solver *)
  ( "weight-locations"        , fun v -> Settings.constraint_weight_locations       := Some(get_int v));
  ( "weight-component-types"  , fun v -> Settings.constraint_weight_component_types := Some(get_int v));
  ( "weight-packages"         , fun v -> Settings.constraint_weight_packages        := Some(get_int v));

  ( "solver-use-linear-constraint" , fun v -> Settings.constraint_solver_classic_linear   := Some(get_bool v));
  ( "solver"                       , fun v -> Settings.constraint_main_solver     := Some(solver_kind_of_string (get_ident v)));
  ( "solver-bin-packing"           , fun v -> Settings.constraint_solver_bin_packing_kind := Some(solver_bin_packing_of_string (get_ident v)));


(* 05. Temporary Files *)

  ( "preprocess-constraint-file"      , fun v -> Settings.pre_process_input_file       := Some(get_ident v));
  ( "preprocess-solution-file"        , fun v -> Settings.pre_process_output_file      := Some(get_ident v));
  ( "preprocess-keep-constraint-file" , fun v -> Settings.pre_process_input_file_keep  := Some(get_bool v));
  ( "preprocess-keep-solution-file"   , fun v -> Settings.pre_process_output_file_keep := Some(get_bool v));

  ( "solver-flat-constraint-file"      , fun v -> Settings.constraint_solver_flat_input_file       := Some(get_ident v));
  ( "solver-flat-solution-file"        , fun v -> Settings.constraint_solver_flat_output_file      := Some(get_ident v));
  ( "solver-flat-keep-constraint-file" , fun v -> Settings.constraint_solver_flat_input_file_keep  := Some(get_bool v));
  ( "solver-flat-keep-solution-file"   , fun v -> Settings.constraint_solver_flat_output_file_keep := Some(get_bool v));

  ( "solver-constraint-file"      , fun v -> Settings.constraint_solver_classic_input_file       := Some(get_ident v));
  ( "solver-solution-file"        , fun v -> Settings.constraint_solver_classic_output_file      := Some(get_ident v));
  ( "solver-keep-constraint-file" , fun v -> Settings.constraint_solver_classic_input_file_keep  := Some(get_bool v));
  ( "solver-keep-solution-file"   , fun v -> Settings.constraint_solver_classic_output_file_keep := Some(get_bool v));


(* 06. Configuration Generation *)

  ( "generate-bindings" , fun v -> Settings.configuration_generation_bindings := Some(conf_gen_bindings_of_string (get_ident v)));
  ( "generate-packages" , fun v -> Settings.configuration_generation_packages := Some(conf_gen_packages_of_string (get_ident v)));


(* 07. Output Configuration *)

  ( "results"                           , fun v -> List.iter 
    (fun v' -> let (v1,v2) = get_pair v' in Settings.output_file := (out_file_of_string (get_ident v1), get_ident v2)::!Settings.output_file) (get_list v));
  ( "append-repository-to-package-name" , fun v -> Settings.data_package_name_extended := Some(get_bool v));


(* 08. Verbose Options *)

  ( "verbose-stage" , fun v -> Settings.verbose_stage := Some(get_bool v));

(* settings *)

  ( "verbose-settings-check-warning"         , fun v -> Settings.verbose_settings_check_warning := Some(get_bool v));
  ( "verbose-settings-check-error"           , fun v -> Settings.verbose_settings_check_error   := Some(get_bool v));
  ( "verbose-settings-check-setting-non-set" , fun v -> Settings.verbose_settings_non_set       := Some(get_bool v));

(* 08.1. inputs *)

  ( "verbose-input-warning" , fun v -> Settings.verbose_input_warning := Some(get_bool v));
  ( "verbose-input-error"   , fun v -> Settings.verbose_input_error   := Some(get_bool v));

  ( "print-data"                  , fun v -> Settings.verbose_data                        := Some(get_bool v));
  ( "print-input-universe"        , fun v -> Settings.verbose_input_universe              := Some(get_bool v));
  ( "print-input-repositories"    , fun v -> Settings.verbose_input_repositories          := Some(get_bool v));
  ( "print-full-universe"         , fun v -> Settings.verbose_input_universe_full         := Some(get_bool v));
  ( "print-initial-configuration" , fun v -> Settings.verbose_input_initial_configuration := Some(get_bool v));
  ( "print-specification"         , fun v -> Settings.verbose_input_specification         := Some(get_bool v));
  ( "print-optimization-function" , fun v -> Settings.verbose_input_optimization_function := Some(get_bool v));

  ( "extra-log-universe-check"              , fun v -> Settings.verbose_input_universe_check              := Some(get_bool v));
  ( "extra-log-repositories-check"          , fun v -> Settings.verbose_input_repositories_check          := Some(get_bool v));
  ( "extra-log-initial-configuration-check" , fun v -> Settings.verbose_input_initial_configuration_check := Some(get_bool v));
  ( "extra-log-specification-check"         , fun v -> Settings.verbose_input_specification_check         := Some(get_bool v));

(* 08.2. pre-processs *)

  ( "print-is-spec-well-formed"     , fun v -> Settings.verbose_spec_fw_detection                        := Some(get_bool v));
  ( "print-is-spec-empty"           , fun v -> Settings.verbose_spec_empty_detection                     := Some(get_bool v));
  ( "extra-log-is-spec-well-formed" , fun v -> Settings.verbose_spec_fw_detection_activities             := Some(get_bool v));
  ( "extra-log-is-spec-empty"       , fun v -> Settings.verbose_spec_empty_detection_activities          := Some(get_bool v));

  ( "print-universe-graph"               , fun v -> Settings.verbose_universe_graph                           := Some(get_bool v));
  ( "print-has-component-types-loop"     , fun v -> Settings.verbose_universe_loop_detection                  := Some(get_bool v));
  ( "print-component-types-bounds"       , fun v -> Settings.verbose_universe_bound_detection                 := Some(get_bool v));
  ( "extra-log-has-component-types-loop" , fun v -> Settings.verbose_universe_loop_detection_activities       := Some(get_bool v));
  ( "extra-log-component-types-bounds"   , fun v -> Settings.verbose_universe_bound_detection_activities      := Some(get_bool v));

  ( "print-packages-trimmed"            , fun v -> Settings.verbose_universe_package_trim_package            := Some(get_bool v));
  ( "print-component-types-trimmed"     , fun v -> Settings.verbose_universe_component_types_trim            := Some(get_bool v));
  ( "extra-log-packages-trimmed"        , fun v -> Settings.verbose_universe_package_trim_package_activities := Some(get_bool v));
  ( "extra-log-component-types-trimmed" , fun v -> Settings.verbose_universe_component_types_trim_activities := Some(get_bool v));

(* 08.3. constraints *)

  ( "print-main-constraint"             , fun v -> Settings.verbose_constraint                   := Some(get_bool v));
  ( "print-main-solution"               , fun v -> Settings.verbose_constraint_solver_activities := Some(get_bool v));
  ( "extra-log-main-constraint-solving" , fun v -> Settings.verbose_solution                     := Some(get_bool v));

(* 08.4. configuration generation *)

  ( "print-result-location+components"        , fun v -> Settings.verbose_gen_configuration_init    := Some(get_bool v));
  ( "print-result-location+components+pakage" , fun v -> Settings.verbose_gen_configuration_package := Some(get_bool v));
  ( "print-result-full"                       , fun v -> Settings.verbose_gen_configuration_full    := Some(get_bool v))

]


let map = Data_common.MapString.map_of_associated_list assign_values_to_settings_functions

let manage_element ident value = 
  try 
    let ident = String.lowercase ident in
    let assign_value_to_setting_function = Data_common.MapString.find ident map 
    in
    Printf.printf "setting %-45s := %s\n" ident (string_of_value value);
    assign_value_to_setting_function value
  with
  | Not_found   -> Zephyrus_log.log_input_settings_unknown_setting ident
  | Wrong_value -> Zephyrus_log.log_input_settings_wrong_value     ident

%}

%token Equals Left_bracket Right_bracket Left_paren Right_paren Comma Semicolon EOF
%token<bool> Bool
%token<string> Ident
%token<int> Int


%start main
%type<unit> main


%%

main:
  | element main { () }
  | EOF          { () }

element:
  | Ident Equals value { manage_element $1 $3 }

value:
  | Bool                                      { BoolValue($1) }
  | Int                                       { IntValue($1) }
  | Ident                                     { IdentValue($1) }
  | Left_paren value Comma value Right_paren  { PairValue($2,$4) }
  | Left_bracket Right_bracket                { ListValue([]) }
  | Left_bracket list Right_bracket           { ListValue($2) }

list:
  | value                { [$1] }
  | value Semicolon list { $1 :: $3 }


