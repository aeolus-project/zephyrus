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

let zephyrus_mode : mode option ref = ref None (* NotUsedYet *)

(* 01. Input Files *)

let input_file_universe              : string option ref          = ref None (* the main file of the universe *)
let input_file_repositories          : (string * string) list ref = ref []   (* the file for the repositories, with their names *)
let input_file_initial_configuration : string option ref          = ref None (* initial configuration *)
let input_file_specification         : string option ref          = ref None (* specification file *)
let input_optimization_function      : optim option ref           = ref None (* Optimization function *)

let input_file_settings              : string option ref          = ref None  (* NotUsedYet *)

(* TODO: Add options for the syntax of files ? *)


(* 02. Which initial Data to Generate *)

let data_generation_universe              : bool option ref = ref None
let data_generation_repositories          : bool option ref = ref None
let data_generation_initial_configuration : bool option ref = ref None
let data_generation_specification         : bool option ref = ref None
let data_generation_optimization_function : bool option ref = ref None

let data_package_name_extended : bool option ref = ref None

let data_check_universe              : bool option ref = ref None (* NotUsedYet *)
let data_check_repositories          : bool option ref = ref None (* NotUsedYet *)
let data_check_initial_configuration : bool option ref = ref None (* NotUsedYet *)
let data_check_universe_full         : bool option ref = ref None (* NotUsedYet *)
let data_check_specification         : bool option ref = ref None (* NotUsedYet *)

let data_check_settings : bool option ref = ref None (* NotUsedYet *)


(* 03. Pre Processing *)

let pre_process_spec_wf_detection           : bool option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection        : bool option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection_solver : solver option ref = ref None (* NotUsedYet *)

let pre_process_universe_loop_detection     : bool option ref = ref None (* NotUsedYet *)
let pre_process_universe_bound_computation  : bool option ref = ref None (* NotUsedYet *)

let pre_process_package_coinst              : bool option ref = ref None (* NotUsedYet *)
let pre_process_package_trim_package        : bool option ref = ref None (* NotUsedYet *)
let pre_process_component_types_trim        : bool option ref = ref None (* NotUsedYet *)


(* 04. Constraint Solver *)

let constraint_weight_locations        : int option ref = ref None (* NotUsedYet *)
let constraint_weight_component_types  : int option ref = ref None (* NotUsedYet *)
let constraint_weight_packages         : int option ref = ref None (* NotUsedYet *)

let constraint_solver_classic_linear   : bool option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_kind     : solver option ref = ref None (* NotUsedYet *)

let constraint_solver_bin_packing_kind : solver_bin_packing option ref = ref None (* NotUsedYet *)


(* 05. Temporary Files *)

let pre_process_spec_empty_detection_input_file       : string option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection_output_file      : string option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection_input_file_keep  : bool option ref = ref None (* NotUsedYet *)
let pre_process_spec_empty_detection_output_file_keep : bool option ref = ref None (* NotUsedYet *)

let constraint_solver_flat_input_file       : string option ref = ref None (* NotUsedYet *)
let constraint_solver_flat_output_file      : string option ref = ref None (* NotUsedYet *)
let constraint_solver_flat_input_file_keep  : bool option ref = ref None (* NotUsedYet *)
let constraint_solver_flat_output_file_keep : bool option ref = ref None (* NotUsedYet *)

let constraint_solver_classic_input_file       : string option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_output_file      : string option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_input_file_keep  : bool option ref = ref None (* NotUsedYet *)
let constraint_solver_classic_output_file_keep : bool option ref = ref None (* NotUsedYet *)


(* 06. Configuration Generation *)

let configuration_generation_bindings : conf_gen_bindings option ref = ref None (* NotUsedYet *)
let configuration_generation_packages : conf_gen_packages option ref = ref None (* NotUsedYet *)


(* 07. Output Configuration *)

let output_file                  : (out_file * string) list ref = ref [] (* NotUsedYet *)


(* 08. Verbose Options *)

let verbose_stage : bool option ref = ref None (* NotUsedYet *)

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

(* 08.2. pre-process *)

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


let get_bool_basic r = match !r with | None -> false | Some(b) -> b
let get_weight w = match !w with | None -> 1 | Some(i) -> i




(* PRINTING *)

type setting =
  | StringSetting               of string option
  | BoolSetting                 of bool option
  | IntSetting                  of int option
  | FileRepositoriesSetting     of (string * string) list
  | ModeSetting                 of mode option
  | OptimizationFunctionSetting of optim option
  | SolverKindSetting           of solver option
  | SolverBinPackingKindSetting of solver_bin_packing option
  | ConfGenBindingsSetting      of conf_gen_bindings option
  | ConfGenPackagesSetting      of conf_gen_packages option
  | OutputFileSetting           of (out_file * string) list

let string_of_mode = function
  | Mode_classic     -> "classic"
  | Mode_flat        -> "flat"
  | Mode_bin_packing -> "bin_packing"

let string_of_optim = function     
  | Optim_none         -> "none"
  | Optim_simple       -> "simple"
  | Optim_compact      -> "compact"
  | Optim_spread       -> "spread"
  | Optim_conservative -> "conservative"

let string_of_solver = function
  | Solver_none   -> "none"
  | Solver_gcode  -> "gcode"
  | Solver_g12    -> "g12"
  | Solver_facile -> "facile"

let string_of_solver_bin_packing = function
  | Solver_bin_packing_unknown -> "Solver_bin_packing_unknown"

let string_of_conf_gen_bindings = function
  | Conf_gen_bindings_none       -> "Conf_gen_bindings_none"
  | Conf_gen_bindings_candy      -> "Conf_gen_bindings_candy"
  | Conf_gen_bindings_constraint -> "Conf_gen_bindings_constraint"

let string_of_conf_gen_packages = function
  | Conf_gen_packages_none     -> "Conf_gen_packages_none"
  | Conf_gen_packages_universe -> "Conf_gen_packages_universe"

let string_of_out_file = function
  | Out_file_plain            -> "Out_file_plain"
  | Out_file_json             -> "Out_file_json"
  | Out_file_graph_deployment -> "Out_file_graph_deployment"
  | Out_file_graph_simplified -> "Out_file_graph_simplified"
  | Out_file_graph_components -> "Out_file_graph_components"
  | Out_file_graph_packages   -> "Out_file_graph_packages"

let string_of_file_repositories (file_repositories : (string * string) list) =
  let repositories_strings =
    List.map (fun (repository_name, file) ->
      Printf.sprintf "%s : \"%s\"" repository_name file
    ) file_repositories
  in
  Printf.sprintf "[%s]" (String.concat ", " repositories_strings)

let string_of_out_files (out_files : (out_file * string) list) =
  let out_files_strings =
    List.map (fun (out_file, file) ->
      Printf.sprintf "%s : \"%s\"" (string_of_out_file out_file) file
    ) out_files
  in
  Printf.sprintf "[%s]" (String.concat ", " out_files_strings)

let string_of_setting = 
  let none_or x string_of_x = 
    match x with
    | None   -> "[None]"
    | Some x -> string_of_x x
  in
  function
  | StringSetting               x -> none_or x (fun s -> Printf.sprintf "\"%s\"" s)
  | BoolSetting                 x -> none_or x (fun b -> if b then "true" else "false")
  | IntSetting                  x -> none_or x (fun i -> Printf.sprintf "\"%d\"" i)
  | FileRepositoriesSetting     l -> string_of_file_repositories l
  | ModeSetting                 x -> none_or x string_of_mode
  | OptimizationFunctionSetting x -> none_or x string_of_optim
  | SolverKindSetting           x -> none_or x string_of_solver
  | SolverBinPackingKindSetting x -> none_or x string_of_solver_bin_packing
  | ConfGenBindingsSetting      x -> none_or x string_of_conf_gen_bindings
  | ConfGenPackagesSetting      x -> none_or x string_of_conf_gen_packages
  | OutputFileSetting           l -> string_of_out_files l

let settings_printing_functions : (string * (unit -> setting)) list = [
  ("zephyrus-mode"                , fun () -> ModeSetting(!zephyrus_mode));

(* 01. Input Files *)
  ("input-file-universe"          , fun () -> StringSetting(!input_file_universe)                      );
  ("input-file-configuration"     , fun () -> StringSetting(!input_file_initial_configuration)         );
  ("input-file-specification"     , fun () -> StringSetting(!input_file_specification)                 );
  ("input-file-repositories"      , fun () -> FileRepositoriesSetting(!input_file_repositories)        );
  ("input-optimization-function"  , fun () -> OptimizationFunctionSetting(!input_optimization_function));

(* 02. Which initial Data to Generate *)
  ("import-universe"              , fun () -> BoolSetting(!data_generation_universe)             );
  ("import-repositories"          , fun () -> BoolSetting(!data_generation_repositories)         );
  ("import-initial-configuration" , fun () -> BoolSetting(!data_generation_initial_configuration));
  ("import-specification"         , fun () -> BoolSetting(!data_generation_specification)        );
  ("import-optimization-function" , fun () -> BoolSetting(!data_generation_optimization_function));

  ("check-universe"               , fun () -> BoolSetting(!data_check_universe)             );
  ("check-repositories"           , fun () -> BoolSetting(!data_check_repositories)         );
  ("check-initial-configuration"  , fun () -> BoolSetting(!data_check_initial_configuration));
  ("check-universe_full"          , fun () -> BoolSetting(!data_check_universe_full)        );
  ("check-specification"          , fun () -> BoolSetting(!data_check_specification)        );

  ("check-settings"               , fun () -> BoolSetting(!data_check_settings));

(* 03. Pre Processing *)

  ( "detect-spec-well-formedness"      , fun () -> BoolSetting(!pre_process_spec_wf_detection)                );
  ( "detect-spec-is-empty"             , fun () -> BoolSetting(!pre_process_spec_empty_detection)             );
  ( "detect-spec-solver"               , fun () -> SolverKindSetting(!pre_process_spec_empty_detection_solver));

  ( "detect-component-types-have-loop" , fun () -> BoolSetting(!pre_process_universe_loop_detection)   );
  ( "detect-component-types-bounds"    , fun () -> BoolSetting(!pre_process_universe_bound_computation));

  ( "package-coinst"                   , fun () -> BoolSetting(!pre_process_package_coinst)      );
  ( "package-trim"                     , fun () -> BoolSetting(!pre_process_package_trim_package));
  ( "component-types-trim"             , fun () -> BoolSetting(!pre_process_component_types_trim));


(* 04. Constraint Solver *)
  ( "weight-locations"             , fun () -> IntSetting(!constraint_weight_locations)      );
  ( "weight-component-types"       , fun () -> IntSetting(!constraint_weight_component_types));
  ( "weight-packages"              , fun () -> IntSetting(!constraint_weight_packages)       );

  ( "solver-use-linear-constraint" , fun () -> BoolSetting                (!constraint_solver_classic_linear)  );
  ( "solver"                       , fun () -> SolverKindSetting          (!constraint_solver_classic_kind)    );
  ( "solver-bin-packing"           , fun () -> SolverBinPackingKindSetting(!constraint_solver_bin_packing_kind));


(* 05. Temporary Files *)

  ( "detect-spec-is-empty-constraint-file"      , fun () -> StringSetting(!pre_process_spec_empty_detection_input_file)    );
  ( "detect-spec-is-empty-solution-file"        , fun () -> StringSetting(!pre_process_spec_empty_detection_output_file)   );
  ( "detect-spec-is-empty-keep-constraint-file" , fun () -> BoolSetting(!pre_process_spec_empty_detection_input_file_keep) );
  ( "detect-spec-is-empty-keep-solution-file"   , fun () -> BoolSetting(!pre_process_spec_empty_detection_output_file_keep));

  ( "solver-flat-constraint-file"               , fun () -> StringSetting(!constraint_solver_flat_input_file)    );
  ( "solver-flat-solution-file"                 , fun () -> StringSetting(!constraint_solver_flat_output_file)   );
  ( "solver-flat-keep-constraint-file"          , fun () -> BoolSetting(!constraint_solver_flat_input_file_keep) );
  ( "solver-flat-keep-solution-file"            , fun () -> BoolSetting(!constraint_solver_flat_output_file_keep));

  ( "solver-constraint-file"                    , fun () -> StringSetting(!constraint_solver_classic_input_file)    );
  ( "solver-solution-file"                      , fun () -> StringSetting(!constraint_solver_classic_output_file)   );
  ( "solver-keep-constraint-file"               , fun () -> BoolSetting(!constraint_solver_classic_input_file_keep) );
  ( "solver-keep-solution-file"                 , fun () -> BoolSetting(!constraint_solver_classic_output_file_keep));


(* 06. Configuration Generation *)

  ( "generate-bindings" , fun () -> ConfGenBindingsSetting(!configuration_generation_bindings));
  ( "generate-packages" , fun () -> ConfGenPackagesSetting(!configuration_generation_packages));


(* 07. Output Configuration *)

  ( "results"                           , fun () -> OutputFileSetting(!output_file)         );
  ( "append-repository-to-package-name" , fun () -> BoolSetting(!data_package_name_extended));


(* 08. Verbose Options *)

  ( "verbose-stage" , fun () -> BoolSetting(!verbose_stage));

(* settings *)

  ( "verbose-settings-check-warning"         , fun () -> BoolSetting(!verbose_settings_check_warning));
  ( "verbose-settings-check-error"           , fun () -> BoolSetting(!verbose_settings_check_error)  );
  ( "verbose-settings-check-setting-non-set" , fun () -> BoolSetting(!verbose_settings_non_set)      );

(* 08.1. inputs *)

  ( "verbose-input-warning"                 , fun () -> BoolSetting(!verbose_input_warning));
  ( "verbose-input-error"                   , fun () -> BoolSetting(!verbose_input_error)  );

  ( "print-input-universe"                  , fun () -> BoolSetting(!verbose_input_universe)             );
  ( "print-input-repositories"              , fun () -> BoolSetting(!verbose_input_repositories)         );
  ( "print-full-universe"                   , fun () -> BoolSetting(!verbose_input_universe_full)        );
  ( "print-initial-configuration"           , fun () -> BoolSetting(!verbose_input_initial_configuration));
  ( "print-specification"                   , fun () -> BoolSetting(!verbose_input_specification)        );
  ( "print-optimization-function"           , fun () -> BoolSetting(!verbose_input_optimization_function));

  ( "extra-log-universe-check"              , fun () -> BoolSetting(!verbose_input_universe_check)             );
  ( "extra-log-repositories-check"          , fun () -> BoolSetting(!verbose_input_repositories_check)         );
  ( "extra-log-initial-configuration-check" , fun () -> BoolSetting(!verbose_input_initial_configuration_check));
  ( "extra-log-specification-check"         , fun () -> BoolSetting(!verbose_input_specification_check)        );

(* 08.2. pre-processs *)

  ( "print-is-spec-well-formed"          , fun () -> BoolSetting(!verbose_spec_fw_detection)                       );
  ( "print-is-spec-empty"                , fun () -> BoolSetting(!verbose_spec_empty_detection)                    );
  ( "extra-log-is-spec-well-formed"      , fun () -> BoolSetting(!verbose_spec_fw_detection_activities)            );
  ( "extra-log-is-spec-empty"            , fun () -> BoolSetting(!verbose_spec_empty_detection_activities)         );

  ( "print-universe-graph"               , fun () -> BoolSetting(!verbose_universe_graph)                          );
  ( "print-has-component-types-loop"     , fun () -> BoolSetting(!verbose_universe_loop_detection)                 );
  ( "print-component-types-bounds"       , fun () -> BoolSetting(!verbose_universe_bound_detection)                );
  ( "extra-log-has-component-types-loop" , fun () -> BoolSetting(!verbose_universe_loop_detection_activities)      );
  ( "extra-log-component-types-bounds"   , fun () -> BoolSetting(!verbose_universe_bound_detection_activities)     );

  ( "print-packages-trimmed"             , fun () -> BoolSetting(!verbose_universe_package_trim_package)           );
  ( "print-component-types-trimmed"      , fun () -> BoolSetting(!verbose_universe_component_types_trim)           );
  ( "extra-log-packages-trimmed"         , fun () -> BoolSetting(!verbose_universe_package_trim_package_activities));
  ( "extra-log-component-types-trimmed"  , fun () -> BoolSetting(!verbose_universe_component_types_trim_activities));

(* 08.3. constraints *)

  ( "print-main-constraint"              , fun () -> BoolSetting(!verbose_constraint)                  );
  ( "print-main-solution"                , fun () -> BoolSetting(!verbose_constraint_solver_activities));
  ( "extra-log-main-constraint-solving"  , fun () -> BoolSetting(!verbose_solution)                    );

(* 08.4. configuration generation *)

  ( "print-result-location+components"        , fun () -> BoolSetting(!verbose_gen_configuration_init)   );
  ( "print-result-location+components+pakage" , fun () -> BoolSetting(!verbose_gen_configuration_package));
  ( "print-result-full"                       , fun () -> BoolSetting(!verbose_gen_configuration_full)   )

]

let string_of_settings () =
  let settings_strings =
    List.map (fun (setting_name, get_setting_function) ->
      let setting = get_setting_function () in
      Printf.sprintf "%-45s = %s\n" setting_name (string_of_setting setting)
    ) settings_printing_functions
  in
  String.concat "" settings_strings