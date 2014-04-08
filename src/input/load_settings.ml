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
 - Sys (from ocaml standard library)
 - Lexing (from ocaml standard library)
 - input/Settings_parser
 - input/Settings_lexer
*)

let usage = 
    "usage: " ^ Sys.argv.(0)
  ^ " [-settings settings-file]"
  ^ " [-u universe-file]"
  ^ " [-ic initial-configuration-file]"
  ^ " [-spec specification-file]"
  ^ " [-repo repository-name packages-file]*" 
  ^ " [-opt optimization-function]"
  ^ " [-solver solver]"
  ^ " [-out output-format output-file]*"

(* Load a settings file *)
let load_settings_file file = 
  ignore (Input_helper.parse_standard Settings_parser.main Settings_lexer.token file)

(* Default settings *)
let default_settings_file_path = "default.settings"

(* Repositories to import *)
let repository_names = ref []
let repository_files = ref []

(* Outputs *)
let output_types = ref []
let output_files = ref []

(* Benchmarks *)
let benchmark_choice        = ref None
let benchmark_option_keys   = ref []
let benchmark_option_values = ref []


(* Mechanism simplifying on/off switch options. *)

(* Available switch option strings. *)
let on_off_switch_symbols = ["on"; "off"]

(* Convert a on/off switch option string (i.e. "on" or "off") to a boolean value. *)
let bool_of_on_off_switch_symbol s = match s with
  | "on"  -> true
  | "off" -> false
  | _     -> failwith (Printf.sprintf "Unrecognised on/off switch option value \"%s\"!" s)

(* Set a given setting to true or false using the given on/off switch option string. *)
let switch_on_off_setting setting (s : string) =
  Settings.add setting (Settings.bool_value_of_bool (bool_of_on_off_switch_symbol s))

(* Create an Arg.Symbol on/off switch for a given setting. *)
let arg_on_off_switch setting =
  Arg.Symbol (on_off_switch_symbols, (switch_on_off_setting setting))


(* Specification of command line options  *)

let speclist = 
  Arg.align [

    (* Input arguments *)
    ("-settings", Arg.String (fun filename -> load_settings_file filename), " The settings file (you can use this option more than once, subsequent setting files will be loaded in the given order).");
    ("-u",        Arg.String (fun filename -> Settings.add_string Settings.input_file_universe      filename), " The universe input file.");
    ("-ic",       Arg.String (fun filename -> Settings.add_string Settings.input_file_configuration filename), " The initial configuration input file.");
    ("-spec",     Arg.String (fun filename -> Settings.add_string Settings.input_file_specification filename), " The specification input file.");
    ("-repo",     
      Arg.Tuple ([
        Arg.String (fun repository_name -> repository_names := repository_name::!repository_names);
        Arg.String (fun repository_file -> repository_files := repository_file::!repository_files) 
      ]), " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories).");

    (* Benchmarks *)
    ("-benchmark", Arg.Symbol (Settings.benchmark_choice_names, (fun benchmark_choice_name -> benchmark_choice := Some (List.assoc benchmark_choice_name Settings.benchmark_choice_assoc))), " The benchmark choice.");
    ("-benchmark-option", 
      Arg.Tuple ([
        Arg.String (fun benchmark_option_key   -> benchmark_option_keys   := benchmark_option_key::!benchmark_option_keys);
        Arg.String (fun benchmark_option_value -> benchmark_option_values := benchmark_option_value::!benchmark_option_values)
      ]), " Specify a benchmark option: (key, value).");

    (* Solving options *)
    ("-prefix-repos", Arg.Unit   (Settings.enable_package_name_extension), " Prefix all package names in imported repositories by the repository name.");
    ("-mode",         Arg.Symbol (Settings.mode_names, Settings.add_string Settings.mode), " The functioning mode" (* ^ ": \n\"classic\" generates the final configuration normally, \n\"validate\" validates the initial one, \n\"no-solving\" uses the initial configuration directly as the final one" *) ^ ".");
    
    (* Preprocessing options *)
    ("-no-packages",                     arg_on_off_switch Settings.eliminate_packages,          " Eliminate the packages from solving, use component incompatibilities instead (default: off).");
    ("-use-all-locations",               arg_on_off_switch Settings.no_location_trimming,        " Do not try to reduce the number of locations during the preprocessing (default: off).");
    ("-use-ralfs-redundant-constraints", arg_on_off_switch Settings.ralfs_redundant_constraints, " Generate Ralf's redundant constraints (default: off)." );

    (* Optimization function argument, solver choice *)
    ("-opt",    Arg.Symbol (Settings.optimization_function_names, Settings.add_string Settings.input_optimization_function), " The optimization function.");
    ("-solver", Arg.Symbol (Settings.solver_names,                Settings.add_string Settings.solver),                      " The solver choice."); 
    ("-custom-solver-command",  Arg.String (fun custom_solver_command  -> Settings.add_string Settings.custom_solver_command custom_solver_command),   " The custom solver command" (* ^ "(example: \"flatzinc -o <OUT> <IN>\", where <IN>/<OUT> will be replaced by the input/output file path before execution), used only if the custom solver option is chosen" *) ^ ".");
    ("-custom-fzn2mzn-command", Arg.String (fun custom_mzn2fzn_command -> Settings.add_string Settings.custom_mzn2fzn_command custom_mzn2fzn_command), " The custom mzn2fzn converter command" (* ^ "(example: \"mzn2fzn -o <OUT> <IN>\", where <IN>/<OUT> will be replaced by the input/output file path before execution), used only if the custom solver option is chosen" *) ^ ".");

    (* Output arguments *)
    ("-out",
      Arg.Tuple ([
        Arg.Symbol ( Settings.output_files_names,  (fun s -> output_types := s::!output_types));
        Arg.String (fun filename -> output_files := filename::!output_files)
      ]), " The final configuration output file and the output format (you can specify multiple output files with different formats).");

    (* Other *)
    ("-print-path",         Arg.Unit ( fun () -> Unix.system "echo $PATH"; ignore (exit 0) ), " Print the $PATH variable and exit.");
    ("-stop-after-solving", Arg.Unit (Settings.enable_stop_after_solving),                    " Do not generate the final configuration, exit directly after the solving phase is over (useful for benchmarking).");
  ]

open Settings

let load () = 

  (* 1. Load settings from the default settings file (if it exists). *)
  begin
    if Sys.file_exists default_settings_file_path
    then load_settings_file default_settings_file_path
  end;


  (* 2. Handle command line settings. *)

  (* Handle the command line *)
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;

  (* Post-treatment of command line settings: *)

  (* 2.1. Additional repositories. *)
  Settings.add_double_lists Settings.input_file_repositories !repository_names !repository_files;

  (* 2.2. Outputs. *)
  Settings.add_double_lists Settings.results !output_types !output_files;

  (* 2.3. Benchmarks. *)
  (match !benchmark_choice with
  | Some benchmark_choice -> 
      let benchmark_options = List.combine !benchmark_option_keys !benchmark_option_values in
      let benchmark : Settings.benchmark = (benchmark_choice, benchmark_options) in
      Settings.add_benchmark benchmark
  | None -> ());

  Zephyrus_log.log_settings () (* Print settings as they are now. *)


let check_settings () = (* TODO : what to do? *) ()
