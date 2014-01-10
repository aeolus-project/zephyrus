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


(* for now, keep the same semantics as before *)
(* TODO: change everything *)

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

let load_file file = Pervasives.ignore (Input_helper.parse_standard Settings_parser.main Settings_lexer.token file)

let default_settings_file_path = "default.settings"

let repository_names = ref []
let repository_files = ref []

let out_kinds = ref []
let out_files = ref []


let speclist = 
  Arg.align [
    (* Input arguments *)
    ("-settings", Arg.String (fun filename -> load_file filename), " The settings file (you can use this option more than once, subsequent setting files will be loaded in the given order).");
    ("-u",        Arg.String (fun filename -> Settings.add_string Settings.input_file_universe      filename), " The universe input file.");
    ("-ic",       Arg.String (fun filename -> Settings.add_string Settings.input_file_configuration filename), " The initial configuration input file.");
    ("-spec",     Arg.String (fun filename -> Settings.add_string Settings.input_file_specification filename), " The specification input file.");
    ("-repo",     Arg.Tuple (
                     [Arg.String (fun repository_name -> repository_names := repository_name::!repository_names);
                      Arg.String (fun repository_file -> repository_files := repository_file::!repository_files) ]
                    ), " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories).");

    ("-prefix-repos", Arg.Unit (Settings.enable_package_name_extension), " Prefix all package names in imported repositories by the repository name.");
    ("-mode",         Arg.Symbol ( Settings.mode_names, Settings.add_string Settings.mode), " The functioning mode" (* ^ ": \n\"classic\" generates the final configuration normally, \n\"validate\" validates the initial one, \n\"no-solving\" uses the initial configuration directly as the final one" *) ^ ".");

    (* Optimization function argument, solver choice *)
    ("-opt",    Arg.Symbol ( Settings.optim_names,  Settings.add_string Settings.input_optimization_function), " The optimization function.");
    ("-solver", Arg.Symbol ( Settings.solver_names, Settings.add_string Settings.solver),                      " The solver choice."); 
    ("-custom-solver-command",  Arg.String (fun custom_solver_command  -> Settings.add_string Settings.custom_solver_command custom_solver_command),   " The custom solver command" (* ^ "(example: \"flatzinc -o <OUT> <IN>\", where <IN>/<OUT> will be replaced by the input/output file path before execution), used only if the custom solver option is chosen" *) ^ ".");
    ("-custom-fzn2mzn-command", Arg.String (fun custom_mzn2fzn_command -> Settings.add_string Settings.custom_mzn2fzn_command custom_mzn2fzn_command), " The custom mzn2fzn converter command" (* ^ "(example: \"mzn2fzn -o <OUT> <IN>\", where <IN>/<OUT> will be replaced by the input/output file path before execution), used only if the custom solver option is chosen" *) ^ ".");

    (* Output arguments *)
    ("-out",        Arg.Tuple (
                      [Arg.Symbol ( Settings.out_files_names,  (fun s -> out_kinds := s::!out_kinds));
                       Arg.String (fun filename -> out_files := filename::!out_files) ]
                    ), " The final configuration output file and the output format (you can specify multiple output files with different formats).");

    (* Other *)
    ("-print-path", Arg.Unit ( fun () -> Unix.system "echo $PATH"; exit 0 ), " Print the $PATH variable and exit.");
  ]

open Settings

let load () = 

  (* 1. Load the default settings if they are declared. *)
  begin
    if Sys.file_exists default_settings_file_path
    then load_file default_settings_file_path
  end;

  (* 2. Handle directly command line settings. *)
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;

  (* 3. Post-treatment of settings. *)
  Settings.add_double_lists Settings.input_file_repositories !repository_names !repository_files; (* Additional repositories. *)
  Settings.add_double_lists Settings.results                 !out_kinds        !out_files;        (* Outputs. *)

  Zephyrus_log.log_settings (); (* Print settings as they are now. *)

  (* add missing import settings *)
  (if not (Settings.mem Settings.import_universe)              then Settings.add Settings.import_universe              (Settings.BoolValue true));
  (if not (Settings.mem Settings.import_repositories)          then Settings.add Settings.import_repositories          (Settings.BoolValue true));
  (if not (Settings.mem Settings.import_initial_configuration) then Settings.add Settings.import_initial_configuration (Settings.BoolValue true));
  (if not (Settings.mem Settings.import_specification)         then Settings.add Settings.import_specification         (Settings.BoolValue true));
  (if not (Settings.mem Settings.import_optimization_function) then Settings.add Settings.import_optimization_function (Settings.BoolValue true));

  (* easy fix for when we don't input a configuration *)
  (if 
    (not (Settings.find Settings.import_initial_configuration)) || 
    (not (Settings.mem Settings.input_file_configuration)) 
    then ( 
      Settings.add Settings.import_initial_configuration (Settings.BoolValue true);
      Settings.add Settings.input_file_configuration (Settings.IdentValue "default/ic-ex-empty-100loc.json");
      Settings.add Settings.modifiable_configuration (Settings.BoolValue true)
  ) else ( Settings.add Settings.modifiable_configuration (Settings.BoolValue false) ))

let check_settings () = (* TODO : what to do? *) ()



