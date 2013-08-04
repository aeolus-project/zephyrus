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

let usage = "usage: " ^ Sys.argv.(0)
  ^ "[-s settings-file]"
  ^ "[-u universe-file]"
  ^ "[-ic initial-configuration-file]"
  ^ "[-spec specification-file]"
  ^ "[-repo repository-name packages-file]*" 
  ^ "[-opt optimization-function]"
  ^ "[-solver solver]"
  ^ "[-out output-format output-file]*"

let load_file file = Pervasives.ignore (Input_helper.parse_standard Settings_parser.main Settings_lexer.token file)


let repository_names = ref []
let repository_files = ref []

let out_kinds = ref []
let out_files = ref []


let speclist = 
  Arg.align [
    (* Input arguments *)
    ("-settings", Arg.String (fun filename -> Settings.add_settings_file filename; load_file filename), " The settings file");
    ("-u",        Arg.String (fun filename -> Settings.set_universe_input_file filename), " The universe input file");
    ("-ic",       Arg.String (fun filename -> Settings.set_input_configuration_file filename), " The initial configuration input file");
    ("-spec",     Arg.String (fun filename -> Settings.set_specification_file filename), " The specification input file");
    ("-repo",     Arg.Tuple (
                     [Arg.String (fun repository_name -> repository_names := repository_name::!repository_names);
                      Arg.String (fun repository_file -> repository_files := repository_file::!repository_files) ]
                    ), " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories)");

    ("-prefix-repos", Arg.Unit (Settings.extend_package_name_with_repository), " Prefix all package names in imported repositories by the repository name.");

    (* Optimization function argument, solver choice *)
    ("-opt",        Arg.Symbol ( Settings.optim_names, Settings.set_optimization_function  ), " The optimization function");
    ("-solver",     Arg.Symbol ( Settings.solver_names,                Settings.set_constraint_main_solver ), " The solver choice"); 

    (* Output arguments *)
    ("-out",        Arg.Tuple (
                      [Arg.Symbol ( Settings.out_files_names,  (fun s -> out_kinds := s::!out_kinds));
                       Arg.String (fun filename -> out_files := filename::!out_files) ]
                    ), " The final configuration output file and the output format (you can specify multiple output files with different formats).");
  ]


let load () = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
  List.iter2 (fun name file -> Settings.add_external_repository name file) !repository_names !repository_files;
  List.iter2 (fun kind file -> Settings.add_output_file kind file) !out_kinds !out_files

let check_settings () = (* TODO *) ()



