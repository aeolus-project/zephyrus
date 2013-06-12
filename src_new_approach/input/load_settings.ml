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
 - Arg (from ocaml standard library)
 - Lexing (from ocaml standard library)
 - input/Settings
 - input/Settings_parser
 - input/Setgings_lexer
*)


(* for now, keep the same semantics as before *)
(* TODO: change everything *)

let usage =  "usage "
    ^ Sys.argv.(0)
    ^ "[-u universe-file]"
    ^ "[-ic initial-configuration-file]"
    ^ "[-spec specification-file]"
    ^ "[-repo repository-name packages-file]*" 
    ^ "[-opt optimization-function]"
    ^ "[-solver solver]"
    ^ "[-out output-format output-file]*"

open Settings

let set_optimization_function str = (* conversion to datatype *)
  input_optimization_function := Some(optimization_function_of_string str)

let set_solver str = (* conversion to datatype *)
  constraint_solver_classic := Some true;
  constraint_solver_classic_kind := Some(solver_kind_of_string str)

let add_new_repository name file = input_file_repositories := (name, file)::(!input_file_repositories)

let enable_package_name_extended _ = output_package_name_extended := Some true

let add_new_output kind file = output_file := ( output_file_kind_of_string kind, file)::(!output_file)

let setting_of_string  str = let lexbuf = Lexing.from_string str in Settings_parser.main Settings_lexer.token lexbuf


let speclist = 
  let tmp_repo_name = ref "" in (* for setting the list of repositories *)
  let tmp_output_kind = ref "" in (* for setting the list of repositories *)
  Arg.align [
    (* Input arguments *)
    ("-u",          Arg.String (fun filename -> input_file_universe              := Some(filename)), " The universe input file");
    ("-ic",         Arg.String (fun filename -> input_file_initial_configuration := Some(filename)), " The initial configuration input file");
    ("-spec",       Arg.String (fun filename -> input_file_specification         := Some(filename)), " The specification input file");
    ("-raw-spec",   Arg.Unit (fun _ -> ()),                                         " The specification is given directly in JSON, not using the nice syntax");
    
    ("-repo",       Arg.Tuple ([
                       Arg.String (fun repository_name -> tmp_repo_name := repository_name);
                       Arg.String (fun repository_file -> add_new_repository !tmp_repo_name repository_file)]),
                    " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories)");

    ("-prefix-repos", Arg.Unit (enable_package_name_extended), " Prefix all package names in imported repositories by the repository name.");

    (* Optimization function argument, solver choice *)
    ("-opt",        Arg.Symbol ( ["simple"; "compact"; "conservative"; "spread"; "none"], (fun s -> set_optimization_function s) ), " The optimization function");
    ("-solver",     Arg.Symbol ( ["facile"; "g12"; "gecode"],                             (fun s -> set_solver s) ), " The solver choice"); 

    (* Output arguments *)
    ("-out",        Arg.Tuple ([
                      Arg.Symbol ( ["plain"; "json"; "graph"; "deployment-graph"; 
                                     "simplified-deployment-graph"; 
                                     "components-graph"; "packages-graph"], 
                                     (fun s -> tmp_output_kind := s));
                      Arg.String (fun filename -> add_new_output !tmp_output_kind filename)]
                    ),
                    " The final configuration output file and the output format (you can specify multiple output files with different formats)."
                  ^ " Output formats available: {plain|json|graph|deployment-graph|simplified-deployment-graph|components-graph|packages-graph}");

    ("-only-check-spec", Arg.Unit (fun _ -> ()),  " Deprecated and has no effect. If you only want to check the spec, don't give other inputs to zephyrus");
    ("-do-not-solve",    Arg.Unit (fun _ -> ()),     " Do not use the solver (exit directly after generating generic constraints)");

    (* Printing options arguments *)
    ("-print-u",             Arg.Unit (fun _ -> ()),                      " Print the raw universe");
    ("-print-tu",            Arg.Unit (fun _ -> ()),                     " Print the trimmed universe");
    ("-print-ic",            Arg.Unit (fun _ -> ()),                     " Print the raw initial configuration");
    ("-print-spec",          Arg.Unit (fun _ -> ()),                   " Print the raw specification");
    ("-print-cstrs",         Arg.Unit (fun _ -> ()),                  " Print the constraints");
    ("-print-solver-vars",   Arg.Unit (fun _ -> ()),            " Print the solver specific variables");
    ("-print-solver-cstrs",  Arg.Unit (fun _ -> ()),           " Print the solver specific constraints");
    ("-print-solver-exe",    Arg.Unit (fun _ -> ()),             " Print the solver execution details");
    ("-print-all-solutions", Arg.Unit (fun _ -> ()), " Print all the intermediate solutions found");
    ("-print-solution",      Arg.Unit (fun _ -> ()),               " Print the final solution");
    ("-print-all",           Arg.Unit (fun _ -> ()),                    " Print everything");
  ]



let load_settings _ = Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage

