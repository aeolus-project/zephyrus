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

(* 
  "Aeolus, the Keeper of the Winds, gives Odysseus a tightly closed bag
   full of the captured winds so he could sail easily home to Ithaca on
   the gentle west wind."

   Zephyrus : one of the Anemoi and the Greek god of the west wind.
*)

open Aeolus_types_t

open Helpers

open Facile
open Easy

open Typing_context
open Facile_variables
open Facile_constraints
open Constraints_generation
open Solution
open Configuration_generation

open Configuration_output

module TestCompile1 = Aeolus_types_translation
module TestCompile2 = Typing_context_new


(* === Handling the arguments === *)

(* Variables corresponding to arguments *)

(* input / output *)
let universe_channel              = ref stdin
let specification_channel         = ref stdin
let initial_configuration_channel = ref stdin

let output_channels               = ref [stdout]
let output_format_strings         = ref ["plain"]

let optimization_function_string  = ref "simple"
let solver_choice_string          = ref "g12"

let import_repository_names       = ref []
let import_repository_filenames   = ref []
let prefix_repositories           = ref false

let raw_specification             = ref false
let only_check_spec               = ref false

let do_not_solve                  = ref false

(* printing settings *)
let print_u                       = ref false
let print_tu                      = ref false
let print_ic                      = ref false
let print_spec                    = ref false
let print_cstrs                   = ref false
let print_solver_vars             = ref false
let print_solver_cstrs            = ref false
let print_solver_exe              = ref false
let print_intermediate_solutions  = ref false
let print_solution                = ref false
let print_all                     = ref false

(* Arg module settings *)

let usage = 
  Printf.sprintf
    "usage: %s %s %s %s %s %s %s %s"
    Sys.argv.(0)
    "[-u universe-file]"
    "[-ic initial-configuration-file]"
    "[-spec specification-file]"
    "[-repo repository-name packages-file]*" 
    "[-opt optimization-function]"
    "[-solver solver]"
    "[-out output-format output-file]*"

let speclist = 
  Arg.align [
    (* Input arguments *)
    ("-u",          Arg.String (fun filename -> universe_channel              := (open_in filename)), " The universe input file");
    ("-ic",         Arg.String (fun filename -> initial_configuration_channel := (open_in filename)), " The initial configuration input file");
    ("-spec",       Arg.String (fun filename -> specification_channel         := (open_in filename)), " The specification input file");
    ("-raw-spec",   Arg.Set (raw_specification),                                                      " The specification is given directly in JSON, not using the nice syntax");
    
    ("-repo",       Arg.Tuple 
                    (
                      [Arg.String (fun repository_name     -> import_repository_names     := (!import_repository_names     @ [repository_name]    ));
                       Arg.String (fun repository_filename -> import_repository_filenames := (!import_repository_filenames @ [repository_filename]))]
                    ),
                    " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories)");

    ("-prefix-repos", Arg.Set (prefix_repositories), " Prefix all package names in imported repositories by the repository name.");

    (* Optimization function argument, solver choice *)
    ("-opt",        Arg.Symbol ( ["simple"; "compact"; "conservative"; "spread"; "none"], (fun s -> optimization_function_string := s) ), " The optimization function");
    ("-solver",     Arg.Symbol ( ["facile"; "g12"; "gecode"],                     (fun s -> solver_choice_string         := s) ), " The solver choice"); 

    (* Output arguments *)
    ("-out",        Arg.Tuple
                    (
                      [Arg.Symbol ( ["plain"; 
                                     "json"; 
                                     "graph"; 
                                     "deployment-graph"; 
                                     "simplified-deployment-graph"; 
                                     "components-graph"; 
                                     "packages-graph"], 
                                     (fun s -> output_format_strings := !output_format_strings @ [s]));

                       Arg.String (fun filename -> output_channels := (!output_channels @ [(open_out filename)]))]
                    ),
                    " The final configuration output file and the output format (you can specify multiple output files with different formats). Output formats available: {plain|json|graph|deployment-graph|simplified-deployment-graph|components-graph|packages-graph}");

    ("-only-check-spec", Arg.Set (only_check_spec),  " Just parse specification and exit");
    ("-do-not-solve",    Arg.Set (do_not_solve),     " Do not use the solver (exit directly after generating generic constraints)");

    (* Printing options arguments *)
    ("-print-u",             Arg.Set (print_u),                      " Print the raw universe");
    ("-print-tu",            Arg.Set (print_tu),                     " Print the trimmed universe");
    ("-print-ic",            Arg.Set (print_ic),                     " Print the raw initial configuration");
    ("-print-spec",          Arg.Set (print_spec),                   " Print the raw specification");
    ("-print-cstrs",         Arg.Set (print_cstrs),                  " Print the constraints");
    ("-print-solver-vars",   Arg.Set (print_solver_vars),            " Print the solver specific variables");
    ("-print-solver-cstrs",  Arg.Set (print_solver_cstrs),           " Print the solver specific constraints");
    ("-print-solver-exe",    Arg.Set (print_solver_exe),             " Print the solver execution details");
    ("-print-all-solutions", Arg.Set (print_intermediate_solutions), " Print all the intermediate solutions found");
    ("-print-solution",      Arg.Set (print_solution),               " Print the final solution");
    ("-print-all",           Arg.Set (print_all),                    " Print everything");
  ]

(* Read the arguments *)
let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage

(* Handle the print-all argument. *)
let () =
  if !print_all
  then (
  (*print_u                      := true;*) (* If we have any imported repositories the untrimmed universe gets huge and illisible so we don't print it by default. *)
    print_tu                     := true;
    print_ic                     := true;
    print_spec                   := true;
    print_cstrs                  := true;
    print_solver_vars            := true;
    print_solver_cstrs           := true;
    print_solver_exe             := true;
    print_intermediate_solutions := true;
    print_solution               := true;
  )

(* Handle the output format choice argument. *)
type output_format = Plain_output | JSON_output | Graph_output of graph_type

let output_channels = !output_channels

let output_formats =

  List.map (fun output_format_string ->
    match output_format_string with
    | "plain"                       ->  Plain_output
    | "json"                        ->  JSON_output
    | "graph"                       ->  Graph_output Deployment_graph (* default *)
    | "deployment-graph"            ->  Graph_output Deployment_graph
    | "simplified-deployment-graph" ->  Graph_output Simplified_deployment_graph
    | "components-graph"            ->  Graph_output Components_graph
    | "packages-graph"              ->  Graph_output Packages_graph
    | _ -> failwith "Invalid output format have passed through the Arg.Symbol!"
  ) !output_format_strings

(* Handle the optimization function choice argument. *)
type optimization_function = 
  | Simple_optimization_function
  | Compact_optimization_function
  | Conservative_optimization_function
  | Spread_optimization_function
  | None_optimization_function

let optimization_function =
  match !optimization_function_string with
  | "simple"       -> Simple_optimization_function
  | "compact"      -> Compact_optimization_function
  | "conservative" -> Conservative_optimization_function
  | "spread"       -> Spread_optimization_function
  | "none"         -> None_optimization_function
  | _ -> failwith "Invalid optimization function choice have passed through the Arg.Symbol!"

(* Handle the solver choice argument. *)
type solver_choice = 
  | FaCiLeSolver
  | G12Solver
  | GeCodeSolver

let solver_choice =
  match !solver_choice_string with
  | "facile" -> FaCiLeSolver
  | "g12"    -> G12Solver
  | "gecode" -> GeCodeSolver
  | _ -> failwith "Invalid solver choice have passed through the Arg.Symbol!"



(* === Set up everything === *)


(* Handle the imported repositories. *)

(* Prefix all package names in the repository with the repository name. *)
let prefix_repository repository = 
  let open Aeolus_types_output.Plain 
  in
  let prefix package_name = Printf.sprintf "%s-%s" (string_of_repository_name repository.repository_name) (string_of_package_name package_name)
  in
  {
    repository_name     = repository.repository_name;
    repository_packages = 
      List.map (fun package ->
        {
          package_name     = prefix package.package_name;
          package_depend   = List.map (fun disj -> List.map prefix disj) package.package_depend;
          package_conflict = List.map prefix package.package_conflict;
          package_consume  = package.package_consume;
        }
      )
      repository.repository_packages;
  }

(* Import the repositories. *)
let imported_repositories =
  try
    List.rev (List.map2 (fun repository_name repository_filename -> 
      let open Aeolus_types_output.Plain 
      in
      Printf.printf "Importing repository %s from file %s...\n" (string_of_repository_name repository_name) repository_filename;
      flush stdout;

      let repository_channel = (open_in repository_filename) 
      in
      let packages = 
        Universe_input.JSON.packages_of_string (string_of_input_channel repository_channel)
      in

      (* The imported repository : *)
      let repository =
      {
        repository_name     = repository_name;
        repository_packages = packages;
      }

      in

      (* Return the repository, prefix all packages inside with the repository name if needed. *)
      if !prefix_repositories 
      then prefix_repository repository
      else repository

    ) !import_repository_names !import_repository_filenames)
  with
  Invalid_argument s -> 
    failwith (Printf.sprintf "Number of imported repository names does not match the number of imported repository filenames! %s" s)




(* Read the input. *)

(* Read the universe. *)
let my_universe =
  let universe =
    Universe_input.JSON.universe_of_string (string_of_input_channel !universe_channel)
  in
  (* Append the imported repositories.*)
  {
    universe_component_types = universe.universe_component_types;
    universe_implementation = universe.universe_implementation;
    universe_repositories = universe.universe_repositories @ imported_repositories
  }

(* Read the initial configuration. *)
let my_initial_configuration =
  Configuration_input.JSON.configuration_of_string (string_of_input_channel !initial_configuration_channel)

(* Read the specification. *)
let specification_string = string_of_input_channel !specification_channel

let my_specification =
  if (!raw_specification)
  then
    (* If specification is in JSON format we read it directly through ATD. *)
    Specification_input.JSON.specification_of_string specification_string
  else
    (* If specification is written in Aeolus specifiaction syntax, we parse it using our parser. *)
    (
      let specification = Specification_input.Aeolus_specification_language.specification_of_string specification_string
      in

      if !only_check_spec 
      then
        begin
          Printf.printf "%s\n\n" specification_string;
          Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_specification specification));
          exit 0;
        end;
      
      specification
    )



(* Print the input. *)

let () = 

  if(!print_u)
  then (
    Printf.printf "\n===> THE UNIVERSE <===\n\n";    
    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_universe my_universe));
    flush stdout;
  );

  if(!print_ic)
  then (
    Printf.printf "\n===> THE INITIAL CONFIGURATION <===\n\n";
    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_configuration my_initial_configuration));
    flush stdout;
  );

  if(!print_spec)
  then (
    Printf.printf "\n===> THE SPECIFICATION <===\n\n";

    if(not !raw_specification)
    then begin 
      Printf.printf "> Unparsed specification:\n\n%s\n\n" specification_string;
      Printf.printf "> Parsed specification:\n\n";
      flush stdout;
    end;

    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_specification my_specification));
    flush stdout;
  )


(* Trim the universe. *)
let my_universe = 
  Universe_trimming.trim my_universe my_initial_configuration my_specification

let () = 
  if(!print_tu)
  then (
    Printf.printf "\n===> THE UNIVERSE AFTER TRIMMING <===\n\n";    
    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_universe my_universe));
    flush stdout;
  )


(* Generate the constraints from the resource types and the specification *)
let my_universe_constraints = 
  translate_universe_and_initial_configuration my_universe my_initial_configuration

let my_specification_constraints =
  translate_specification my_specification my_initial_configuration

let my_generated_constraints = 
  my_universe_constraints @ my_specification_constraints

(* Print the generated constraints. *)
let () =
  if(!print_cstrs)
  then (
    Printf.printf "\n===> THE CONSTRAINTS <===\n";
    Printf.printf "%s" (string_of_generated_constraints my_generated_constraints);
    flush stdout;
  )

(* Prepare the optimization expression. *)


let optimization_exprs =
    let open Optimization_functions 
    in
    match optimization_function with
    | Simple_optimization_function       -> [Minimize (cost_expr_number_of_all_components my_universe)]
    | Compact_optimization_function      -> compact      my_initial_configuration my_universe
    | Conservative_optimization_function -> conservative my_initial_configuration my_universe
    | Spread_optimization_function       -> spread       my_initial_configuration my_universe
    | None_optimization_function         -> [Satisfy]


(* Solve! *)

let () =
  if !do_not_solve 
  then exit 0

let solution =
  
  let open Solvers in

  let solver_settings = {
    print_solver_vars            = !print_solver_vars;
    print_solver_cstrs           = !print_solver_cstrs;
    print_solver_exe             = !print_solver_exe;
    print_intermediate_solutions = !print_intermediate_solutions;
  }
  in

  let variables = 
    Model_variables.get_all_variables my_universe my_initial_configuration my_specification
  in

  match solver_choice with

  | G12Solver ->
    fst (
      G12.solve_lex
        variables 
        my_generated_constraints
        optimization_exprs
        solver_settings
    )

  | GeCodeSolver ->
    fst (
      GeCode.solve_lex
        variables 
        my_generated_constraints
        optimization_exprs
        solver_settings
    )

  | FaCiLeSolver ->
      fst (
        FaCiLe.solve
          variables 
          my_generated_constraints
          (List.hd optimization_exprs)
          solver_settings
      )


(* Print the solution. *)

let () =
  if(!print_solution)
  then (
    Printf.printf "\n===> THE SOLUTION <===\n";
    Printf.printf "%s" (string_of_solution solution);
    flush stdout;
  )

  
(* Convert the constraint problem solution to a configuration. *)
let final_configuration = 
  configuration_of_solution my_universe my_initial_configuration solution


(* Output the final configuration. *)
let () =

  List.iter2 (fun output_channel output_format -> 

    let output_string = (match output_format with
     | Plain_output            -> Simple  .string_of_configuration final_configuration
     | JSON_output             -> JSON    .string_of_configuration final_configuration
     | Graph_output graph_type -> Graphviz.string_of_configuration (graph_settings_of_graph_type graph_type) my_universe final_configuration
    )
    in

    Printf.fprintf output_channel "%s" output_string;
    flush output_channel

  ) output_channels output_formats;

  (* Then we print the plain text version on the standard output anyway. *)
  Printf.printf "\n===> THE GENERATED CONFIGURATION <===\n";
  Printf.printf "\n%s\n\n" (Simple.string_of_configuration final_configuration);
  flush stdout
