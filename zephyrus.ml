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
open Constraints
open Solution
open Configuration_generation

open Configuration_output_facade


(* === Handling the arguments === *)

(* Variables corresponding to arguments *)

(* input / output *)
let universe_channel              = ref stdin
let specification_channel         = ref stdin
let initial_configuration_channel = ref stdin
let output_channel                = ref stdout

let output_format_string         = ref "plain"
let optimization_function_string = ref "simple"
let solver_choice_string         = ref "g12"

let import_repository_names      = ref []
let import_repository_filenames  = ref []
let prefix_repositories          = ref false

let raw_specification            = ref false
let only_check_spec              = ref false

let do_not_solve                 = ref false

(* printing settings *)
let print_u                      = ref false
let print_tu                     = ref false
let print_ic                     = ref false
let print_spec                   = ref false
let print_cstrs                  = ref false
let print_facile_vars            = ref false
let print_facile_cstrs           = ref false
let print_intermediate_solutions = ref false
let print_solution               = ref false
let print_all                    = ref false

(* Arg module settings *)

let usage = 
  Printf.sprintf
    "usage: %s %s %s %s %s %s %s %s %s"
    Sys.argv.(0)
    "[-u universe-file]"
    "[-ic initial-configuration-file]"
    "[-spec specification-file]"
    "[-repo repository-name packages-file]*" 
    "[-opt optimization-function]"
    "[-solver solver]"
    "[-out output-file]"
    "[-out-format {plain|json}]"

let speclist = 
  Arg.align [
    (* Input arguments *)
    ("-u",          Arg.String (fun filename -> universe_channel              := (open_in  filename)), " The universe input file");
    ("-ic",         Arg.String (fun filename -> initial_configuration_channel := (open_in  filename)), " The initial configuration input file");
    ("-spec",       Arg.String (fun filename -> specification_channel         := (open_in  filename)), " The specification input file");
    ("-raw-spec",   Arg.Set (raw_specification),                                                       " The specification is given directly in JSON, not using the nice syntax");
    
    ("-repo",       Arg.Tuple 
                    (
                      [Arg.String (fun repository_name     -> import_repository_names     := repository_name     :: !import_repository_names);
                       Arg.String (fun repository_filename -> import_repository_filenames := repository_filename :: !import_repository_filenames)]
                    ),
                    " Import additional repository: specify the repository name and the packages input file (you can import multiple repositories)");

    ("-prefix-repos", Arg.Set (prefix_repositories), " Prefix all package names in imported repositories by the repository name.");
  ] @

  Arg.align [
    (* Optimization function argument, solver choice *)
    ("-opt",        Arg.Symbol ( ["simple"; "compact"; "conservative"], (fun s -> optimization_function_string := s) ), " The optimization function");
    ("-solver",     Arg.Symbol ( ["facile"; "g12"],                     (fun s -> solver_choice_string := s) ),         " The solver choice"); 
  ] @ 

  Arg.align [
    (* Output arguments *)
    ("-out",        Arg.String (fun filename -> output_channel := (open_out filename)),      " The final configuration output file");  
    ("-out-format", Arg.Symbol ( ["plain"; "json"], (fun s -> output_format_string := s) ),  " The final configuration output format (only for the output file)");
  ] @

  Arg.align [
    ("-only-check-spec", Arg.Set (only_check_spec),  " Just parse specification and exit");
    ("-do-not-solve",    Arg.Set (do_not_solve),     " Do not use the solver (exit directly after generating generic constraints)");
  ] @

  Arg.align [
    (* Printing options arguments *)
    ("-print-u",             Arg.Set (print_u),                      " Print the raw universe");
    ("-print-tu",            Arg.Set (print_tu),                     " Print the trimmed universe");
    ("-print-ic",            Arg.Set (print_ic),                     " Print the raw initial configuration");
    ("-print-spec",          Arg.Set (print_spec),                   " Print the raw specification");
    ("-print-cstrs",         Arg.Set (print_cstrs),                  " Print the constraints");
    ("-print-facile-vars",   Arg.Set (print_facile_vars),            " Print the FaCiLe variables");
    ("-print-facile-cstrs",  Arg.Set (print_facile_cstrs),           " Print the FaCiLe constraints");
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
  (* print_u                      := true; *)
    print_tu                     := true;
    print_ic                     := true;
    print_spec                   := true;
    print_cstrs                  := true;
    print_facile_vars            := true;
    print_facile_cstrs           := true;
    print_intermediate_solutions := true;
    print_solution               := true;
  )

(* Handle the output format choice argument. *)
type output_format = Plain_output | JSON_output

let output_format =
  match !output_format_string with
  | "plain" -> Plain_output
  | "json"  -> JSON_output
  | _ -> failwith "Invalid output format have passed through the Arg.Symbol!"

(* Handle the optimization function choice argument. *)
type optimization_function = 
  | Simple_optimization_function
  | Compact_optimization_function
  | Conservative_optimization_function

let optimization_function =
  match !optimization_function_string with
  | "simple"       -> Simple_optimization_function
  | "compact"      -> Compact_optimization_function
  | "conservative" -> Conservative_optimization_function
  | _ -> failwith "Invalid optimization function choice have passed through the Arg.Symbol!"

(* Handle the solver choice argument. *)
type solver_choice = 
  | FaCiLeSolver
  | G12Solver

let solver_choice =
  match !solver_choice_string with
  | "facile" -> FaCiLeSolver
  | "g12"    -> G12Solver
  | _ -> failwith "Invalid solver choice have passed through the Arg.Symbol!"


(* === Set up everything === *)

(* Prepare input modules. *)

module MyUniverseInput             = Universe_input_facade.JSON_universe_input
module MyInitialConfigurationInput = Configuration_input_facade.JSON_configuration_input
module MySpecificationInput        = Specification_input_facade.JSON_specification_input


(* Handle the imported repositories. *)

(* Prefix all package names in the repository with the repository name. *)
let prefix_repository repository = 
  let open Aeolus_types_output_facade.Aeolus_types_plain_output 
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
    List.rev_map2 (fun repository_name repository_filename -> 
      let open Aeolus_types_output_facade.Aeolus_types_plain_output 
      in
      Printf.printf "Importing repository %s from file %s...\n" (string_of_repository_name repository_name) repository_filename;
      flush stdout;

      let repository_channel = (open_in repository_filename) 
      in
      let packages = 
        MyUniverseInput.packages_of_string (string_of_input_channel repository_channel)
      in

      (* The imported repository : *)
      let repository =
      {
        repository_name     = repository_name;
        repository_packages = packages;
      }

      in

      if !prefix_repositories 
      then prefix_repository repository
      else repository

    ) !import_repository_names !import_repository_filenames
  with
  Invalid_argument s -> 
    failwith (Printf.sprintf "Number of imported repository names does not match the number of imported repository filenames! %s" s)




(* Read the input. *)

let my_universe =
  let universe =
    MyUniverseInput.universe_of_string (string_of_input_channel !universe_channel)
  in
  {
    universe_component_types = universe.universe_component_types;
    universe_implementation = universe.universe_implementation;
    universe_repositories = universe.universe_repositories @ imported_repositories
  }

let my_initial_configuration =
  MyInitialConfigurationInput.configuration_of_string (string_of_input_channel !initial_configuration_channel)


let specification_string = string_of_input_channel !specification_channel

let my_specification =
  if (!raw_specification)
  then 
    MySpecificationInput.specification_of_string specification_string
  else
    (
      (* Specification_lexer.initialize_names_table my_universe; *)
      let lexbuf = Lexing.from_string specification_string in
      let specification = Specification_parser.main Specification_lexer.token lexbuf in
      
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
      flush stdout;
    end;

    Printf.printf "> Parsed specification:\n\n%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_specification my_specification));
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
let my_translation_constraints = 
  translate_universe_and_initial_configuration my_universe my_initial_configuration

let my_specification_constraints =
  translate_specification my_specification my_initial_configuration


(* Print the generated constraints. *)
let () =
  if(!print_cstrs)
  then (
    Printf.printf "\n===> THE CONSTRAINTS <===\n";
    Printf.printf "%s" (string_of_generated_constraints (my_translation_constraints @ my_specification_constraints));
    flush stdout;
  )

(* Prepare the optimization expression. *)
let generic_optimization_expr =
  match optimization_function with
  | Simple_optimization_function       -> Optimization_functions.cost_expr_number_of_all_components                          my_universe
  | Compact_optimization_function      -> Optimization_functions.cost_expr_compact                  my_initial_configuration my_universe
  | Conservative_optimization_function -> Optimization_functions.cost_expr_conservative             my_initial_configuration my_universe


(* Solve! *)

let () =
  if !do_not_solve 
  then (exit 0)

let solution =
  match solver_choice with

  | G12Solver -> (

      (* Check if commands "mzn2fzn" and "flatzinc" are available. *)
      List.iter (fun program ->
        if not (did_process_exit_ok (Unix.system (Printf.sprintf "which %s" program)))
        then failwith (Printf.sprintf "The \"%s\" program is not available on this machine!" program)
      ) ["mzn2fzn"; "flatzinc"];

      (* Preparing variables for MiniZinc translation. *)
      Printf.printf "\n===> Preparing variables for MiniZinc translation...\n";
      let minizinc_variables = 
        Minizinc_constraints.create_minizinc_variables
        (Variables.get_variable_keys my_universe my_initial_configuration my_specification)
      in

      (* Translating constraint into MiniZinc. *)
      Printf.printf "\n===> Translating constraint into MiniZinc...\n";
      let minizinc_constraints = 
        Minizinc_constraints.translate_constraints 
          minizinc_variables
          (List.flatten (List.map snd (my_translation_constraints @ my_specification_constraints) ))
          generic_optimization_expr
      in

      Printf.printf "\n\n%s\n\n" (minizinc_constraints);

      (* Printing the MiniZinc constraints to a temporary .mzn file. *)
      Printf.printf "\n===> Printing the MiniZinc constraints to a temporary .mzn file...\n";
      let (minizinc_filepath, minizinc_out) = Filename.open_temp_file "zephyrus" ".mzn" in
      Printf.fprintf minizinc_out "%s" minizinc_constraints;
      flush minizinc_out;
      close_out minizinc_out;



      (* Converting MiniZinc to FlatZinc. *)
      Printf.printf "\n===> Converting MiniZinc to FlatZinc...\n";
      let flatzinc_filepath = Filename.temp_file "zephyrus" ".fzn" in
      let mzn2fzn_process_status = Unix.system (Printf.sprintf "mzn2fzn --no-output-ozn %s -o %s" minizinc_filepath flatzinc_filepath) in
      (if not (did_process_exit_ok mzn2fzn_process_status)
      then failwith "mzn2fzn error!");
      Sys.remove minizinc_filepath;

      (* Solving the problem encoded in FlatZinc using G12 solver. *)
      let solution_filepath = Filename.temp_file "zephyrus" ".solution" in
      Printf.printf "\n===> Solving the problem using G12 solver...\n";
      let flatzinc_process_status = Unix.system (Printf.sprintf "flatzinc %s -o %s" flatzinc_filepath solution_filepath) in
      (if not (did_process_exit_ok flatzinc_process_status)
      then failwith "flatzinc error!");
      Sys.remove flatzinc_filepath;

      (* Reading the solution found by the G12 solver. *)
      Printf.printf "\n===> Getting the solution found by the G12 solver...\n";
      let solution_in = (open_in solution_filepath) in
      let solution_string = string_of_input_channel solution_in in
      close_in solution_in;
      Sys.remove solution_filepath;

      Printf.printf "\nThe solution:\n%s\n" solution_string;
      
      (* Parsing the solution. *)
      Printf.printf "\n===> Parsing the solution found by the G12 solver...\n";
      let lexbuf = Lexing.from_string solution_string in
      let minizinc_solution = Flatzinc_output_parser.main Flatzinc_output_lexer.token lexbuf in
      let solution = Minizinc_constraints.solution_of_bound_minizinc_variables minizinc_variables minizinc_solution in

      (* Returning the solution in the right format. *)
      solution
    )

  | FaCiLeSolver -> (

      (* Prepare the problem: FaCiLe variables, FaCiLe constraints, optimization function and the goal. *)

      let my_facile_variables =
        create_facile_variables my_universe my_initial_configuration my_specification
      in

      let my_facile_constraints : generated_constraints = 
        List.map (fun (constraints_group_name, constraints) ->
          let facile_constraints = 
            List.map 
              (Facile_constraints.translate_cstr my_facile_variables)
              constraints
          in
          (constraints_group_name, facile_constraints)
        ) (my_translation_constraints @ my_specification_constraints)
      
      and cost_expr = Facile_constraints.translate_expr my_facile_variables generic_optimization_expr

      (* A placeholder for the solution.*)
      and facile_solution = ref []
      in

      (* The goal.*)
      let goal = create_optimized_goal my_facile_variables cost_expr facile_solution !print_intermediate_solutions
      in

      Printf.printf "\n===> INITIALIZING THE FACILE CONSTRAINTS... <===\n\n";
      post_translation_constraints my_facile_constraints;

      if(!print_facile_vars)
      then (
        Printf.printf "\n===> THE FACILE VARIABLES <===\n";
        Printf.printf "%s" (string_of_facile_variables my_facile_variables);
        flush stdout;
      );

      if(!print_facile_cstrs)
      then (
        Printf.printf "\n===> THE FACILE CONSTRAINTS <===\n";
        Printf.printf "%s" (string_of_constraints my_facile_constraints);
        flush stdout;
      );

      Printf.printf "\n===> SOLVING! <===\n"; 
      flush stdout;

      let _ = Goals.solve (goal ||~ Goals.success) in

      !facile_solution
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
let final_configuration = configuration_of_solution my_universe my_initial_configuration solution


(* Output the final configuration. *)
let () =

  (* If user has specified an output file, we print a formatted verion (i.e. either plain text or JSON) there. *)
  if( !output_channel != stdout )
  then (
    
    let output_string = (match output_format with
     | Plain_output ->                      (Simple_configuration_output.string_of_configuration final_configuration)
     | JSON_output  -> Yojson.Safe.prettify (JSON_configuration_output  .string_of_configuration final_configuration) 
    )
    in

    Printf.fprintf !output_channel "%s" output_string;
    flush !output_channel;
  );

  (* Then we print the plain text version on the standard output anyway. *)
  Printf.printf "\n===> THE GENERATED CONFIGURATION <===\n";
  Printf.printf "\n%s\n\n" (Simple_configuration_output.string_of_configuration final_configuration);
  flush stdout
