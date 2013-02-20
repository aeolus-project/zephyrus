(* 
  Aeolus, the Keeper of the Winds, gives Odysseus a tightly closed bag
  full of the captured winds so he could sail easily home to Ithaca on
  the gentle west wind.
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

let import_repository_names     = ref []
let import_repository_filenames = ref []

(* printing settings *)
let print_u                      = ref false
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
    "usage: %s %s %s %s %s %s %s %s"
    Sys.argv.(0)
    "[-u universe-file]"
    "[-ic initial-configuration-file]"
    "[-spec specification-file]"
    "([-repo repository-name packages-file])*" 
    "[-opt optimization-function]"
    "[-out output-file]"
    "[-out-format {plain|json}]"

let speclist = 
  Arg.align [

  (* Input arguments *)
  ("-u",          Arg.String (fun filename -> universe_channel              := (open_in  filename)), " The universe input file");
  ("-ic",         Arg.String (fun filename -> initial_configuration_channel := (open_in  filename)), " The initial configuration input file");
  ("-spec",       Arg.String (fun filename -> specification_channel         := (open_in  filename)), " The specification input file");
  
  ("-repo",       Arg.Tuple 
                  (
                    [Arg.String (fun repository_name     -> import_repository_names     := repository_name     :: !import_repository_names);
                     Arg.String (fun repository_filename -> import_repository_filenames := repository_filename :: !import_repository_filenames)]
                  ),
                  " The additional repository import: repository name and packages input file");

  (* Output arguments *)
  ("-out",        Arg.String (fun filename -> output_channel := (open_out filename)),      " The output file");  
  ("-out-format", Arg.Symbol ( ["plain"; "json"], (fun s -> output_format_string := s) ),  " The typed system output format (only for the output file)");

  (* Optimization function argument *)
  ("-opt",        Arg.Symbol ( ["simple"; "compact"; "conservative"], (fun s -> optimization_function_string := s) ), " The optimization function");
  
  ] @ 
  Arg.align [

  (* Printing options arguments *)
  ("-print-u",             Arg.Set (print_u),                      " Print the raw universe");
  ("-print-ic",            Arg.Set (print_ic),                     " Print the raw initial configuration");
  ("-print-spec",          Arg.Set (print_spec),                   " Print the raw specification");
  ("-print-cstrs",         Arg.Set (print_cstrs),                  " Print the constraints");
  ("-print-facile-vars",   Arg.Set (print_facile_cstrs),           " Print the FaCiLe variables");
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
    print_u                      := true;
    print_ic                     := true;
    print_spec                   := true;
    print_cstrs                  := true;
    print_facile_vars            := true;
    print_facile_cstrs           := true;
    print_intermediate_solutions := true;
    print_solution               := true;
  )


(* Handle the output-format argument. *)
type output_format = Plain_output | JSON_output

let output_format =
  match !output_format_string with
  | "plain" -> Plain_output
  | "json"  -> JSON_output
  | _ -> failwith "Invalid output format have passed through the Arg.Symbol!"

(* Handle the optimization-function argument. *)
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


(* === Set up everything === *)

(* Prepare input modules. *)

module MyUniverseInput             = Universe_input_facade.JSON_universe_input
module MyInitialConfigurationInput = Configuration_input_facade.JSON_configuration_input
module MySpecificationInput        = Specification_input_facade.JSON_specification_input


(* Handle the imported repositories arguments. *)

let imported_repositories =
  try
    List.rev_map2 (fun repository_name repository_filename -> 
    
      let repository_channel = (open_in repository_filename) 
      in
      let packages = 
        MyUniverseInput.packages_of_string (string_of_input_channel repository_channel)
      in

      (* The imported repository : *)
      {
        repository_name     = repository_name;
        repository_packages = packages;
      }

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

let my_specification =
  MySpecificationInput.specification_of_string (string_of_input_channel !specification_channel)


(* Print the input. *)

let () = 

  if(!print_u)
  then (
    Printf.printf "\n===> THE UNIVERSE <===\n\n";    
    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_universe my_universe));
  );

  if(!print_ic)
  then (
    Printf.printf "\n===> THE INITIAL CONFIGURATION <===\n\n";
    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_configuration my_initial_configuration));
  );

  if(!print_spec)
  then (
    Printf.printf "\n===> THE SPECIFICATION <===\n\n";
    Printf.printf "%s\n" (Yojson.Safe.prettify (Aeolus_types_j.string_of_specification my_specification));
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
  )


(* Prepare the problem: FaCiLe variables, FaCiLe constraints, optimization function and the goal. *)

let my_facile_variables =
  create_facile_variables my_universe my_initial_configuration my_specification

let my_facile_constraints : generated_constraints = 
  List.map (fun (constraints_group_name, constraints) ->
    let facile_constraints = 
      List.map 
        (Facile_constraints.translate_cstr my_facile_variables)
        constraints
    in
    (constraints_group_name, facile_constraints)
  ) (my_translation_constraints @ my_specification_constraints)


(* Handle the optimization function argument. *)

let generic_optimization_expr =
  match optimization_function with
  | Simple_optimization_function       -> Optimization_functions.cost_expr_number_of_all_components my_universe
  | Compact_optimization_function      -> Optimization_functions.cost_expr_compact my_initial_configuration my_universe
  | Conservative_optimization_function -> Optimization_functions.cost_expr_difference_of_components my_initial_configuration my_universe

let cost_expr = Facile_constraints.translate_expr my_facile_variables generic_optimization_expr


(* A placeholder for the solution.*)
let solution = ref []

(* The goal.*)
let goal = create_optimized_goal my_facile_variables cost_expr solution !print_intermediate_solutions


(* === Main program === *)

let () =

  Printf.printf "\n===> INITIALIZING THE FACILE CONSTRAINTS... <===\n\n";
  post_translation_constraints my_facile_constraints;

  if(!print_facile_vars)
  then (
    Printf.printf "\n===> THE FACILE VARIABLES <===\n";

    Printf.printf "%s" (string_of_facile_variables my_facile_variables)
  );

  if(!print_facile_cstrs)
  then (
    Printf.printf "\n===> THE FACILE CONSTRAINTS <===\n";
    Printf.printf "%s" (string_of_constraints my_facile_constraints);
  );



  Printf.printf "\n===> SOLVING! <===\n"; 
  let _ = Goals.solve (goal ||~ Goals.success) in

  if(!print_solution)
  then (
    Printf.printf "\n===> THE SOLUTION <===\n";
    Printf.printf "%s" (string_of_solution !solution);
  );

  
  (* Convert the constraint problem solution to a typed system. *)
  let final_configuration = configuration_of_solution my_universe my_initial_configuration !solution
  in

  (* If user has specified an output file, we print a formatted verion (i.e. either plain text or JSON) there. *)
  if( !output_channel != stdout )
  then (
    
    let output_string = (match output_format with
     | Plain_output ->                      (Simple_configuration_output.string_of_configuration final_configuration)
     | JSON_output  -> Yojson.Safe.prettify (JSON_configuration_output  .string_of_configuration final_configuration) 
    )
    in

    Printf.fprintf !output_channel "%s" output_string;

  );

  (* Then we print the plain text version on the standard output anyway. *)
  Printf.printf "\n===> THE GENERATED CONFIGURATION <===\n";
  Printf.printf "\n%s\n\n" (Simple_configuration_output.string_of_configuration final_configuration);

  ()

