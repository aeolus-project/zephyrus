

open Helpers

type solver_settings = {
  print_solver_vars            : bool;
  print_solver_cstrs           : bool;
  print_intermediate_solutions : bool;
}

(*
module type SOLVER_FACADE =
  sig
  	val solve : 
  end
*)

module G12 =
  struct

    let solve 
      variable_keys 
      generated_constraints
      generic_optimization_expr
      solver_settings

      =

      let open Minizinc_constraints in

      let minizinc_to_flatzinc_converter_command input_minizinc_filepath output_flatzinc_filepath =
        Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s" output_flatzinc_filepath input_minizinc_filepath

      and flatzinc_solver_command input_flatzinc_file output_solution_file =
        Printf.sprintf "flatzinc -o %s %s" output_solution_file input_flatzinc_file
        (*Printf.sprintf "fz -o %s %s" output_solution_file input_flatzinc_file*)

      in

      (* Check if commands "mzn2fzn" and "flatzinc" are available. *)
      List.iter (fun program ->
        if not (did_process_exit_ok (Unix.system (Printf.sprintf "which %s" program)))
        then failwith (Printf.sprintf "The \"%s\" program is not available on this machine!" program)
      ) ["mzn2fzn"; "flatzinc" (*"fz"*)];


      (* Preparing variables for MiniZinc translation. *)
      Printf.printf "\n===> Preparing variables for MiniZinc translation...\n";
      let minizinc_variables = 
        create_minizinc_variables variable_keys
      in

      if(solver_settings.print_solver_vars)
      then (
        Printf.printf "\n===> THE MINIZINC VARIABLES <===\n\n";
        Printf.printf "%s\n" (string_of_minizinc_variables minizinc_variables);
        flush stdout;
      );


      (* Translating constraints into MiniZinc. *)
      Printf.printf "\n===> Translating constraints into MiniZinc...\n";
      let minizinc_constraints = 
        translate_constraints 
          minizinc_variables
          generated_constraints
          generic_optimization_expr
      in

      if(solver_settings.print_solver_cstrs)
      then (
        Printf.printf "\n===> THE MINIZINC CONSTRAINTS <===\n\n";
        Printf.printf "%s" minizinc_constraints;
        flush stdout;
      );


      (* Printing the MiniZinc constraints to a temporary .mzn file. *)
      Printf.printf "\n===> Printing the MiniZinc constraints to a temporary .mzn file...\n";
      let (minizinc_filepath, minizinc_out) = Filename.open_temp_file "zephyrus" ".mzn" in
      Printf.fprintf minizinc_out "%s" minizinc_constraints;
      flush minizinc_out;
      close_out minizinc_out;


      (* Converting MiniZinc to FlatZinc. *)
      Printf.printf "\n===> Converting MiniZinc to FlatZinc...\n";
      let flatzinc_filepath = Filename.temp_file "zephyrus" ".fzn" in
      let mzn2fzn_process_status = Unix.system (minizinc_to_flatzinc_converter_command minizinc_filepath flatzinc_filepath) in
      (if not (did_process_exit_ok mzn2fzn_process_status)
      then failwith "mzn2fzn error!");
      Sys.remove minizinc_filepath;

      (* Solving the problem encoded in FlatZinc using G12 solver. *)
      let solution_filepath = Filename.temp_file "zephyrus" ".solution" in
      Printf.printf "\n===> Solving the problem using G12 solver...\n";
      let flatzinc_process_status = Unix.system (flatzinc_solver_command flatzinc_filepath solution_filepath) in
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
      let (solution, _) = solution_of_bound_minizinc_variables minizinc_variables minizinc_solution in

      (* Returning the solution in the right format. *)
      solution

  end


module FaCiLe =
  struct

    let solve 
      variable_keys 
      generated_constraints
      generic_optimization_expr
      solver_settings

      =

      let open Facile_variables in
      let open Facile_constraints in
      let open Facile in
      let open Easy in

      (* Prepare the problem: FaCiLe variables, FaCiLe constraints, optimization function and the goal. *)

      let facile_variables =
        create_facile_variables variable_keys
      in

      let facile_constraints : generated_constraints = 
        List.map (fun (constraints_group_name, constraints) ->
          let facile_constraints = 
            List.map 
              (Facile_constraints.translate_cstr facile_variables)
              constraints
          in
          (constraints_group_name, facile_constraints)
        ) generated_constraints
      
      and cost_expr = Facile_constraints.translate_expr facile_variables generic_optimization_expr

      (* A placeholder for the solution.*)
      and facile_solution = ref []
      in

      (* The goal.*)
      let goal = create_optimized_goal facile_variables cost_expr facile_solution solver_settings.print_intermediate_solutions
      in

      Printf.printf "\n===> INITIALIZING THE FACILE CONSTRAINTS... <===\n\n";
      post_translation_constraints facile_constraints;

      if(solver_settings.print_solver_vars)
      then (
        Printf.printf "\n===> THE FACILE VARIABLES <===\n";
        Printf.printf "%s" (string_of_facile_variables facile_variables);
        flush stdout;
      );

      if(solver_settings.print_solver_cstrs)
      then (
        Printf.printf "\n===> THE FACILE CONSTRAINTS <===\n";
        Printf.printf "%s" (string_of_constraints facile_constraints);
        flush stdout;
      );

      Printf.printf "\n===> SOLVING! <===\n"; 
      flush stdout;

      let _ = Goals.solve (goal ||~ Goals.success) in

      !facile_solution

  end