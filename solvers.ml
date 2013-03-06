

open Helpers

type solver_settings = {
  print_solver_vars            : bool;
  print_solver_cstrs           : bool;
  print_solver_exe             : bool;
  print_intermediate_solutions : bool;
}

type solve_function =
  Variable_keys.variable_key list ->
  Constraints.generated_constraints ->
  Generic_constraints.expr -> (* A single optimization expression. *)
  solver_settings ->
  Solution.solution_with_cost

module type SOLVER =
  sig
    val solve : solve_function
  end

type solve_lex_function =
  Variable_keys.variable_key list ->
  Constraints.generated_constraints ->
  Generic_constraints.expr list -> (* List of optimization expressions. *)
  solver_settings ->
  Solution.solution_with_costs (* List of optimization expressions. *)

module type SOLVER_LEX =
  sig 
    include SOLVER
    val solve_lex : solve_lex_function
  end

let solve_lex 
  (solve_function : solve_function) 
  variable_keys 
  generated_constraints
  generic_optimization_exprs
  solver_settings
  =

  let (solution, costs, _) = 
    List.fold_left 
    ( 
      fun 
        
        ( (_, previous_costs, additional_constraints) : (Solution.solution * int list * Generic_constraints.cstr list) ) 
        (optimization_expr : Generic_constraints.expr) 

      -> 
    
        let constraints =
          generated_constraints @ [("previous optimization"), additional_constraints]

        in

        let (solution, cost) =
          solve_function
            variable_keys 
            constraints
            optimization_expr
            solver_settings

        in

        let new_constraint =
          let open Generic_constraints in
          optimization_expr =~ (int2expr cost)

        in

        (
          solution,
          cost :: previous_costs,
          new_constraint :: additional_constraints
        )

  ) ([], [], []) generic_optimization_exprs

  in
  (solution, costs)



type in_out_program = {
  name    : string;
  command : string;
  exe     : string -> string -> string;
}

let is_program_available program =
  did_process_exit_ok (Unix.system (Printf.sprintf "which %s > /dev/null" program))

let check_if_programs_available programs =

  (* Check if given commands are available on the machine. *)
  List.iter (fun program ->
    if not (is_program_available program.command)
    then failwith (Printf.sprintf "The program \"%s\" (command \"%s\") is not available on this machine!" program.name program.command)
  ) programs




let standard_flatzinc_command_line_solver 

  (minizinc_to_flatzinc_converter : in_out_program)
  (flatzinc_solver                : in_out_program)

  (variable_keys             :  Variable_keys.variable_key list)
  (generated_constraints     : Constraints.generated_constraints)
  (generic_optimization_expr : Generic_constraints.expr)
  (solver_settings           : solver_settings) 

  : Solution.solution_with_cost
  
  =

  let open Minizinc_constraints in

  let printf = 
  (
    if solver_settings.print_solver_exe
    then print_string
    else (fun _ -> ())
  )
  in

  printf (Printf.sprintf "\n===> Checking if required programs are available...\n"); flush stdout;
  check_if_programs_available [minizinc_to_flatzinc_converter; flatzinc_solver];
  printf (Printf.sprintf "\nOK!\n"); flush stdout;

  (* Preparing variables for MiniZinc translation. *)
  printf (Printf.sprintf "\n===> Preparing variables for MiniZinc translation...\n"); flush stdout;
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
  printf (Printf.sprintf "\n===> Translating constraints into MiniZinc...\n"); flush stdout;
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
  printf (Printf.sprintf "\n===> Printing the MiniZinc constraints to a temporary .mzn file...\n"); flush stdout;
  let (minizinc_filepath, minizinc_out) = Filename.open_temp_file "zephyrus" ".mzn" in
  Printf.fprintf minizinc_out "%s" minizinc_constraints;
  flush minizinc_out;
  close_out minizinc_out;


  (* Converting MiniZinc to FlatZinc. *)
  printf (Printf.sprintf "\n===> Converting MiniZinc to FlatZinc using converter %s...\n" minizinc_to_flatzinc_converter.name); flush stdout;
  let flatzinc_filepath = Filename.temp_file "zephyrus" ".fzn" in
  let minizinc_to_flatzinc_converter_exe = minizinc_to_flatzinc_converter.exe minizinc_filepath flatzinc_filepath in
  printf (Printf.sprintf "\nExecuting command: %s\n" minizinc_to_flatzinc_converter_exe); flush stdout;
  let mzn2fzn_process_status = Unix.system minizinc_to_flatzinc_converter_exe in
  (if not (did_process_exit_ok mzn2fzn_process_status)
  then failwith (Printf.sprintf "%s error!" minizinc_to_flatzinc_converter.command));
  Sys.remove minizinc_filepath;

  (* Solving the problem encoded in FlatZinc using an external solver. *)
  let solution_filepath = Filename.temp_file "zephyrus" ".solution" in
  printf (Printf.sprintf "\n===> Solving the problem using flatzinc solver %s...\n" flatzinc_solver.name); flush stdout;
  let flatzinc_solver_exe = flatzinc_solver.exe flatzinc_filepath solution_filepath in
  printf (Printf.sprintf "\nExecuting command: %s\n" flatzinc_solver_exe); flush stdout;
  let flatzinc_process_status = Unix.system flatzinc_solver_exe in
  (if not (did_process_exit_ok flatzinc_process_status)
  then failwith (Printf.sprintf "%s error!" flatzinc_solver.command));
  Sys.remove flatzinc_filepath;

  (* Reading the solution found by the an external solver. *)
  printf (Printf.sprintf "\n===> Getting the solution found by the flatzinc solver...\n"); flush stdout;
  let solution_in = (open_in solution_filepath) in
  let solution_string = string_of_input_channel solution_in in
  close_in solution_in;
  Sys.remove solution_filepath;

  if(solver_settings.print_intermediate_solutions)
  then (
    Printf.printf "\nThe solution:\n%s\n" solution_string;
    flush stdout;
  );
  
  
  (* Parsing the solution. *)
  printf (Printf.sprintf "\n===> Parsing the solution found by the flatzinc solver...\n"); flush stdout;
  let lexbuf = Lexing.from_string solution_string in
  let minizinc_solution = Flatzinc_output_parser.main Flatzinc_output_lexer.token lexbuf in
  let solution_with_cost = solution_of_bound_minizinc_variables minizinc_variables minizinc_solution in

  (* Returning the solution in the right format. *)
  solution_with_cost


let mzn2fzn = {
  name    = "G12 mzn2fzn";
  command = "mzn2fzn";
  exe     = (fun input_minizinc_filepath output_flatzinc_filepath ->
              Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s" output_flatzinc_filepath input_minizinc_filepath)
}

let g12_flatzinc_solver = {
  name    = "G12";
  command = "flatzinc";
  exe     = (fun input_flatzinc_file output_solution_file ->
              Printf.sprintf "flatzinc -o %s %s" output_solution_file input_flatzinc_file)
}

module G12 : SOLVER_LEX =
  struct

    let solve = standard_flatzinc_command_line_solver mzn2fzn g12_flatzinc_solver

    let solve_lex = solve_lex solve

  end


let gecode_flatzinc_solver = {
  name    = "GeCode";
  command = "fz";
  exe     = (fun input_flatzinc_file output_solution_file ->
              Printf.sprintf "fz -o %s %s" output_solution_file input_flatzinc_file)
}

module GeCode : SOLVER_LEX =
  struct

    let solve = standard_flatzinc_command_line_solver mzn2fzn gecode_flatzinc_solver

    let solve_lex = solve_lex solve

  end


module FaCiLe : SOLVER =
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

      (!facile_solution, 0)

  end