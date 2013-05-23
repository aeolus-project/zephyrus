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

open Helpers
open Optimization_functions

let get_all_variables 
  (generated_constraints : Constraints_generation.generated_constraints)
  (optimization_function : Optimization_functions.optimization_function)
  : Variables.variable list
  =

  (* Extract variables from all constraints *)
  let constraints_vars =
    List.flatten_map Generic_constraints.extract_variables_of_cstr (Constraints_generation.constraints_of_generated_constraints generated_constraints)

  (* Extract variables from the optimization function *)
  and optimization_function_vars =
    match optimization_function with
    | Satisfy -> []
    | Maximize (expr)
    | Minimize (expr) -> Generic_constraints.extract_variables_of_expr expr

  in

  (* Put all the variables together, remove doubles *)
  List.unique (constraints_vars @ optimization_function_vars)


type solver_settings = {
  print_solver_vars            : bool;
  print_solver_cstrs           : bool;
  print_solver_exe             : bool;
  print_intermediate_solutions : bool;
}

type solve_function =
  Constraints_generation.generated_constraints ->
  Optimization_functions.optimization_function -> (* A single optimization function. *)
  solver_settings ->
  Solution.solution_with_cost (* It returns the solution and its cost. *)

module type SOLVER =
  sig
    val solve : solve_function
  end

type solve_lex_function =
  Constraints_generation.generated_constraints ->
  Optimization_functions.optimization_function list -> (* List of optimization functions. *)
  solver_settings ->
  Solution.solution_with_costs (* It returns the solution and a list of costs (one for each optimization expression). *)

module type SOLVER_LEX =
  sig 
    include SOLVER
    val solve_lex : solve_lex_function
  end

let solve_lex 
  (solve_function : solve_function) 
  generated_constraints
  optimization_functions
  solver_settings
  =

  let (solution, costs, _) = 
    List.fold_left 
    ( 
      fun 
        
        ( (_, previous_costs, additional_constraints) : (Solution.solution * int list * Generic_constraints.cstr list) ) 
        (optimization_function : Optimization_functions.optimization_function) 

      -> 
    
        (* Constraints for the problem should include:
           1. the standard generated constraints
           2. constraints which embody the results of previous optimizations. *)
        let constraints =
          generated_constraints @ [("previous optimization"), additional_constraints]

        in

        (* Solve the problem using the current optimization function. *)
        let (solution, cost) =
          solve_function
            constraints           (* Our generated constraints + previous optimizations constraints. *)
            optimization_function (* The current optimization function. *)
            solver_settings

        in

        (* Create a new constraint representing the current optimization result. *)
        let new_constraint =
          let open Generic_constraints in
          (* For all the subsequent steps this expression's value must be equal to the optimal value found. *)
          match optimization_function with
          | Satisfy                -> truecstr
          | Minimize optimize_expr
          | Maximize optimize_expr -> optimize_expr =~ (int2expr cost)

        in

        (
          solution,                                (* Pass on the current solution (which takes into account all previous optimizations). *)
          cost :: previous_costs,                  (* Append the optimal cost for the current optimization expression to the accumulator. *)
          new_constraint :: additional_constraints (* Append the new optimization constraint to the accumulator. *)
        )

  ) ([], [], []) optimization_functions

  in
  (solution, costs)



let standard_flatzinc_command_line_solver 

  (minizinc_to_flatzinc_converter : in_out_program)
  (flatzinc_solver                : in_out_program)

  (generated_constraints : Constraints_generation.generated_constraints)
  (optimization_function : Optimization_functions.optimization_function)
  (solver_settings       : solver_settings) 

  : Solution.solution_with_cost
  
  =

  let variables = get_all_variables generated_constraints optimization_function in

  let open Minizinc_constraints in

  (* Solver execution details printing. *)
  let solver_exe_printf = 
    if solver_settings.print_solver_exe
    then (fun (s : string) -> print_string s; flush stdout)
    else (fun (s : string) -> ())

  in

  (* Checking if required programs are available. *)
  solver_exe_printf (Printf.sprintf "\n===> Checking if required programs are available...\n");
  check_if_programs_available [minizinc_to_flatzinc_converter; flatzinc_solver];
  solver_exe_printf (Printf.sprintf "\nOK!\n");

  (* Preparing variables for MiniZinc translation. *)
  solver_exe_printf (Printf.sprintf "\n===> Preparing variables for MiniZinc translation...\n");
  let minizinc_variables = 
    create_minizinc_variables variables
  in

  if(solver_settings.print_solver_vars)
  then (
    Printf.printf "\n===> THE MINIZINC VARIABLES <===\n\n";
    Printf.printf "%s\n" (string_of_minizinc_variables minizinc_variables);
    flush stdout;
  );


  (* Translating constraints into MiniZinc. *)
  solver_exe_printf (Printf.sprintf "\n===> Translating constraints into MiniZinc...\n");
  let minizinc_constraints = 
    translate_constraints 
      minizinc_variables
      generated_constraints
      optimization_function
  in

  if(solver_settings.print_solver_cstrs)
  then (
    Printf.printf "\n===> THE MINIZINC CONSTRAINTS <===\n\n";
    Printf.printf "%s" minizinc_constraints;
    flush stdout;
  );


  (* Printing the MiniZinc constraints to a temporary .mzn file. *)
  solver_exe_printf (Printf.sprintf "\n===> Printing the MiniZinc constraints to a temporary .mzn file...\n");
  let (minizinc_filepath, minizinc_out) = Filename.open_temp_file "zephyrus" ".mzn" in
  Printf.fprintf minizinc_out "%s" minizinc_constraints;
  flush minizinc_out;
  close_out minizinc_out;


  (* Converting MiniZinc to FlatZinc. *)
  solver_exe_printf (Printf.sprintf "\n===> Converting MiniZinc to FlatZinc using converter %s...\n" minizinc_to_flatzinc_converter.name);
  let flatzinc_filepath = Filename.temp_file "zephyrus" ".fzn" in
  let minizinc_to_flatzinc_converter_exe = minizinc_to_flatzinc_converter.exe minizinc_filepath flatzinc_filepath in
  solver_exe_printf (Printf.sprintf "\nExecuting command: %s\n" minizinc_to_flatzinc_converter_exe);
  let mzn2fzn_process_status = Unix.system minizinc_to_flatzinc_converter_exe in
  (if not (did_process_exit_ok mzn2fzn_process_status)
  then failwith (Printf.sprintf "%s error!" minizinc_to_flatzinc_converter.command));
  Sys.remove minizinc_filepath;

  (* Solving the problem encoded in FlatZinc using an external solver. *)
  let solution_filepath = Filename.temp_file "zephyrus" ".solution" in
  solver_exe_printf (Printf.sprintf "\n===> Solving the problem using flatzinc solver %s...\n" flatzinc_solver.name);
  let flatzinc_solver_exe = flatzinc_solver.exe flatzinc_filepath solution_filepath in
  solver_exe_printf (Printf.sprintf "\nExecuting command: %s\n" flatzinc_solver_exe);
  let flatzinc_process_status = Unix.system flatzinc_solver_exe in
  (if not (did_process_exit_ok flatzinc_process_status)
  then failwith (Printf.sprintf "%s error!" flatzinc_solver.command));
  Sys.remove flatzinc_filepath;

  (* Reading the solution found by the an external solver. *)
  solver_exe_printf (Printf.sprintf "\n===> Getting the solution found by the flatzinc solver...\n");
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
  solver_exe_printf (Printf.sprintf "\n===> Parsing the solution found by the flatzinc solver...\n");
  let lexbuf = Lexing.from_string solution_string in
  let minizinc_solution = Flatzinc_solution_parser.main Flatzinc_solution_lexer.token lexbuf in
  let solution_with_cost = solution_of_bound_minizinc_variables minizinc_variables minizinc_solution in

  (* Returning the solution in the right format. *)
  solution_with_cost


let mzn2fzn = {
  name    = "G12 mzn2fzn";
  command = "mzn2fzn";
  exe     = (fun input_minizinc_filepath output_flatzinc_filepath ->
              Printf.sprintf "mzn2fzn --no-output-ozn -o %s %s" output_flatzinc_filepath input_minizinc_filepath)
}

module G12 : SOLVER_LEX =
  struct

    let g12_flatzinc_solver = {
      name    = "G12";
      command = "flatzinc";
      exe     = (fun input_flatzinc_file output_solution_file ->
                  Printf.sprintf "flatzinc -o %s %s" output_solution_file input_flatzinc_file)
    }

    let solve = standard_flatzinc_command_line_solver mzn2fzn g12_flatzinc_solver

    let solve_lex = solve_lex solve

  end

module GeCode : SOLVER_LEX =
  struct

    let gecode_flatzinc_solver = {
      name    = "GeCode";
      command = "fz";
      exe     = (fun input_flatzinc_file output_solution_file ->
                  Printf.sprintf "fz -o %s %s" output_solution_file input_flatzinc_file)
    }

    let solve = standard_flatzinc_command_line_solver mzn2fzn gecode_flatzinc_solver

    let solve_lex = solve_lex solve

  end


module FaCiLe : SOLVER =
  struct

    let solve 
      generated_constraints
      optimization_function
      solver_settings

      =

      let variables = get_all_variables generated_constraints optimization_function in

      let open Facile_variables in
      let open Facile_constraints in
      let open Facile in
      let open Easy in

      (* Prepare the problem: FaCiLe variables, FaCiLe constraints, optimization function and the goal. *)

      let minimize_expr =
        let open Generic_constraints 
        in
        match optimization_function with 
        | Satisfy                -> failwith "Sorry, satisfy problems are not implemented in Zephyrus FaCiLe interface..."
        | Minimize minimize_expr -> minimize_expr
        | Maximize maximize_expr -> ((int2expr 0) -~ maximize_expr)
      in

      let facile_variables =
        create_facile_variables variables
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
      
      and cost_expr = Facile_constraints.translate_expr facile_variables minimize_expr

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