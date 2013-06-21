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
    - datatypes/Data_constraint
*)


type solver_settings = {
  print_solver_variables       : bool;
  print_solver_constraints     : bool;
  print_solver_execution       : bool;
  print_intermediate_solutions : bool;
  input_file_prefix            : string;
  output_file_prefix           : string;
  keep_input_file              : bool;
  keep_output_file             : bool;  
}


module type SOLVER = sig
  val solve : solver_settings ->
      (string * Data_constraint.konstraint) list ->
      Data_constraint.optimization_function ->
      (Data_constraint.solution * (int list)) (* It returns the solution and its cost. *)
end


(* 1. Generic extension of a one-optimum solver to an n-optimum solver *)

let solve_lexicographic preprocess solve_step postprocess c f settings = 
  let initial_data = preprocess c f in
  let rec iterative_solve data f = match f with
    | Data_constraint.Lexicographic(f1::l1) -> let (data, solution, costs) = iterative_solve f1 in
      let (data', solution', costs) = iterative_solve data (Data_constraint.Lexicographic(l1)) in (data', solution', cost @ costs)
    | _ -> let (solution, cost) = solve_step settings data f in (postprocess data f cost, solution, [cost]) in
  let (_, solution, costs) = iterative_solve initial_data f in (solution, costs)


(* 2. generic minizinc handling *)
module type Minizinc_parameter = sig
  val 
end


module MiniZinc = struct
  open Minizinc_of

  let preprocess c f =
    let v_map = variables c f in core v_map Data_state.get_variable_bound_function c

  let solve_step program settings data f = let mzn = optimization_goal data f in
    
    Zephyrus_log.log_solver_execution (lazy "\n===> Checking if required programs are available...\n");
  check_if_programs_available [minizinc_to_flatzinc_converter; flatzinc_solver];
  Zephyrus_log.log_solver_execution (Printf.sprintf "\nOK!\n");

  (* Preparing variables for MiniZinc translation. *)
  Zephyrus_log.log_solver_execution (lazy "\n===> Preparing variables for MiniZinc translation...\n");
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


end








let get_all_variables 
  (generated_constraints : Constraints_generation.generated_constraints)
  (optimization_function : Optimization_functions.optimization_function)
  : Variables.variable list
  =

  (* Extract variables from all constraints *)
  let constraints_vars =
    List.flatten_map Constraints.extract_variables_of_cstr (Constraints_generation.constraints_of_generated_constraints generated_constraints)

  (* Extract variables from the optimization function *)
  and optimization_function_vars =
    match optimization_function with
    | Satisfy -> []
    | Maximize (expr)
    | Minimize (expr) -> Constraints.extract_variables_of_expr expr

  in

  (* Put all the variables together, remove doubles *)
  List.unique (constraints_vars @ optimization_function_vars)



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



