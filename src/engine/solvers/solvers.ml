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
    - engine/Engine_helper
*)

(* TODO: replace the log_panic with option types, or exceptions *)

type solver_settings = {
  input_file            : string;
  output_file           : string;
  keep_input_file       : bool;
  keep_output_file      : bool;  
}

type t = Data_constraint.variable_bounds -> Data_state.structured_constraints -> Data_constraint.optimization_function -> (Data_constraint.solution * (int list)) option
type t_full = solver_settings -> t
      

module type SOLVER = sig
  val solve : t_full (* It returns the solution and its cost. *)
end


(* 1. Generic extension of a one-optimum solver to an n-optimum solver *)
exception No_solution (* to break the normal control flow of solution computation in case there are none. Too painful to deal with options *)

let solve_multi_objective settings bounds preprocess solve_step postprocess structured_constraints (multi_objective_solve_goal : Data_constraint.Multi_objective.solve_goal) = 

  let rec iterative_solve data (multi_objective_optimization : Data_constraint.Multi_objective.optimization) = 
    match multi_objective_optimization with

    (* If there are multiple optimizations to perform lexicographically, we perform the first one as a single optimization and then recursively the remaining ones. *)
    | Data_constraint.Multi_objective.Lexicographic(single_objective_optimization, remaining_optimization) -> 
      let (data' , solution  , costs  ) = iterative_solve data  (Data_constraint.Multi_objective.Single single_objective_optimization) in
      let (data'', solution'', costs'') = iterative_solve data' remaining_optimization in 
      (data'', solution'', costs @ costs'')

    (* If there is only a single objective, we can pass it to the single objective solving function. *)
    | Data_constraint.Multi_objective.Single single_objective_optimization -> 
      let single_objective_solve_goal = Data_constraint.Single_objective.Optimize single_objective_optimization in
      let solve_step_solution = solve_step settings data single_objective_solve_goal in
      match solve_step_solution with
      | None                 -> raise No_solution
      | Some(solution, cost) -> (postprocess data single_objective_solve_goal cost, solution, [cost]) in

  let initial_data = preprocess settings bounds structured_constraints multi_objective_solve_goal in
  
  try 
    match multi_objective_solve_goal with

    | Data_constraint.Multi_objective.Optimize multi_objective_optimization ->
      let (_, solution, costs) = iterative_solve initial_data multi_objective_optimization in 
      Some(solution, costs)

    | Data_constraint.Multi_objective.Satisfy -> 
      let solve_step_solution = solve_step settings initial_data Data_constraint.Single_objective.Satisfy in
      match solve_step_solution with
      | None              -> None
      | Some(solution, _) -> Some (solution, [])
      
  with 
    No_solution -> None


(* 2. generic minizinc handling *)

module MiniZinc_generic = struct
  open Data_constraint
  open Minizinc

  type solver = {
    is_solver_available : unit -> bool;
    solve_function : string -> string -> bool;
  }

  let make_solver_of_program (solver_program : Engine_helper.program) =
    let is_solver_available = (fun () -> Engine_helper.program_is_available solver_program) in
    let solve_function input_file output_file =
      let status = Engine_helper.program_sync_exec solver_program [input_file; output_file] in
      Engine_helper.did_program_exit_ok status in 
    {
      is_solver_available = is_solver_available;
      solve_function      = solve_function;
    }

  let make_portfolio_solver_of_programs (solver_programs : Engine_helper.program list) =
    let is_solver_available = (fun () -> Engine_helper.programs_are_available solver_programs) in
    let solve_function input_file output_file =
      Engine_helper.portfolio solver_programs (fun _ -> true) input_file output_file in
    {
      is_solver_available = is_solver_available;
      solve_function      = solve_function;
    }

  let solver : solver ref = ref (make_solver_of_program Engine_helper.gecode_minizinc_solver)

  let input  : Engine_helper.file    ref = ref Engine_helper.file_default
  let output : Engine_helper.file    ref = ref Engine_helper.file_default

  let preprocess settings bounds structured_constraints (solve_goal : Data_constraint.Multi_objective.solve_goal) =
    Zephyrus_log.log_solver_execution ("Checking if required programs are available... ");
    if (!solver).is_solver_available ()
    then 
      begin
        Zephyrus_log.log_solver_execution "yes\n";

        Zephyrus_log.log_solver_execution ("Preparing file names for minizinc and solution storage...\n");
        input  := Engine_helper.file_process_name settings.input_file;
        output := Engine_helper.file_process_name settings.output_file;

        Zephyrus_log.log_solver_execution ("Preparing variables for MiniZinc translation...");

        let vs : Variable_set.t = 
          List.fold_left (fun acc (_, ks) -> 
            List.fold_left (fun acc k -> 
              Variable_set.union (variables_of_konstraint k) acc
            ) acc ks
          ) (Data_constraint.Multi_objective.variables_of_solve_goal solve_goal) structured_constraints in

        Zephyrus_log.log_solver_execution ("  we have " ^ (string_of_int (Variable_set.cardinal vs)) ^ " variables\n");

        let v_map = get_named_variables vs in
    (*  Zephyrus_log.log_solver_data "Minizinc Variables" (lazy (string_of_named_variables v_map));*)

        Zephyrus_log.log_solver_execution ("Translating constraints into MiniZinc...\n");
        let res = core_translation v_map bounds structured_constraints in
        Zephyrus_log.log_solver_data "Minizinc main constraints" (lazy (res.mzn_main_constraint ^ "\n% end constraint\n"));
        res
      end
    else 
      begin
        Zephyrus_log.log_solver_execution (" no\n");
        Zephyrus_log.log_panic "the constraint solver cannot be found. Aborting execution\n"
      end

  let solve_step settings data (solve_goal : Data_constraint.Single_objective.solve_goal) =
    let mzn_constraint = add_optimization_goal data solve_goal in

    Zephyrus_log.log_solver_execution "Printing the MiniZinc constraints to the file...\n";
    let filename_input = Engine_helper.file_print settings.keep_input_file !input mzn_constraint in

    Zephyrus_log.log_solver_execution "Creating the file for the solution...\n";
    let filename_output = Engine_helper.file_create settings.keep_output_file !output in

    Zephyrus_log.log_solver_execution "Solving the constraints...\n";
    let status = (!solver).solve_function filename_input filename_output in

    if not status then
      Zephyrus_log.log_panic "Error during the solving of the constraint. Aborting execution\n"
    else
      Zephyrus_log.log_solver_execution "Getting the solution found by the flatzinc solver...\n";
      match Flatzinc_solution_parser.main Flatzinc_solution_lexer.token (Lexing.from_channel (open_in filename_output)) with
      | None -> None
      | Some solution_tmp -> (
     (* Zephyrus_log.log_solver_data "The solution:" (lazy (String_of.string_map string_of_int solution_tmp)); *)

        (* Note: the cost variable is always declared, so it will always have a value (even if the value is not optimised, thus has no sense). *)
        let cost = Name_map.find cost_variable_name solution_tmp in

        let solution =
          let domain = data.mzn_variables#variables in
         
          let minizinc_solution = 
            Variable_set.fold (fun v res -> 
              Variable_map.add v (Name_map.find (data.mzn_variables#get_name v) solution_tmp) res
            ) data.mzn_variables#variables Variable_map.empty in
         
          let variable_values = (fun v -> 
            try Data_constraint.Variable_map.find v minizinc_solution 
            with Not_found -> -1
          ) in

          {
            Data_constraint.domain          = domain;
            Data_constraint.variable_values = variable_values;
          } in 

        Some(solution, cost))

  let postprocess data (solve_goal : Data_constraint.Single_objective.solve_goal) cost = 
    let open Data_constraint.Single_objective in
    match solve_goal with
    | Optimize(Minimize(e)) -> add_extra_constraint data e cost
    | Optimize(Maximize(e)) -> add_extra_constraint data e cost
    | Satisfy               -> data
    
end

(* 3. Main Modules *)

module G12 : SOLVER = struct
  let solve settings bounds structured_constraints optimization_function = 
    MiniZinc_generic.solver := MiniZinc_generic.make_solver_of_program Engine_helper.g12_minizinc_solver;
    solve_multi_objective settings bounds MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess structured_constraints optimization_function    
end

module G12_cpx : SOLVER = struct
  let solve settings bounds structured_constraints optimization_function = 
    MiniZinc_generic.solver := MiniZinc_generic.make_solver_of_program Engine_helper.g12_cpx_minizinc_solver;
    solve_multi_objective settings bounds MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess structured_constraints optimization_function    
end

module GeCode : SOLVER = struct
  let solve settings bounds structured_constraints optimization_function = 
    MiniZinc_generic.solver := MiniZinc_generic.make_solver_of_program Engine_helper.gecode_minizinc_solver;
    solve_multi_objective settings bounds MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess structured_constraints optimization_function    
end

let make_custom_solver_module (solver_program : Engine_helper.program) =
  let module Solver = struct
    let solve settings bounds structured_constraints optimization_function = 
      MiniZinc_generic.solver := MiniZinc_generic.make_solver_of_program solver_program;
      solve_multi_objective settings bounds MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess structured_constraints optimization_function
    end
  in
  (module Solver : SOLVER)

let make_portfolio_solver_module (solver_programs : Engine_helper.program list) =
  let module Solver = struct
    let solve settings bounds structured_constraints optimization_function = 
      MiniZinc_generic.solver := MiniZinc_generic.make_portfolio_solver_of_programs solver_programs;
      solve_multi_objective settings bounds MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess structured_constraints optimization_function
    end
  in
  (module Solver : SOLVER)

(* Annex functions *)
type settings_kind = Preprocess | Main

let settings_of_settings kind =
  match kind with
  | Preprocess ->
    {
      input_file        = Settings.get_preprocess_input_file ();
      output_file       = Settings.get_preprocess_output_file ();
      keep_input_file   = Settings.find Settings.preprocess_keep_constraint_file;
      keep_output_file  = Settings.find Settings.preprocess_keep_constraint_file;
    }
  | Main ->
    {
      input_file        = Settings.get_main_input_file ();
      output_file       = Settings.get_main_output_file ();
      keep_input_file   = Settings.find Settings.solver_keep_constraint_file;
      keep_output_file  = Settings.find Settings.solver_keep_constraint_file;
    }

let full_of_settings kind = 
  let solver = 
    match kind with 
    | Preprocess -> Settings.find Settings.preprocess_solver
    | Main       -> Settings.find Settings.solver in
  match solver with
  | Settings.Solver_none      -> GeCode.solve (* default *)
  | Settings.Solver_gecode    -> GeCode.solve
  | Settings.Solver_g12       -> G12.solve
  | Settings.Solver_g12_cpx   -> G12_cpx.solve
  
  | Settings.Solver_portfolio -> 
      let module PortfolioSolver = (val (make_portfolio_solver_module [Engine_helper.g12_minizinc_solver; Engine_helper.g12_cpx_minizinc_solver; Engine_helper.gecode_minizinc_solver]) : SOLVER) in
      PortfolioSolver.solve

  | Settings.Solver_custom    -> (
      match Settings.get_custom_solver_command () with
      | None -> failwith "Cannot use a custom solver as the custom solver command is not specified!"
      | Some command ->
          let solver_program = Engine_helper.make_minizinc_solver_of_custom_flatzinc_solver_command command in
          let module CustomSolver = (val (make_custom_solver_module solver_program) : SOLVER) in
          CustomSolver.solve)

let of_settings kind = (full_of_settings kind) (settings_of_settings kind)

