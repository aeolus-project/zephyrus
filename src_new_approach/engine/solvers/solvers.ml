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
  bounds                : Data_constraint.variable_bounds;
  input_file            : string;
  output_file           : string;
  keep_input_file       : bool;
  keep_output_file      : bool;  
}


module type SOLVER = sig
  val solve : solver_settings ->
      (string * Data_constraint.konstraint) list ->
      Data_constraint.optimization_function ->
      (Data_constraint.solution * (int list)) (* It returns the solution and its cost. *)
end


(* 1. Generic extension of a one-optimum solver to an n-optimum solver *)

let solve_lexicographic settings preprocess solve_step postprocess c f = 
  let initial_data = preprocess settings c f in
  let rec iterative_solve data f = match f with
    | Data_constraint.Lexicographic(f1::l1) -> 
      let (data , solution , costs ) = iterative_solve data f1 in
      let (data', solution', costs') = iterative_solve data (Data_constraint.Lexicographic(l1)) in 
      (data', solution', costs @ costs')
    | _ -> let (solution, cost) = solve_step settings data f in (postprocess data f cost, solution, [cost]) in
  let (_, solution, costs) = iterative_solve initial_data f in (solution, costs)


(* 2. generic minizinc handling *)


module MiniZinc_generic = struct
  open Data_constraint
  open Minizinc

  let solver : Engine_helper.program ref = ref Engine_helper.gecode_minizinc_solver
  let input  : Engine_helper.file ref = ref Engine_helper.file_default
  let output : Engine_helper.file ref = ref Engine_helper.file_default

  let preprocess settings c f =
    Zephyrus_log.log_solver_execution ("Checking if required programs are available... ");
    if Engine_helper.program_is_available !solver then (
      Zephyrus_log.log_solver_execution "yes\n";
      Zephyrus_log.log_solver_execution ("Preparing file names for minizinc and solution storage...\n");
      input  := Engine_helper.file_process_name settings.input_file;
      output := Engine_helper.file_process_name settings.output_file;
      Zephyrus_log.log_solver_execution ("Preparing variables for MiniZinc translation...\n");
      let v_map = get_named_variables c f in
(*      Zephyrus_log.log_solver_data "Minizinc Variables" (lazy (string_of_named_variables v_map));*)
      Zephyrus_log.log_solver_execution ("Translating constraints into MiniZinc...\n");
      let res = core_translation v_map settings.bounds c in
      Zephyrus_log.log_solver_data "Minizinc main constraints" (lazy res.mzn_main_constraint); res
    ) else (
      Zephyrus_log.log_solver_execution ("no\n");
      Zephyrus_log.log_panic "the constraint solver cannot be found. Aborting execution\n")

  let solve_step settings data f =
    let mzn_constraint = add_optimization_goal data f in
    Zephyrus_log.log_solver_execution "Printing the MiniZinc constraints to the file...\n";
    let filename_input = Engine_helper.file_print settings.keep_input_file !input mzn_constraint in
    Zephyrus_log.log_solver_execution "Creating the file for the solution...\n";
    let filename_output = Engine_helper.file_create settings.keep_output_file !output in
    Zephyrus_log.log_solver_execution "Solving the constraints...\n";
    let status = Engine_helper.program_sync_exec !solver [filename_input; filename_output] in
    if not (Engine_helper.program_did_exit_ok status) then
      Zephyrus_log.log_panic "Error during the solving of the constraint. Aborting execution\n"
    else (
      Zephyrus_log.log_solver_execution "Getting the solution found by the flatzinc solver...\n";
      let solution_tmp = Flatzinc_solution_parser.main Flatzinc_solution_lexer.token (Lexing.from_channel (open_in filename_output)) in
(*      Zephyrus_log.log_solver_data "The solution:" (lazy (String_of.string_map int_of_string solution_tmp));*)
      let cost = Name_map.find cost_variable_name solution_tmp in
      let minizinc_solution = Variable_set.fold (fun v res -> Variable_map.add v (Name_map.find (data.mzn_variables#get_name v) solution_tmp) res)
          data.mzn_variables#variables Variable_map.empty
      in
      let solution =
        let domain = data.mzn_variables#variables 
        and variable_values = (fun v -> try Data_constraint.Variable_map.find v minizinc_solution with Not_found -> -1)
        in {
          Data_constraint.domain          = domain;
          Data_constraint.variable_values = variable_values;
        } in
      (solution, cost)
    )

  let postprocess data f cost = match f with
    | Data_constraint.Minimize(e) -> add_extra_constraint data e cost
    | Data_constraint.Maximize(e) -> add_extra_constraint data e cost
    | _ -> data
    (* Kuba: Ad-hoc bug correction, I don't know if it's what should be done, but it works... *)
    (* | _ -> raise Wrong_optimization_function *)
end

(* 3. Main Modules *)

module G12 : SOLVER = struct
  let solve settings cs f = MiniZinc_generic.solver := Engine_helper.g12_minizinc_solver;
    solve_lexicographic settings MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess cs f    
end

module GeCode : SOLVER = struct
  let solve settings cs f = MiniZinc_generic.solver := Engine_helper.gecode_minizinc_solver;
    solve_lexicographic settings MiniZinc_generic.preprocess MiniZinc_generic.solve_step MiniZinc_generic.postprocess cs f    
end



