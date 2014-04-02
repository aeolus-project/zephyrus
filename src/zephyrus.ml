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

(** Zephyrus main module. *)

(** {i
    Aeolus, the Keeper of the Winds, gives Odysseus a tightly closed bag
    full of the captured winds so he could sail easily home to Ithaca on
    the gentle west wind.
    }

    Zephyrus in the Greek mythology is one of the Anemoi and the god of the west wind.
*)

(* for now, I just trigger the compilation of all files with the opening of the different key modules *)

open Input_helper

open Load_settings
open Data_model
open Load_model
open Constraint_of
open Solvers
open Variable_bounds
open Location_bound
open Dot_of

(* Latest version of JSON syntax is used for internal printing and as a default. *)
module Json_of = Json_v1_of

let check_option desc o = match o with
  | Some(a) -> a
  | None -> Zephyrus_log.log_panic ("The element \"" ^ desc ^ "\" is not set")

let print_to_file kind filename u c = Output_helper.print_output filename (
  match kind with
  | Settings.Out_file_plain              -> String_of.configuration                                 u c
  | Settings.Out_file_json               -> Json_v0_of.configuration (* Json_of.configuration *)    u c (* For a while we will use the v0 by default for backwards compatibility. *)
  | Settings.Out_file_json_v0            -> Json_v0_of.configuration                                u c
  | Settings.Out_file_json_v1            -> Json_v1_of.configuration                                u c
  | Settings.Out_file_graph_deployment   -> Dot_of.configuration Dot_of.Deployment_graph            u c
  | Settings.Out_file_graph_simplified   -> Dot_of.configuration Dot_of.Simplified_deployment_graph u c
  | Settings.Out_file_graph_components   -> Dot_of.configuration Dot_of.Components_graph            u c
  | Settings.Out_file_graph_packages     -> Dot_of.configuration Dot_of.Packages_graph              u c
  | Settings.Out_file_binpacking_problem -> Json_binpacking_problem_of.configuration                u c
  | Settings.Out_file_statistics         -> Data_statistics.to_string ()
)


(* === Handling the arguments === *)
let () = 
  Load_settings.load ();
  Zephyrus_log.log_settings ()


(* === load everything  === *)
let () = 

  (* Handle benchmarks *)
  let (benchmark : Benchmarks.benchmark option) = Benchmarks.create_benchmark_of_benchmark_settings (Settings.find Settings.benchmark) in

  let (catalog, initial_model) =
    (match benchmark with
    | None           -> Load_model.initial_model_of_settings ()
    | Some benchmark -> Load_model.initial_model_of_benchmark benchmark) in

  Name_of.set_catalog (Some catalog);

  Zephyrus_log.log_stage_new "LOAD SECTION";

  (* In every mode we need at least the universe and an initial configuration. *)
  let universe              = check_option "universe"              initial_model.universe in
  let initial_configuration = check_option "initial configuration" initial_model.initial_configuration in
  
  (* If ==> NO-SOLVING MODE <== was chosen: *)
  if Settings.find Settings.mode = Settings.Mode_no_solving
  then
    let final_configuration = initial_configuration in
    Zephyrus_log.log_data "FINAL CONFIGURATION ==>\n" (lazy ((Json_of.configuration universe final_configuration) ^ "\n"));
    List.iter (fun (kind, filename) -> print_to_file kind filename universe final_configuration) (Settings.find Settings.results);
    print_string "\n\n\n <==========> THE END <==========>  \n\n"
  else

  (* In validation mode we need also to have a specification. *)
  let specification = check_option "specification" initial_model.specification in

  (* Validation *)
  Zephyrus_log.log_stage_new "MODEL VALIDATION";
  let validation_results = Validate.standard_model_check universe initial_configuration specification in
  Zephyrus_log.log_stage_end ();

  (* If ==> VALIDATION MODE <== was chosen: *)
  if Settings.find Settings.mode = Settings.Mode_validate_initial_config 
  then
    if Validate.validation_passed validation_results (* Were all validation tests passed successfuly? *)
    then ( (* There were no errors, all the tests were passed. Return success. *)
      Printf.printf "\nInitial configuration has passed all checks and is completely valid!\n";
      ignore (exit 0) )
    else ( (* There were some errors. Print them and return failure. *)
      (* Extrect errors from the validation results. *)
      let initial_configuration_validation_errors = Validate.validation_results_filter_errors validation_results in
      Printf.printf "\nInitial configuration validation errors:\n";
      List.iter (fun validation_result -> Printf.printf "%s\n%!" (Validate.String_of.validation_result validation_result)) initial_configuration_validation_errors;
      ignore (exit 1) )
  else
  
  (* In the classic mode we also need an optimization function. *)
  let optimization_function = check_option "optimization function" initial_model.optimization_function in

  (* If we've got this far, we are in the ==> CLASSIC MODE <==. *)

  (* Set some initial parameters. *)
  let keep_initial_configuration = 
    match optimization_function with 
    | Optimization_function_conservative -> true 
    | _                                  -> false in

  let preprocess_solver = Solvers.of_settings Solvers.Main (* Solvers.Preprocess *) in
  let main_solver       = Solvers.of_settings Solvers.Main in

  (* Print the initial data. *)
  Zephyrus_log.log_data "\nINITIAL CONFIGURATION ==>\n" (lazy (Json_of.configuration universe initial_configuration));
  Zephyrus_log.log_data "\nSPECIFICATION ==> "          (lazy (String_of.specification specification));
  Zephyrus_log.log_data "\nOPTIMIZATION FUNCTION ==> "  (lazy ((String_of.model_optimization_function optimization_function) ^ "\n\n\n"));
  
  (* End LOAD SECTION *)
  Zephyrus_log.log_stage_end ();


  (* === Perform the trimming === *)
  Zephyrus_log.log_stage_new "TRIMMING SECTION";

  (* ====  Trim the universe ==== *)
  Zephyrus_log.log_stage_new "TRIMMING UNIVERSE";
  
  Zephyrus_log.log_execution "Trimming component types...";
  let universe_trimmed_component_types = Trim.trim_component_types universe                         initial_configuration specification in
  Zephyrus_log.log_execution " ok\n";
  
  Zephyrus_log.log_execution "Trimming repositories...";
  let universe_trimmed_packages        = Trim.trim_repositories    universe_trimmed_component_types initial_configuration specification in
  Zephyrus_log.log_execution " ok\n";
  
  Zephyrus_log.log_data "TRIMMED UNIVERSE ==>\n" (lazy ((Json_of.universe universe_trimmed_packages) ^ "\n"));

  (* TODO: We should not reassign variables. *)
  let universe = universe_trimmed_packages in
  
  Zephyrus_log.log_stage_end ();

  
  (* ==== Compute bounds ==== *)
  Zephyrus_log.log_stage_new "TRIMMING BOUNDS";
  
  let cat = Location_categories.full_categories universe initial_configuration in (* WaC <- may not work *)
  let sol = 
    let initial_variable_bounds = Data_state.get_variable_bounds None in
    match Variable_bounds.get_initial_mins preprocess_solver initial_variable_bounds universe specification (Location_categories.domain cat) with (* WaC <- may not work *)
    | None     -> Zephyrus_log.log_panic "The specification does not have a solution. Exiting."
    | Some sol -> 
        let (mp, mt) = Variable_bounds.core_solution sol in
        Zephyrus_log.log_data "\nCORE LOWER BOUNDS ==>\n" (lazy 
          ("  ports " ^ (String_of.Name_string_of.port_id_map           string_of_int mp) ^ "\n" ^
           "  types " ^ (String_of.Name_string_of.component_type_id_map string_of_int mt) ^ "\n"));
        sol in

  let flat_universe = Variable_bounds.create universe in
  Variable_bounds.add_bound_min_all sol flat_universe;
  Zephyrus_log.log_data "FLAT UNIVERSE INIT ==>\n"                          (lazy ((Variable_bounds.to_string flat_universe) ^ "\n\n"));
  Variable_bounds.propagate_lower_bound flat_universe;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH LOWER BOUNDS PROPAGATED ==>\n"  (lazy ((Variable_bounds.to_string flat_universe) ^ "\n\n"));
  Variable_bounds.propagate_conflicts flat_universe;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH CONFLICTS PROPAGATED ==>\n"     (lazy ((Variable_bounds.to_string flat_universe) ^ "\n\n"));
  Variable_bounds.propagate_upper_bound flat_universe;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH UPPER BOUNDS PROPAGATED ==>\n"  (lazy ((Variable_bounds.to_string flat_universe) ^ "\n\n"));
  (* Variable_bounds.finalize_bound_roots flat_universe; *)
  Variable_bounds.mirror_bounds flat_universe;
  Variable_bounds.minimize_upper_bound flat_universe;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH UPPER BOUNDS MINIMIZED ==>\n"   (lazy ((Variable_bounds.to_string flat_universe) ^ "\n\n"));

  let constraint_variable_bounds = Some(Variable_bounds.variable_bounds initial_configuration#get_location flat_universe) in
  let variable_bounds = Data_state.get_variable_bounds (
    (* Use the computed variable bounds or not? *)
    if false
    then constraint_variable_bounds (* ERRORS! *) 
    else None 
  ) in
  Zephyrus_log.log_stage_end ();


  (* ====  Trim locations ==== *)
  Zephyrus_log.log_stage_new "TRIMMING LOCATIONS";
  Zephyrus_log.log_data "INITIAL CATEGORIES ==> " (lazy ((String_of.location_categories cat) ^ "\n"));
  
  let trim_cat cat =
    
    let cat' = Variable_bounds.trim_categories cat flat_universe in (* for each location category, only keep MIN(nb_location_in_cat, nb_max_component) *)
    Zephyrus_log.log_data "CATEGORIES FIRST TRIM ==> " (lazy ((String_of.location_categories cat') ^ "\n"));
     
    let cat'' = 
      match Location_bound.fit_categories (preprocess_solver variable_bounds) (universe, initial_configuration, specification) cat' with
      | None -> Location_categories.empty
      | Some(cat') -> cat' in
    Zephyrus_log.log_data "CATEGORIES FULLY TRIMMED ==> " (lazy ((String_of.location_categories cat') ^ "\n\n"));

    cat'' in

  Zephyrus_log.log_execution "\nTrimming configuration...";
  let domain_init = 
    Location_categories.domain (if Settings.find Settings.no_location_trimming then cat else trim_cat cat) in
  
  let domain = 
    if keep_initial_configuration 
    then Trim.transitive_closure_domain initial_configuration domain_init 
    else domain_init in

  let (core_conf, annex_conf_init) = Trim.configuration initial_configuration domain in

  (* NO MORE initial_configuration FROM THIS POINT ON *)

  let annex_conf = 
    if keep_initial_configuration
    then annex_conf_init
    else Trim.empty annex_conf_init in

  Zephyrus_log.log_execution " ok\n";  
  Zephyrus_log.log_data "TRIMMED CONFIGURATION ==>\n" (lazy ((Json_of.configuration universe core_conf) ^ "\n\n"));
(*  Printf.printf "initial configuration = %s\n"  (Json_of.configuration u initial_configuration); *)
(*  Printf.printf "core    configuration = %s\n"  (Json_of.configuration u core_conf); *)
  (if not (Settings.find Settings.modifiable_configuration) 
   then Zephyrus_log.log_data "ANNEX CONFIGURATION ==>\n"  (lazy ((Json_of.configuration universe annex_conf) ^ "\n\n")));
  
  Zephyrus_log.log_stage_end ();
  Zephyrus_log.log_stage_end ();



  (* === Prepare the constraint problem === *)
  Zephyrus_log.log_stage_new "CONSTRAINT SECTION";

  let with_packages =
    if Settings.find Settings.eliminate_packages
    then false
    else true in

  let constraint_universe              : Data_state.constraint_universe               = Constraint_of.universe_full              ~with_packages (Some universe)      (Some core_conf) in
  let constraint_specification         : Data_constraint.konstraint option            = Constraint_of.specification_full         ~with_packages (Some specification) (Some core_conf) in
  let constraint_configuration         : Data_constraint.konstraint list              = Constraint_of.configuration_full         ~with_packages (Some universe)      (Some core_conf) in
  let constraint_optimization_function : Data_constraint.optimization_function option = Constraint_of.optimization_function_full ~with_packages (Some universe)      (Some core_conf) (Some optimization_function) in

  let categories      : Location_categories.t      = Location_categories.generate_categories (Some universe) (Some core_conf) (Some optimization_function) in
  let cat_constraints : Data_state.structured_constraints = 
    let cat_constraint : Data_constraint.konstraint = Location_categories.generate_constraint (Settings.find Settings.eliminate_packages) (Some universe) categories in
    [("  category = ", [cat_constraint])] in
  
  let solver_input_structured_constraints : Data_state.structured_constraints = 
    (Data_state.get_constraint_full constraint_universe constraint_specification constraint_configuration)
    @ cat_constraints in

  Zephyrus_log.log_data "ALL CONSTRAINTS ==>\n" (lazy ((String_of.Name_string_of.structured_constraints solver_input_structured_constraints) ^ "\n\n"));

  let solver_input_optimization_function : Data_constraint.optimization_function = 
    Data_state.get_constraint_optimization_function constraint_optimization_function in

  Zephyrus_log.log_data "OPTIMIZATION FUNCTION ==>\n" (lazy ((String_of.constraint_optimization_function solver_input_optimization_function) ^ "\n\n"));

  Zephyrus_log.log_stage_end ();


  (* === Solve the constraint problem === *)
  Zephyrus_log.log_stage_new "SOLVING SECTION";

  let solution = main_solver variable_bounds solver_input_structured_constraints solver_input_optimization_function in

  match solution with
  | None -> Zephyrus_log.log_panic "no solution for the given input"
  | Some(solution) -> (
    Zephyrus_log.log_stage_end ();
    Zephyrus_log.log_data "SOLUTION ==>\n" (lazy ((String_of.Name_string_of.solution (fst solution)) ^ "\n"));

    if (Settings.find Settings.stop_after_solving)
    then
      Zephyrus_log.log_execution "Exiting after the solving phase, because the stop-after-solving option was specified!"
    else

    Zephyrus_log.log_stage_new "GENERATING FINAL CONFIGURATION SECTION";
    
    let final_configuration = 
      let partial_final_configuration = Configuration_of.solution (Some catalog) universe core_conf (fst solution) in
  (*  Printf.printf "\nPartial Final Configuration\n\n%s" (Json_of.configuration u partial_final_configuration r); *)
      if Settings.find Settings.modifiable_configuration
      then partial_final_configuration
      else merge_configurations annex_conf partial_final_configuration in
    
    Zephyrus_log.log_stage_end ();
    
    Zephyrus_log.log_data "FINAL CONFIGURATION ==>\n" (lazy ((Json_of.configuration universe final_configuration) ^ "\n"));

    (* Final configuration statistics *)
    Data_statistics.add "FinalConfigurationComponents" (Printf.sprintf "%d" (Component_id_set.cardinal final_configuration#get_component_ids));
    Data_statistics.add "FinalConfigurationBindings"   (Printf.sprintf "%d" (Binding_set     .cardinal final_configuration#get_bindings));

    (* Output the outputs *)
    List.iter (fun (kind, filename) -> print_to_file kind filename universe final_configuration) (Settings.find Settings.results);

    (* Validation *)
    Zephyrus_log.log_stage_new "FINAL CONFIGURATION VALIDATION";
    (* NOTE: We never expect the packages in the final configuration to pass the validation tests as some
             of them are often not generated (we usually assume that it is not useful to generate them all). *)
    let validation_results = Validate.standard_model_check ~with_packages:false universe final_configuration specification in
    if Validate.validation_passed validation_results
    then Zephyrus_log.log_execution "\nFinal configuration has passed all checks and is completely valid!\n"
    else begin
      let final_configuration_validation_errors = Validate.validation_results_filter_errors validation_results in
      Zephyrus_log.log_execution "\nFinal configuration validation errors:\n";
      List.iter (fun validation_result -> Zephyrus_log.log_execution (Printf.sprintf "%s\n%!" (Validate.String_of.validation_result validation_result))) final_configuration_validation_errors;
      ignore (exit 13)
    end;
    Zephyrus_log.log_stage_end ()

  );

  Zephyrus_log.log_execution "\n\n\n <==========> THE END <==========>  \n\n"
