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

(* for now, I just trigger the compilation of all files with the opening of the different key modules *)

open Input_helper

open Load_settings
open Data_model
open Load_model
open Constraint_of
open Solvers
open Json_of
open Variable_bounds
open Location_bound

let check_option desc o = match o with
  | Some(a) -> a
  | None -> Zephyrus_log.log_panic ("The element \"" ^ desc ^ "\" is not set")

(* === Handling the arguments === *)
let () = Load_settings.load ();
  Zephyrus_log.log_settings (Settings.string_of_settings ())
(* === Set up everything === *)

let () =
(* === load everything  === *)
  Load_model.set_initial_model_of_settings ();
  Zephyrus_log.log_stage_new "LOAD SECTION";
  let r = check_option "resources"             !Data_state.resources_full in
  let u = check_option "universe"              !Data_state.universe_full in
  let c = check_option "configuration"         !Data_state.initial_configuration_full in
  let s = check_option "specification"         !Data_state.specification_full in
  let f = check_option "optimization function" !Data_state.optimization_function in
  let keep_initial_configuration = match f with Optimization_function_conservative -> true | _ -> false in
  let preprocess_solver = Solvers.of_settings Solvers.Preprocess in
  let main_solver = Solvers.of_settings Solvers.Main in
  Zephyrus_log.log_data "\n\n\n  ==> INITIAL CONFIGURATION <== \n\n" (lazy (Json_of.configuration_string c u r));
  Zephyrus_log.log_data "\n\n\n     ==> SPECIFICATION <==      \n\n" (lazy (String_of.specification s));
  Zephyrus_log.log_data "\n\n\n ==> OPTIMIZATION FUNCTION <==  \n\n" (lazy (String_of.model_optimization_function f));

(* === Perform the trimming === *)
  Zephyrus_log.log_execution "\nTrimming component types...";
  let universe_trimmed_component_types = Trim.trim_component_types u c s in
  Zephyrus_log.log_execution " ok";
  (* print_string (Json_of.universe_string universe_trimmed_component_types r); *)
  Zephyrus_log.log_execution "\nTrimming repositories...";
  let universe_trimmed_package         = Trim.trim_repositories universe_trimmed_component_types  c s in
  Zephyrus_log.log_execution " ok";
  Zephyrus_log.log_data "\n" (lazy ((Json_of.universe_string universe_trimmed_package r) ^ "\n"));

  (* TODO: we should never re-assign variables in Data_state (or in Settings) *)
  Data_state.universe_full := Some(universe_trimmed_package);


  let u = universe_trimmed_package in

  let cat = Location_categories.full_categories r u c in
  Zephyrus_log.log_data "\n\n\n     ==> INITIAL CATEGORIES <==  \n\n" (lazy (String_of.location_categories cat));
  
  let cat' = ( cat ) (* match Variable_bounds.get_initial_mins preprocess_solver u s (Location_categories.domain cat) with
  | None -> Zephyrus_log.log_panic "The specification does not have a solution. Exiting."
  | Some(sol) -> let fu = Variable_bounds.create u in print_string "flat universe created";
    Variable_bounds.add_bound_min_all sol fu; (* <- should fail, as the domain of universe is not set right *) print_string "my pont exactly";
    Variable_bounds.propagate_lower_bound fu; Variable_bounds.propagate_conflicts fu; Variable_bounds.propagate_upper_bound fu;
    Variable_bounds.finalize_bound_roots fu; Variable_bounds.minimize_upper_bound fu;
    (* TODO: we should never re-assign variables in Data_state (or in Settings) *)
    Data_state.constraint_variable_bounds := Some(Variable_bounds.variable_bounds c#get_location fu);  
    Variable_bounds.trim_categories cat fu *) in (* TODO: debug Graph operations. *)
    
  Zephyrus_log.log_data "\n\n\n     ==> CATEGORIES FIRST TRIM <==  \n\n" (lazy (String_of.location_categories cat'));
    
  let cat'' = match Location_bound.fit_categories preprocess_solver (r#resource_ids,u,c,s) cat' with
      | None -> Location_categories.empty
      | Some(cat') -> cat' in
  Zephyrus_log.log_data "\n\n\n     ==> CATEGORIES FULLY TRIMMED <==  \n" (lazy (String_of.location_categories cat'));
  Zephyrus_log.log_execution "\nTrimming configuration...";
  let domain_init = Location_categories.domain cat'' in
  let domain = if keep_initial_configuration then Trim.transitive_closure_domain c domain_init else domain_init in
  let (core_conf, annex_conf_init) = Trim.configuration c domain in
  let annex_conf = if keep_initial_configuration then annex_conf_init else Trim.empty annex_conf_init in
  Zephyrus_log.log_data "\n\n\n  ==> TRIMMED CONFIGURATION <== \n\n" (lazy (Json_of.configuration_string core_conf u r));
  Printf.printf "initial configuration = %s\n"  (Json_of.configuration_string c u r);
  Printf.printf "core    configuration = %s\n"  (Json_of.configuration_string core_conf u r);
  print_string ("annex   configuration = " ^ (Json_of.configuration_string annex_conf u r) ^ "\n");
  
  (* TODO: we should never re-assign variables in Data_state (or in Settings) *)
  Data_state.initial_configuration_full := Some(core_conf);

  Zephyrus_log.log_stage_end ();
  Zephyrus_log.log_stage_new "CONSTRAINT SECTION";

  Constraint_of.universe_full ();
  Constraint_of.specification_full ();
  Constraint_of.configuration_full ();
  Constraint_of.optimization_function_full ();
  Location_categories.generate_categories ();
  let cat_constraint = Location_categories.generate_constraint () in

  Zephyrus_log.log_stage_end ();
  Zephyrus_log.log_stage_new "SOLVING SECTION";

  let solver_input_k = ("  category = ", cat_constraint)::(Data_state.get_constraint_full ()) in
  Zephyrus_log.log_data "All constraints" (lazy (String_of.described_konstraint_list solver_input_k));
  let solver_input_f = Data_state.get_constraint_optimization_function () in
  Zephyrus_log.log_data "Optimization function" (lazy (String_of.constraint_optimization_function solver_input_f));
  match main_solver solver_input_k solver_input_f with
  | None -> Zephyrus_log.log_panic "no solution for the given input"
  | Some(solution) -> (
    Printf.printf "=== SOLUTION ===\n%s\n" (String_of.solution (fst solution));

    let partial_final_configuration = Configuration_of.solution u core_conf (fst solution) in
    let final_configuration = Configuration_of.merge annex_conf partial_final_configuration in
    Printf.printf "\nPartial Final Configuration\n\n%s" (Json_of.configuration_string partial_final_configuration u r);
    Printf.printf "\nFinal Configuration\n\n%s" (Json_of.configuration_string final_configuration u r);
    
(*
    Printf.printf "\nLocation domain of the final configuration = %s\n" (String_of.location_id_set final_configuration#get_location_ids);
    Printf.printf "\nLocation names of the final configuration = %s\n" (String_of.location_name_set final_configuration#get_location_names);


    let final_configuration = 
      let solution = fst solution in
      match !Data_state.universe_full with
      | None -> Printf.printf "\nZephyrus is proud to announce you, that the universe does not exist!...\n"; None
      | Some universe -> 
        begin
          match !Data_state.initial_configuration_full with
          | None -> Printf.printf "\nZephyrus is proud to announce you, that the initial configuration does not exist!...\n"; None
          | Some initial_configuration -> 
              let final_configuration = Configuration_of.solution universe initial_configuration solution in
              begin
                match !Data_state.resources_full with
                | None           -> Printf.printf "\nZephyrus is proud to announce you, that resources are not set!...\n"
                | Some resources -> Printf.printf "\nFinal Configuration\n\n%s" (Json_of.configuration_string final_configuration universe resources) (* (String_of.configuration universe final_configuration) *)
              end;
              Some(final_configuration)
        end
  in*)

  print_string "\n\n\n <==========> THE END <==========>  \n\n")

(*
(* just a test! *)
let my_universe =
  Input_helper.parse_json Json_j.read_universe Settings.input_file_universe

let () =
  match my_universe with
  | None -> Printf.printf "\nZephyrus is proud to announce you, that the universe does not exist!...\n" 
  | Some json_universe -> Printf.printf "\nThe universe:\n%s\n" (Yojson.Safe.prettify (Json_j.string_of_universe json_universe))

(* just a test! *)
let my_specification =
  Input_helper.parse_standard Specification_parser.main Specification_lexer.token Settings.input_file_specification

let () =
  Load_model.load_model ();
  match !Data_state.universe_full with
  | None -> Printf.printf "\nZephyrus is proud to announce you, that the universe does not exist!...\n" 
  | Some u -> ()
*)
