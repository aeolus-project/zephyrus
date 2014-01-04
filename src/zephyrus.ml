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
)

(* === Handling the arguments === *)
let () = Load_settings.load ();
  Zephyrus_log.log_settings ()


(* === load everything  === *)
let () = Load_model.set_initial_model_of_settings ();
  (* Load_model.set_initial_model_of_benchmark (new Benchmarks.Master_worker.create 10 Benchmarks.Master_worker.Machine_park_100s Benchmarks.Master_worker.One_worker_type); *)
  Zephyrus_log.log_stage_new "LOAD SECTION";
  let u = check_option "universe"              !Data_state.universe_full in
  let c = check_option "initial configuration" !Data_state.initial_configuration_full in
  
  (* If no-solving mode was chosen: *)
  (* Remark: I've put this as early as possible in order to work also when specification and optimisation function are not given. *)
  if Settings.find Settings.mode = Settings.Mode_no_solving
  then
    let final_configuration = c in
    Zephyrus_log.log_data "FINAL CONFIGURATION ==>\n" (lazy ((Json_of.configuration u final_configuration) ^ "\n"));
    List.iter (fun (kind, filename) -> print_to_file kind filename u final_configuration) (Settings.find Settings.results);
    print_string "\n\n\n <==========> THE END <==========>  \n\n"
  else

  let s = check_option "specification"         !Data_state.specification_full in
  let f = check_option "optimization function" !Data_state.optimization_function in

  (* Validation *)
  Zephyrus_log.log_stage_new "MODEL VALIDATION";
  let validation_results = Validate.standard_model_check u c s in
  Zephyrus_log.log_stage_end ();

  (* If validation mode was chosen:*)
  if Settings.find Settings.mode = Settings.Mode_validate_initial_config 
  then
    let initial_configuration_validation_errors = Validate.filter_initial_configuration_validation_errors validation_results in
    if initial_configuration_validation_errors = []
    then Printf.printf "\nInitial configuration has passed all checks and is completely valid!\n"
    else 
      Printf.printf "\nInitial configuration validation errors:\n";
      List.iter (fun validation_result -> Printf.printf "%s\n%!" (Validate.String_of.validation_result validation_result)) initial_configuration_validation_errors
  else

  let keep_initial_configuration = match f with Optimization_function_conservative -> true | _ -> false in
  let preprocess_solver = Solvers.of_settings Solvers.Main (* Solvers.Preprocess *) in
  let main_solver = Solvers.of_settings Solvers.Main in
  Zephyrus_log.log_data "\nINITIAL CONFIGURATION ==>\n" (lazy (Json_of.configuration u c));
  Zephyrus_log.log_data "\nSPECIFICATION ==> " (lazy (String_of.specification s));
  Zephyrus_log.log_data "\nOPTIMIZATION FUNCTION ==> " (lazy ((String_of.model_optimization_function f) ^ "\n\n\n"));
  Zephyrus_log.log_stage_end ();


  (* TEST: Output a CUDF file for each repository. *)
  (*
  Repository_id_set.iter (fun repository_id ->
    Json_binpacking_problem_of.conflicts_of_repository u repository_id
  ) u#get_repository_ids;
  *)

  (* === Perform the trimming === *)
  Zephyrus_log.log_stage_new "TRIMMING SECTION";
  (* ====  Trim the universe ==== *)
  Zephyrus_log.log_stage_new "TRIMMING UNIVERSE";
  Zephyrus_log.log_execution "Trimming component types...";
  let universe_trimmed_component_types = Trim.trim_component_types u c s in
  Zephyrus_log.log_execution " ok\n";
  (* print_string (Json_of.universe universe_trimmed_component_types r); *)
  Zephyrus_log.log_execution "Trimming repositories...";
  let universe_trimmed_package         = Trim.trim_repositories universe_trimmed_component_types  c s in
  Zephyrus_log.log_execution " ok\n";
  Zephyrus_log.log_data "TRIMMED UNIVERSE ==>\n" (lazy ((Json_of.universe universe_trimmed_package) ^ "\n"));

  (* TODO: we should never re-assign variables in Data_state *)
  Data_state.universe_full := Some(universe_trimmed_package);
  Zephyrus_log.log_stage_end ();

  
  (* ==== Compute bounds ==== *)
  Zephyrus_log.log_stage_new "TRIMMING BOUNDS";
  let u = universe_trimmed_package in
  let cat = Location_categories.full_categories u c in (* WaC <- may not work *)

  let sol = match Variable_bounds.get_initial_mins preprocess_solver u s (Location_categories.domain cat) with (* WaC <- may not work *)
  | None -> Zephyrus_log.log_panic "The specification does not have a solution. Exiting."
  | Some(sol) -> let (mp, mt) =  Variable_bounds.core_solution sol in
    Zephyrus_log.log_data "\nCORE LOWER BOUNDS ==>\n" (lazy ("  ports " ^ (String_of.port_id_map string_of_int mp) ^ "\n  types " ^ (String_of.component_type_id_map string_of_int mt) ^ "\n"));
    sol in
  let fu = Variable_bounds.create u in
  Variable_bounds.add_bound_min_all sol fu;
  Zephyrus_log.log_data "FLAT UNIVERSE INIT ==>\n" (lazy ((Variable_bounds.to_string fu) ^ "\n\n"));
  Variable_bounds.propagate_lower_bound fu;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH LOWER BOUNDS PROPAGATED ==>\n"  (lazy ((Variable_bounds.to_string fu) ^ "\n\n"));
  Variable_bounds.propagate_conflicts fu;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH CONFLICTS PROPAGATED ==>\n" (lazy ((Variable_bounds.to_string fu) ^ "\n\n"));
  Variable_bounds.propagate_upper_bound fu;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH UPPER BOUNDS PROPAGATED ==>\n" (lazy ((Variable_bounds.to_string fu) ^ "\n\n"));
  (* Variable_bounds.finalize_bound_roots fu; *)
  Variable_bounds.mirror_bounds fu;
  Variable_bounds.minimize_upper_bound fu;
  Zephyrus_log.log_data "FLAT UNIVERSE WITH UPPER BOUNDS MINIMIZED ==>\n" (lazy ((Variable_bounds.to_string fu) ^ "\n\n"));

  Data_state.constraint_variable_bounds := Some(Variable_bounds.variable_bounds c#get_location fu);  
  Zephyrus_log.log_stage_end ();


  (* ====  Trim locations ==== *)
  Zephyrus_log.log_stage_new "TRIMMING LOCATIONS";
  Zephyrus_log.log_data "INITIAL CATEGORIES ==> " (lazy ((String_of.location_categories cat) ^ "\n"));
  
  let cat' = Variable_bounds.trim_categories cat fu in
  Zephyrus_log.log_data "CATEGORIES FIRST TRIM ==> " (lazy ((String_of.location_categories cat') ^ "\n"));
   
  let cat'' = match Location_bound.fit_categories preprocess_solver (u,c,s) cat' with
      | None -> Location_categories.empty
      | Some(cat') -> cat' in
  Zephyrus_log.log_data "CATEGORIES FULLY TRIMMED ==> " (lazy ((String_of.location_categories cat') ^ "\n\n"));
  Zephyrus_log.log_execution "\nTrimming configuration...";
  let domain_init = Location_categories.domain cat'' in
  let domain = if keep_initial_configuration then Trim.transitive_closure_domain c domain_init else domain_init in
  let (core_conf, annex_conf_init) = Trim.configuration c domain in
  let annex_conf = if keep_initial_configuration then annex_conf_init else Trim.empty annex_conf_init in
  Zephyrus_log.log_execution " ok\n";  
  Zephyrus_log.log_data "TRIMMED CONFIGURATION ==>\n" (lazy ((Json_of.configuration u core_conf) ^ "\n\n"));
(*Printf.printf "initial configuration = %s\n"  (Json_of.configuration u c);
  Printf.printf "core    configuration = %s\n"  (Json_of.configuration u core_conf); *)
  (if not (Settings.find Settings.modifiable_configuration) then Zephyrus_log.log_data "ANNEX CONFIGURATION ==>\n"  (lazy ((Json_of.configuration u annex_conf) ^ "\n\n")));
  
  (* TODO: we should never re-assign variables in Data_state *)
  Data_state.initial_configuration_full := Some(core_conf);
  Zephyrus_log.log_stage_end ();
  Zephyrus_log.log_stage_end ();



(* === Generate and solve the main constraint === *)
  Zephyrus_log.log_stage_new "CONSTRAINT SECTION";

  Constraint_of.universe_full ();
  Constraint_of.specification_full ();
  Constraint_of.configuration_full ();
  Constraint_of.optimization_function_full ();
  Location_categories.generate_categories ();
  let cat_constraint = Location_categories.generate_constraint () in
  let solver_input_k = ("  category = ", cat_constraint)::(Data_state.get_constraint_full ()) in
  Zephyrus_log.log_data "ALL CONSTRAINTS ==>\n" (lazy ((String_of.described_konstraint_list solver_input_k) ^ "\n\n"));
  let solver_input_f = Data_state.get_constraint_optimization_function () in
  Zephyrus_log.log_data "OPTIMIZATION FUNCTION ==>\n" (lazy ((String_of.constraint_optimization_function solver_input_f) ^ "\n\n"));

  Zephyrus_log.log_stage_end ();
  Zephyrus_log.log_stage_new "SOLVING SECTION";

  match main_solver solver_input_k solver_input_f with
  | None -> Zephyrus_log.log_panic "no solution for the given input"
  | Some(solution) -> (
    Zephyrus_log.log_data "SOLUTION ==>\n" (lazy ((String_of.solution (fst solution)) ^ "\n"));

    let partial_final_configuration = Configuration_of.solution u core_conf (fst solution) in
(*  Printf.printf "\nPartial Final Configuration\n\n%s" (Json_of.configuration u partial_final_configuration r); *)
    let final_configuration = if Settings.find Settings.modifiable_configuration then partial_final_configuration else Configuration_of.merge annex_conf partial_final_configuration in
    Zephyrus_log.log_data "FINAL CONFIGURATION ==>\n" (lazy ((Json_of.configuration u final_configuration) ^ "\n"));
    
    List.iter (fun (kind, filename) -> print_to_file kind filename u final_configuration) (Settings.find Settings.results)
  );

  print_string "\n\n\n <==========> THE END <==========>  \n\n"


(*
(* test the database *)
(* TODO: Remove this from here! *)
module Database_test = struct
  open Data_common_database.Database
  module DBBase = struct
    type 'a column = int
    type key = int
    let compare = (-)
  end

  module DBBool = struct
    include DBBase
    type t = bool
    let name : bool column = 1
  end

  module DBString = struct
    include DBBase
    type t = string
    let name : string column = 2
  end

  module T = Table.AddOptional(Table.WithoutAggregate(Table.WithDefaultValue(Table.WithoutChecking((DBBool)))(struct let default = false end))) (
             Table.AddOptional(Table.WithoutAggregate(Table.WithDefaultValue(Table.WithoutChecking((DBString)))(struct let default = "no one" end))) (
               Data_common.Database.Table.Empty(struct include DBBase type t = key end)))
  let () = 
    let table = T.create 5 in
      print_string "step 1\n"; flush stdout;
      T.add table 0;
      print_string "step 2\n"; flush stdout;
      T.add_to_column table DBString.name 0 "is_working? ";
      print_string "step 3\n"; flush stdout;
      T.add_to_column table DBBool.name 0 true;
      print_string "step 4\n"; flush stdout;
      print_string ((T.find table DBString.name 0) ^ (string_of_bool (T.find table DBBool.name 0)) ^ "\n");
      print_string "step 5\n"; flush stdout;
      T.add table 1;
      print_string "step 6\n"; flush stdout;
      print_string ((T.find table DBString.name 1) ^ (string_of_bool (T.find table DBBool.name 1)) ^ "\n");
      print_string "step 7\n"; flush stdout
  end
*)


(*
(* test the graph *)
(* TODO: Remove this from here! *)
module Test_graph_data = struct type t = int end
module Test_graph = Data_common_graph.Graph.Make(Test_graph_data)(Test_graph_data)
let () =
  let g = Test_graph.create () in
(*                               *)
(*              +----+           *)
(*              v    |           *)
(*            +-O-+  |           *)
(*           5|   |10|           *)
(*            v   v  |           *)
(*        1-->2   3  |           *)
(*          6 |7  |8 |           *)
(*            +>4<+  |9          *)
(*              |    |           *)
(*              +----+           *)
(*                               *)
(*                               *)
  let v1 = (Test_graph.add_vertice 1 g) in (* 1 *)
  let v2 = (Test_graph.add_vertice 2 g) in (* 2 *)
  let v0 = (Test_graph.add_vertice 0 g) in (* 3 *)
  let v3 = (Test_graph.add_vertice 3 g) in (* 4 *)
  let v4 = (Test_graph.add_vertice 4 g) in (* 5 *)
  (* order = 1 2 3 4 5 *)

  Pervasives.ignore (Test_graph.add_edge v0  5 v2 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v1  6 v2 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v2  7 v4 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v3  8 v4 g); (* order = 1 3 2 4 5 *)
  Pervasives.ignore (Test_graph.add_edge v4  9 v0 g); (* merge { 2 3 5 } ; order = 1 (2 3 5) 4 *)
  Pervasives.ignore (Test_graph.add_edge v0 10 v3 g)  (* merge { 4 (2 3 5) }; order = 1 (2 3 4 5) *)

(* must add a to_string in dot format in the generic graph *)
*)
