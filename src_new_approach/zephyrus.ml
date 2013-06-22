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

let check_option desc o = match o with
  | Some(a) -> a
  | None -> Zephyrus_log.log_panic ("The element \"" ^ desc ^ "\" is not set")

let () = 
(* === Default settings === *)
  Settings.input_file_universe      := Some("./tests/u_1_new.json");
  Settings.input_file_initial_configuration := Some("./tests/ic_1.json");
  Settings.input_file_specification := Some("./tests/spec_1.spec");
  Settings.input_optimization_function := Some(Settings.Optim_simple);
  Settings.data_generation_universe               := Some(true);
  Settings.data_generation_repositories           := Some(true);
  Settings.data_generation_initial_configuration  := Some(true);
  Settings.data_generation_specification          := Some(true);
  Settings.data_generation_optimization_function  := Some(true);
  Settings.verbose_constraint_solver_activities   := Some(true);

(* === load everything  === *)
  Load_model.load_model ();
  print_string "\n ===============================";
  print_string "\n      ==> LOAD SECTION <==      \n";
  let r = check_option "resources" !Data_state.resources_full in
  let u = check_option "universe" !Data_state.universe_full in
  let c = check_option "configuration" !Data_state.initial_configuration_full in
  let s = check_option "specification" !Data_state.specification_full in
  let f = check_option "optimization function" !Data_state.optimization_function in
  print_string "\n        ==> UNIVERSE <==        \n\n";
  print_string (Json_of.universe_string u r);
  print_string "\n\n\n  ==> INITIAL CONFIGURATION <== \n\n";
  print_string (Json_of.configuration_string c u r);
  print_string "\n\n\n     ==> SPECIFICATION <==      \n\n";
  print_string (String_of.specification s);
  print_string "\n\n\n ==> OPTIMIZATION FUNCTION <==  \n\n";
  print_string (String_of.model_optimization_function f);

  print_string "\n ===============================";
  print_string "\n   ==> CONSTRAINT SECTION <==  \n";
  Constraint_of.universe_full ();
  Constraint_of.specification_full ();
  Constraint_of.configuration_full ();
  Constraint_of.optimization_function ();
  Constraint_of.basic_bounds ();
  print_string "\n        ==> UNIVERSE <==        \n\n";
  print_string ("  require  = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_component_type_require))) ^ "\n");
  print_string ("  provide  = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_component_type_provide))) ^ "\n");
  print_string ("  conflict = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_component_type_conflict))) ^ "\n");
  print_string ("  implem   = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_component_type_implementation))) ^ "\n");
  print_string ("  unicity  = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_binding_unicity))) ^ "\n");
  print_string ("  distrib1 = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_location_component_type))) ^ "\n");
  print_string ("  distrib2 = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_location_package))) ^ "\n");
  print_string ("  distrib3 = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_location_port))) ^ "\n");
  print_string ("  port_cal = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_definition_port))) ^ "\n");
  print_string ("  repo_1   = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_repository_unicity))) ^ "\n");
  print_string ("  repo_pac = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_repository_package))) ^ "\n");
  print_string ("  pack_dep = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_package_dependency))) ^ "\n");
  print_string ("  pack_pb  = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_package_conflict))) ^ "\n");
  print_string ("  resource = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_resource_consumption))) ^ "\n");
  print_string ("  detlete  = " ^ (String_of.konstraint (Data_constraint.And(!Data_state.constraint_universe_deprecated_element))) ^ "\n");
  print_string "\n\n\n     ==> SPECIFICATION <==      \n\n";
  print_string ((String_of.konstraint (check_option "specification constraint" !Data_state.constraint_specification_full)) ^ "\n");
  print_string "\n\n\n  ==> INITIAL CONFIGURATION <== \n\n";
  print_string ((String_of.konstraint (Data_constraint.And(!Data_state.constraint_configuration_full))) ^ "\n");
(*
let constraint_optimization_function : optimization_function option ref = ref None
let constraint_variable_bounds       : variable_bounds option ref = ref None
 *) 

  print_string "\n ===============================";
  print_string "\n    ==> SOLVING SECTION <==  \n";
  let solver_settings = {
      Solvers.bounds                = check_option "variable bounds" !Data_state.constraint_variable_bounds;
      Solvers.input_file            = "tests/minizinc/zephyrus-.mzn";
      Solvers.output_file           = "zephyrus-.sol";
      Solvers.keep_input_file       = true;
      Solvers.keep_output_file      = true  
    } in let solver_input = [
    ("  require  " , (Data_constraint.And(!Data_state.constraint_universe_component_type_require)));
    ("  provide  " , (Data_constraint.And(!Data_state.constraint_universe_component_type_provide)));
    ("  conflict " , (Data_constraint.And(!Data_state.constraint_universe_component_type_conflict)));
    ("  implem   " , (Data_constraint.And(!Data_state.constraint_universe_component_type_implementation)));
    ("  unicity  " , (Data_constraint.And(!Data_state.constraint_universe_binding_unicity)));
    ("  distrib1 " , (Data_constraint.And(!Data_state.constraint_universe_location_component_type)));
    ("  distrib2 " , (Data_constraint.And(!Data_state.constraint_universe_location_package)));
    ("  distrib3 " , (Data_constraint.And(!Data_state.constraint_universe_location_port)));
    ("  port_cal " , (Data_constraint.And(!Data_state.constraint_universe_definition_port)));
    ("  repo_1   " , (Data_constraint.And(!Data_state.constraint_universe_repository_unicity)));
    ("  repo_pac " , (Data_constraint.And(!Data_state.constraint_universe_repository_package)));
    ("  pack_dep " , (Data_constraint.And(!Data_state.constraint_universe_package_dependency)));
    ("  pack_pb  " , (Data_constraint.And(!Data_state.constraint_universe_package_conflict)));
    ("  resource " , (Data_constraint.And(!Data_state.constraint_universe_resource_consumption)));
    ("  detlete  " , (Data_constraint.And(!Data_state.constraint_universe_deprecated_element)));
    ("specification constraint" , ((check_option "specification constraint" !Data_state.constraint_specification_full)));
    ("  configuration " , ((Data_constraint.And(!Data_state.constraint_configuration_full)))) ] in
  let opt_f = check_option "optimization function constraint" !Data_state.constraint_optimization_function in
  let solution = Solvers.G12.solve solver_settings solver_input opt_f in

  print_string "\n\n\n <==========> THE END <==========>  \n\n"

(*
let () =
  Printf.printf "\nSETTINGS:\n\n%s\n" (Settings.string_of_settings ())


(* === Default settings === *)
let () =
  Settings.input_file_settings      := Some("./src_new_approach/zephyrus-settings.example");
  Settings.input_file_universe      := Some("./tests/u_1_new.json");
  Settings.input_file_specification := Some("./tests/spec_1.spec")


(* === Handling the arguments === *)

(* Arg module settings *)
let usage = 
  Printf.sprintf
    "usage: %s %s"
    Sys.argv.(0)
    "[-s settings-file]"

let speclist = 
  Arg.align [
    (* Input arguments *)
    ("-settings", Arg.String (fun filename -> Settings.input_file_settings := Some(filename)), " The settings file");
  ]

(* Read the arguments *)
let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage



(* === Set up everything === *)

(* Read the input. *)

(* Read settings. *)

let string_of_string_option_ref string_option_ref =
  match !string_option_ref with
  | None   -> "[unspecified]"
  | Some s -> s

(* just a test! *)

let my_settings =
  Printf.printf "Reading settings from file %s...\n" (string_of_string_option_ref Settings.input_file_settings);
  flush stdout;
  let settings_option =
    parse_standard Settings_parser.main Settings_lexer.token Settings.input_file_settings
  in
  match settings_option with
  | None -> failwith "No settings were found!"
  | Some settings -> settings

let () =
  Printf.printf "\nSETTINGS:\n\n%s\n" (Settings.string_of_settings ())
  *)




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
(*     let json_universe = Save_model.universe u
     in Printf.printf "\nThe universe:\n%s\n" (Json_j.string_of_universe json_universe)
*)



