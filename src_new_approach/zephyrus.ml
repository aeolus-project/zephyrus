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
let my_universe =
  Input_helper.parse_json Json_j.read_universe Settings.input_file_universe

(* just a test! *)
let my_specification =
  Input_helper.parse_standard Specification_parser.main Specification_lexer.token Settings.input_file_specification

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
  
