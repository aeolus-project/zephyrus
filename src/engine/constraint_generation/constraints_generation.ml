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


open Model_j

open Helpers
open Typing_context
open Variables

open Constraints


type generated_constraints = (string * (cstr list)) list


(* Printing *)

let string_of_generated_constraint = string_of_cstr

let string_of_generated_constraints constraints =
  let string_of_generated_constraint_list constraints =
    let strings = List.map string_of_generated_constraint constraints in
    lines_of_strings strings
  in
  let strings =
    List.map (fun (constraints_group_name, constraints) ->

      Printf.sprintf
        "+ %s constraints:\n%s\n"
        constraints_group_name 
        (string_of_generated_constraint_list constraints)
      
    ) constraints
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)


(* Converting *)

let constraints_of_generated_constraints generated_constraints =
  List.flatten_map snd generated_constraints


(* Translating *)

let translate_universe_and_initial_configuration universe initial_configuration =
  let create_constraints_functions = [
    ("component types",  Component_type_global_constraints. create_component_type_global_constraints);
    ("location",         Location_constraints.   create_location_constraints   initial_configuration);
    ("repository",       Repository_constraints. create_repository_constraints initial_configuration);
    ("package",          Package_constraints.    create_package_constraints    initial_configuration);
    ("resource",         Resource_constraints.   create_resource_constraints   initial_configuration)
  ]
  in
  List.map (fun (constraints_group_name, create_constraints_function) ->
    let constraints = create_constraints_function universe
    in
    (constraints_group_name, constraints)
  ) create_constraints_functions

let translate_specification specification initial_configuration =
  [("specification", Specification_constraints.create_specification_constraints initial_configuration specification)]
(* TODO: generate a specific message if specification is using names which don't exist in the universe or initial configuration. *)
