
open Aeolus_types_j

open Helpers
open Typing_context
open Variable_keys

open Generic_constraints


(* Translating the universe *)

let translate_universe universe bare_architecture =
  let create_constraints_functions = [
    ("component types",  Component_type_global_constraints.create_component_type_global_constraints);
    ("location",         Location_constraints.create_location_constraints bare_architecture);
    ("repository",       Repository_constraints.create_repository_constraints bare_architecture)
  ]
  in
  List.map (fun (constraints_group_name, create_constraints_function) ->
    let constraints = create_constraints_function universe
    in
    (constraints_group_name, constraints)
  ) create_constraints_functions


(* Translating the specification *)

let translate_specification universe = []


(*
(* Translating the typing context *)

type typing_context_constraints = (string * (cstr list)) list

let translate_typing_context (typing_context : Typing_context.t)  : typing_context_constraints = 
  let create_constraints_functions = [
    ("provide",  Capacity_constraints.create_provide_constraints);
    (* ("require",  Capacity_constraints.create_require_constraints);  *)
    ("conflict", Capacity_constraints.create_conflict_constraints);
    ("binding",  Binding_constraints.create_binding_constraints);
    (* ("max_rest", Max_rest_constraints.create_max_rest_constraints); *)
  ]
  in
  List.map (fun (constraints_group_name, create_constraints_function) ->
    let constraints = create_constraints_function typing_context
    in
    (constraints_group_name, constraints)
  ) create_constraints_functions



(* Translating the specification *)

type specification_constraints = cstr list

let translate_specification : specification -> specification_constraints =
  Specification_constraints.create_specification_constraints
*)

let string_of_generated_constraint = string_of_cstr

(* Printing *)
let string_of_generated_constraints translation_constraints specification_constraints =
  let string_of_generated_constraint_list constraints =
    let strings = List.map string_of_generated_constraint constraints in
    lines_of_strings strings
  in
  let all_constraints : (string * (cstr list)) list =
    translation_constraints @ [("specification", specification_constraints)]
  in
  let strings =
    List.map (fun (constraints_group_name, constraints) ->

      Printf.sprintf "+ %s constraints:\n%s\n" constraints_group_name (string_of_generated_constraint_list constraints)
      
    ) all_constraints
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)
