
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

let create_resource_constraints bare_architecture universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    []
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function bare_architecture universe 

  ) create_constraints_functions )