
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints


let create_local_resource_constraints bare_architecture universe : cstr list = []
(*
  let location_names       = get_location_names       bare_architecture
  and resource_names       = get_resource_names       universe
  and component_type_names = get_component_type_names universe
  in
  List.map (fun location_name ->
    List.map (fun resource_name ->

      (* The left side expression: *)
      let local_resource_var =
        var (LocalResourceVariable (location_name, resource_name))

      in

      (* The rights side expression: *)

      (* First part of the sum: component types *)
      let first_exprs_to_sum = 
          List.map ( fun component_type_name ->
            

              
          ) component_type_names
        in
        let sum_of_local_consumption_by_components = (sum first_exprs_to_sum)

      (* Second part of the sum: packages *)

    ) resource_names
  ) location_names
*)


let create_resource_constraints bare_architecture universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_local_resource_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function bare_architecture universe 

  ) create_constraints_functions )