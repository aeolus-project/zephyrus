

open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints



let create_one_repository_per_location_constraints bare_architecture universe : cstr list =

  let location_names   = get_locations bare_architecture
  and repository_names = get_repository_names universe
  in

  List.map (fun location_name ->

    (* The left side expression: *)
  	let exprs_to_sum = 
      List.map ( fun repository_name ->

      	let local_repository_var = 
          var (LocalRepositoryVariable (location_name, repository_name))
        in

      	(* Part of the sum: R(location_name, repository_name) *)
      	(var2expr local_repository_var)

      ) repository_names

    in
    let sum_of_repository_vars = (sum exprs_to_sum)

    (* The right side expression is a constant = 1. *)

    in

    (* The constraint :  [for each location l] ( sum (over all r from the universe) R(l,r) ) = 1 *)
    ( sum_of_repository_vars =~ (const2expr 1) )

	(* Name        : *)
	(* Description : *)
	(* Constraint  : *)

  ) location_names




let create_repository_constraints bare_architecture universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_location_constraints_functions =
    [create_one_repository_per_location_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function bare_architecture universe 

  ) create_location_constraints_functions )