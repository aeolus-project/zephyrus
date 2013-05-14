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



open Aeolus_types_t
open Typing_context
open Variables
open Generic_constraints



let create_one_repository_per_location_constraints configuration universe : cstr list =

  let location_names   = get_location_names configuration
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
    ( sum_of_repository_vars =~ (int2expr 1) )

	(* Name        : *)
	(* Description : *)
	(* Constraint  : *)

  ) location_names



let create_packages_in_location_must_come_from_its_repository_contraints configuration universe : cstr list =

  let location_names    = get_location_names configuration
  and repository_names  = get_repository_names universe
  and all_package_names = get_package_names universe
  in

  List.flatten ( List.flatten (
    List.map (fun location_name ->
      List.map ( fun repository_name ->

        (* The left side expression: *)
        let local_repository_var = 
            var (LocalRepositoryVariable (location_name, repository_name))
        in

        let left_side_expr = ( (var2expr local_repository_var) =~ (int2expr 1) )
        in

        (* All the right side expressions: *)
        let repository = get_repository universe repository_name
        in
        let repository_package_names = get_repository_package_names repository
        in

        let right_side_exprs =
          List.map (fun package_name ->
          	let package_var = var (LocalElementVariable (location_name, (Package package_name)))
            in

            (* Do package k belong to the repository r () ?*)
          	if List.mem package_name repository_package_names

          	then 
          	  (* If the package k belongs to the repository r, then N(l,k) <= 1 *)
              ( ( var2expr package_var ) <=~ ( int2expr 1 ) )

            else
              (* If the k package does not belong to the repository r, then N(l,k) = 0 *)
              ( ( var2expr package_var ) =~ ( int2expr 0 ) )

          ) all_package_names
    
        in
    
    	(* We put together the left side expression with every right side expression. *)
        List.map ( fun right_side_expr ->

          (* The constraint :  [for each location l] [for each repository r] [for each package k] ( R(l,r) = 1 ) implies (if k belongs to r then ( N(l,k) <= 1 ) if it does not then ( N(l,k) = 0 ) *)
          ( left_side_expr =>~~ right_side_expr )
    
          (* Name        : *)
          (* Description : *)
          (* Constraint  : *)

        ) right_side_exprs

        (* TODO: 
           We can easily transform this list of constraints into a single constraint 
           with a big logic sum, question is: which one has a better performance? 
           For now we stick with this version, because it's more lisible.
        *)
  
      ) repository_names
    ) location_names
  ) )





let create_repository_constraints configuration universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_one_repository_per_location_constraints;
     create_packages_in_location_must_come_from_its_repository_contraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions )