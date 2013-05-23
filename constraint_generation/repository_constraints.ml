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

open Helpers

open Aeolus_types_t
open Typing_context
open Variables
open Generic_constraints



let create_one_repository_per_location_constraints configuration universe : cstr list =

  (* Get all the locations from the configuration *)
  let locations    = get_locations configuration

  (* Get all the package repositories from the universe *)
  and repositories = get_repositories universe
  in

  (* For all the available locations *)
  List.map (fun location (* = l *) ->

    (* = sum (over all repositories r) R(l,r) *)
  	let sum_of_repository_vars =
      let exprs_to_sum = 
        List.map ( fun repository (* = r *) ->
  
        	(* Part of the sum: *)
          (* = R(l,r)         *)
          var2expr (LocalRepositoryVariable (location.location_name, repository.repository_name))
          
        ) repositories
      in
      sum exprs_to_sum

    in

    (* The constraint : 
        [for each location l]
        ( sum (over all repositories r) R(l,r) ) = 1 
    *)
    sum_of_repository_vars =~ (int2expr 1)

  ) locations



let create_packages_in_location_must_come_from_its_repository_contraints configuration universe : cstr list =

  (* Get all the location names from the configuration *)
  let locations    = get_locations configuration
  
  (* Get all the package repository names from the universe *)
  and repositories = get_repositories universe

  (* Get all the package names from the universe *)
  and packages     = get_packages universe
  in

  (* For all the available locations *)
  List.flatten_map (fun location (* = l *) ->

    (* For all the available repositories *)
    List.flatten_map ( fun repository (* = r *)->

      (* The left side expression: *)

      (* = R(l,r) *)
      let local_repository_var = 
          var2expr (LocalRepositoryVariable (location.location_name, repository.repository_name))
      in

      (* Left side expression is always the same:  R(l,r) = 1 *)
      let left_side_expr = ( local_repository_var =~ (int2expr 1) )
      in


      (* All the right side expressions: *)

      let right_side_exprs =
        List.map (fun package (* = k *) ->

          (* = N(l,k) *)
        	let package_var = 
            var2expr (LocalElementVariable (location.location_name, (Package package.package_name)))
          in

          (* Does package k belong to the repository r ?*)
        	if is_package_in_the_repository repository package

        	then 
        	  (* If the package k belongs to the repository r, 
              then the right side expression is:  N(l,k) <= 1 *)
            package_var <=~ ( int2expr 1 )

          else
            (* If the k package does not belong to the repository r, 
              then the right side expression is:  N(l,k) = 0 *)
            package_var  =~ ( int2expr 0 )

        ) packages
  
      in
  
  	  (* We put together the left side expression with every right side expression. *)
      List.map ( fun right_side_expr ->

        (* The constraint :
            [for each location l]
            [for each repository r]
            [for each package k] 
              ( R(l,r) = 1 )  implies  (if k belongs to r then ( N(l,k) <= 1 ) if it does not then ( N(l,k) = 0 ) 
        *)
        left_side_expr =>~~ right_side_expr

      ) right_side_exprs

      (* TODO: 
         We can easily transform this list of constraints into a single constraint 
         with a big logic sum, question is: which one has a better performance? 
         For now we stick with this version, because it seems more lisible.
      *)

    ) repositories
  ) locations





let create_repository_constraints configuration universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_one_repository_per_location_constraints;
     create_packages_in_location_must_come_from_its_repository_contraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten_map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions