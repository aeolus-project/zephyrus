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

let create_package_implementation_constraints configuration universe =

  (* Get all the location names from the configuration *)
  let location_names       = get_location_names configuration

  (* Get all the component type names from the universe *)
  and component_type_names = get_component_type_names universe
  in

  List.flatten (

    (* For all the available locations *)
    List.map (fun location_name (* = l *)->

      (* For all the available component types *)
      List.map (fun component_type_name (* = t *) ->

        (* The left side expression: *)

        (* = N(l,t) *)
        let local_component_type_var =
          var2expr (LocalElementVariable (location_name, (ComponentType component_type_name)))
        in
  
        (* The right side expression: *)

        (* Get the names of all packages which can implement the component type t *)
        let package_names =
          get_component_type_implementation universe component_type_name 
        in
  
        (* = sum (over all packages k which can implement the component type t) N(l,k) *)
        let sum_of_local_package_vars =
          let exprs_to_sum = 
            List.map ( fun package_name (* = k *) ->
      
              (* Part of the sum: *)
              (* = N(l,k)         *)
              var2expr (LocalElementVariable (location_name, (Package package_name)))
                
          ) package_names 
          in
          sum exprs_to_sum
  
        in
  
        (* The constraint : 
             [for each location l]
             [for each component type t]
               ( N(l,t) >= 1 )  implies  ( sum (over all packages k which can implement the component type t) N(l,k) ) >= 1 
        *)
        ( local_component_type_var >=~ (int2expr 1) ) =>~~ ( sum_of_local_package_vars >=~ (int2expr 1) )
  
      ) component_type_names
    ) location_names
  )

let create_package_dependency_constraints configuration universe =

  (* Get all the location names from the configuration *)
  let location_names = get_location_names configuration

  (* Get all the package names from the universe *)
  and packages       = get_packages       universe
  in

  (* For all the available locations *)
  List.flatten_map (fun location_name (* = l *) ->

    (* For all the available packages *)
    List.flatten_map (fun package (* = k *) ->

      let depending_package_name         = package.package_name
      and depended_on_package_names_sets = package.package_depend

      in

      (* For all the sets of packages on which package k depends *)
      List.map (fun depended_on_package_names_set (* = g *) ->
        
        (* The left side expression: *)

        (* = N(l,k) *)
        let local_depending_package_var = 
            var2expr (LocalElementVariable (location_name, (Package depending_package_name)))
          
        in

        (* The right side expression: *)

        (* = sum (over all packages k' from the set g) N(l,k') *)
        let sum_of_depended_on_package_vars =
          let exprs_to_sum = 
            List.map ( fun depended_on_package_name (* = k' *) ->
      
              (* Part of the sum: *)
              (* = N(l,k')        *)
              var2expr (LocalElementVariable (location_name, (Package depended_on_package_name)))
                
            ) depended_on_package_names_set
          in
          sum exprs_to_sum

        in

        (* The constraint : 
             [for each location l]
             [for each package k] 
             [for each set g of packages that k depends on] 
               N(l,k) <= sum (over all packages k' from the set g) N(l,k') 
        *)
        local_depending_package_var <=~ sum_of_depended_on_package_vars

      ) depended_on_package_names_sets
    ) packages
  ) location_names

let create_package_conflict_constraints configuration universe =

  (* Get all the location names from the configuration *)
  let location_names = get_location_names configuration

  (* Get all the package names from the universe *)
  and packages       = get_packages       universe
  in

  (* For all the available locations *)
  List.flatten_map (fun location_name (* = l *)->

    (* For all the available packages *)
    List.flatten_map (fun package (* = k1 *) ->

      let conflicting_package_name_1  = package.package_name
      and conflicting_package_names_2 = package.package_conflict
      in

      (* For all the packages conflicting with k1 *)
      List.map (fun conflicting_package_name_2 (* = k2 *) ->
        
        (* The left side expression: *)

        (* = N(l,k1) *)
        let local_conflicting_package_var_1 = 
            var2expr (LocalElementVariable (location_name, (Package conflicting_package_name_1)))
          
        (* = N(l,k2) *)
        and local_conflicting_package_var_2 =
            var2expr (LocalElementVariable (location_name, (Package conflicting_package_name_2)))

        in

        (* The right side expression is a constant equal 1. *)

        (* The constraint :
             [for each location l] 
             [for each package k1]
             [for each package k2 that k1 conflicts] 
               N(l,k1) + N(l,k2) <= 1 
        *)
        ( local_conflicting_package_var_1 +~ local_conflicting_package_var_2 ) <=~ (int2expr 1)

      ) conflicting_package_names_2
    ) packages
  ) location_names




let create_package_constraints configuration universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_package_implementation_constraints;
     create_package_dependency_constraints;
     create_package_conflict_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten_map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions