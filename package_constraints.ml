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


open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

let create_package_implementation_constraints configuration universe =

  let location_names       = get_location_names configuration
  and component_type_names = get_component_type_names universe
  in

  List.flatten (
    List.map (fun location_name ->
      List.map (fun component_type_name ->

        (* The left side expression: *)
        let local_component_type_var =
          var (LocalElementVariable (location_name, (ComponentType component_type_name)))
        in
        let left_side_expr =
          ( (var2expr local_component_type_var) >=~ (int2expr 1) )
        in
  
        (* The right side expression: *)
        let package_names =
          get_component_type_implementation universe component_type_name 
        in
  
        let exprs_to_sum = 
          List.map ( fun package_name ->
    
            let local_package_var = 
              var (LocalElementVariable (location_name, (Package package_name)))
            in
    
            (* Part of the sum: N(location_name, package_name) *)
            (var2expr local_package_var)
              
        ) package_names 
        in
        let sum_of_local_package_vars = (sum exprs_to_sum)
        in
        let right_side_expr =
          ( sum_of_local_package_vars >=~ (int2expr 1) )
  
        in
  
        (* The constraint :  *)
        ( left_side_expr =>~~ right_side_expr )
        
        (* Name        : *)
        (* Description : *)
        (* Constraint  : *)
  
      ) component_type_names
    ) location_names
  )

let create_package_dependency_constraints configuration universe =

  let location_names = get_location_names configuration
  and packages       = get_packages       universe
  in

  List.flatten (
    List.map (fun location_name ->
      List.flatten (
        List.map (fun package ->
  
          let depending_package_name = package.package_name
          in
  
          List.map (fun depended_on_package_names_group ->
            
            (* The left side expression: *)
            let local_depending_package_var = 
                var (LocalElementVariable (location_name, (Package depending_package_name)))
              
            in

            (* The right side expression: *)
            let exprs_to_sum = 
              List.map ( fun depended_on_package_name ->
        
                let local_package_var = 
                  var (LocalElementVariable (location_name, (Package depended_on_package_name)))
                in
        
                (* Part of the sum: N(location_name, depended_on_package_name) *)
                (var2expr local_package_var)
                  
              ) depended_on_package_names_group
            in
            let sum_of_depended_on_package_vars = (sum exprs_to_sum)
  
            in
  
            (* The constraint :  *)
            ( (var2expr local_depending_package_var) <=~ sum_of_depended_on_package_vars )
            
            (* Name        : *)
            (* Description : *)
            (* Constraint  : *)
  
          ) package.package_depend
        ) packages
      )
    ) location_names
  )

let create_package_conflict_constraints configuration universe =

  let location_names = get_location_names configuration
  and packages       = get_packages       universe
  in

  List.flatten (
    List.map (fun location_name ->
      List.flatten (
        List.map (fun package ->
  
          let conflicting_package_name_1 = package.package_name
          in
  
          List.map (fun conflicting_package_name_2 ->
            
            (* The left side expression: *)
            let local_conflicting_package_var_1 = 
                var (LocalElementVariable (location_name, (Package conflicting_package_name_1)))
              
            and local_conflicting_package_var_2 =
                var (LocalElementVariable (location_name, (Package conflicting_package_name_2)))

            in

            (* The right side expression is a constant equal 1. *)
  
            (* The constraint :  *)
            ( ( (var2expr local_conflicting_package_var_1) +~ (var2expr local_conflicting_package_var_2) ) <=~ (int2expr 1) )
            
            (* Name        : *)
            (* Description : *)
            (* Constraint  : *)
  
          ) package.package_conflict
        ) packages
      )
    ) location_names
  )




let create_package_constraints configuration universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_package_implementation_constraints;
     create_package_dependency_constraints;
     create_package_conflict_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions )