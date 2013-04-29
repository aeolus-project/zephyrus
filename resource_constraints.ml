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

let element_exprs_to_sum
  ( get_element_resource_consumption_funtion : 'a -> resource_consumption ) 
  ( local_element_variable_function      : 'a -> variable )
  ( elements : 'a list )
  : expr list = 
  
  List.map ( fun element ->
    
    let consume_arity     = get_element_resource_consumption_funtion element
    and local_element_var = var (local_element_variable_function element)
    in

    (* Part of the sum: consumes(element_name, resource_name) * N(location_name, element_name) *)
    ( (int2expr consume_arity) *~ (var2expr local_element_var) )
      
  ) elements



let create_local_resource_constraints_with_or_without_packages (include_packages_resource_consumption : bool) configuration universe =

  let location_names       = get_location_names  configuration
  and resource_names       = get_resource_names  universe
  
  and component_types      = get_component_types universe
  and packages             = get_packages        universe
  
  in
  
  List.flatten (
    List.map (fun location_name ->
      List.map (fun resource_name ->
  
        (* The left side expression: *)
        let local_resource_var =
          var (LocalResourceVariable (location_name, resource_name))
  
        in
  
        (* The rights side expression: *)
  
        (* First part of the sum: component types *)
        let sum_of_local_consumption_by_components = 
          
          let component_type_exprs_to_sum = 
            element_exprs_to_sum
              (fun (component_type : component_type) -> get_component_type_resource_consumption component_type resource_name)
              (fun component_type -> (LocalElementVariable (location_name, (ComponentType component_type.component_type_name))))
              component_types
          in
          (sum component_type_exprs_to_sum)
        
        in
        
        (* Do we include the resources consumed by packages? *)
        if include_packages_resource_consumption
        then

          (* We count in the resources consumed by packages. *)

          (* Second part of the sum: packages *)
          let sum_of_local_consumption_by_packages = 
            
            let package_exprs_to_sum = 
              element_exprs_to_sum
                (fun (package : package) -> get_package_resource_consumption package resource_name)
                (fun (package : package) -> (LocalElementVariable (location_name, (Package package.package_name))))
                (packages : package list)
            in
            (sum package_exprs_to_sum)
          
          in
          
          (* The constraint :  *)
          ( (var2expr local_resource_var) >=~ (sum_of_local_consumption_by_components +~ sum_of_local_consumption_by_packages) )

        else

          (* We don't count in the resources consumed by packages. *)

          (* The constraint :  *)
          ( (var2expr local_resource_var) >=~ (sum_of_local_consumption_by_components) )

  
        (* Name        : *)
        (* Description : *)
        (* Constraint  : *)
  
      ) resource_names
    ) location_names
  )

let create_local_resource_constraints           = create_local_resource_constraints_with_or_without_packages true  (* Count the resources consumed by both the components and the packages. *)
let create_local_component_resource_constraints = create_local_resource_constraints_with_or_without_packages false (* Count only the resources consumed by the components. *)

(* 
  Note: 
    The "components + packages" constraints are strictly stronger (or equal) to the "components only" constraints.
    Therefore we can safely use both of these types in the same problem and we will have the same effect as if we used only the stronger ones.
*)


let create_initial_configuration_resource_constraints configuration universe =

  let locations       = get_locations      configuration
  and resource_names  = get_resource_names universe
  in

  List.flatten (
    List.map (fun location ->

      let location_name = location.location_name
      in

      List.map (fun resource_name ->

        (* The left side expression: *)
        let local_resource_var =
          var (LocalResourceVariable (location_name, resource_name))
  
        in

        (* The rights side expression: *)
        let resource_provide_arity =
          get_resource_provide_arity location resource_name

        in

        (* The constraint :  *)
        ( (var2expr local_resource_var) =~ (int2expr resource_provide_arity) )

        (* Name        : *)
        (* Description : *)
        (* Constraint  : *)

      ) resource_names
    ) locations
  )



let create_resource_constraints configuration universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_local_resource_constraints;
     create_initial_configuration_resource_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions )