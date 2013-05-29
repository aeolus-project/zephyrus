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

open Model_t
open Typing_context
open Variables
open Constraints

(* Generic function computing total consumption of a given resource on a given location by a given list of "elements" (of type 'a). *)
let elements_resource_consumption_sum
  ( get_element_resource_consumption_funtion : resource_name -> 'a -> resource_consumption ) (* Takes a resource name and an element, returns how much of that resource does this element consume. *)
  ( local_element_variable_function : location -> 'a -> expr )                               (* Takes a location and an element, returns an expression saying how many instances of that element are present at that location. *)
  ( location : location )                                                                    (* The location. *)
  ( resource_name : resource_name )                                                          (* The resource name. *)
  ( elements : 'a list )                                                                     (* A list of elements. *)
  : expr = 
  
  let exprs_to_sum =
    List.map ( fun element (* = el *) ->
      
      (* = resource_consumption(el,o) *)
      let consume_arity     = int2expr (get_element_resource_consumption_funtion resource_name element)

      (* = N(l,el) *)
      and local_element_var = local_element_variable_function location element
      in

      (* Part of the sum:                       *)
      (* = resource_consumption(el,o) * N(l,el) *)
      consume_arity *~ local_element_var
        
    ) elements
  
  in

  (* = sum (over all elements el) resource_consumption(el,o) *)
  sum exprs_to_sum


(* Apply our generic resource consumption computing function to components. *)
let sum_of_local_consumption_by_components = 
  elements_resource_consumption_sum
    (fun (resource_name : resource_name) (component_type : component_type) -> get_component_type_resource_consumption component_type resource_name)
    (fun (location : location) (component_type : component_type) -> var2expr (LocalElementVariable (location.location_name, (ComponentType component_type.component_type_name))))


(* Apply our generic resource consumption computing function to packages. *)
let sum_of_local_consumption_by_packages = 
  elements_resource_consumption_sum
    (fun (resource_name : resource_name) (package : package) -> get_package_resource_consumption package resource_name)
    (fun (location : location) (package : package) -> var2expr (LocalElementVariable (location.location_name, (Package package.package_name))))


let create_local_resource_constraints_with_or_without_packages (include_packages_resource_consumption : bool) configuration universe =

  (* Get all the location names from the configuration *)
  let locations       = get_locations       configuration

  (* Get all the resource names from the universe *)
  and resource_names  = get_resource_names  universe
  
  (* Get all the component types from the universe *)
  and component_types = get_component_types universe

  (* Get all the packages from the universe *)
  and packages        = get_packages        universe
  
  in
  
  List.flatten (

    (* For all the available locations *)
    List.map (fun location (* = l *)->

      (* For all the available resources *)
      List.map (fun resource_name (* = o *) ->

        (* The left side expression: *)

        (* = N(l,o) *)
        let local_resource_var =
          var2expr (LocalResourceVariable (location.location_name, resource_name))
  
        in
  
        (* The right side expression: *)
  
        (* First part of the sum: resources consumed by component types *)
        (* = sum (over all component types t) resource_consumption(t,o) *)
        let sum_of_local_consumption_by_components = 
          sum_of_local_consumption_by_components location resource_name component_types
        
        in
        
        (* Do we include the resources consumed by packages? *)
        if include_packages_resource_consumption
        then

          (* We count in the resources consumed by packages. *)

          (* Second part of the sum: resources consumed by packages *)
          (* = sum (over all packages k) resource_consumption(k,o) *)
          let sum_of_local_consumption_by_packages = 
            sum_of_local_consumption_by_packages location resource_name packages
          
          in
          
          (* The constraint :
              [for each location l]
              [for each resource o] 
                N(l,o) >= ( sum (over all component types t) resource_consumption(t,o) ) + ( sum (over all packages k) resource_consumption(k,o) ) 
          *)
          local_resource_var >=~ (sum_of_local_consumption_by_components +~ sum_of_local_consumption_by_packages)

        else

          (* We do not count in the resources consumed by packages. *)

          (* The constraint :
              [for each location l]
              [for each resource o] 
                N(l,o) >= sum (over all component types t) resource_consumption(t,o)
          *)
          local_resource_var >=~ sum_of_local_consumption_by_components
          
      ) resource_names
    ) locations
  )

let create_local_resource_constraints           = create_local_resource_constraints_with_or_without_packages true  (* Count the resources consumed by both the components and the packages. *)
let create_local_component_resource_constraints = create_local_resource_constraints_with_or_without_packages false (* Count only the resources consumed by the components. *)

(* 
  Note: 
    The "components + packages" constraints are strictly stronger (or equal) to the "components only" constraints.
    Therefore we can safely use both of these constraint versions in the same problem and we will have the same effect as if we used only the stronger ones.
*)


let create_initial_configuration_resource_constraints configuration universe =

  (* Get all the location names from the configuration *)
  let locations       = get_locations      configuration

  (* Get all the resource names from the universe *)
  and resource_names  = get_resource_names universe
  in

  List.flatten (

    (* For all the available locations *)
    List.map (fun location ->

      (* For all the available resources *)
      List.map (fun resource_name ->

        (* The left side expression: *)

        (* = O(l,o) *)
        let local_resource_var =
          var2expr (LocalResourceVariable (location.location_name, resource_name))
  
        in

        (* The right side expression: *)

        (* = resource_provide_arity(l,o) *)
        let resource_provide_arity =
          int2expr (get_location_resource_provide_arity location resource_name)

        in

        (* The constraint : 
            [for each location l]
            [for each resource o] 
              O(l,o) = resource_provide_arity(l,o) 
        *)
        ( local_resource_var =~ resource_provide_arity )

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
  List.flatten_map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions