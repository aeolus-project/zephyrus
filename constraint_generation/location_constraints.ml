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

(* Helper function converting a provide_arity to an expression. *)
let providearity2expr (provide_arity : provide_arity) : expr =
  let const_of_provide_arity (provide_arity : provide_arity) : const =
    match provide_arity with
    | `FiniteProvide   (const) -> Int const
    | `InfiniteProvide         -> Inf true
  in
  const2expr (const_of_provide_arity provide_arity)
  


(* Generic function generating certain location constraints for a given type of "elements". *)
(* These location constraint tie the "global element constraint" (how many instances of that element are there in the whole configuration)
   to a sum of "local element constraints" (how many instances of that element are there at the given location). *)
let create_element_location_constraints
  (get_element_names_function : universe -> 'a list) (* Gets the list of all available elements. *)
  (global_var_function : 'a -> variable)             (* Converts an element to a "global element variable" (how many instances of that element are there in the whole configuration). *)
  (local_var_function : location -> 'a -> variable)  (* Takes a location and an element, returns a "local element variable" (how many instances of that element are there at the given location). *)
  configuration                                      (* The initial configuration. *)
  universe                                           (* The universe. *)
  : cstr list =

  (* Get all the locations from the configuration *)
  let locations = get_locations configuration

  (* Get all the element names from the universe *)
  and element_names  = get_element_names_function universe
  in
  
  (* For all the available elements *)
  List.map (fun element_name (* = el *) ->

    (* The left side expression: *)

    (* = N(el) *)
    let global_element_var = 
      var2expr (global_var_function element_name)

    in

    (* The right side expression: *)

    (* = sum (over all location names l) N(l,el) *)
    let sum_of_local_element_vars =
      let exprs_to_sum = 
        List.map ( fun location ->

          (* Part of the sum: *)
          (* = N(l,el)        *)
          var2expr (local_var_function location element_name)
            
        ) locations
      in
      sum exprs_to_sum
    
    in

    (* The constraint : 
        N(el) = sum (over all location names l) N(l,el) 
    *)
    global_element_var =~ sum_of_local_element_vars

  ) element_names


(* Apply aur generic "elements location constraints" function for components. *)
let create_component_type_location_constraints :  configuration -> universe -> cstr list =
  create_element_location_constraints
    ( fun universe -> List.map (fun component_type -> component_type.component_type_name) (get_component_types universe) ) (* get element names function *)
    ( fun component_type_name -> GlobalElementVariable (ComponentType component_type_name) ) (* element name -> global element variable *)
    ( fun location component_type_name -> LocalElementVariable (location.location_name, (ComponentType component_type_name)) ) (* location, element name -> local element variable *)

(* Apply aur generic "elements location constraints" function for ports. *)
let create_port_location_constraints :  configuration -> universe -> cstr list =
  create_element_location_constraints
    get_port_names (* get element names function *)
    ( fun port_name -> GlobalElementVariable (Port port_name) ) (* element name -> global element variable *)
    ( fun location port_name -> LocalElementVariable (location.location_name, (Port port_name)) ) (* location, element name -> local element variable *)

(* Apply aur generic "elements location constraints" function for packages. *)
let create_package_location_constraints :  configuration -> universe -> cstr list =
  (* Use the general function for package elements *)
  create_element_location_constraints
    get_package_names (* get element names function *)
    ( fun package_name -> GlobalElementVariable (Package package_name) ) (* element name -> global element variable *)
    ( fun location package_name -> LocalElementVariable (location.location_name, (Package package_name)) ) (* location, element name -> local element variable*)



let create_port_provided_at_location_constraints configuration universe : cstr list =

  (* Get all the locationfrom the configuration *)
  let locations  = get_locations configuration

  (* Get all the port names mentioned in the universe *)
  and port_names = get_port_names universe
  in

  List.flatten (
    
    (* For all the available locations *)
    List.map (fun location (* = l *) ->

      (* For all the available ports *)
      List.map (fun port_name (* = p *)->
  
        (* The left side expression: *)

        (* = N(l,p) *)
        let local_port_var = 
          var2expr (LocalElementVariable (location.location_name, (Port port_name)))
        in
    
        (* The right side expression: *)
  
        (* Get all component types which provide port p *)
        let providers = providers universe port_name
        in

        (* = sum (over all component types t which provide port p) provide_arity(l,p) x N(l,t) *)
        let sum_of_local_provides =
          let exprs_to_sum = 
            List.map ( fun component_type (* = t *) ->
              
              (* = provide_arity(l,t) *)
              let provide_arity = 
                providearity2expr (get_provide_arity component_type port_name)

              (* = N(l,t) *)
              and local_component_type_var = 
                var2expr (LocalElementVariable (location.location_name, (ComponentType component_type.component_type_name)))

              in
    
              (* Part of the sum: *)
              (* = provide_arity(l,t) * N(l,t) *)
              provide_arity *~ local_component_type_var
                
            ) providers
          in
          sum exprs_to_sum
        
        in
    
        (* The constraint : 
            [for each port p]
            [for each location l]
              N(l,p) = sum (over all component types t which provide port p) provide_arity(t,p) x N(l,t) = N(l,p) 
        *)
        local_port_var =~ sum_of_local_provides
  
      ) port_names
    ) locations
  )



let create_location_constraints configuration universe : cstr list =

  (* A list of constraint generating functions to use: *)
  let create_constraints_functions =
    [create_component_type_location_constraints;
     create_port_location_constraints;
     create_package_location_constraints;
     create_port_provided_at_location_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten_map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions
