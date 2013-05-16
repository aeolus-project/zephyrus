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

let const_of_provide_arity (provide_arity : provide_arity) : const =
  match provide_arity with
  | `FiniteProvide   (const) -> Int const
  | `InfiniteProvide         -> Inf true
  
let providearity2expr (provide_arity : provide_arity) : expr =
  const2expr (const_of_provide_arity provide_arity)

let create_element_location_constraints
  (get_element_names_function : universe -> 'a list)
  (global_var_function : 'a -> variable)
  (local_var_function : location_name -> 'a -> variable)
  configuration
  universe 
  : cstr list =

  let location_names = get_location_names configuration
  and element_names  = get_element_names_function universe
  in
  
  List.map (fun element_name ->

    (* The left side expression: *)
    let global_element_var = global_var_function element_name
    in

    (* The right side expression: *)
    let exprs_to_sum = 
      List.map ( fun location_name ->

        let local_element_var = 
          local_var_function location_name element_name
        in

        (* Part of the sum: N(location_name, element_name) *)
        var2expr local_element_var
          
      ) location_names
    in
    let sum_of_local_element_vars = sum exprs_to_sum
    
    in

    (* The constraint :  *)
    ( (var2expr global_element_var) =~ sum_of_local_element_vars )

    (* Name        : Elements location. *)
    (* Description : Global number of components of type t is equal to sum of local numbers of components of this type in all locations. *)
    (* Constraint  : *)

  ) element_names


let create_component_type_location_constraints :  configuration -> universe -> cstr list =
  create_element_location_constraints
    get_component_type_names
    ( fun component_type_name -> GlobalElementVariable (ComponentType component_type_name) )
    ( fun location_name component_type_name -> LocalElementVariable (location_name, (ComponentType component_type_name)) )

let create_port_location_constraints :  configuration -> universe -> cstr list =
  create_element_location_constraints
    get_port_names
    ( fun port_name -> GlobalElementVariable (Port port_name) )
    ( fun location_name port_name -> LocalElementVariable (location_name, (Port port_name)) )

let create_package_location_constraints :  configuration -> universe -> cstr list =
  create_element_location_constraints
    get_package_names
    ( fun package_name -> GlobalElementVariable (Package package_name) )
    ( fun location_name package_name -> LocalElementVariable (location_name, (Package package_name)) )



let create_port_provided_at_location_constraints configuration universe : cstr list =

  let location_names = get_location_names configuration
  and port_names     = get_port_names universe
  in

  List.flatten (
    List.map (fun location_name ->
      List.map (fun port_name ->
  
        (* The left side expression: *)
        let local_port_var = LocalElementVariable (location_name, (Port port_name))
        in
    
        (* The right side expression: *)
  
        (* Get all component types which: *)
        let provider_names = providers universe port_name (* provide some quantity of the port *)
        in
  
        let exprs_to_sum = 
          List.map ( fun component_type_name ->
            
            let provide_arity            = get_provide_arity (get_component_type universe component_type_name) port_name
            and local_component_type_var = LocalElementVariable (location_name, (ComponentType component_type_name))
            in
  
            (* Part of the sum: provide_arity(location_name, component_type_name) * N(location_name, component_type_name) *)
            ( (providearity2expr provide_arity) *~ (var2expr local_component_type_var) )
              
          ) provider_names
        in
        let sum_of_local_provides = sum exprs_to_sum
        
        in
    
        (* The constraint :  *)
        ( (var2expr local_port_var) =~ sum_of_local_provides )
    
        (* Name        : Ports provided at location. *)
        (* Description : Number of ports provided at location is a sum of all ports provided by components which are present at this location. *)
        (* Constraint  : *)
  
      ) port_names
    ) location_names
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
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function configuration universe 

  ) create_constraints_functions )
