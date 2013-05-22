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

let const_of_provide_arity (provide_arity : provide_arity) : const =
  match provide_arity with
  | `FiniteProvide   (const) -> Int const
  | `InfiniteProvide         -> Inf true
  
let providearity2expr (provide_arity : provide_arity) : expr =
  const2expr (const_of_provide_arity provide_arity)

let create_require_binding_constraints universe port_name : cstr list =

  (* Get all component types which: *)
  let providers = providers universe port_name (* provide port p *)
  and requirers = requirers universe port_name (* require port p *)
  in
  
  (* For all the component types which require port p *)
  List.map (fun requiring_component_type (* = t_r *) -> 

    (* The left side expression: *)

    (* = require_arity(t_r,p) *)
    let require_arity = 
      int2expr (get_require_arity requiring_component_type port_name)

    (* = N(t_r) *)
    and requiring_component_type_var = 
      var2expr (GlobalElementVariable (ComponentType requiring_component_type.component_type_name))

    in
    

    (* The right side expression: *)

    (* = sum (over all t_p from the universe which provide port p) B(t_p,t_r,p) *)
    let sum_of_provided_bindings = 
      let exprs_to_sum =
        List.map ( fun providing_component_type (* = t_p *) ->

          (* Part of the sum: *)
          (* = B(p,t_r,t_p)   *)
          var2expr (BindingVariable (
            port_name, 
            providing_component_type.component_type_name, 
            requiring_component_type.component_type_name
          ))
          
        ) providers
      in
      sum exprs_to_sum

    in
    
    (* The constraint : 
        [for each component type t_r which requires port p]
          require_arity(t_r,p) x N(t_r) = sum (over all t_p from the universe which provide port p) B(t_p,t_r,p) 
    *)
    ( require_arity *~ requiring_component_type_var ) =~ sum_of_provided_bindings

  ) requirers



let create_provide_binding_constraints universe port_name : cstr list =
  
  (* Get all component types which: *)
  let providers = providers universe port_name (* provide port p *)
  and requirers = requirers universe port_name (* require port p *)
  in
  
  (* For all the component types which provide port p *)
  List.map (fun providing_component_type (* = t_p *)->

    (* Get the provide arity just to check if it is finite or not. *)
    let provide_arity =
      get_provide_arity providing_component_type port_name

    in

    match provide_arity with
    | `InfiniteProvide -> 
        (* If the provide arity is infinite, then this constraint will be always true. *)
        truecstr

    | `FiniteProvide _ ->
    
        (* The left side expression: *)
    
        (* = provide_arity(t_p,p) *)
        let provide_arity = 
          providearity2expr (get_provide_arity providing_component_type port_name)
    
        (* = N(t_p) *)
        and providing_component_type_var = 
          var2expr (GlobalElementVariable (ComponentType providing_component_type.component_type_name))
        in
      
        (* The right side expression: *)

        (* = sum (over all t_r from the universe which provide port p) B(t_p,t_r,p) *)
        let sum_of_required_bindings =
          let exprs_to_sum =
              List.map ( fun requiring_component_type (* = t_r *) ->

                (* Part of the sum: *)
                (* = B(p,t_p,t_r)   *)
                var2expr (BindingVariable (
                  port_name, 
                  providing_component_type.component_type_name, 
                  requiring_component_type.component_type_name
                ))
              
            ) requirers
          in
          sum exprs_to_sum

        in

        (* The constraint : 
            [for each component type t_p which provides port p]
              provide_arity(t_p,p) x N(t_p) = sum (over all t_r from the universe which provide port p) B(t_p,t_r,p)
        *)
        ( provide_arity *~ providing_component_type_var ) >=~ sum_of_required_bindings

  ) providers



let create_binding_unicity_constraints universe port_name : cstr list  =
  
  (* Get all component types which: *)
  let providers = providers universe port_name (* provide port p *)
  and requirers = requirers universe port_name (* require port p *)
  in

  (* For all the possible pairs of component types providing and requiring port p *)
  List.flatten (
    List.map (fun providing_component_type (* = t_p *) ->
      List.map (fun requiring_component_type (* = t_r *) ->
  
        (* = B(t_p,t_r,p) *)
        let binding_var = 
          var2expr (BindingVariable (
            port_name, 
            providing_component_type.component_type_name, 
            requiring_component_type.component_type_name
          ))

        (* = N(t_p) *)
        and providing_component_type_var = 
          var2expr (GlobalElementVariable (ComponentType providing_component_type.component_type_name))

        (* = N(t_r) *)
        and requiring_component_type_var = 
          var2expr (GlobalElementVariable (ComponentType requiring_component_type.component_type_name))
        in
  
        (* The constraint : 
            [for each component type t_p which provides port p]
            [for each component type t_r which requires port p]
              B(t_p,t_r,p) <= N(t_p) x N(t_r)
        *)
        binding_var <=~ ( providing_component_type_var *~ requiring_component_type_var )

      ) requirers
    ) providers )



let create_conflict_constraints universe port_name : cstr list  =
  
  (* Get all component types which are in conflict with port p *)
  let conflicter_names = conflicters universe port_name
  in

  (* For all the component types which are in conflict with port p *)
  List.map (fun conflicting_component_type  (* = t *) ->
      
      (* = N(t) *)
      let conflicting_component_type_var =
        var2expr (GlobalElementVariable (ComponentType conflicting_component_type.component_type_name))

      (* = provide_arity(t,p) *)
      and conflicting_component_provide_arity =
        providearity2expr (get_provide_arity conflicting_component_type port_name)
      
      (* = N(p) *)      
      and port_var =
        var2expr (GlobalElementVariable (Port port_name))

      in
  
      (* The constraint : 
          [for each component type t which conflicts with port p]
            ( N(t) >= 1 )  implies  ( N(p) = provide_arity(t,p) ) 
      *)
      ( conflicting_component_type_var >=~ (int2expr 1) ) =>~~ ( port_var =~ conflicting_component_provide_arity )

  ) conflicter_names



let create_component_type_global_constraints universe : cstr list =

  (* A list of constraint generating functions to use with each port. *)
  let create_component_type_global_constraints_functions =
    [create_provide_binding_constraints;
     create_require_binding_constraints;
     create_binding_unicity_constraints;
     create_conflict_constraints]
  in

  (* Generate the constraints! *)

  (* For each port *)
  List.flatten_map (fun port_name ->

    (* For each constraint generating function *)
    List.flatten_map (fun create_binding_constraint_function -> 

        (* Create the constraint *)
        create_binding_constraint_function universe port_name

      ) create_component_type_global_constraints_functions
    
  ) (get_port_names universe)
