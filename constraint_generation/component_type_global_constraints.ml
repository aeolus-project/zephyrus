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
  let provider_names = providers universe port_name (* provide port p *)
  and requirer_names = requirers universe port_name (* require port p *)
  in
  
  (* For all the component types which require port p *)
  List.map (fun requiring_component_type_name (* = t_r *) -> 

    (* The left side expression: *)

    (* = require_arity(t_r,p) *)
    let require_arity                = get_require_arity (get_component_type universe requiring_component_type_name) port_name

    (* = N(t_r) *)
    and requiring_component_type_var = GlobalElementVariable (ComponentType requiring_component_type_name)
    in
    

    (* The right side expression: *)

    (* = sum (over all t_p from the universe which provide port p) B(t_p,t_r,p) *)
    let sum_of_provided_bindings = 
      let exprs_to_sum =
        List.map ( fun providing_component_type_name ->

          (* Part of the sum: *)
          (* = B(p,t_r,t_p)   *)
          var2expr (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
          
        ) provider_names
      in
      sum exprs_to_sum

    in
    
    (* The constraint : [for each component type t_r which requires port p]  require_arity(t_r,p) x N(t_r) = sum (over all t_p from the universe which provide port p) B(t_p,t_r,p) *)
    ( (int2expr require_arity) *~ (var2expr requiring_component_type_var) ) =~ sum_of_provided_bindings

  ) requirer_names



let create_provide_binding_constraints universe port_name : cstr list =
  
  (* Get all component types which: *)
  let provider_names = providers universe port_name (* provide port p *)
  and requirer_names = requirers universe port_name (* require port p *)
  in
  
  (* For all the component types which provide the port *)
  List.map (fun providing_component_type_name (* = t_p *)->
    
    (* The left side expression: *)

    (* = provide_arity(t_p,p) *)
    let provide_arity                = get_provide_arity (get_component_type universe providing_component_type_name) port_name

    (* = N(t_p) *)
    and providing_component_type_var = GlobalElementVariable (ComponentType providing_component_type_name)
    in

    match provide_arity with
    | `InfiniteProvide -> 
        (* If the provide arity is infinite, then this constraint will be always true. *)
        truecstr

    | `FiniteProvide provide_arity ->

        (* The right side expression: *)

        (* = sum (over all t_r from the universe which provide port p) B(t_p,t_r,p) *)
        let sum_of_required_bindings =
          let exprs_to_sum =
              List.map ( fun requiring_component_type_name (* = t_r *) ->

                (* Part of the sum: *)
                (* = B(p,t_p,t_r)   *)
                var2expr (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
              
            ) requirer_names
          in
          sum exprs_to_sum

        in

        (* The constraint : [for each component type t_p which provides port p]  provide_arity(t_p,p) x N(t_p) = sum (over all t_r from the universe which provide port p) B(t_p,t_r,p) *)
        ( (int2expr provide_arity) *~ (var2expr providing_component_type_var) ) >=~ sum_of_required_bindings

  ) provider_names



let create_binding_unicity_constraints universe port_name : cstr list  =
  
  (* Get all component types which: *)
  let provider_names = providers universe port_name (* provide port p *)
  and requirer_names = requirers universe port_name (* require port p *)
  in

  (* For all the combinations of providing and requiring elements *)
  List.flatten (
    List.map (fun providing_component_type_name (* = t_p *) ->
      List.map (fun requiring_component_type_name (* = t_r *) ->
  
        (* = B(t_p,t_r,p) *)
        let binding_var                  = BindingVariable (port_name, providing_component_type_name, requiring_component_type_name)

        (* = N(t_p) *)
        and providing_component_type_var = GlobalElementVariable (ComponentType providing_component_type_name)

        (* = N(t_r) *)
        and requiring_component_type_var = GlobalElementVariable (ComponentType requiring_component_type_name)
        in
  
        (* The constraint : [for each component type t_p which provides port p] [for each component type t_r which requires port p]  B(t_p,t_r,p) <= N(t_p) x N(t_r) *)
        (var2expr binding_var) <=~ ( (var2expr providing_component_type_var) *~ (var2expr requiring_component_type_var) )

      ) requirer_names
    ) provider_names )



let create_conflict_constraints universe port_name : cstr list  =
  
  (* Get all component types which are in conflict with port p *)
  let conflicter_names = conflicters universe port_name
  in

  (* For all the component types which are in conflict with port p *)
  List.map (fun conflicting_component_type_name  (* = t *) ->
      
      (* = N(t) *)
      let conflicting_component_type_var      = GlobalElementVariable (ComponentType conflicting_component_type_name)

      (* = provide_arity(t,p) *)
      and conflicting_component_provide_arity = get_provide_arity (get_component_type universe conflicting_component_type_name) port_name
      
      (* = N(p) *)      
      and port_var                            = GlobalElementVariable (Port port_name)

      in
  
      (* The constraint : [for each component type t which conflicts with port p]  ( N(t) >= 1 )  implies  ( N(p) = provide_arity(t,p) ) *)
      ( (var2expr conflicting_component_type_var) >=~ (int2expr 1) ) =>~~ ( (var2expr port_var) =~ (providearity2expr conflicting_component_provide_arity) )

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
