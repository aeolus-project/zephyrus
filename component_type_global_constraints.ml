
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

let const_of_provide_arity (provide_arity : provide_arity) : const =
  match provide_arity with
  | `FiniteProvide   (const) -> Int const
  | `InfiniteProvide         -> Inf true
  
let providearity2expr (provide_arity : provide_arity) : expr =
  const2expr (const_of_provide_arity provide_arity)

let create_require_binding_constraints universe port_name : cstr list =

  (* Get all component types which: *)
  let provider_names = providers universe port_name (* provide some quantity of the port *)
  and requirer_names = requirers universe port_name (* require some quantity of the port *)
  in
  
  (* For all the component types which require the port *)
  List.map (fun requiring_component_type_name ->

    (* The left side expression: *)
    let requiring_arity              = get_require_arity (get_component_type universe requiring_component_type_name) port_name
    and requiring_component_type_var = var (GlobalElementVariable (ComponentType requiring_component_type_name))
    in
    
    (* The right side expression: *)
    let exprs_to_sum = 
      List.map ( fun providing_component_type_name ->
        let binding_var = 
          var (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
        in

        (* Part of the sum: B(port_name, providing_component_type_name, requiring_component_type_name) *)
        (var2expr binding_var)
        
      ) provider_names
    in
    let sum_of_provided_bindings = (sum exprs_to_sum)
    in
    
    (* The constraint : [for each component type t_r which requires port p]  require_arity(t_r,p) x N(t_r) = sum (over all t_p from the universe which provide port p) B(t_p,t_r,p) *)
    ( (int2expr requiring_arity) *~ (var2expr requiring_component_type_var) ) =~ sum_of_provided_bindings

    (* Name           : Components of type t requiring port p. *)
    (* Description    : All the require ports p of the components of type t must be bound. So the total number of ports p required by all the components of type t is equal to the total number of bindings providing these components with port p. *)
    (* Constraint     : N(t) multiplied by number of ports p that each components of type t requires is equal to the sum of all bindings on port p where the component of type t is the requiring one and any other component is providing. *)

  ) requirer_names



let create_provide_binding_constraints universe port_name : cstr list =
  
  (* Get all component types which: *)
  let provider_names = providers universe port_name (* provide some quantity of the port *)
  and requirer_names = requirers universe port_name (* require some quantity of the port *)
  in
  
  (* For all the component types which provide the port *)
  List.map (fun providing_component_type_name ->
    
    (* The left side expression: *)
    let providing_arity              = get_provide_arity (get_component_type universe providing_component_type_name) port_name
    and providing_component_type_var = var (GlobalElementVariable (ComponentType providing_component_type_name))
    in

    (* The right side expression: *)
    let exprs_to_sum =
      List.map ( fun requiring_component_type_name ->
        let binding_var = 
          var (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
        in

        (* Part of the sum: B(port_name, providing_component_type_name, requiring_component_type_name) *)
        (var2expr binding_var)
        
      ) requirer_names
    in
    let sum_of_required_bindings = (sum exprs_to_sum)
    in

    (* The constraint : [for each component type t_p which provides port p]  provide_arity(t_p,p) x N(t_p) = sum (over all t_r from the universe which provide port p) B(t_p,t_r,p) *)
    ( (providearity2expr providing_arity) *~ (var2expr providing_component_type_var) ) >=~ sum_of_required_bindings

    (* Name           : Components of type t providing port p. *)
    (* Description    : There cannot exist more bindings providing port p from the components of type t than the total number of ports p they provide together. The total number of ports p provided by all the components of type t is equal or greater to the total number of bindings providing port p from these components. *)
    (* Constraint     : N(t) multiplied by number of ports p that each component of type t provides is equal or greater to the sum of all bindings on port p where the component of type t is the providing one and any other component is the requiring one. *)

  ) provider_names



let create_binding_unicity_constraints universe port_name : cstr list  =
  
  (* Get all component types which: *)
  let provider_names = providers universe port_name (* provide some quantity of the port *)
  and requirer_names = requirers universe port_name (* require some quantity of the port *)
  in

  (* For all the combinations of providing and requiring elements *)
  List.flatten (
    List.map (fun providing_component_type_name ->
      List.map (fun requiring_component_type_name ->
  
        let binding_var                  = var (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
        and providing_component_type_var = var (GlobalElementVariable (ComponentType providing_component_type_name))  
        and requiring_component_type_var = var (GlobalElementVariable (ComponentType requiring_component_type_name))
        in
  
        (* The constraint : [for each component type t_p which provides port p] [for each component type t_r which requires port p]  B(t_p,t_r,p) <= N(t_p) x N(t_r) *)
        (var2expr binding_var) <=~ ( (var2expr providing_component_type_var) *~ (var2expr requiring_component_type_var) )

        (* Name           : Unicity of bindings between t_1 and t_2 on port p. *)
        (* Description    : There cannot be more than one binding providing port p from a given component instance (of type t_1) to another given component instance (of type t_2). *)
        (* Constraint     : The number of bindings where a component of type t_1 is providing a component of type t_2 with port p is not bigger than number of components of type t_1 multiplied by number of components of type t_2.*)

      ) requirer_names
    ) provider_names )



let create_conflict_constraints universe port_name : cstr list  =
  
  (* Get all component types which: *)
  let conflicter_names = conflicters universe port_name (* are in conflict with the port *)
  in

  let port_var = var (GlobalElementVariable (Port port_name))
  in

  (* For all the component types which are in conflict with the port *)
  List.map (fun conflicting_component_type_name ->
      
      let conflicting_component_type_var      = var (GlobalElementVariable (ComponentType conflicting_component_type_name))
      and conflicting_component_provide_arity = get_provide_arity (get_component_type universe conflicting_component_type_name) port_name
      in
  
      (* The constraint : [for each component type t which conflicts with port p]  ( N(t) >= 1 )  implies  ( N(p) = provides(t,p) ) *)
      ( (var2expr conflicting_component_type_var) >=~ (int2expr 1) ) =>~~ ( (var2expr port_var) =~ (providearity2expr conflicting_component_provide_arity) )

      (* Name           : Conflict of t on port p. *)
      (* Description    : If a component t which is in conflict with the port p is present in the configuration, then the total number of port p provided in the configuration must be equal to the number of port p which t provides. *)
      (* Explanation    : If N(t) is greater than zero, then N(p) must be equal to the number of port p which t provides. *)

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
  List.flatten (
    List.map (fun port_name ->

      (* For each constraint generating function *)
      List.flatten (
          List.map (fun create_binding_constraint_function -> 

            (* Create the constraint *)
            create_binding_constraint_function universe port_name

          ) create_component_type_global_constraints_functions )
    
    ) (get_port_names universe) )
