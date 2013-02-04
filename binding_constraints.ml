
open Typing_context
open Variable_keys
open Generic_constraints

let elements_providing_port typing_context port_name = 
  List.filter (fun domain_element -> 
    ((delta typing_context) domain_element port_name Provides) > 0
  ) (domain typing_context)

and elements_requiring_port typing_context port_name = 
  List.filter (fun domain_element -> 
    ((delta typing_context) domain_element port_name Requires) > 0
  ) (domain typing_context)
  


let create_require_binding_constraints typing_context port_name : cstr list =

  (* Get all domain elements which: *)
  let providing_elements = elements_providing_port typing_context port_name (* provide some quantity of the port *)
  and requiring_elements = elements_requiring_port typing_context port_name (* require some quantity of the port *)
  in
  
  (* For all the domain elements which require the port *)
  List.map (fun requiring_element ->

    (* The left side expression: *)
    let n = (delta typing_context) requiring_element port_name Requires
    and requiring_element_var = var (DomainVariable requiring_element)
    in
    
    (* The right side expression: *)
    let exprs_to_sum = 
      List.map ( fun providing_element ->
        let binding_var = 
          var (BindingVariable (port_name, providing_element, requiring_element))
        in

        (* Part of the sum: B(port_name, providing_element, requiring_element) *)
        (var2expr binding_var)
        
      ) providing_elements
    in
    let sum_of_provided_bindings = (sum exprs_to_sum)
    in
    
    (* The constraint : [for each resource type (t_r,s_r) which requires port p]  delta((t_r,s_r),p,Requires) x N(t,s) = sum (over all (t_p,s_p) from the domain which provide port p) B((t_p,s_p),(t_r,s_r),p) *)
    ( (const2expr n) *~ (var2expr requiring_element_var) ) =~ sum_of_provided_bindings

    (* Name           : Resources of type (t,s) requiring port p. *)
    (* Description    : All the require ports p of the resources of type (t,s) must be bound. So the total number of ports p required by all the resources of type (t,s) is equal to the total number of bindings providing these resources with port p. *)
    (* Constraint     : N(t,s) multiplied by number of ports p that each resource of type (t,s) requires is equal to the sum of all bindings on port p where the resource of type (t,s) is the requiring one and any other resource is providing. *)

  ) requiring_elements



let create_provide_binding_constraints typing_context port_name : cstr list =
  
  (* Get all domain elements which: *)
  let providing_elements = elements_providing_port typing_context port_name (* provide some quantity of the port *)
  and requiring_elements = elements_requiring_port typing_context port_name (* require some quantity of the port *)
  in
  
  (* For all the domain elements which provide the port *)
  List.map (fun providing_element ->
    
    (* The left side expression: *)
    let n = (delta typing_context) providing_element port_name Provides
    and providing_element_var = var (DomainVariable providing_element)
    in

    (* The right side expression: *)
    let exprs_to_sum =
      List.map ( fun requiring_element ->
        let binding_var = 
          var (BindingVariable (port_name, providing_element, requiring_element))
        in

        (* Part of the sum: B(port_name, providing_element, requiring_element) *)
        (var2expr binding_var)
        
      ) requiring_elements
    in
    let sum_of_required_bindings = (sum exprs_to_sum)
    in

    (* The constraint : [for each resource type (t_p,s_p) which provides port p]  delta((t_p,s_p),p,Provides) x N(t_p,s_p) >= sum (over all (t_r,s_r) from the domain which require port p) B((t_p,s_p),(t_r,s_r),p) *)
    ( (const2expr n) *~ (var2expr providing_element_var) ) >=~ sum_of_required_bindings

    (* Name           : Resources of type (t,s) providing port p. *)
    (* Description    : There cannot exist more bindings providing port p from the resources of type (t,s) than the total number of ports p they provide together. The total number of ports p provided by all the resources of type (t,s) is equal or greater to the total number of bindings providing port p from these resources. *)
    (* Constraint     : N(t,s) multiplied by number of ports p that each resource of type (t,s) provides is equal or greater to the sum of all bindings on port p where the resource of type (t,s) is the providing one and any other resource is the requiring one. *)

  ) providing_elements



let create_unicity_binding_constraints typing_context port_name : cstr list  =
  
  (* Get all domain elements which: *)
  let providing_elements = elements_providing_port typing_context port_name (* provide some quantity of the port *)
  and requiring_elements = elements_requiring_port typing_context port_name (* require some quantity of the port *)
  in

  (* For all the combinations of providing and requiring elements *)
  List.flatten (
    List.map (fun providing_element ->
      List.map (fun requiring_element ->
  
        let binding_var           = var (BindingVariable (port_name, providing_element, requiring_element))
        and providing_element_var = var (DomainVariable providing_element)  
        and requiring_element_var = var (DomainVariable requiring_element)
        in
  
        (* The constraint : [for each resource type (t_p,s_p) which provides port p] [for each resource type (t_r,s_r) which requires port p]  B((t_p,s_p),(t_r,s_r),p) <= N(t_p,s_p) x N(t_r,s_r) *)
        (var2expr binding_var) <=~ ( (var2expr providing_element_var) *~ (var2expr requiring_element_var) )

        (* Name           : Unicity of bindings between (t1, s1) and (t2, s2) on port p. *)
        (* Description    : There cannot be more than one binding providing port p from a given resource instance (of type (t1, s1)) to another given resource instance (of type (t2, s2)). *)
        (* Constraint     : The number of bindings where a resource of type (t1, s1) is providing a resource of type (t2, s2) with port p is not bigger than number of resources of type (t1, s1) multiplied by number of resources of type (t1, s1).*)

      ) requiring_elements
    ) providing_elements )



let create_binding_constraints typing_context : cstr list =

  (* A list of constraint generating functions to use with each port. *)
  let create_binding_constraints_functions =
    [create_provide_binding_constraints;
     create_require_binding_constraints;
     create_unicity_binding_constraints]
  in

  (* Generate the constraints! *)

  (* For each port *)
  List.flatten (
    List.map (fun port_name ->

      (* For each constraint generating function *)
      List.flatten (
          List.map (fun create_binding_constraint_function -> 

            (* Create the constraint *)
            create_binding_constraint_function typing_context port_name

          ) create_binding_constraints_functions )
    ) (ports typing_context) )
