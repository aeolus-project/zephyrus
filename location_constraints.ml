
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

let create_element_location_constraints
  (get_element_names_function : universe -> 'a list)
  (global_var_key_function : 'a -> variable_key)
  (local_var_key_function : location_name -> 'a -> variable_key)
  bare_architecture
  universe 
  : cstr list =

  let location_names = get_locations bare_architecture
  in

  let element_names = get_element_names_function universe
  in
  List.map (fun element_name ->

    (* The left side expression: *)
    let global_element_var = var (global_var_key_function element_name)
    in

    (* The right side expression: *)
    let exprs_to_sum = 
      List.map ( fun location_name ->
        let local_element_var = 
          var (local_var_key_function location_name element_name)
        in

        (* Part of the sum: N(location_name, element_name) *)
        (var2expr local_element_var)
          
      ) location_names
    in
    let sum_of_local_element_vars = (sum exprs_to_sum)
    
    in

    (* The constraint : *)
    ( (var2expr global_element_var) =~ sum_of_local_element_vars )

  ) element_names


let create_component_type_location_constraints :  bare_architecture -> universe -> cstr list =

  let get_element_names_function = get_component_type_names
  and global_var_key_function component_type_name = (GlobalElementVariable (ComponentType component_type_name))
  and local_var_key_function location_name component_type_name = (LocalElementVariable (location_name, (ComponentType component_type_name)))
  in
  create_element_location_constraints
    get_element_names_function
    global_var_key_function
    local_var_key_function

let create_port_location_constraints :  bare_architecture -> universe -> cstr list =

  let get_element_names_function = get_port_names
  and global_var_key_function port_name = (GlobalElementVariable (Port port_name))
  and local_var_key_function location_name port_name = (LocalElementVariable (location_name, (Port port_name)))
  in
  create_element_location_constraints
    get_element_names_function
    global_var_key_function
    local_var_key_function

let create_package_location_constraints :  bare_architecture -> universe -> cstr list =

  let get_element_names_function = get_package_names
  and global_var_key_function package_name = (GlobalElementVariable (Package package_name))
  and local_var_key_function location_name package_name = (LocalElementVariable (location_name, (Package package_name)))
  in
  create_element_location_constraints
    get_element_names_function
    global_var_key_function
    local_var_key_function


let create_location_constraints bare_architecture universe : cstr list =

  (* A list of constraint generating functions to use with each port. *)
  let create_location_constraints_functions =
    [create_component_type_location_constraints;
     create_port_location_constraints;
     create_package_location_constraints]
  in

  (* Generate the constraints! *)

  (* For each constraint generating function *)
  List.flatten (
    List.map (fun create_constraints_function -> 
    
    (* Create the constraint *)
    create_constraints_function bare_architecture universe 

  ) create_location_constraints_functions )
