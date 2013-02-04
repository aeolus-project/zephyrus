
open Resource_types_t

open Facile
open Easy

open Helpers
open Typing_context

open Variable_keys


(* Types *)

type variable = Facile.Var.Fd.t

let string_of_variable =
  Helpers.string_of_printing_function (Facile.Var.Fd.fprint)

type typing_context_variables = {
  domain_variables    : (domain_variable_key   * Facile.Var.Fd.t) list;
  port_variables      : (port_variable_key     * Facile.Var.Fd.t) list;
  binding_variables   : (binding_variable_key  * Facile.Var.Fd.t) list;
  max_rest_variables  : (max_rest_variable_key * Facile.Var.Fd.t) list;
}



(* Accessing *)

let get_variable variables variable_key =
  
  let get_variable_from_assoc_list variable_key_contents key_variable_assoc_list =
  try
    List.assoc variable_key_contents key_variable_assoc_list
  with
  | _ -> 
    failwith (Printf.sprintf 
      "the requested %s %s does not exist!"
      (descr_of_variable_key  variable_key)
      (string_of_variable_key variable_key) )

  in

  match variable_key with
  | DomainVariable  domain_element                                    -> get_variable_from_assoc_list (domain_element)                                  variables.domain_variables
  | PortVariable    port_name                                         -> get_variable_from_assoc_list (port_name)                                       variables.port_variables
  | BindingVariable (port_name, providing_element, requiring_element) -> get_variable_from_assoc_list (port_name, providing_element, requiring_element) variables.binding_variables
  | MaxRestVariable (var_group_name, port_name, i)                    -> get_variable_from_assoc_list (var_group_name, port_name, i)                    variables.max_rest_variables

let domain_variables variables =
  List.map (fun (key, variable) -> variable) variables.domain_variables

and port_variables variables =
  List.map (fun (key, variable) -> variable) variables.port_variables

and binding_variables variables =
  List.map (fun (key, variable) -> variable) variables.binding_variables



(* Creating *)

(* Our variables' domain contains in theory all natural numbers. 
 * In practice we restrain it for some obvious reasons (infinite calculation is
 * difficult to perform in finite time) and some less obvious reasons (to avoid
 * encountering integer overflow problem when computing a sum of the upper
 * bounds of our variables). *)

let variable_max = 100000

let create_new_variable var_name =
  (* Prepare a natural numbers domain. *)
  let var_domain = 
    Domain.interval 0 variable_max
  in
  (* Create the variable. *)
  (Facile.Var.Fd.create ~name:var_name var_domain)

let create_domain_variables domain = 
  List.map ( fun domain_element ->
    let key = domain_element in
    let var_name = string_of_domain_variable_key key in
    let new_var  = create_new_variable var_name in
    (domain_element, new_var) 
  ) domain

let create_port_variables ports =
  List.map ( fun port_name ->
    let key = port_name in
    let var_name = string_of_port_variable_key key in
    let new_var  = create_new_variable var_name in
    (port_name, new_var)
  ) ports

let create_binding_variables typing_context =
  List.flatten (
    List.map (fun port_name -> 
      let providing_elements = List.filter (fun domain_element -> ((delta typing_context) domain_element port_name Provides) > 0) (domain typing_context)
      and requiring_elements = List.filter (fun domain_element -> ((delta typing_context) domain_element port_name Requires) > 0) (domain typing_context)
      in
      List.flatten (
        List.map (fun providing_element ->
          List.map (fun requiring_element ->
            let key      = (port_name, providing_element, requiring_element) in
            let var_name = string_of_binding_variable_key key in
            let new_var  = create_new_variable var_name in
            (key, new_var)
          ) requiring_elements
        ) providing_elements )
  
    ) (ports typing_context) )

let create_group_of_max_rest_variables typing_context var_group_name : (max_rest_variable_key * Facile.Var.Fd.t) list  =
  let all_ports = ports typing_context
  in
  let ports_with_an_r =
    List.filter (fun port_name -> 
      match (r typing_context port_name) with
        None   -> failwith ("Port '" ^ port_name ^ "' does not have a r value!")
      | Some 0 -> false
      | Some 1 -> false
      | Some n -> true
    ) all_ports
  in
  let t_variables_for_ports_with_an_r =
    List.map ( fun port_name ->
      let max_provide = max_provide typing_context port_name
      in
      let list_of_ints_from_zero_to_max_provide =
        BatList.init (max_provide + 1) (fun i -> i)
      in
      let t_variables_for_port : (max_rest_variable_key * Facile.Var.Fd.t) list =
        List.map ( fun i ->
          let key      = (var_group_name, port_name, i) in
          let var_name = string_of_max_rest_variable_key key in
          let new_var  = create_new_variable var_name in
          (key, new_var)
        ) list_of_ints_from_zero_to_max_provide
      in
      t_variables_for_port
    ) ports_with_an_r
  in
  List.flatten t_variables_for_ports_with_an_r

let create_max_rest_variables typing_context =
  List.flatten (
    List.map (fun var_group_name ->
      create_group_of_max_rest_variables typing_context var_group_name
    ) ([T; MAX; REST]) )

let create_typing_context_variables typing_context = {
  domain_variables     = create_domain_variables (domain typing_context);
  port_variables       = create_port_variables   (ports  typing_context);
  max_rest_variables   = []; (* create_max_rest_variables typing_context; *)
  binding_variables    = create_binding_variables typing_context;
}



(* Printing *)

let string_of_variable_assoc_list key_variable_assoc_list string_of_key =
  let strings = 
    List.map (fun ( key, variable ) ->
  
      Printf.sprintf 
        "%s = %s" 
        (string_of_key key)
        (string_of_variable variable)
  
    ) key_variable_assoc_list
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)

let string_of_domain_variables   variables =
  string_of_variable_assoc_list variables.domain_variables   string_of_domain_variable_key

let string_of_port_variables     variables =
  string_of_variable_assoc_list variables.port_variables     string_of_port_variable_key

let string_of_binding_variables  variables =
  string_of_variable_assoc_list variables.binding_variables  string_of_binding_variable_key

let string_of_max_rest_variables variables =
  string_of_variable_assoc_list variables.max_rest_variables string_of_max_rest_variable_key


(* Extracting the solution from variables *)

type solution = {
  solution_domain_elements : (domain_variable_key  * int) list;
  solution_bindings        : (binding_variable_key * int) list;
}

let get_solution variables = 
  let solution_of_key_variable_assoc_list key_variable_assoc_list =
    List.map (fun (key, variable) ->
      (key, Facile.Var.Fd.elt_value variable)
    ) key_variable_assoc_list
  in
  {
    solution_domain_elements = solution_of_key_variable_assoc_list variables.domain_variables;
    solution_bindings        = solution_of_key_variable_assoc_list variables.binding_variables;
  }

let string_of_solution solution =
  let strings_of_solution_assoc_list solution_assoc_list string_of_key =
    List.map (fun ( key, i ) -> 
          
          Printf.sprintf 
            "%s = %d" 
            (string_of_key key)
            i
  
        ) solution_assoc_list
  in
  let strings = 
    strings_of_solution_assoc_list solution.solution_domain_elements string_of_domain_variable_key
    @
    strings_of_solution_assoc_list solution.solution_bindings string_of_binding_variable_key
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)