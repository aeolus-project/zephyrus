
open Aeolus_types_t

open Facile
open Easy

open Helpers
open Typing_context

open Variable_keys


(* Types *)

type facile_variable = Facile.Var.Fd.t

let string_of_variable =
  Helpers.string_of_printing_function (Facile.Var.Fd.fprint)

type variables = (variable_key * facile_variable) list

(* Accessing *)

let get_variable variables variable_key =
  
  try
    List.assoc variable_key variables
  with
  | _ -> 
    failwith (Printf.sprintf 
      "the requested %s %s does not exist!"
      (descr_of_variable_key  variable_key)
      (string_of_variable_key variable_key) )


let facile_variables variables =
  List.map (fun (key, variable) -> variable) variables

let get_global_element_variables variables =
  BatList.filter_map ( fun (key, variable) ->
    match key with
    | GlobalElementVariable _ -> Some (variable)
    | _ -> None
  ) variables


(* Creating *)

(* Our variables' domain contains in theory all natural numbers. 
 * In practice we restrain it for some obvious reasons (infinite calculation is
 * difficult to perform in finite time) and some less obvious reasons (to avoid
 * encountering integer overflow problem when computing a sum of the upper
 * bounds of our variables). *)

let variable_max = 100000

type var_kind =
  | BooleanVariable
  | NaturalVariable

let create_new_variable var_kind var_name =
  (* Prepare the domain : *)
  let var_domain = 
    match var_kind with
    | NaturalVariable ->
        (* The natural numbers domain. *)
        Domain.interval 0 variable_max

    | BooleanVariable ->
        (* The boolean domain. *)
        Domain.boolean
  in
  (* Create the variable. *)
  (Facile.Var.Fd.create ~name:var_name var_domain)

let elements universe =
  let component_type_elements =
    List.map (fun component_type_name -> ComponentType component_type_name) (get_component_type_names universe)
  and port_elements =
    List.map (fun port_name -> Port port_name) (get_port_names universe)
  and package_elements =
    List.map (fun package_name -> Package package_name) (get_package_names universe)
  in
  (component_type_elements @ port_elements @ package_elements)

let create_global_element_variables universe =
  List.map (fun element ->
    let key = (GlobalElementVariable element) in
    let var_name = string_of_variable_key key in
    let new_var  = create_new_variable NaturalVariable var_name in
    (key, new_var) 
  ) (elements universe)

let create_local_element_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun element ->
        let key = (LocalElementVariable (location_name, element)) in
        let var_name = string_of_variable_key key in
        let new_var  = create_new_variable NaturalVariable var_name in
        (key, new_var) 
      ) (elements universe)
    ) (get_location_names configuration)
  )

let create_binding_variables universe =
  List.flatten ( List.flatten (
    List.map (fun port_name ->
      List.map (fun providing_component_type_name ->
        List.map (fun requiring_component_type_name ->
          let key = (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name)) in
          let var_name = string_of_variable_key key in
          let new_var  = create_new_variable NaturalVariable var_name in
          (key, new_var) 
        ) (requirers universe port_name)
      ) (providers universe port_name)
    ) (get_port_names universe)
  ))

let create_local_repository_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun repository_name ->
        let key = (LocalRepositoryVariable (location_name, repository_name)) in
        let var_name = string_of_variable_key key in
        let new_var  = create_new_variable BooleanVariable var_name in
        (key, new_var) 
      ) (get_repository_names universe)
    ) (get_location_names configuration)
  )

let create_local_resource_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun resource_name ->
        let key = (LocalResourceVariable (location_name, resource_name)) in
        let var_name = string_of_variable_key key in
        let new_var  = create_new_variable NaturalVariable var_name in
        (key, new_var) 
      ) (get_resource_names universe)
    ) (get_location_names configuration)
  )

let create_variables universe configuration = 
  List.flatten
    [create_global_element_variables   universe;
     create_local_element_variables    universe configuration;
     create_binding_variables          universe;
     create_local_repository_variables universe configuration;
     create_local_resource_variables   universe configuration]


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

let string_of_variables variables =
  string_of_variable_assoc_list variables string_of_variable_key

(* Extracting the solution from variables *)


type solution = (variable_key * int) list 

let get_solution variables = 
  let solution_of_key_variable_assoc_list key_variable_assoc_list =
    List.map (fun (key, variable) ->
      (key, Facile.Var.Fd.elt_value variable)
    ) key_variable_assoc_list
  in
  solution_of_key_variable_assoc_list variables

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
    strings_of_solution_assoc_list solution string_of_variable_key
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)