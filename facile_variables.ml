
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


let filter_variables_by_key (predicate : variable_key -> bool) (variables : variables) =
  BatList.filter_map ( fun (key, variable) ->
    if predicate key
    then Some (variable)
    else None
  ) variables

let get_global_element_variables   = filter_variables_by_key pred_global_element_variable
let get_local_element_variables    = filter_variables_by_key pred_local_element_variable
let get_binding_variables          = filter_variables_by_key pred_binding_variable
let get_local_repository_variables = filter_variables_by_key pred_local_repository_variable
let get_local_resource_variables   = filter_variables_by_key pred_local_resource_variable
let get_specification_variables    = filter_variables_by_key pred_specification_variable

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

let create_new_facile_variable var_kind var_name =
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

let create_new_variable (kind : var_kind) (key : variable_key) =
  let var_name = string_of_variable_key key in
  let new_var  = create_new_facile_variable kind var_name in
  (key, new_var)

let create_global_element_variables universe =
  List.map (fun element ->
    create_new_variable NaturalVariable (GlobalElementVariable element)
  ) (get_elements universe)

let create_local_element_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun element ->
        create_new_variable NaturalVariable (LocalElementVariable (location_name, element))
      ) (get_elements universe)
    ) (get_location_names configuration)
  )

let create_binding_variables universe =
  List.flatten ( List.flatten (
    List.map (fun port_name ->
      List.map (fun providing_component_type_name ->
        List.map (fun requiring_component_type_name ->
          create_new_variable NaturalVariable (BindingVariable (port_name, providing_component_type_name, requiring_component_type_name))
        ) (requirers universe port_name)
      ) (providers universe port_name)
    ) (get_port_names universe)
  ))

let create_local_repository_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun repository_name ->
        create_new_variable BooleanVariable (LocalRepositoryVariable (location_name, repository_name))
      ) (get_repository_names universe)
    ) (get_location_names configuration)
  )

let create_local_resource_variables universe configuration =
  List.flatten (
    List.map (fun location_name ->
      List.map (fun resource_name ->
        create_new_variable NaturalVariable (LocalResourceVariable (location_name, resource_name))
      ) (get_resource_names universe)
    ) (get_location_names configuration)
  )

let create_specification_variables specification configuration =
  let all_variable_keys_from_specification =
    Specification_constraints.extract_variable_keys_from_specification configuration specification
  in
  let specification_variable_keys =
    List.filter pred_specification_variable all_variable_keys_from_specification
  in
  List.map (fun variable_key ->
    create_new_variable NaturalVariable variable_key
  ) specification_variable_keys


let create_variables universe configuration specification = 
  List.flatten
    [create_global_element_variables   universe                   ;
     create_local_element_variables    universe      configuration;
     create_binding_variables          universe                   ;
     create_local_repository_variables universe      configuration;
     create_local_resource_variables   universe      configuration;
     create_specification_variables    specification configuration]


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