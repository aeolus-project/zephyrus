
open Aeolus_types_j

open Helpers

let get_component_type universe component_type_name =
  try
    List.find (fun component_type ->
      component_type.component_type_name = component_type_name 
    ) universe.universe_component_types
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the component type %s does not exist in this universe" 
        (string_of_component_type_name component_type_name))

let get_component_type_implementation universe component_type_name =
  try
    List.assoc component_type_name universe.universe_implementation
  with
  (* If this component type is not on the universe implementation list, 
   * this does not mean it does not exist, but that it simply does not
   * need any packages to implement it. *)
  | Not_found -> [] 

let get_repository universe repository_name =
  try
    List.find (fun repository ->
      repository.repository_name = repository_name 
    ) universe.universe_repositories
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the repository %s does not exist in this universe" 
        (string_of_repository_name repository_name))

let get_package repository package_name =
  try
    List.find (fun package ->
      package.package_name = package_name 
    ) repository.repository_packages
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the package %s does not exist in this repository" 
        (string_of_package_name package_name))

let get_ports universe =
  BatList.unique ( 
    List.flatten ( 
      List.map ( fun component_type -> 
        
        (
          List.map (fun (port_name, port_arity) -> 
            port_name
          ) (component_type.component_type_provide @ component_type.component_type_require)
        )
        @
        component_type.component_type_conflict
    
      ) universe.universe_component_types
    ) 
  )

let get_provide_arity component_type port_name =
  try
    List.assoc port_name component_type.component_type_provide
  with
  | Not_found -> 0

let get_require_arity component_type port_name =
  try
    List.assoc port_name component_type.component_type_require
  with
  | Not_found -> 0

let requirers universe port_name =
  BatList.filter_map (fun component_type ->
    if List.exists (fun (required_port_name, require_arity) ->
         (required_port_name = port_name) && (require_arity > 0)
       ) component_type.component_type_require
    then Some (component_type.component_type_name)
    else None
  ) universe.universe_component_types

let providers universe port_name =
  BatList.filter_map (fun component_type ->
    if List.exists (fun (provided_port_name, provide_arity) ->
         (provided_port_name = port_name) && (provide_arity > 0)
       ) component_type.component_type_provide
    then Some (component_type.component_type_name)
    else None
  ) universe.universe_component_types

let conflicters universe port_name =
  BatList.filter_map (fun component_type ->
    if List.exists (fun conflicted_port_name ->
         conflicted_port_name = port_name
       ) component_type.component_type_conflict
    then Some (component_type.component_type_name)
    else None
  ) universe.universe_component_types
