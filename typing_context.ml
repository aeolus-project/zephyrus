
open Aeolus_types_j
open Variable_keys

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

let get_component_types universe = universe.universe_component_types

let get_packages universe =
  List.flatten ( 
    List.map ( fun repository -> 
      repository.repository_packages  
    ) universe.universe_repositories
  ) 

let get_repository_package_names universe repository_name =
  let repository = get_repository universe repository_name
  in
  List.map ( fun package ->
    package.package_name
  ) repository.repository_packages


let get_component_type_names universe =
  List.map ( fun component_type -> 
      component_type.component_type_name
  ) universe.universe_component_types

let get_port_names universe =
  BatList.unique ( 
    List.flatten ( 
      List.map ( fun component_type -> 
        
        (
          List.map (fun (port_name, _) -> 
            port_name
          ) component_type.component_type_provide
        )
        @
        (
          List.map (fun (port_name, _) -> 
            port_name
          ) component_type.component_type_require
        )
        @
        component_type.component_type_conflict
    
      ) universe.universe_component_types
    ) 
  )

let get_repository_names universe =
  List.map ( fun repository -> 
      repository.repository_name
  ) universe.universe_repositories

let get_package_names universe =
  BatList.unique ( 
    List.flatten ( 
      List.map ( fun repository -> 
        
        List.map (fun package -> 
          package.package_name
        ) repository.repository_packages
    
      ) universe.universe_repositories
    ) 
  )

let consumed_resources_of_resource_consumption_list 
  (resource_consumption_list : (resource_name * resource_consumption) list)
  : resource_name list =

  BatList.filter_map (fun (resource_name, resource_consumption) ->
    if resource_consumption > 0
    then Some(resource_name)
    else None
  ) resource_consumption_list


let get_resource_names universe =

  BatList.unique (

    (* Resource names mentioned in all component types. *)
    List.flatten ( 
      List.map (fun component_type -> 
        consumed_resources_of_resource_consumption_list 
          component_type.component_type_consume
      ) universe.universe_component_types
    )
    
    @

    (* Resource names mentioned in all packages. *)
    List.flatten ( 
      List.map ( fun repository -> 
        List.flatten (
          List.map (fun package -> 
            consumed_resources_of_resource_consumption_list 
              package.package_consume
          ) repository.repository_packages
        )
      ) universe.universe_repositories
    )

  )

let get_elements universe =
  let component_type_elements =
    List.map (fun component_type_name -> ComponentType component_type_name) (get_component_type_names universe)
  and port_elements =
    List.map (fun port_name -> Port port_name) (get_port_names universe)
  and package_elements =
    List.map (fun package_name -> Package package_name) (get_package_names universe)
  in
  (component_type_elements @ port_elements @ package_elements)

let get_location_names configuration =
  BatList.unique (
    List.map (fun location -> 
      location.location_name
    ) configuration.configuration_locations
  )

let get_locations configuration = configuration.configuration_locations

let get_resource_provide_arity location resource_name =
  try
    List.assoc resource_name location.location_provide_resources
  with
  | Not_found -> 0


let get_provide_arity component_type port_name =
  try
    List.assoc port_name component_type.component_type_provide
  with
  | Not_found -> (`FiniteProvide 0)

let get_require_arity component_type port_name =
  try
    List.assoc port_name component_type.component_type_require
  with
  | Not_found -> 0

let get_component_type_resource_consumption component_type resource_name =
  try
    List.assoc resource_name component_type.component_type_consume
  with
  | Not_found -> 0

let get_package_resource_consumption package resource_name =
  try
    List.assoc resource_name package.package_consume
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
         (provided_port_name = port_name) 
         && 
         (match provide_arity with
          | `FiniteProvide i -> i > 0
          | `InfiniteProvide -> true )
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

(*
let consumers universe resource_name =
  let component_types =
    BatList.filter_map (fun component_type ->
      if List.exists (fun (consumed_resource_name, resource_consumption) ->
           (consumed_resource_name = resource_name) && (resource_consumption > 0)
         ) component_type.component_type_consume
      then Some (component_type.component_type_name)
      else None
    ) universe.universe_component_types

  and packages =
    BatList.filter_map (fun component_type ->
      if List.exists (fun (consumed_resource_name, resource_consumption) ->
           (consumed_resource_name = resource_name) && (resource_consumption > 0)
         ) component_type.component_type_consume
      then Some (component_type.component_type_name)
      else None
    ) universe.universe_component_types
*)