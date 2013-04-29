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


open Aeolus_types_t
open Aeolus_types_output.Plain

open Variables

open ExtLib

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
  List.unique ( 
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
  List.unique ( 
    List.flatten ( 
      List.map ( fun repository -> 
        List.flatten ( 
          List.map (fun package -> 
            
            (
              [package.package_name]
              @
              (List.flatten package.package_depend)
              @
              (package.package_conflict)
            )

          ) repository.repository_packages
        )
      ) universe.universe_repositories
    ) 
  )

let consumed_resources_of_resource_consumption_list 
  (resource_consumption_list : (resource_name * resource_consumption) list)
  : resource_name list =

  List.filter_map (fun (resource_name, resource_consumption) ->
    if resource_consumption > 0
    then Some(resource_name)
    else None
  ) resource_consumption_list


let get_resource_names universe =

  List.unique (

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
  List.unique (
    List.map (fun location -> 
      location.location_name
    ) configuration.configuration_locations
  )

let get_locations configuration = configuration.configuration_locations

let get_location configuration location_name = 
  try
    List.find (fun location ->
      location.location_name = location_name 
    ) (get_locations configuration)
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the location %s does not exist in this configuration" 
        (string_of_location_name location_name))

let get_location_components configuration location_name =
  List.filter (fun component -> 
    component.component_location = location_name
  ) configuration.configuration_components

let get_location_packages_installed configuration location_name =
  let location = get_location configuration location_name
  in
  location.location_packages_installed

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
  List.filter_map (fun component_type ->
    if List.exists (fun (required_port_name, require_arity) ->
         (required_port_name = port_name) && (require_arity > 0)
       ) component_type.component_type_require
    then Some (component_type.component_type_name)
    else None
  ) universe.universe_component_types

let providers universe port_name =
  List.filter_map (fun component_type ->
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
  List.filter_map (fun component_type ->
    if List.exists (fun conflicted_port_name ->
         conflicted_port_name = port_name
       ) component_type.component_type_conflict
    then Some (component_type.component_type_name)
    else None
  ) universe.universe_component_types

(*
let consumers universe resource_name =
  let component_types =
    List.filter_map (fun component_type ->
      if List.exists (fun (consumed_resource_name, resource_consumption) ->
           (consumed_resource_name = resource_name) && (resource_consumption > 0)
         ) component_type.component_type_consume
      then Some (component_type.component_type_name)
      else None
    ) universe.universe_component_types

  and packages =
    List.filter_map (fun component_type ->
      if List.exists (fun (consumed_resource_name, resource_consumption) ->
           (consumed_resource_name = resource_name) && (resource_consumption > 0)
         ) component_type.component_type_consume
      then Some (component_type.component_type_name)
      else None
    ) universe.universe_component_types
*)
