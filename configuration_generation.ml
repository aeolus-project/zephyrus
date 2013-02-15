
open Resource_types_t

open Typing_context
open Facile_variables
open Helpers

let fresh_name domain_element number =
  Printf.sprintf 
  "%s #%d" (string_of_domain_element domain_element) number


open Matching_algorithm.List_match_requirers_with_providers
open Matching_algorithm.String_list_requirer_provider_types

let generate_bindings (universe : universe) (components : component list) : binding list =

  let ports = get_ports universe
  in

  let assoc_or_zero a l =
    if List.mem_assoc a l
    then List.assoc a l
    else 0
  
  in
  
  List.flatten (
    List.map (fun port_name ->
      let requirers = (* the mapping from component name to their require multiplicity on p *) 
        List.map (fun component ->
          let component_type = get_component_type universe component.component_type
          in
          (component.component_name, assoc_or_zero port_name component_type.component_type_require)
        ) components
  
      and providers = 
        List.map (fun component ->
          let component_type = get_component_type universe component.component_type
          in
          (component.component_name, assoc_or_zero port_name component.resource_provides)
        ) components
  
      in
  
      match matching_algorithm requirers providers with
      | None -> 
          failwith (Printf.sprintf
            "Matching algorithm has failed for a solution which should be correct (for port %s)!"
            (string_of_port_name port_name) )

      | Some results -> 
    
        List.map (fun result -> {
            binding_port = port_name;
            binding_provider = result.provides;
            binding_requirer = result.requires;
        }) results
  
    ) ports )



open Facile_constraints

let solution_repository_for_location (solution : solution) (location_name : location_name) : repository_name =
  let repository_names =
    BatList.filter_map (fun (var_key, value) -> 
      match var_key with    
      | LocalRepositoryVariable (var_location_name, var_repository_name) ->
          if (var_location_name = location_name) && (value = 1)
          then Some var_repository_name
      | _ -> None
    ) solution
  in
  match repository_names with
  | [repository_name] -> repository_name
  | []                -> failwith (Printf.sprintf "in the solution no repository is assigned to location %s"                  location_name)
  | _                 -> failwith (Printf.sprintf "in the solution there is more than one repository assigned to location %s" location_name)

let solution_packages_for_location (solution : solution) (location_name : location_name) : package_name list =
  let package_names =
    BatList.filter_map (fun (var_key, value) -> 
      match var_key with    
      | LocalElementVariable (var_location_name, (Package var_package_name)) ->
          if (var_location_name = location_name) && (value = 1)
          then Some var_package_name
      | _ -> None
    ) solution
  in
  package_names

let solution_number_of_components_for_location (solution : solution) (location_name : location_name) (component_type_name :component_type_name) : int =
  let components_counts =
    BatList.filter_map (fun (var_key, value) -> 
      match var_key with    
      | LocalElementVariable (var_location_name, (ComponentType var_component_type_name)) ->
          if (var_location_name = location_name) && (var_component_type_name = component_type_name)
          then Some value
      | _ -> None
    ) solution
  in
  match components_counts with
  | [components_count] -> components_count
  | []                 -> failwith (Printf.sprintf "in the solution number of components of type %s in location %s is unknown"                location_name component_type_name)
  | _                  -> failwith (Printf.sprintf "in the solution number of components of type %s in location %s is defined more than once" location_name component_type_name)


(*
module type FRESH_COMPONENT_NAMES =
  sig
    type t
    val fresh_component_name : t -> location_name -> component_type_name -> (t, component_type_name)
  end
*)

module Fresh_component_names : FRESH_COMPONENT_NAMES =
  struct
    
    type t = component_type_name list

    let fresh_component_name (location_name : location_name) (component_type_name : component_type_name) (used_names : t) =
      let fresh_name = Printf.sprintf "%s %s" location_name component_type_name
      in
      ( (fresh_name :: used_names), fresh_name )

    let fresh_component_names (n : int) (location_name : location_name) (component_type_name : component_type_name) (used_names : t) =

      let f (n : int) (used_names : t) (a : component_type_name list) =
        match n with
        | 0 -> (used_names, a)
        | i -> let (new_used_names, fresh_name) = fresh_component_name location_name component_type_name used_names
               in
               f (n - 1) new_used_names (fresh_name :: a)

      in 
      f n used_names []

  end

let old_components_that_remain_for_location (initial_configuration : configuration) (solution : solution) (location_name : location_name) (component_type_name : component_type_name) : component list =
  
  let number_of_components =
    solution_number_of_components_for_location solution location_name component_type_name
  
  and initial_configuration_components =
    List.filter_map ( fun component ->
      if (component.component_type = component_type_name) && (component.component_location = location_name)
      then Some component.component
      else None
    ) initial_configuration.configuration_components

  in

  BatList.take number_of_components initial_configuration_component_names

let old_components_that_remain (universe : universe) (initial_configuration : configuration) (solution : solution) : component list =
  let location_names = get_location_names initial_configuration
  and component_type_names = get_component_type_names universe
  in
  List.flatten ( List.flatten (
    List.map (fun location_name ->
      List.map (fun component_type_name ->
        old_components_that_remain_for_location initial_configuration solution location_name component_type_name
      ) component_type_names
    ) location_names
  ) )

let new_components_for_location (initial_configuration : configuration) (solution : solution) (location_name : location_name) (component_type_name : component_type_name) (used_names : Fresh_component_names.t) : component list =
  
  let right_number_of_components =
    solution_number_of_components_for_location solution location_name component_type_name
  
  and initial_configuration_components =
    List.filter_map ( fun component ->
      if (component.component_type = component_type_name) && (component.component_location = location_name)
      then Some component.component
      else None
    ) initial_configuration.configuration_components
  in

  let number_of_initial_components = List.length initial_configuration_components
  in

  let fresh_component_names =
    Fresh_component_names.fresh_component_names
      (right_number_of_components - number_of_initial_components)
      location_name
      component_type_name
      used_names

  (* HERE *)

let configuration_of_solution (universe : universe) (solution : solution) (initial_configuration : configuration) : configuration = 

  let configuration_locations =
    List.map (fun location -> {
        location_name               : location.location_name;
        location_provide_resources  : location.location_provide_resources;
        location_repository         : solution_repository_for_location solution location.location_name;
        location_packages_installed : solution_packages_for_location   solution location.location_name;
      }
    ) initial_configuration.configuration_locations


  let resources_list =

    List.flatten (
      
      List.map (function (domain_element, number) ->
        
        let numbers = BatList.init number (fun x -> x + 1)
        in
        List.map (function number ->
          let name     = fresh_name domain_element number
          and requires = requires typing_context domain_element
          and provides = provides typing_context domain_element
          in
          ( 
            (name, domain_element),
            {
              resource_name     = name;
              resource_requires = requires;
              resource_provides = provides;
            }
          )
        ) numbers
      
      ) solution.solution_domain_elements )

  in
  let typing_environment = List.map fst resources_list
  and resources          = List.map snd resources_list
  in
  let bindings           = generate_bindings typing_context resources

  in
  {
    typing_environment = typing_environment;
    resources          = resources;
    bindings           = bindings;
  }
