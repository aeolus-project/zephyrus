
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

let configuration_of_solution (universe : universe) (solution : solution) : configuration = 

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
