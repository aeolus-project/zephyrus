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

open Typing_context
open Variable_keys
open Solution
open Facile_variables
open Helpers
open ExtLib

(* Set up for the matching algorithm. *)

module My_matching_algorithm = Matching_algorithm

open My_matching_algorithm.List_match_requirers_with_providers
open My_matching_algorithm.String_list_requirer_provider_types

let make_require_arity (i : int)        = i

let make_finite_provide_arity (i : int) = My_matching_algorithm.DecrementableIntegerWithInfinity.FiniteInteger i
let make_infinite_provide_arity         = My_matching_algorithm.DecrementableIntegerWithInfinity.InfiniteInteger


(* Generating bindings using the matching algorithm. *)

let generate_bindings (universe : universe) (components : component list) : binding list =

  let ports = get_port_names universe
  in
  
  List.flatten (
    List.map (fun port_name ->
      let requirers = (* the mapping from component name to their require multiplicity on p *) 
        List.map (fun component ->
          let component_type = get_component_type universe component.component_type
          in
          let require_arity = 
            if List.mem_assoc port_name component_type.component_type_require
            then List.assoc port_name component_type.component_type_require
            else 0
          in
          (component.component_name, require_arity)
        ) components
  
      and providers = 
        List.map (fun component ->
          let component_type = get_component_type universe component.component_type
          in
          let provide_arity = 
            if List.mem_assoc port_name component_type.component_type_provide
            then List.assoc port_name component_type.component_type_provide
            else (`FiniteProvide 0)
          in
          let provide_arity =
            match provide_arity with
            | `InfiniteProvide -> (make_infinite_provide_arity)
            | `FiniteProvide i -> (make_finite_provide_arity i)
          in
          (component.component_name, provide_arity)
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
    List.filter_map (fun (var_key, value) -> 
      match var_key with
      | LocalRepositoryVariable (var_location_name, var_repository_name) ->
          if (var_location_name = location_name) && (value = 1)
          then Some var_repository_name
          else None
      | _ -> None
    ) solution
  in
  match repository_names with
  | [repository_name] -> repository_name
  | []                -> failwith (Printf.sprintf "in the solution no repository is assigned to location %s"                  location_name)
  | _                 -> failwith (Printf.sprintf "in the solution there is more than one repository assigned to location %s" location_name)

let solution_packages_for_location (solution : solution) (location_name : location_name) : package_name list =
  let package_names =
    List.filter_map (fun (var_key, value) -> 
      match var_key with    
      | LocalElementVariable (var_location_name, (Package var_package_name)) ->
          if (var_location_name = location_name) && (value = 1)
          then Some var_package_name
          else None
      | _ -> None
    ) solution
  in
  package_names

let solution_number_of_components_for_location (solution : solution) (location_name : location_name) (component_type_name :component_type_name) : int =
  let components_counts =
    List.filter_map (fun (var_key, value) -> 
      match var_key with    
      | LocalElementVariable (var_location_name, (ComponentType var_component_type_name)) ->
          if (var_location_name = location_name) && (var_component_type_name = component_type_name)
          then Some value
          else None
      | _ -> None
    ) solution
  in
  match components_counts with
  | [components_count] -> components_count
  | []                 -> failwith (Printf.sprintf "in the solution number of components of type %s in location %s is unknown"                location_name component_type_name)
  | _                  -> failwith (Printf.sprintf "in the solution number of components of type %s in location %s is defined more than once" location_name component_type_name)

type used_names = component_name list

let fresh_component_name (location_name : location_name) (component_type_name : component_type_name) (used_names : used_names ref) : component_name =

  let build_component_name = 
    Printf.sprintf
    "%s-%s-%d" 
    (string_of_location_name location_name)
    (string_of_component_type_name component_type_name)

  in

  let i = ref 1 in
  let component_name = ref (build_component_name !i) in
  while List.mem !component_name !used_names do
    i := !i + 1;
    component_name := build_component_name !i;
  done;

  used_names := !component_name :: !used_names;
  !component_name


let component_type_count_by_location 
  (location_names : location_name list) 
  (component_type_names : component_type_name list) 
  (solution : solution)
  : ((location_name * component_type_name) * int) list =
  
  let number_of_components_for_location_function =
    solution_number_of_components_for_location solution
  in
  
  List.flatten (
    List.map (fun location_name ->
      List.map (fun component_type_name ->
        
        let number = 
          number_of_components_for_location_function location_name component_type_name
        in
        ( (location_name, component_type_name), number)

      ) component_type_names
    ) location_names
  )

type almost_done_component =
  | ReusedComponent of component
  | NewComponent    of (used_names ref -> component)

let components_f 
  (initial_components : component list)
  (location_names : location_name list)
  (component_type_names : component_type_name list)
  (solution : solution)
  : component list =

  let component_type_count_by_location =
    component_type_count_by_location location_names component_type_names solution

  in

  let (almost_done_components : almost_done_component list) =
    List.flatten (
      List.map (fun ((location_name, component_type_name), required_number_of_components) ->

        let initial_components =
          List.filter_map ( fun component ->
            if (component.component_type = component_type_name) && (component.component_location = location_name)
            then Some component
            else None
          ) initial_components
      
        in
      
        let reused_components =
          List.take required_number_of_components initial_components (* We take first components declared! *)

        in

        let number_of_new_components =
          (required_number_of_components - (List.length reused_components))

        in
        assert (number_of_new_components >= 0);


        (* Reused components *)

        (List.map 
          (fun component -> 
            ReusedComponent component
          ) reused_components
        )

        @

        (* New components *)

        (List.make 
          number_of_new_components
          (NewComponent (fun used_names -> 
            let component_name = fresh_component_name location_name component_type_name used_names
            in 
            ({
              component_name     = component_name;
              component_type     = component_type_name;
              component_location = location_name;
            })
        )))
    ) component_type_count_by_location )

  in

  let used_names = ref (List.filter_map (fun almost_done_component ->
    match almost_done_component with
    | ReusedComponent component -> Some component.component_name
    | NewComponent    _         -> None
  ) almost_done_components)

  in

  List.map (fun almost_done_component ->
      match almost_done_component with
    | ReusedComponent component   -> component
    | NewComponent    component_f -> component_f used_names
  ) almost_done_components







let configuration_of_solution (universe : universe) (initial_configuration : configuration) (solution : solution) : configuration = 

  (* Locations *)
  let configuration_locations =
    List.map (fun location -> {
        location_name               = location.location_name;
        location_provide_resources  = location.location_provide_resources;
        location_repository         = solution_repository_for_location solution location.location_name;
        location_packages_installed = solution_packages_for_location   solution location.location_name;
      }
    ) initial_configuration.configuration_locations

  in

  (* Components *)
  let configuration_components = 
    let location_names = get_location_names initial_configuration
    and component_type_names = get_component_type_names universe
    in
    components_f initial_configuration.configuration_components location_names component_type_names solution
  
  in

  (* Bindings *)
  let configuration_bindings = 
    generate_bindings universe configuration_components

  in
  {
    configuration_locations  = configuration_locations;
    configuration_components = configuration_components;
    configuration_bindings   = configuration_bindings;
  }
