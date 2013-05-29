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


open Model_t
open Model_output.Plain

open Typing_context
open Variables
open Solution
open Helpers


(* Set up for the matching algorithm. *)
module My_matching_algorithm = Matching_algorithm

open My_matching_algorithm.List_match_requirers_with_providers
open My_matching_algorithm.String_list_requirer_provider_types


(* Functions which convert model require and provide arities 
   into the format required for the matching algorithm. *)

let make_require_arity (i : int)        = i

let make_provide_arity provide_arity =
  let make_finite_provide_arity (i : int) = My_matching_algorithm.DecrementableIntegerWithInfinity.FiniteInteger i
  and make_infinite_provide_arity         = My_matching_algorithm.DecrementableIntegerWithInfinity.InfiniteInteger
  in
  match provide_arity with
  | `InfiniteProvide -> (make_infinite_provide_arity)
  | `FiniteProvide i -> (make_finite_provide_arity i)


(* Generate bindings which will be present in the final configuration (using the matching algorithm). *)
let generate_bindings (universe : universe) (components : component list) : binding list =

  (* Get all the ports mentioned in the universe. *)
  let ports = get_port_names universe
  in
  
  (* Generate bindings for each port p. *)
  List.flatten (
    List.map (fun port_name ->

      (* Prepare the inputs for the matching algorithm: *)

      (* 1. Mapping from component name to their require arity on port p. *)
      let requirers = 
        List.map (fun component ->
          let component_type = get_component_type universe component.component_type
          in
          let require_arity = get_require_arity component_type port_name
          in
          (component.component_name, make_require_arity require_arity)
        ) components
  
      (* 2. Mapping from component name to their provide arity on port p. *)
      and providers = 
        List.map (fun component ->
          let component_type = get_component_type universe component.component_type
          in
          let provide_arity = get_provide_arity component_type port_name
          in
          (component.component_name, make_provide_arity provide_arity)
        ) components
  
      in
  
      (* Launch the matching alogrithm with the prepared inputs! *)
      match matching_algorithm requirers providers with

      (* If there is no way to generate required bindings between these components (which should never happen). *)
      | None -> 
          failwith (Printf.sprintf
            "Matching algorithm has failed for a solution which should be correct (for port %s)!"
            (string_of_port_name port_name) )

      (* The matching algoritm has yielded a result. *)
      | Some results -> 
    
        (* We convert the matching algorithm result to actual bindings. *)
        List.map (fun result -> {
            binding_port = port_name;
            binding_provider = result.provides;
            binding_requirer = result.requires;
        }) results
  
    ) ports )


(* Which repository should be installed on a given location have according to this solution? *)
let solution_repository_for_location (solution : solution) (location_name : location_name) : repository_name =
  let repository_names =
    List.filter_map (fun (var, value) -> 
      match var with
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

(* Which packages should be installed on a given location according to this solution? *)
let solution_packages_for_location (solution : solution) (location_name : location_name) : package_name list =
  let package_names =
    List.filter_map (fun (var, value) -> 
      match var with    
      | LocalElementVariable (var_location_name, (Package var_package_name)) ->
          if (var_location_name = location_name) && (value = 1)
          then Some var_package_name
          else None
      | _ -> None
    ) solution
  in
  package_names

(* How many components of a given component type should be installed on a given location according to this solution? *)
let solution_number_of_components_for_location (solution : solution) (location_name : location_name) (component_type_name :component_type_name) : int =
  let components_counts =
    List.filter_map (fun (var, value) -> 
      match var with    
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



(* Utility to generate fresh names for components. *)
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


(* Returns an association list which tells us for each location and component type
   how many components of this type should be installed at this location. *)
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
        
        let number_of_components_for_location = 
          number_of_components_for_location_function location_name component_type_name
        in
        ( (location_name, component_type_name), number_of_components_for_location)

      ) component_type_names
    ) location_names
  )

(* Intermediary type to represent a component which will be present in the final configuration. *)
type almost_done_component =
  | ReusedComponent of component (* Either it is a component which was already present in the initial configuration. In this case he will have the same name as before. *)
  | NewComponent    of (used_names ref -> component) (* Or it is a new component, it will need a new name, but in order to give him one we need to know all the component names which have been already used in the configuration. *)


(* Generate the components which will be present in the final configuration. *)
let generate_components 
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

        (* In the given location (-> location_name)
           there should be exacly this many (-> required_number_of_components)
           components of the given type (-> component_type_name). *)

        (* Get the components of this type installed at this location in the initial configuration. *)
        let initial_components =
          List.filter_map ( fun component ->
            if (component.component_type = component_type_name) 
            && (component.component_location = location_name)
            then Some component
            else None
          ) initial_components
      
        in

        (* Some of these can be reused in the final configuration (up to the required number). *)
        let reused_components =
          List.take required_number_of_components initial_components (* We take first components declared! *)

        in

        (* If it is not enough, we need to create a certain number of new components. *)
        let number_of_new_components =
          (required_number_of_components - (List.length reused_components))

        in
        assert (number_of_new_components >= 0);


        (* We construct the list of almost-done-components which will be in the final configuration: *)

        (* 1. reused components *)

        (* We can reuse the inital components almost directly. *)
        (List.map 
          (fun component -> 
            ReusedComponent component 
          ) reused_components
        )

        @

        (* 2. new components *)

        (* We have to create a required number of new components of the given type. *)
        (List.make 
          number_of_new_components
          (* The new components need to have unique names. And in order to give them unique names we
             have to know all the names which are already present in the configuration. We cannot know
             it now, so we postpone the name giving part until all almost-done-components are ready. *)
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

  (* We have prepared all the almost-done-components. Now we can extract all their names
     and we will have the initial set of names already used in the configuration
     (all coming from reused components, cause the new ones are unnamed for now). *)
  let used_names = ref (List.filter_map (fun almost_done_component ->
    match almost_done_component with
    | ReusedComponent component -> Some component.component_name
    | NewComponent    _         -> None
  ) almost_done_components)

  in

  (* Now we proceed to name the new components and make them "done" (in opposition to "almost-done"). 
     We pass the "used_names" around in order to do that: it is a reference and it will not only serve
     to generate a fresh name for each new component, but it also collects the new names as we go. *)
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
    let location_names       = get_location_names       initial_configuration
    and component_type_names = get_component_type_names universe
    in
    generate_components 
      initial_configuration.configuration_components
      location_names
      component_type_names
      solution
  
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
