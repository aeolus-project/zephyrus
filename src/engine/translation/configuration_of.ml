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

(** Translate a constraint problem solution to a final configuration. *)

open Data_model
open Data_model_catalog
open Data_constraint
open Data_common

class type extended_solution_iface =
  object
    method domain          : Variable_set.t
    method variable_values : variable -> int

    method get_repository_id_of_a_location        : location_id -> repository_id
    method get_package_ids_of_a_location          : location_id -> Package_id_set.t
    method get_number_of_components_of_a_location : location_id -> component_type_id -> int 
  end

class extended_solution (solution : solution) : extended_solution_iface = 
  let domain = solution.domain in
  let value  = solution.variable_values in

  object
    method domain          = domain
    method variable_values = value

    method get_repository_id_of_a_location location_id = 
      let module Repository_id_set_of_variable_set = Set.Convert(Variable_set)(Repository_id_set) in
      let location_repository_ids : Repository_id_set.t =
        Repository_id_set_of_variable_set.filter_convert (fun variable ->
          match variable with
          | Local_repository_variable (var_location_id, var_repository_id) -> 
              if (var_location_id = location_id) && ((value variable) = 1)
              then Some(var_repository_id)
              else None
          | _ -> None
        ) domain
      in
      match Repository_id_set.cardinal location_repository_ids with
      | 1 -> Repository_id_set.choose location_repository_ids
      | 0 -> failwith "No repository attributed to a location!"
      | _ -> failwith "Many repositories attributed to a location!"

    method get_package_ids_of_a_location location_id = 
      let module Package_id_set_of_variable_set = Set.Convert(Variable_set)(Package_id_set) in
      let location_package_ids : Package_id_set.t =
        Package_id_set_of_variable_set.filter_convert (fun variable ->
          match variable with
          | Local_variable (var_location_id, Package(var_package_id)) -> 
              if (var_location_id = location_id) && ((value variable) = 1)
              then Some(var_package_id)
              else None
          | _ -> None
        ) domain
      in
      location_package_ids

    (* How many components of a given component type should be installed on a given location according to this solution? *)
    method get_number_of_components_of_a_location location_id component_type_id =
      let components_count =
        Variable_set.filter (fun variable -> 
          match variable with    
          | Local_variable (var_location_id, (Component_type var_component_type_id)) ->
              (var_location_id = location_id) && (var_component_type_id = component_type_id)
          | _ -> false
        ) domain
      in
      match Variable_set.cardinal components_count with
      | 1 -> value (Variable_set.choose components_count)
      | 0 -> failwith "No number of a certain component type attributed to a location!"
      | _ -> failwith "Many numbers of a certain component type attributed to a location!"

  end


(* Get all the "root" packages, i.e. the packages that implement something. *)
let get_root_packages (universe : universe) : Package_id_set.t = 
  Component_type_id_set.fold (fun component_type_id root_package_ids ->
    Package_id_set.union root_package_ids (universe#get_implementation component_type_id)
  ) universe#get_implementation_domain Package_id_set.empty


(* Utility to generate fresh names for components. *)
module Used_names = Used_tokens_string
type used_names = Used_names.t

(* Begin: a hack to optimize the component naming process. *)
let last_used_i : int Component_type_name_map.t ref = ref Component_type_name_map.empty

let update_last_used_i component_type_name i : unit =
  last_used_i := Component_type_name_map.add component_type_name i !last_used_i

let get_last_used_i component_type_name : int =
  try
    Component_type_name_map.find component_type_name !last_used_i
  with Not_found ->
    let i = 1 in
    update_last_used_i component_type_name i; i
(* End: a hack to optimize the component naming process. *)

let fresh_component_name (location_name : location_name) (component_type_name : component_type_name) (used_names : used_names) : component_name =

  (* Prepare the proper component type name which should be used. *)
  let component_type_name =
    (* Was the component type was converted from the stateful form? *)
    match (Settings.find Settings.input_stateful) with
    (* No! Them the component type name is the right one. *)
    | false -> component_type_name
    (* Yes! Then the name of the initial stateful component type should be used, not the one created artificially for its stateless form. *)
    | true  -> fst (Stateful_converter.To_stateful.stateless_component_type_name component_type_name) in

  let build_component_name = 
    Printf.sprintf
    "%s-%d" 
    (String_of.component_type_name component_type_name)

  in

  let i = ref (get_last_used_i component_type_name) in (* a hack to optimize the component naming process *)
  let component_name = ref (build_component_name !i) in
  while Used_names.mem !component_name used_names do
    i := !i + 1;
    component_name := build_component_name !i;
  done;
  update_last_used_i component_type_name !i; (* a hack to optimize the component naming process *)

  Used_names.add !component_name used_names;
  !component_name

(* Intermediary type to represent a component which will be present in the final configuration. *)
type almost_done_component =
  | ReusedComponent of (Data_model.component_id * component) (* Either it is a component which was already present in the initial configuration. In this case he will have the same name as before. *)
  | NewComponent    of (used_names -> component)           (* Or it is a new component, it will need a new name, but in order to give him one we need to know all the component names which have been already used in the configuration. *)


(* Generate the components which will be present in the final configuration. *)
let generate_components 
  (new_component_catalog     : Component_catalog.catalog)
  (initial_component_ids     : Component_id_set.t)
  (get_component             : component_id -> component)
  (location_ids              : Location_id_set.t)
  (location_name_of_id       : location_id -> location_name)
  (component_type_ids        : Component_type_id_set.t)
  (component_type_name_of_id : component_type_id -> component_type_name)
  (solution                  : extended_solution_iface)
  : Component_set.t =

  let almost_done_components : almost_done_component list ref  = ref [] in

  Location_id_set.iter (fun location_id ->
    Component_type_id_set.iter (fun component_type_id ->
      
      let required_number_of_components = solution#get_number_of_components_of_a_location location_id component_type_id in

      (* In the given location (-> location_id)
           there should be exacly this many (-> required_number_of_components)
           components of the given type (-> component_type_id). *)

      (* Get the components of this type installed at this location in the initial configuration. *)
      let initial_component_ids : Component_id_set.t =
        Component_id_set.filter (fun component_id ->
          let component = get_component component_id in
          (component#typ = component_type_id) && (component#location = location_id)
        ) initial_component_ids
    
      in

      let initial_components_left : Component_id_set.t ref          = ref initial_component_ids in 
      let new_components          : almost_done_component list ref  = ref [] in

      while (List.length !new_components) < required_number_of_components do
        if not (Component_id_set.is_empty !initial_components_left)
        then begin
          (* If we have still some initial components left we reuse them. *)
          let component_id_to_reuse = Component_id_set.choose !initial_components_left in
          let component_to_reuse    = get_component component_id_to_reuse in
          initial_components_left := Component_id_set.remove component_id_to_reuse !initial_components_left;
          new_components := ReusedComponent(component_id_to_reuse, component_to_reuse) :: !new_components
        end else begin
          (* If we don't have any more initial components left we create new ones. *)
          let new_component : (used_names -> component) =
            (fun used_names -> 
              let component_name = fresh_component_name (location_name_of_id location_id) (component_type_name_of_id component_type_id) used_names in
              let component_id   = new_component_catalog#get_else_add component_name in
              new component
                ~typ:      component_type_id
                ~location: location_id
                ()
            ) in
          new_components := NewComponent(new_component) :: !new_components
        end
      done;

      almost_done_components := !new_components @ !almost_done_components

    ) component_type_ids
  ) location_ids;


  (* We have prepared all the almost-done-components. Now we can extract all their names
     and we will have the initial set of names already used in the configuration
     (all coming from reused components, cause the new ones are unnamed for now). *)
  let reused_component_names = List.filter_map (function
    | ReusedComponent (component_id, component) -> Some (Name_of.component_id component_id)
    | NewComponent    _                         -> None
  ) !almost_done_components in

  let (used_names : used_names) = Used_names.empty () in 
  List.iter (fun name -> Used_names.add name used_names) reused_component_names;

  (* Now we proceed to name the new components and make them "done" (in opposition to "almost-done"). 
     We pass the "used_names" around in order to do that: it is a reference and it will not only serve
     to generate a fresh name for each new component, but it also collects the new names as we go. *)
  Component_set.of_list_directly (List.map (fun almost_done_component ->
    match almost_done_component with
    | ReusedComponent (component_id, component) -> component
    | NewComponent     component_f              -> component_f used_names
  ) !almost_done_components)
  

(* Set up for the matching algorithm. *)
module My_matching_algorithm = Candy_algorithm

(* TMP
open My_matching_algorithm.Int_list_match_requirers_with_providers
open My_matching_algorithm.Int_list_requirer_provider_types
*)

open My_matching_algorithm.Int_set_map_match_requirers_with_providers
open My_matching_algorithm.Int_set_map_requirer_provider_types


(* Generate bindings which will be present in the final configuration (using the matching algorithm). *)
let generate_bindings (universe : universe) (component_ids : Component_id_set.t) (get_component : component_id -> component) : Binding_set.t =

  (* Get all the ports mentioned in the universe. *)
  let port_ids = universe#get_port_ids in

  let bindings : Binding_set.t ref = ref Binding_set.empty in
  
  (* Generate bindings for each port p. *)
  Port_id_set.iter (fun port_id ->

      (* Prepare the inputs for the matching algorithm: *)

      (* 1. Mapping from component name to their require arity on port p. *)
      let requirers : My_matching_algorithm.Int_list_requirer_provider_types.Requirers.t = 
        List.filter_map (fun component_id ->
          let component      = get_component component_id in
          let component_type = universe#get_component_type component#typ in
          if Port_id_set.mem port_id component_type#require_domain
          then begin
            let require_arity  = component_type#require port_id in
            if require_arity > 0
            then Some (component_id, require_arity)
            else None
          end else None
        ) (Component_id_set.elements component_ids)
  
      (* 2. Mapping from component name to their provide arity on port p. *)
      and providers : My_matching_algorithm.Int_list_requirer_provider_types.Providers.t = 
        List.filter_map (fun component_id ->
          let component      = get_component component_id in
          let component_type = universe#get_component_type component#typ in
          if Port_id_set.mem port_id component_type#provide_domain
          then begin
            let provide_arity  = component_type#provide port_id in
            match provide_arity with
            | Infinite_provide -> Some (component_id, My_matching_algorithm.DecrementableIntegerWithInfinity.InfiniteInteger)
            | Finite_provide i -> if i > 0
                                  then Some (component_id, My_matching_algorithm.DecrementableIntegerWithInfinity.FiniteInteger i)
                                  else None          
          end else None
        ) (Component_id_set.elements component_ids)
  
      in
  
      (* TMP *)
      let providers = {
        Providers.set = Providers.Provider_key_provider_arity_set.of_list_directly providers;
        Providers.map = Providers.Provider_key_map.of_assoc_list                   providers;
      } in

      (* Launch the matching alogrithm with the prepared inputs! *)
      match matching_algorithm requirers providers with

      (* If there is no way to generate required bindings between these components (which should never happen). *)
      | None -> 
          failwith (Printf.sprintf
            "Matching algorithm has failed for a solution which should be correct (for port %s)!"
            (String_of.port_id port_id) )

      (* The matching algoritm has yielded a result. *)
      | Some results -> 
    
        (* We convert the matching algorithm result to actual bindings. *)
        List.iter (fun result -> 
          let binding = 
            new binding
              ~port:     port_id
              ~provider: result.provides
              ~requirer: result.requires
          in
          bindings := Binding_set.add binding !bindings
        ) results
  
    ) port_ids;

    !bindings



let solution (catalog : Data_model_catalog.closed_model_catalog option) (universe : universe) (initial_configuration : configuration) (solution_prev : solution) : configuration =

  let solution = new extended_solution solution_prev in

  (* Locations *)
  let location_ids : Location_id_set.t = initial_configuration#get_location_ids in

  let location_id_to_location_map : location Location_id_map.t =
    Location_id_set.fold (fun location_id location_id_to_location_map ->

      (* the location object *)
      let location = initial_configuration#get_location location_id in

      (* repository *)
      let repository = solution#get_repository_id_of_a_location location_id in

      (* packages installed *)
      let packages_installed      = solution#get_package_ids_of_a_location location_id in
      let root_package_ids        = get_root_packages universe in
      let root_packages_installed = Package_id_set.inter root_package_ids packages_installed in

      (* resources provided *)
      let provide_resources : resource_provide_arity Resource_id_map.t =
        Resource_id_set.fold (fun resource_id resource_id_to_provide_arity_map ->
          let resource_provide_arity = 
            if Settings.find Settings.modifiable_configuration
            then solution_prev.variable_values (Local_resource_variable(location_id, resource_id)) (* if we can modify the location, we set its provides to the right value *)
            else location#provide_resources resource_id in
          Resource_id_map.add resource_id resource_provide_arity resource_id_to_provide_arity_map
        ) universe#get_resource_ids Resource_id_map.empty in

      let new_location : location =
        location#copy
          ~repository:         repository
          ~packages_installed: (if true then root_packages_installed else packages_installed)
          ~provide_resources:  provide_resources
          () in

      Location_id_map.add location_id new_location location_id_to_location_map

    ) location_ids Location_id_map.empty in

  let location_obj_catalog : Location_obj_catalog.obj_catalog_iface = 
    Location_obj_catalog.of_id_to_obj_map location_id_to_location_map in

  let component_catalog : Component_catalog.catalog = 
    match catalog with
    | None         -> new Component_catalog.catalog
    | Some catalog -> Component_catalog.of_id_to_name_map catalog#component#id_to_name_map  in

  (* components *)
  let component_set : Component_set.t =
    generate_components
      component_catalog                       (* Will be updated with new components during the execution of generate_components. *)
      initial_configuration#get_component_ids (* Domain of component ids. *)
      initial_configuration#get_component     (* Function component_id -> component. *)
      location_ids                            (* Domain of location ids. *)
      Name_of.location_id                     (* Function location_id -> location_name. *)
      universe#get_component_type_ids         (* Domain of component_type ids. *)
      Name_of.component_type_id               (* Function component_type_id -> component_type_name. *)
      solution                                (* The constraint problem solution. *)
  in

  (* Update the main catalog with new components catalog. TODO: This is quite cumbersome... Maybe a copy method? *)
  let updated_catalog = (
    match catalog with
    | None         -> None
    | Some catalog -> Some (new closed_model_catalog 
                              ~component_type_catalog: catalog#component_type
                              ~port_catalog:           catalog#port
                              ~repository_catalog:     catalog#repository
                              ~package_catalog:        catalog#package
                              ~resource_catalog:       catalog#resource
                              ~location_catalog:       catalog#location
                              ~component_catalog:      (Component_catalog.close_catalog component_catalog))) in

  Name_of.set_catalog updated_catalog;

  let component_obj_catalog : Component_obj_catalog.obj_catalog_iface = Component_obj_catalog.of_set_of_objs component_set in

  (* bindings *)
  let bindings = 
    let component_ids : Component_id_set.t        = component_obj_catalog#ids in
    let get_component : component_id -> component = component_obj_catalog#obj_of_id in
    generate_bindings universe component_ids get_component in

  new configuration
    ~locations:  location_obj_catalog#id_to_obj_map
    ~components: component_obj_catalog#id_to_obj_map
    ~bindings:   bindings
    ()
