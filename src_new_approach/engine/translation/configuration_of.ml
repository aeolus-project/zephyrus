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



(* Utility to generate fresh names for components. *)
type used_names = Component_name_set.t

let fresh_component_name (location_name : location_name) (component_type_name : component_type_name) (used_names : used_names ref) : component_name =

  let build_component_name = 
    Printf.sprintf
    "%s-%d" 
    (String_of.component_type_name component_type_name)

  in

  let i = ref 1 in
  let component_name = ref (build_component_name !i) in
  while Component_name_set.mem !component_name !used_names do
    i := !i + 1;
    component_name := build_component_name !i;
  done;

  used_names := Component_name_set.add !component_name !used_names;
  !component_name

(* Intermediary type to represent a component which will be present in the final configuration. *)
type almost_done_component =
  | ReusedComponent of component                     (* Either it is a component which was already present in the initial configuration. In this case he will have the same name as before. *)
  | NewComponent    of (used_names ref -> component) (* Or it is a new component, it will need a new name, but in order to give him one we need to know all the component names which have been already used in the configuration. *)


(* Generate the components which will be present in the final configuration. *)
let generate_components 
  (initial_components        : Component_set.t)
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
      let initial_components : Component_set.t =
        Component_set.filter (fun component ->
          (component#typ = component_type_id) && (component#location = location_id)
        ) initial_components
    
      in

      let initial_components_left : Component_set.t ref             = ref initial_components in 
      let new_components          : almost_done_component list ref  = ref [] in

      while (List.length !new_components) < required_number_of_components do
        if not (Component_set.is_empty !initial_components_left)
        then begin
          (* If we have still some initial components left we reuse them. *)
          let component_to_reuse = Component_set.choose !initial_components_left in
          initial_components_left := Component_set.remove component_to_reuse !initial_components_left;
          new_components := ReusedComponent(component_to_reuse) :: !new_components
        end else begin
          (* If we don't have any more initial components left we create new ones. *)
          let new_component : (used_names ref -> component) =
            (fun used_names -> 
              let component_name = fresh_component_name (location_name_of_id location_id) (component_type_name_of_id component_type_id) used_names
              in 
              object
                method name     = component_name;
                method id       = -1 (* TODO: to fix when we will add the new components to the catalog ... *) 
                method typ      = component_type_id;
                method location = location_id;
              end) in
          new_components := NewComponent(new_component) :: !new_components
        end
      done;

      almost_done_components := !new_components @ !almost_done_components

    ) component_type_ids
  ) location_ids;

  (* We have prepared all the almost-done-components. Now we can extract all their names
     and we will have the initial set of names already used in the configuration
     (all coming from reused components, cause the new ones are unnamed for now). *)
  let (used_names : used_names ref) = ref (Component_name_set.set_of_direct_list (
    Output_helper.filter_map (function
    | ReusedComponent component -> Some component#name
    | NewComponent    _         -> None
    ) !almost_done_components)) in

  (* Now we proceed to name the new components and make them "done" (in opposition to "almost-done"). 
     We pass the "used_names" around in order to do that: it is a reference and it will not only serve
     to generate a fresh name for each new component, but it also collects the new names as we go. *)
  Component_set.set_of_direct_list (List.map (fun almost_done_component ->
    match almost_done_component with
    | ReusedComponent component   -> component
    | NewComponent    component_f -> component_f used_names
  ) !almost_done_components)
  

(* Set up for the matching algorithm. *)
module My_matching_algorithm = Candy_algorithm

open My_matching_algorithm.Int_list_match_requirers_with_providers
open My_matching_algorithm.Int_list_requirer_provider_types

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
        Output_helper.filter_map (fun component_id ->
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
        Output_helper.filter_map (fun component_id ->
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
            object
              method port     = port_id
              method provider = result.provides
              method requirer = result.requires
            end
          in
          bindings := Binding_set.add binding !bindings
        ) results
  
    ) port_ids;

    !bindings



let solution (universe : universe) (initial_configuration : configuration) (solution : solution) : configuration =

  let solution = new extended_solution solution in

  (* Locations *)
  let location_ids : Location_id_set.t = initial_configuration#get_location_ids in

  let location_id_to_location_map : (location Location_id_map.t) ref = ref Location_id_map.empty in
  
  Location_id_set.iter (fun location_id ->

    let location = initial_configuration#get_location location_id in

    (* name *)
    let name = location#name in

    (* repository *)
    let repository = solution#get_repository_id_of_a_location location_id in

    (* packages installed *)
    let packages_installed = solution#get_package_ids_of_a_location location_id in

    (* resources provided *)
    let provide_resources = location#provide_resources in

    (* cost *)
    let cost = location#cost in

    let new_location : location =
      object
        method name                = name
        method id                  = location_id
        method repository          = repository
        method packages_installed  = packages_installed
        method provide_resources   = provide_resources
        method cost                = cost
      end

    in
    location_id_to_location_map := Location_id_map.add location_id new_location !location_id_to_location_map;

  ) location_ids;

  let location_catalog : Location_obj_catalog.obj_catalog_iface = Location_obj_catalog.of_id_to_obj_map !location_id_to_location_map in

  let locations    : Location_set.t          = location_catalog#objs in
  let get_location : location_id -> location = location_catalog#obj_of_id in

  let location_name_catalog : Location_catalog.catalog_iface = Location_catalog.of_id_to_name_map (Location_id_map.map get_name !location_id_to_location_map) in
  let location_names        : Location_name_set.t            = location_name_catalog#names in
  let location_name_of_id   : location_id -> location_name   = location_name_catalog#name_of_id in
  let location_id_of_name   : location_name -> location_id   = location_name_catalog#id_of_name in

  let get_local_package location_id package_id : bool =
    Package_id_set.mem package_id (get_location location_id)#packages_installed in
  

  (* components *)
  let components : Component_set.t = generate_components
    initial_configuration#get_components
    location_ids
    location_name_of_id
    universe#get_component_type_ids
    universe#get_component_type_name
    solution in

  let component_catalog : Component_obj_catalog.obj_catalog_iface = Component_obj_catalog.of_set_of_objs components in
  let component_ids = component_catalog#ids in
  
  let get_component : component_id -> component = component_catalog#obj_of_id in

  let component_name_catalog : Component_catalog.catalog_iface = Component_catalog.of_id_to_name_map (Component_id_map.map get_name component_catalog#id_to_obj_map) in
  let component_names        : Component_name_set.t            = component_name_catalog#names in
  let component_name_of_id   : component_id -> component_name  = component_name_catalog#name_of_id in
  let component_id_of_name   : component_name -> component_id  = component_name_catalog#id_of_name in

  let get_local_components location_id component_type_id : Component_id_set.t =
    Component_id_set.filter (fun component_id -> ((get_component component_id)#location = location_id)) component_ids in


  (* bindings *)
  let bindings = generate_bindings universe component_ids get_component in

  object(self)
    (* methods *)
    method get_location  = get_location
    method get_component = get_component

    method get_locations  = locations
    method get_components = components
    method get_bindings   = bindings

    method get_location_ids  = location_ids
    method get_component_ids = component_ids

    method get_location_names  = location_names
    method get_component_names = component_names

    method c_l      = self#get_location_ids
    method c_c      = self#get_component_ids
    method c_type c = (self#get_component c)#typ

    method get_local_component = get_local_components
    
    method get_local_package   = get_local_package

    method get_location_id  = location_id_of_name
    method get_component_id = component_id_of_name

    method get_location_name  = location_name_of_id
    method get_component_name = component_name_of_id
  end

(*/************************************************************************\*)
(*| 4. Merge.                                                              |*)
(*\************************************************************************/*)

let merge c1 c2 = object(self)
 (* we suppose that
   - the two configurations are totally disjoints (which is the case when we merge the annex part of the configuration with the partial solution)
   - except for mappings, where it is tolerated if the two mappings have the same image on the common domain *)
    method get_location l_id  = try c1#get_location l_id with Failure _ -> c2#get_location l_id
    method get_component c_id = try c1#get_component c_id with Failure _ -> c2#get_component c_id
    method get_locations  = Location_set.union c1#get_locations c2#get_locations
    method get_components = Component_set.union c1#get_components c2#get_components
    method get_bindings   = Binding_set.union c1#get_bindings c2#get_bindings

    method get_location_ids  = Location_id_set.union c1#get_location_ids c2#get_location_ids
    method get_component_ids = Component_id_set.union c1#get_component_ids c2#get_component_ids

    method get_location_names  = Location_name_set.union c1#get_location_names c2#get_location_names
    method get_component_names = Component_name_set.union c1#get_component_names c2#get_component_names

    method c_l      = self#get_location_ids
    method c_c      = self#get_component_ids
    method c_type c = (self#get_component c)#typ

    method get_local_component l c = try c1#get_local_component l c with Failure _ -> c2#get_local_component l c
    method get_local_package   l k = try c1#get_local_package l k with Failure _ -> c2#get_local_package l k

    method get_location_id  l = try c1#get_location_id l with Failure _ -> c2#get_location_id l
    method get_component_id c = try c1#get_component_id c with Failure _ -> c2#get_component_id c

    method get_location_name  l = try c1#get_location_name l with Failure _ -> c2#get_location_name l
    method get_component_name c = try c1#get_component_name c with Failure _ -> c2#get_component_name c
 end


