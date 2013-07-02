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
open Data_constraint

class type extended_solution_iface =
  object
    method domain          : Variable_set.t
    method variable_values : variable -> int

    (* location *)
    method get_repository_id_of_a_location : location_id -> repository_id
    method get_package_ids_of_a_location   : location_id -> Package_id_set.t

    method get_number_of_components_of_a_location : location_id -> component_type_id -> int 
  end

class extended_solution (solution : solution) : extended_solution_iface = 
  let domain = solution.domain in
  let value  = solution.variable_values in

  object
    method domain          = domain
    method variable_values = value

    method get_repository_id_of_a_location location_id = 
      let location_repository_variables : Variable_set.t =
        Variable_set.filter (fun variable ->
          match variable with
          | Local_repository_variable (var_location_id, var_repository_id) -> 
              (var_location_id = location_id) && ((value variable) = 1)
          | _ -> false
        ) domain
      in
      match Variable_set.cardinal location_repository_variables with
      | 1 -> value (Variable_set.choose location_repository_variables)
      | 0 -> failwith "No repository attributed to a location!"
      | _ -> failwith "Many repositories attributed to a location!"

    method get_package_ids_of_a_location location_id = 
      let location_package_variables : Variable_set.t =
        Variable_set.filter (fun variable ->
          match variable with
          | Local_variable (var_location_id, Package(var_package_id)) -> 
              var_location_id = location_id
          | _ -> false
        ) domain
      in
      let module Variable_set_of_package_id_set = Data_common.Set.Convert(Variable_set)(Package_id_set) in
      Variable_set_of_package_id_set.convert value location_package_variables

    (* How many components of a given component type should be installed on a given location according to this solution? *)
    method get_number_of_components_of_a_location location_id component_type_id =
      let components_counts =
        Variable_set.filter (fun variable -> 
          match var with    
          | Local_variable (var_location_id, (ComponentType var_component_type_id)) ->
              (var_location_id = location_id) && (var_component_type_id = component_type_id)
          | _ -> None
        ) domain
      in
      match Variable_set.cardinal components_counts with
      | 1 -> value (Variable_set.choose components_count)
      | 0 -> failwith "No number of a certain component type attributed to a location!"
      | _ -> failwith "Many numbers of a certain component type attributed to a location!"

  end



(* Utility to generate fresh names for components. *)
type used_names = Component_name_set.t

let fresh_component_name (location_name : location_name) (component_type_name : component_type_name) (used_names : used_names ref) : component_name =

  let build_component_name = 
    Printf.sprintf
    "%s-%s-%d" 
    (String_of.location_name location_name)
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
  (initial_components : Component_set.t)
  (location_ids       : Location_id_set.t)
  (component_type_ids : Component_type_id_set.t)
  (solution           : extended_solution_iface)
  : Component_set.t =

  let almost_done_components : component list  = ref [] in

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

      let initial_components_left : Component_set.t = ref initial_components in 

      while (Component_set.cardinal !new_components) < required_number_of_components do
        if not (Component_set.is_empty !initial_components_left)
        then begin
          (* If we have still some initial components left we reuse them. *)
          let component_to_reuse = Component_set.choose !initial_components_left in
          initial_components_left := Component_set.remove component_to_reuse !initial_components_left;
          almost_done_components  := ReusedComponent(component_to_reuse) :: !almost_done_components
        end else begin
          (* If we don't have any more initial components left we create new ones. *)
          let new_component : (used_names ref -> component) =
            (fun used_names -> 
              let component_name = fresh_component_name "location_name" "component_type_name" used_names
              in 
              object
                method name     = component_name;
                method typ      = component_type_id;
                method location = location_id;
              end) in
          in
          almost_done_components := NewComponent(new_component) :: !almost_done_components
        end
      done

    ) component_type_ids
  ) location_ids;

  (* finshed here! *)

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

    let new_location : location =
      object
        method name                = name
        method repository          = repository
        method packages_installed  = packages_installed
        method provide_resources   = provide_resources
      end

    in
    location_id_to_location_map := Location_id_map.add location_id new_location !location_id_to_location_map;

  ) location_ids;

  let module Location_id_map_extract_value = Location_id_map.Set_of_values(Location_set) in
  let locations = Location_id_map_extract_value.set_of_values !location_id_to_location_map in

  let get_location id = Location_id_map.find id !location_id_to_location_map in



  (* components *)


  object(self)

    method get_location    = get_location
    method get_component c = failwith "unimplemented" (* Component_id_map.find c (!component_get) *)

    method get_locations  = locations
    method get_components = failwith "unimplemented" (* !components *)
    method get_bindings   = failwith "unimplemented" (* implem_bindings *)

    method get_location_ids  = location_ids
    method get_component_ids = failwith "unimplemented" (* !component_ids *)

    method get_location_names  = failwith "unimplemented" (* !location_names *)
    method get_component_names = failwith "unimplemented" (* !component_names *)

    method c_l = failwith "unimplemented" (* self#get_location_ids *)
    method c_c = failwith "unimplemented" (* self#get_component_ids *)
    method c_type c = failwith "unimplemented" (* (self#get_component c)#typ *)

    method get_local_component l t = failwith "unimplemented" (* (try Location_component_type_map.find (l,t) implem_get_local_component with
           | Not_found -> let tmp = get_local_component l t (!component_get) in
             implem_get_local_component <- Location_component_type_map.add (l,t) tmp implem_get_local_component; tmp) *)
    method get_local_package l k = failwith "unimplemented" (* (try Location_package_map.find (l,k) implem_get_local_package with
           | Not_found -> let tmp = get_local_package l k (!location_get) in
             implem_get_local_package <- Location_package_map.add (l,k) tmp implem_get_local_package; tmp); *)

    method get_location_id l  = failwith "unimplemented" (* Location_name_map.find l (!location_get_id) *)
    method get_component_id c = failwith "unimplemented" (* Component_name_map.find c (!component_get_id) *)

    method get_location_name l = failwith "unimplemented" (* Location_id_map.find l (!location_get_name) *)
    method get_component_name c = failwith "unimplemented" (* Component_id_map.find c (!component_get_name) *)
  end