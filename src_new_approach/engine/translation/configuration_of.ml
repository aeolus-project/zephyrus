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
      let module Variable_set_of_package_id_set = Set.Convert(Variable_set)(Package_id_set) in
      Variable_set_of_package_id_set.convert value location_package_variables

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
                method typ      = component_type_id;
                method location = location_id;
              end) in
          new_components := NewComponent(new_component) :: !new_components
        end
      done;

      almost_done_components := !new_components @ !almost_done_components

    ) component_type_ids
  ) location_ids;

  let filter_map (f : 'a -> 'b option) (l : 'a list) : ('b list) = 
    List.fold_right (fun (el : 'b option) (l : 'b list) -> 
      match el with
      | None   -> l 
      | Some x -> x :: l) 
    (List.map f l) [] in

  (* We have prepared all the almost-done-components. Now we can extract all their names
     and we will have the initial set of names already used in the configuration
     (all coming from reused components, cause the new ones are unnamed for now). *)
  let (used_names : used_names ref) = ref (Component_name_set.set_of_direct_list (
    filter_map (function
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

  let location_catalog : Location_obj_catalog.obj_catalog_iface = Location_obj_catalog.of_id_to_obj_map !location_id_to_location_map in

  let locations    : Location_set.t          = location_catalog#objs in
  let get_location : location_id -> location = location_catalog#obj_of_id in

  let location_name_catalog : Location_catalog.catalog_iface = Location_catalog.of_id_to_name_map (Location_id_map.map get_name !location_id_to_location_map) in
  let location_names        : Location_name_set.t            = location_name_catalog#names in
  let location_name_of_id   : location_id -> location_name   = location_name_catalog#name_of_id in
  let location_id_of_name   : location_name -> location_id   = location_name_catalog#id_of_name in

  

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

  let get_local_package location_id package_id : bool =
    Package_id_set.mem package_id (get_location location_id)#packages_installed in

  object(self)
    (* methods *)
    method get_location  = get_location
    method get_component = get_component

    method get_locations  = locations
    method get_components = components
    method get_bindings   = Binding_set.empty

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