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

module Transitive_closure =
  functor (Node_set : Data_common.Set.S) ->
  struct

    let transitive_closure (initial_nodes : Node_set.t) (get_successors : Node_set.elt -> Node_set.t) : Node_set.t =

      (* Nodes which are already in the dependency closure. *)
      let in_the_closure = ref initial_nodes in

      (* Nodes which are in the closure, but their successors still have to be processed. 
         It is always a subset of in_the_closure. *)
      let to_do          = ref initial_nodes in

      (* While there are still nodes which need their successors to be processed: *)
      while not (Node_set.is_empty !to_do) do

        (* Check if our assertion holds. *)
        assert (Node_set.subset !to_do !in_the_closure);

        let current  = Node_set.choose !to_do in       (* Get any node with successors not processed yet. *)
        to_do       := Node_set.remove current !to_do; (* Remove it from the to_do set. *)

        let current_succesors = get_successors current in (* Get successors and process them: *)
        Node_set.iter (fun node ->

          (* Invariant: 
             If a node is in the in_the_closure set, it means that it was (or still is) in the to_do set.
             So either it (and its successors) has been already processed or they will surely
             be processed in the future. *)
          if (not (Node_set.mem node !in_the_closure)) then begin
            
            (* We add the node to the closure. *)
            in_the_closure := Node_set.add node !in_the_closure;

            (* As the node was not in the closure just before, we know that
               its successors were not processed yet, so they should be processed. *)
            to_do          := Node_set.add node !to_do

          end

        ) current_succesors;
        
      done;

      (* Finished! The set in_the_closure contains exactly the ids of nodes which are in the closure. *)
      !in_the_closure

  end

module Package_transitive_closure = Transitive_closure(Package_id_set)

let transitive_closure : Package_id_set.t -> (package_id -> Package_id_set.t) -> Package_id_set.t = Package_transitive_closure.transitive_closure


(* A function for quickly getting dependencies of a package with a given package id. *)
let get_dependencies (get_package : package_id -> package) (package_id : package_id) : Package_id_set.t =
  let package = get_package package_id in
  Package_id_set_set.fold Package_id_set.union package#depend Package_id_set.empty

(* A function for quickly determining if a given package is in conflict with any other package. *)
let is_in_conflict (get_package : package_id -> package) (package_id : package_id) : bool =
  let package = get_package package_id in
  not (Package_id_set.is_empty package#conflict)


let dependency_transitive_closure (initial_package_ids : Package_id_set.t) (get_package : (package_id -> package)) : Package_id_set.t =

  (* A function for quickly getting dependencies of a package with a given package id. *)
  let get_dependencies : package_id -> Package_id_set.t = get_dependencies get_package in

  transitive_closure initial_package_ids get_dependencies


let remove_always_installable_packages (all_package_ids : Package_id_set.t) (get_package : (package_id -> package)) : Package_id_set.t =
  
  (* Preparing the reverse dependency map. *)
  let get_reverse_dependencies (package_id : package_id) : Package_id_set.t = 
  
    (* A function for quickly getting dependencies of a package with a given package id. *)
    let get_dependencies : package_id -> Package_id_set.t = get_dependencies get_package in

    let reverse_dependency_map : (Package_id_set.t ref) Package_id_map.t ref = ref Package_id_map.empty in
    
    (* Initialize the map with no reverse dependencies. *)
    Package_id_set.iter (fun package_id -> 
      reverse_dependency_map := Package_id_map.add package_id (ref Package_id_set.empty) !reverse_dependency_map
    ) all_package_ids;

    (* Add all reverse dependencies one by one. *)
    Package_id_set.iter (fun depending_package_id ->   
      let dependencies = get_dependencies depending_package_id in
      Package_id_set.iter (fun depended_on_package_id -> 
        try
          let reverse_dependencies_of_depended_on_package = Package_id_map.find depended_on_package_id !reverse_dependency_map in
          reverse_dependencies_of_depended_on_package := Package_id_set.add depending_package_id !reverse_dependencies_of_depended_on_package
        with Not_found -> failwith "XXX"
      ) dependencies
    ) all_package_ids;

    (* The get reverse dependency function: *)
    !(Package_id_map.find package_id !reverse_dependency_map) 

  in
  
  (* A function for quickly determining if a given package is in conflict with any other package. *)

  let directly_conflicted_packages : Package_id_set.t =
  
    let directly_conflicted_packages : Package_id_set.t ref = ref Package_id_set.empty in

    let is_in_conflict : package_id -> bool = is_in_conflict get_package in
  
      Package_id_set.iter (fun package_id -> 
        let package = get_package package_id in
        if is_in_conflict package_id
        then 
          directly_conflicted_packages := Package_id_set.add package_id         !directly_conflicted_packages;
          directly_conflicted_packages := Package_id_set.union package#conflict !directly_conflicted_packages
    ) all_package_ids;

    !directly_conflicted_packages
  in

  transitive_closure directly_conflicted_packages get_reverse_dependencies


let trim_component_types universe initial_configuration specification =
  
  (* Component types. *)

  (* TODO: *)
  (* We don't trim component types for now. *)
  (* We should keep only a transitive closure of these which appear
     in the specification or in the initial configuration. *)
  (*
  let trimmed_component_types =
    universe.universe_component_types
  in
  *)

  (* Implementation. *)

  (* We trim the implementation information: we keep only the information
     related to component types still present in the trimmed universe. *)
  (*
  let trimmed_implementation =
    (* Get all the names of all component types of the trimmed universe. *)
    let trimmed_component_type_names =
      List.map (fun component_type -> component_type.component_type_name) trimmed_component_types
    in
    (* Filter out the implementation info about component types absent in the trimmed universe. *)
    List.filter (fun (component_type_name, package_names) ->
      List.mem component_type_name trimmed_component_type_names
  ) universe.universe_implementation
  in
  *)

  (* The trimmed universe. *)
  universe


let trim_repositories universe configuration specification =

  (* 1. Set of packages which are implementing one of the component types available in the universe. *)
  let implementing_package_ids : Package_id_set.t =
    let component_type_ids : Component_type_id_set.t = universe#get_component_type_ids in

    let module Package_id_set_set_of_component_type_set = Data_common.Set.Convert(Component_type_id_set)(Package_id_set_set) in
    let set_of_sets_of_implementing_packages : Package_id_set_set.t = Package_id_set_set_of_component_type_set.convert universe#get_implementation component_type_ids in

    Package_id_set_set.fold Package_id_set.union set_of_sets_of_implementing_packages Package_id_set.empty
  in
  Printf.printf "\nimplementing_package_ids: %s\n%!" (String_of.package_id_set implementing_package_ids);


  (* 2. The transitive closure of dependencies. *)
  let packages_in_the_dependency_closure (universe : universe) : Package_id_set.t = 
    let packages_in_the_dependency_closure : Package_id_set.t = 
      dependency_transitive_closure implementing_package_ids universe#get_package in
    Printf.printf "\npackages_in_the_dependency_closure: %s\n%!" (String_of.package_id_set packages_in_the_dependency_closure);
    packages_in_the_dependency_closure in


  (* 3. Only implementing packages and not-always-installable packages. *)
  let core_and_not_always_installable_packages (universe : universe) : Package_id_set.t = 
    let not_always_installable_packages : Package_id_set.t =
      remove_always_installable_packages universe#get_package_ids universe#get_package in
    let core_and_not_always_installable_packages : Package_id_set.t =
      Package_id_set.union implementing_package_ids not_always_installable_packages in
    Printf.printf "\ncore_and_not_always_installable_packages: %s\n%!" (String_of.package_id_set core_and_not_always_installable_packages);
    core_and_not_always_installable_packages in

  (* We prepare the trimming package set generators: *)
  let trimmers : (string * (universe -> Package_id_set.t)) list = [
    ("dependency closure", packages_in_the_dependency_closure);       (* 1. We keep only the transitive closure of the packages mentioned in the implementation. *)
    ("always installable", core_and_not_always_installable_packages); (* 2. We keep only core packages and these packages wich are not always installable. *)
  ] in

  Printf.printf "\nBeginning universe trimming...%!";

  (* We trim the repositories: *)
  let trimmed_universe : universe =
    List.fold_left (fun universe (description, package_ids_to_keep) ->
        Printf.printf "\n+ trimming: %s...%!" description;
        let universe' = universe#trim_packages_by_ids (package_ids_to_keep universe) in
        Printf.printf "+ trimming %s done!%!" description;
        universe'
      ) universe trimmers in

  Printf.printf "\nUniverse trimming done!%!";

  trimmed_universe



(*/************************************************************************\*)
(*| 4. Configuration.                                                      |*)
(*\************************************************************************/*)

let transitive_closure_domain c domain =
  let add lprov lreq map = let s = try Location_id_map.find lprov map with | Not_found -> Location_id_set.empty in
    Location_id_map.add lprov (Location_id_set.add lreq s) map in
  let map = ref (Binding_set.fold (fun b res -> add (c#get_component (b#provider))#location (c#get_component (b#requirer))#location res) c#get_bindings Location_id_map.empty) in
  let to_add = ref domain in
  let res = ref Location_id_set.empty in
  while not (Location_id_set.is_empty !to_add) do
    let l = Location_id_set.choose !to_add in
    res := Location_id_set.add l !res;
    to_add := Location_id_set.remove l !to_add;
    to_add := Location_id_set.union (try Location_id_map.find l !map with Not_found -> Location_id_set.empty) !to_add;
    map := Location_id_map.remove l !map;
  done; !res

let configuration (configuration : configuration) (location_ids : Location_id_set.t) =
  let get_location = configuration#get_location in
  
  let location_ids_trimmed = Location_id_set.inter configuration#get_location_ids location_ids in
  let location_ids_rest    = Location_id_set.diff  configuration#get_location_ids location_ids_trimmed in

  let component_ids_1 = Component_id_set.filter (fun c_id -> Location_id_set.mem (configuration#get_component c_id)#location location_ids) configuration#get_component_ids in
  let bindings_1 = Binding_set.filter (fun b -> Component_id_set.mem b#provider component_ids_1) configuration#get_bindings in
  
  let component_ids_2 = Component_id_set.diff configuration#get_component_ids component_ids_1 in
  let bindings_2 = Binding_set.filter (fun b -> (Component_id_set.mem b#provider component_ids_2) && (Component_id_set.mem b#requirer component_ids_2)) configuration#get_bindings in

  (* print_string ("annex conf location location_ids = " ^ (String_of.location_id_set location_ids_rest) ^ "\n"); *)

 ( object (self)
      method get_location   = get_location
      method get_component  = configuration#get_component
      method get_bindings   = bindings_1
      method get_location_ids  = location_ids_trimmed
      method get_component_ids = component_ids_1
      method get_local_component = configuration#get_local_component
      method get_local_package   = configuration#get_local_package
      method trim location_ids = self
    end , object (self)
      method get_location   = configuration#get_location
      method get_component  = configuration#get_component
      method get_bindings   = bindings_2
      method get_location_ids  = location_ids_rest
      method get_component_ids = component_ids_2
      method get_local_component = configuration#get_local_component
      method get_local_package   = configuration#get_local_package
      method trim location_ids = self
    end )

let empty c = 
  let inner l_id (set, map) = 
    let l = c#get_location l_id in
    let l' = object
      method id = l#id
      method repository = l#repository
      method packages_installed = Package_id_set.empty
      method provide_resources  = l#provide_resources
      method cost               = l#cost
    end in (Location_set.add l' set, Location_id_map.add l#id l' map) in
  let (set, map) = Location_id_set.fold inner c#get_location_ids (Location_set.empty, Location_id_map.empty) in object (self)
    method get_location   = (fun id -> try Location_id_map.find id map with Not_found -> failwith "engine/preprocess/Trim.ml #550")
    method get_component  = (fun _ -> failwith "engine/preprocess/Trim.ml #551")
    method get_bindings   = Binding_set.empty
    method get_location_ids  = c#get_location_ids
    method get_component_ids = Component_id_set.empty
    method get_local_component = (fun _ _ -> Component_id_set.empty)
    method get_local_package   = (fun _ _ -> false)
    method trim location_ids = self
  end
  



