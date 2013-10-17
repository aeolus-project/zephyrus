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



let trim_repository (keep_packages_ids : Package_id_set.t) (repository : repository)  : repository =

  (* 1. Prepare the set of packages to keep. *)

  (* All packages from the repository. *)
  let repository_all_package_ids : Package_id_set.t = repository#package_ids in

  (* The packages that are in this repository and are implementing something. *)
  let repository_keep_package_id : Package_id_set.t = Package_id_set.inter repository_all_package_ids keep_packages_ids in


  (* 2. Prepare the new trimmed repository. *)
  
  (* Trimmed set of ids of packages. *)
  let package_ids : Package_id_set.t = repository_keep_package_id in

  (* Now we have to create trimmed versions of all the packages, i.e. trim their dependencies and conflicts. *)

  (* The new package_id -> package mapping. *)
  let package_of_package_id_map : package Package_id_map.t ref = ref Package_id_map.empty in

  Package_id_set.iter (fun package_id ->
    
    (* The old package. *)
    let package = repository#get_package package_id in

    (* The new package. *)
    let trimmed_package = package#trim_by_package_ids package_ids in
    
    package_of_package_id_map := Package_id_map.add package_id trimmed_package !package_of_package_id_map

  ) package_ids;
  
  new repository
    ~id:       repository#id
    ~packages: !package_of_package_id_map


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



let trim_universe_repositories (trim_repository : repository -> repository) (universe : universe)  : universe =

  (* 1. Convert repositories and remove empty ones. *)
  let repository_of_repository_id_map : repository Repository_id_map.t =

    let repository_of_repository_id_map : repository Repository_id_map.t ref = ref Repository_id_map.empty in
  
    Repository_id_set.iter (fun repository_id ->

      (* Trim the repository. *)
      let repository = universe#get_repository repository_id in
      let trimmed_repository = trim_repository repository in

      (* Keep the repository only if it is non-empty. *)
      if not (Repository_id_set.is_empty trimmed_repository#package_ids)
      then repository_of_repository_id_map := Repository_id_map.add repository_id trimmed_repository !repository_of_repository_id_map
    ) universe#get_repository_ids;

    !repository_of_repository_id_map

  in

  (* 2. Trim repositories *)
  let repository_ids : Repository_id_set.t = Repository_id_map_extract_key.set_of_keys repository_of_repository_id_map in

  let module Repository_id_map_values = Repository_id_map.Set_of_values(Repository_set) in
  let repositories : Repository_set.t = Repository_id_map_values.set_of_values repository_of_repository_id_map in

  let get_repository (repository_id : repository_id) : repository = 
    Repository_id_map.find repository_id repository_of_repository_id_map in

  (* 3. Trim packages *)
  let (package_ids, package_of_package_id_map, repository_id_of_package_id_map) (* : (Package_id_set.t, package Package_id_map.t, repository_id Package_id_map.t) *) = 
    
    let package_ids                     : Package_id_set.t ref               = ref Package_id_set.empty in
    let package_of_package_id_map       : package Package_id_map.t ref       = ref Package_id_map.empty in
    let repository_id_of_package_id_map : repository_id Package_id_map.t ref = ref Package_id_map.empty in

    Repository_id_set.iter (fun repository_id -> 
      
      let repository = get_repository repository_id in
      Package_id_set.iter (fun package_id ->
        
        let package = repository#get_package package_id in

        package_ids                     := Package_id_set.add package_id               !package_ids;
        package_of_package_id_map       := Package_id_map.add package_id package       !package_of_package_id_map;
        repository_id_of_package_id_map := Package_id_map.add package_id repository_id !repository_id_of_package_id_map
      
      ) repository#package_ids
    ) repository_ids;

    (!package_ids, !package_of_package_id_map, !repository_id_of_package_id_map)

  in

  let module Package_id_map_values = Package_id_map.Set_of_values(Package_set) in
  let packages : Package_set.t = Package_id_map_values.set_of_values package_of_package_id_map in

  let get_package (package_id : package_id) : package = 
    Package_id_map.find package_id package_of_package_id_map in

  let repository_of_package (package_id : package_id) : repository_id =
    Package_id_map.find package_id repository_id_of_package_id_map in

  (* The trimmed universe. *)
  object
    (* basic methods *)
    method get_component_type        = universe#get_component_type        (* Irrelevent to repository trimming. *)
    method get_implementation_domain = universe#get_implementation_domain (* Irrelevent to repository trimming. *)
    method get_implementation        = universe#get_implementation        (* Irrelevent to repository trimming. *)
    method get_repository            = get_repository                     (* Updated! *)
    method get_package               = get_package                        (* Updated! *)

    method repository_of_package = repository_of_package (* Updated! *)

    method get_component_types = universe#get_component_types (* Irrelevent to repository trimming. *)
    method get_repositories    = repositories                 (* Updated! *)
    method get_packages        = packages                     (* Updated! *)

    method get_port_ids           = universe#get_port_ids           (* Irrelevent to repository trimming. *)
    method get_component_type_ids = universe#get_component_type_ids (* Irrelevent to repository trimming. *)
    method get_repository_ids     = repository_ids                  (* Updated! *) 
    method get_package_ids        = package_ids                     (* Updated! *)
    method get_resource_ids       = universe#get_resource_ids       (* Irrelevant to repository trimming. *)

    (* methods coming from the paper. Usually, aliases for well-named functions *)
    method u_dt = universe#u_dt  (* Irrelevent to repository trimming. *)
    method u_dp = universe#u_dp  (* Irrelevent to repository trimming. *)
    method u_dr = repository_ids (* Updated! *)
    method u_dk = package_ids    (* Updated! *)
      
    method u_i = universe#u_i (* Irrelevent to repository trimming. *)
    method u_w = get_package  (* Updated! *)

    method ur = universe#ur (* Irrelevent to repository trimming. *)
    method up = universe#up (* Irrelevent to repository trimming. *)
    method uc = universe#uc (* Irrelevent to repository trimming. *)
  end



let trim_repositories universe configuration specification =

  (* 1. Set of packages which are implementing one of the component types available in the universe. *)
  let implementing_package_ids : Package_id_set.t =
    let component_type_ids : Component_type_id_set.t = universe#get_component_type_ids in

    let module Package_id_set_set_of_component_type_set = Data_common.Set.Convert(Component_type_id_set)(Package_id_set_set) in
    let set_of_sets_of_implementing_packages : Package_id_set_set.t = Package_id_set_set_of_component_type_set.convert universe#get_implementation component_type_ids in

    Package_id_set_set.fold Package_id_set.union set_of_sets_of_implementing_packages Package_id_set.empty
  in
  Printf.printf "\nimplementing_package_ids: %s\n%!" (String_of.package_id_set implementing_package_ids);


  (* 1. The transitive closure of dependencies. *)

  let repository_keep_only_dependency_transitive_closure (universe : universe) : (repository -> repository) = 
    let packages_in_the_dependency_closure : Package_id_set.t = 
      dependency_transitive_closure implementing_package_ids universe#get_package in
    Printf.printf "\npackages_in_the_dependency_closure: %s\n%!" (String_of.package_id_set packages_in_the_dependency_closure);
    trim_repository packages_in_the_dependency_closure in


  (* 2. Only implementing packages and not-always-installable packages. *)

  let repository_remove_always_installable_packages (universe : universe) : repository -> repository = 
    let not_always_installable_packages : Package_id_set.t =
      remove_always_installable_packages universe#get_package_ids universe#get_package in
    let core_and_not_always_installable_packages : Package_id_set.t =
      Package_id_set.union implementing_package_ids not_always_installable_packages in
    Printf.printf "\ncore_and_not_always_installable_packages: %s\n%!" (String_of.package_id_set core_and_not_always_installable_packages);
    trim_repository core_and_not_always_installable_packages in

  (* We prepare the trimming functions: *)
  let repository_trimmers : (string * (universe -> (repository -> repository))) list = [
    ("dependency closure", repository_keep_only_dependency_transitive_closure); (* 1. We keep only the transitive closure of the packages mentioned in the implementation. *)
    ("always installable", repository_remove_always_installable_packages);      (* 2. We keep only core packages and these packages wich are not always installable. *)
  ] in

  Printf.printf "\nBeginning universe trimming...%!";

  (* We trim the repositories: *)
  let trimmed_universe : universe =
    List.fold_left (fun universe (description, repository_trimming_function) ->
        Printf.printf "\n+ trimming: %s...%!" description;
        let universe' = trim_universe_repositories (repository_trimming_function universe) universe in
        Printf.printf "+ trimming %s done!%!" description;
        universe'
      ) universe repository_trimmers in

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

let configuration c domain =
  let get_location = c#get_location in
  let locations_1 = Location_set.filter (fun l -> Location_id_set.mem l#id domain) c#get_locations in
  let locations_2 = Location_set.diff c#get_locations locations_1 in
  let location_ids_1 = domain in
  let location_ids_2 = Location_id_set.diff c#get_location_ids location_ids_1 in
  let components_1 = Component_set.filter (fun c -> Location_id_set.mem c#location domain) c#get_components in
  let components_2 = Component_set.diff c#get_components components_1 in
  let component_ids_1 = Component_id_set.filter (fun c_id -> Component_set.mem (c#get_component c_id) components_1) c#get_component_ids in
  let component_ids_2 = Component_id_set.diff c#get_component_ids component_ids_1 in
  let bindings_1 = Binding_set.filter (fun b -> Component_id_set.mem b#provider component_ids_1) c#get_bindings in
  let bindings_2 = Binding_set.filter (fun b -> (Component_id_set.mem b#provider component_ids_2) && (Component_id_set.mem b#requirer component_ids_2)) c#get_bindings in
  (* print_string ("annex conf location domain = " ^ (String_of.location_id_set location_ids_2) ^ "\n"); *)
 ( object
      method get_location   = get_location
      method get_component  = c#get_component
      method get_locations  = locations_1
      method get_components = components_1
      method get_bindings   = bindings_1
      method get_location_ids  = location_ids_1
      method get_component_ids = component_ids_1
      method c_l = location_ids_1
      method c_c = component_ids_1
      method c_type = c#c_type
      method get_local_component = c#get_local_component
      method get_local_package   = c#get_local_package
    end , object
      method get_location   = c#get_location
      method get_component  = c#get_component
      method get_locations  = locations_2
      method get_components = components_2
      method get_bindings   = bindings_2
      method get_location_ids  = location_ids_2
      method get_component_ids = component_ids_2
      method c_l = location_ids_2
      method c_c = component_ids_2
      method c_type = c#c_type
      method get_local_component = c#get_local_component
      method get_local_package   = c#get_local_package
    end )

let empty c = 
  let inner l (set, map) = let l' = object
      method id = l#id
      method repository = l#repository
      method packages_installed = Package_id_set.empty
      method provide_resources  = l#provide_resources
      method cost               = l#cost
    end in (Location_set.add l' set, Location_id_map.add l#id l' map) in
  let (set, map) = Location_set.fold inner c#get_locations (Location_set.empty, Location_id_map.empty) in object(self)
    method get_location   = (fun id -> try Location_id_map.find id map with Not_found -> failwith "engine/preprocess/Trim.ml #550")
    method get_component  = (fun _ -> failwith "engine/preprocess/Trim.ml #551")
    method get_locations  = set
    method get_components = Component_set.empty
    method get_bindings   = Binding_set.empty
    method get_location_ids  = c#get_location_ids
    method get_component_ids = Component_id_set.empty
    method c_l = c#c_l
    method c_c = Component_id_set.empty
    method c_type = (fun c -> (self#get_component c)#typ)
    method get_local_component = (fun _ _ -> Component_id_set.empty)
    method get_local_package   = (fun _ _ -> false)
  end
  



