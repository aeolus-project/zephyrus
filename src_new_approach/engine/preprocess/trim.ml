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
          if (not (Node_set.mem node !in_the_closure))
          then
            
            (* We add the node to the closure. *)
            in_the_closure := Node_set.add node !in_the_closure;

            (* As the node was not in the closure just before, we know that
               its successors were not processed yet, so they should be processed. *)
            to_do          := Node_set.add node !to_do;

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
  
  (* Trimed set of ids of packages. *)
  let package_ids : Package_id_set.t = repository_keep_package_id in

  (* Helper function: takes a set of package ids and returns only these which belong to the trimmed repository. *)
  let trim_package_id_set : (Package_id_set.t -> Package_id_set.t) = 
    Package_id_set.filter (fun package_id -> Package_id_set.mem package_id package_ids) in

  (* Trimmed set of packages. We have to trim their dependencies too! *)
  let module Package_set_of_package_id_set = Data_common.Set.Convert(Package_id_set)(Package_set) in

  let package_of_package_id_map : package Package_id_map.t ref = ref Package_id_map.empty in

  Package_id_set.iter (fun package_id ->
    
    (* The old package. *)
    let package = repository#get_package package_id in

    (* Trimmed dependencies. *)
    let depend = 
        let module Package_id_set_set_of_package_id_set_set = Data_common.Set.Convert(Package_id_set_set)(Package_id_set_set) in
        (* Keep only packages from the trimmed set. *)
        let set_of_sets = Package_id_set_set_of_package_id_set_set.convert trim_package_id_set package#depend in
        (* Keep only sets which are not empty (cause an empty "or" is always false and we remove packages only if they can always be installed). *)
        Package_id_set_set.filter (fun set -> not (Package_id_set.is_empty set)) set_of_sets in

    (* Trimmed conflicts. *)
    let conflict = trim_package_id_set package#conflict in 

    let trimmed_package =
      object
        method name     = package#name
        method depend   = depend
        method conflict = conflict
        method consume  = package#consume
      end in

    package_of_package_id_map := Package_id_map.add package_id trimmed_package !package_of_package_id_map

  ) package_ids;

  let get_package (package_id : package_id) : package = 
    try Package_id_map.find package_id !package_of_package_id_map
    with Not_found -> failwith (Printf.sprintf "In repository %s accessing a package with id=%s which was trimmed out!" (String_of.repository_name repository#name) (String_of.package_id package_id) )
  in
  
  let packages : Package_set.t = Package_set_of_package_id_set.convert get_package package_ids in
  
  object
    method name        = repository#name
    method get_package = get_package
    method packages    = packages
    method package_ids = package_ids
  end



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

  let module Repository_name_set_of_repository_set = Data_common.Set.Convert(Repository_set)(Repository_name_set) in
  let repository_names : Repository_name_set.t = Repository_name_set_of_repository_set.convert (fun repository -> repository#name) repositories in

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

  let module Package_name_set_of_package_set = Data_common.Set.Convert(Package_set)(Package_name_set) in
  let package_names : Package_name_set.t = Package_name_set_of_package_set.convert (fun package -> package#name) packages in
  
  
  (* Restrincting domains of the repository id <-> name mapping *)
  
  let get_repository_id (repository_name : repository_name) : repository_id =
    if   Repository_name_set.mem repository_name repository_names
    then universe#get_repository_id repository_name
    else failwith (Printf.sprintf "In the universe: getting id of a repository with name=%s which was trimmed out!" (String_of.repository_name repository_name) ) in

  let get_repository_name (repository_id   : repository_id) : repository_name =
    if   Repository_id_set.mem repository_id repository_ids
    then universe#get_repository_name repository_id
    else failwith (Printf.sprintf "In the universe: getting name of a repository with id=%s which was trimmed out!" (String_of.repository_id repository_id) ) in
  

  (* 4. Restrincting domains of the package id <-> name mapping *)
  
  (* (* TODO: This will not work so easily as we don't know the names of packages in a given repository. *)
  let get_package_id (repository_id : repository_id) (package_name : package_name) : package_id =
    let repository = get_repository repository_id in
    if   Package_name_set.mem package_name repository#package_names
    then universe#get_package_id repository_id package_name
    else failwith (Printf.sprintf "In the universe: getting id of a package with name=%s which was trimmed out!" (String_of.package_name package_name) ) in
  *)

  let get_package_name (package_id   : package_id) : package_name =
    if   Package_id_set.mem package_id package_ids
    then universe#get_package_name package_id
    else failwith (Printf.sprintf "In the universe: getting name of a package with id=%s which was trimmed out!" (String_of.package_id package_id) ) in
  

  (* The trimmed universe. *)
  object
    (* basic methods *)
    method get_component_type = universe#get_component_type (* Irrelevent to repository trimming. *)
    method get_implementation = universe#get_implementation (* Irrelevent to repository trimming. *)
    method get_repository     = get_repository              (* Updated! *)
    method get_package        = get_package                 (* Updated! *)

    method repository_of_package = repository_of_package (* Updated! *)

    method get_component_types = universe#get_component_types (* Irrelevent to repository trimming. *)
    method get_repositories    = repositories                 (* Updated! *)
    method get_packages        = packages                     (* Updated! *)

    method get_port_ids           = universe#get_port_ids           (* Irrelevent to repository trimming. *)
    method get_component_type_ids = universe#get_component_type_ids (* Irrelevent to repository trimming. *)
    method get_repository_ids     = repository_ids                  (* Updated! *) 
    method get_package_ids        = package_ids                     (* Updated! *)

    method get_port_names           = universe#get_port_names           (* Irrelevent to repository trimming. *)
    method get_component_type_names = universe#get_component_type_names (* Irrelevent to repository trimming. *)
    method get_repository_names     = repository_names                  (* Updated! *)
    method get_package_names        = package_names                     (* Updated! *)

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

    (* methods for naming *)
    method get_port_id           = universe#get_port_id           (* Irrelevent to repository trimming. *)
    method get_component_type_id = universe#get_component_type_id (* Irrelevent to repository trimming. *)
    method get_repository_id     = get_repository_id              (* Domain restricted. *)
    method get_package_id        = universe#get_package_id        (* TODO: Domain should be restricted. *)

    method get_port_name           = universe#get_port_name           (* Irrelevent to repository trimming. *)
    method get_component_type_name = universe#get_component_type_name (* Irrelevent to repository trimming. *)
    method get_repository_name     = get_repository_name              (* Domain restricted. *)
    method get_package_name        = get_package_name                 (* Domain restricted. *)
  end



let trim_repositories universe configuration specification =

  (* 1. Set of packages which are implementing one of the component types available in the universe. *)
  let implementing_package_ids : Package_id_set.t =
    let component_type_ids : Component_type_id_set.t = universe#get_component_type_ids in

    let module Package_id_set_set_of_component_type_set = Data_common.Set.Convert(Component_type_id_set)(Package_id_set_set) in
    let set_of_sets_of_implementing_packages : Package_id_set_set.t = Package_id_set_set_of_component_type_set.convert universe#get_implementation component_type_ids in

    Package_id_set_set.fold Package_id_set.union set_of_sets_of_implementing_packages Package_id_set.empty
  in
  (* Printf.printf "\nimplementing_package_ids: %s\n%!" (String_of.package_id_set implementing_package_ids); *)


  (* 1. The transitive closure of dependencies. *)

  let repository_keep_only_dependency_transitive_closure (universe : universe) : (repository -> repository) = 
    let packages_in_the_dependency_closure : Package_id_set.t = 
      dependency_transitive_closure implementing_package_ids universe#get_package in
    (* Printf.printf "\npackages_in_the_dependency_closure: %s\n%!" (String_of.package_id_set packages_in_the_dependency_closure); *)
    trim_repository packages_in_the_dependency_closure in


  (* 2. Only implementing packages and not-always-installable packages. *)

  let repository_remove_always_installable_packages (universe : universe) : repository -> repository = 
    let not_always_installable_packages : Package_id_set.t =
      remove_always_installable_packages universe#get_package_ids universe#get_package in
    let core_and_not_always_installable_packages : Package_id_set.t =
      Package_id_set.union implementing_package_ids not_always_installable_packages in
    (* Printf.printf "\ncore_and_not_always_installable_packages: %s\n%!" (String_of.package_id_set core_and_not_always_installable_packages); *)
    trim_repository core_and_not_always_installable_packages in

  (* We prepare the trimming functions: *)
  let repository_trimmers : (universe -> (repository -> repository)) list = [
    repository_keep_only_dependency_transitive_closure; (* 1. We keep only the transitive closure of the packages mentioned in the implementation. *)
    repository_remove_always_installable_packages;      (* 2. We keep only core packages and these packages wich are not always installable. *)
  ] in

  (* We trim the repositories: *)
  let trimmed_universe : universe =
    List.fold_left (fun universe repository_trimming_function ->
        trim_universe_repositories (repository_trimming_function universe) universe
      ) universe repository_trimmers

  in

  trimmed_universe
