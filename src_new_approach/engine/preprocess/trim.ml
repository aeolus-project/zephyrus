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

let dependency_transitive_closure (initial_package_ids : Package_id_set.t) (get_package : package_id -> package) : Package_id_set.t =

  (* A function for quickly getting dependencies of a package with a given package id. *)
  let get_dependencies (package_id : package_id) : Package_id_set.t =
    let package = get_package package_id in
    Package_id_set_set.fold Package_id_set.union package#depend Package_id_set.empty
  in

  (* Ids of packages which are already in the dependency closure *)
  let in_the_closure = ref initial_package_ids in

  (* Ids of packages which are in the closure, but their dependencies still have to be processed. *)
  let to_do          = ref initial_package_ids in

  (* While there are still packages which need their dependencies to be processed *)
  while not (Package_id_set.is_empty !to_do) do
    let current  = Package_id_set.choose !to_do in       (* Get any package with dependencies not processed yet *)
    to_do       := Package_id_set.remove current !to_do; (* Remove it from the set of the to_do set *)

    let current_deps = get_dependencies current in (* Get it dependencies and process them *)
    Package_id_set.iter (fun package_id ->

      (* Invariant:
         If the package is in the in_the_closure set, then it
         must have been added at some point to the to_do set,
         (so either it was already processed or it will be processed). *)
      if (not (Package_id_set.mem package_id !in_the_closure))
      then
        
        (* We add the package to the in_the_closure set. *)
        in_the_closure := Package_id_set.add package_id !in_the_closure;

        (* As the package was not in the in_the_closure set, we know that
           it was not processed yet, so it should be processed. *)
        to_do          := Package_id_set.add package_id !to_do;

    ) current_deps;
    
  done;

  (* Finished! The set in_the_closure contains exactly the ids of packages which are in the closure. *)
  !in_the_closure


let trim_repository (repository : repository) (implementing_packages_ids : Package_id_set.t) : repository =
  (* All packages from the repository. *)
  let repository_all_package_ids : Package_id_set.t = repository#package_ids in

  (* The packages that are in this repository and are implementing something. *)
  let repository_implementing_package_id : Package_id_set.t = Package_id_set.inter repository_all_package_ids implementing_packages_ids in

  (* The transitive closure of dependencies. *)
  let packages_in_the_dependency_closure : Package_id_set.t = dependency_transitive_closure repository_implementing_package_id repository#get_package in

  (* Preparing the new trimmed repository. *)
  
  (* Trimed set of ids of packages. *)
  let package_ids : Package_id_set.t = packages_in_the_dependency_closure in

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
        Package_id_set_set_of_package_id_set_set.convert trim_package_id_set package#depend in

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


let trim_repositories universe initial_configuration specification =

  (* Repositories. *)  

  (* We trim the repositories: we keep only a transitive closure of
     the packages mentioned in the implementation. *)
  
  (* 1. Set of packages which are implementing one of the component types available in the universe. *)
  let implementing_package_ids : Package_id_set.t =
    let component_type_ids : Component_type_id_set.t = universe#get_component_type_ids in

    let module Package_id_set_set_of_component_type_set = Data_common.Set.Convert(Component_type_id_set)(Package_id_set_set) in
    let set_of_sets_of_implementing_packages : Package_id_set_set.t = Package_id_set_set_of_component_type_set.convert universe#get_implementation component_type_ids in

    Package_id_set_set.fold Package_id_set.union set_of_sets_of_implementing_packages Package_id_set.empty
  in

  (* 2. Core packages and not-always-installable packages. *)
  (* TODO *)

  (* Set of package id after trimming (a subset of the initial set of packages). *)
  let trimmed_package_ids : Package_id_set.t = implementing_package_ids in
  


  let repository_ids : Repository_id_set.t = universe#get_repository_ids in
    
  let repository_of_repository_id_map : repository Repository_id_map.t =

    let repository_of_repository_id_map : repository Repository_id_map.t ref = ref Repository_id_map.empty in
  
    Repository_id_set.iter (fun repository_id ->
      let repository = universe#get_repository repository_id in
      let trimmed_repository = trim_repository repository implementing_package_ids in
      repository_of_repository_id_map := Repository_id_map.add repository_id trimmed_repository !repository_of_repository_id_map
    ) repository_ids;

    !repository_of_repository_id_map

  in


  let module Repository_id_map_values = Repository_id_map.Set_of_values(Repository_set) in
  let repositories : Repository_set.t = Repository_id_map_values.set_of_values repository_of_repository_id_map in

  let get_repository (repository_id : repository_id) : repository = 
    Repository_id_map.find repository_id repository_of_repository_id_map in

  let module Repository_name_set_of_repository_set = Data_common.Set.Convert(Repository_set)(Repository_name_set) in
  let repository_names : Repository_name_set.t = Repository_name_set_of_repository_set.convert (fun repository -> repository#name) repositories in


  let package_ids : Package_id_set.t = trimmed_package_ids in

  let package_of_package_id_map : package Package_id_map.t = 
    
    let package_of_package_id_map : package Package_id_map.t ref = ref Package_id_map.empty in

    (*
    let repository_id_of_package_id_map : repository_id Package_id_map.t ref = ref Package_id_map.empty in
    *)

    Repository_id_set.iter (fun repository_id -> 
      
      let repository = get_repository repository_id in
      Package_id_set.iter (fun package_id ->
        
        let package = repository#get_package package_id in
        package_of_package_id_map := Package_id_map.add package_id package !package_of_package_id_map
        
        (* repository_id_of_package_id_map := Package_id_map.add package_id repository_id !repository_id_of_package_id_map *)
      
      ) repository#package_ids
    ) repository_ids;

    !package_of_package_id_map

  in


  let module Package_id_map_values = Package_id_map.Set_of_values(Package_set) in
  let packages : Package_set.t = Package_id_map_values.set_of_values package_of_package_id_map in

  let get_package (package_id : package_id) : package = 
    Package_id_map.find package_id package_of_package_id_map in

  let module Package_name_set_of_package_set = Data_common.Set.Convert(Package_set)(Package_name_set) in
  let package_names : Package_name_set.t = Package_name_set_of_package_set.convert (fun package -> package#name) packages in
  
  (* The trimmed universe. *)
  object
    (* basic methods *)
    method get_component_type = universe#get_component_type (* Irrelevent to package trimming. *)
    method get_implementation = universe#get_implementation (* Irrelevent to package trimming. *)
    method get_repository     = get_repository              (* Updated! *)
    method get_package        = get_package                 (* Updated! *)

    method repository_of_package = universe#repository_of_package (* TODO: Domain should be restricted. *)

    method get_component_types = universe#get_component_types (* Irrelevent to package trimming. *)
    method get_repositories    = repositories                 (* Updated! *)
    method get_packages        = packages                     (* Updated! *)

    method get_port_ids           = universe#get_port_ids           (* Irrelevent to package trimming. *)
    method get_component_type_ids = universe#get_component_type_ids (* Irrelevent to package trimming. *)
    method get_repository_ids     = repository_ids                  (* Updated! *) 
    method get_package_ids        = package_ids                     (* Updated! *)

    method get_port_names           = universe#get_port_names           (* Irrelevent to package trimming. *)
    method get_component_type_names = universe#get_component_type_names (* Irrelevent to package trimming. *)
    method get_repository_names     = repository_names                  (* Updated! *)
    method get_package_names        = package_names                     (* Updated! *)

    (* methods coming from the paper. Usually, aliases for well-named functions *)
    method u_dt = universe#u_dt  (* Irrelevent to package trimming. *)
    method u_dp = universe#u_dp  (* Irrelevent to package trimming. *)
    method u_dr = repository_ids (* Updated! *)
    method u_dk = package_ids    (* Updated! *)
      
    method u_i = universe#u_i (* Irrelevent to package trimming. *)
    method u_w = get_package  (* Updated! *)

    method ur = universe#ur (* Irrelevent to package trimming. *)
    method up = universe#up (* Irrelevent to package trimming. *)
    method uc = universe#uc (* Irrelevent to package trimming. *)

    (* methods for naming *)
    method get_port_id           = universe#get_port_id           (* Irrelevent to package trimming. *)
    method get_component_type_id = universe#get_component_type_id (* Irrelevent to package trimming. *)
    method get_repository_id     = universe#get_repository_id     (* TODO: Domain should be restricted. *)
    method get_package_id        = universe#get_package_id        (* TODO: Domain should be restricted. *)

    method get_port_name           = universe#get_port_name           (* Irrelevent to package trimming. *)
    method get_component_type_name = universe#get_component_type_name (* Irrelevent to package trimming. *)
    method get_repository_name     = universe#get_repository_name     (* TODO: Domain should be restricted. *)
    method get_package_name        = universe#get_package_name        (* TODO: Domain should be restricted. *)
  end
