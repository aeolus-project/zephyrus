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

(*
let dependency_transitive_closure (initial_package_names : package_name list) (packages : package list) : package list =

  (* Prepare a structure for quickly getting dependencies of a package *)
  let dependency_table = Hashtbl.create 17 in

  List.iter (fun package ->
    Hashtbl.add dependency_table package.package_name package
  ) packages;

  let get_dependencies (package_name : package_name) : package_name list =
    try
      let package = Hashtbl.find dependency_table package_name
      in
      List.unique (List.flatten package.package_depend)
    with Not_found -> []
  
  in

  (* Names of packages which are already in the dependency closure *)
  let in_the_closure = ref initial_package_names in

  (* Names of packages which still have to be processed *)
  let to_do          = ref initial_package_names in

  (* While there are still packages which need to be processed *)
  while not (!to_do = []) do
    let current      = List.hd !to_do in
    let current_deps = get_dependencies current
    in
    to_do := List.tl !to_do;
    List.iter (fun package_name ->

      (* If the package is on the in_the_closure list, then it
         must have been added at some point to the to_do list,
         so either it was already processed or it will be processed. *)
      if (not (List.mem package_name !in_the_closure))
      then

        (* If the package is not on the in_the_closure list, then
           it should be processed. *)
        to_do          := package_name :: !to_do;
        in_the_closure := package_name :: !in_the_closure;

    ) current_deps;
    
  done;

  (* Finished! List in_the_closure contains exactly the packages which are in the closure. *)

  (* Helper function: takes a list of package names and returns only these which belong to our closure. *)
  let filter_package_names : (package_name list -> package_name list) = 
    List.filter (fun package_name -> List.mem package_name !in_the_closure)
  in

  (* Filter out all the other packages from the package list (and from their dependencies and conflicts). *)
  List.filter_map (fun package ->
    if List.mem package.package_name !in_the_closure
    then Some (
      {
        package_name     = package.package_name; 
        package_depend   = package.package_depend; (* All these packages have to be in the closure, hence no filtering needed. *)
        package_conflict = filter_package_names package.package_conflict; (* However we need to filter the conflict packages. *)
        package_consume  = package.package_consume;
      }
    )
    else None
  ) packages
*)

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
  (*
  let trimmed_repositories =
    
    (* List of packages which are implementing one of the component types available in the universe. *)
    let implementing_packages =
      List.unique ( List.flatten (
        List.map ( fun (component_type_name, package_names) ->
          package_names
        ) trimmed_implementation
      ))
    in

    (* Function trimming packages of a repository. *)
    let trim_repository_packages repository =
      dependency_transitive_closure implementing_packages repository.repository_packages

    in

    (* Apply package trimming to every repository. *)
    List.map (fun repository -> {
        repository_name     = repository.repository_name;
        repository_packages = trim_repository_packages repository;
      }
    ) universe.universe_repositories
  in
  *)

  (* The trimmed universe. *)
  universe

