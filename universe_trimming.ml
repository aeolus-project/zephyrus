
open Aeolus_types_j

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
      BatList.unique (List.flatten package.package_depend)
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

  List.filter (fun package ->
    List.mem package.package_name !in_the_closure
  ) packages

let trim universe initial_configuration specification =
  
  (* We don't trim component types for now. *)
  (* We should keep only a transitive closure of these which appear
     in the specification or in the initial configuration. *)
  let trimmed_component_types =
    universe.universe_component_types
  in
  
  (* We don't trim the implementation for now. *)
  (* We should keep just these which are related to component types
     present in the trimmed component types. *)
  let trimmed_implementation =
    universe.universe_implementation
  in

  (* We trim the repositories: we leave only a transitive closure of
     the packages mentioned in the implementation. *)
  let trimmed_repositories =
    
    (* List of packages which are implementing one of the component types. *)
    let implementing_packages =
      BatList.unique ( List.flatten (
        List.map ( fun (component_type_name, package_names) ->
          package_names
        ) trimmed_implementation
      ))
    in

    List.map (fun repository -> {
        repository_name     = repository.repository_name;
        repository_packages = dependency_transitive_closure implementing_packages repository.repository_packages;
      }
    ) universe.universe_repositories

  in

  (* The trimmed universe. *)
  {
    universe_component_types = trimmed_component_types;
    universe_implementation  = trimmed_implementation;
    universe_repositories    = trimmed_repositories;
  }

