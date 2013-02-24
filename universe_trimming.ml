
open Aeolus_types_j

let dependency_transitive_closure (package_names : package_name list) (packages : package list) : package list =
  let dep_table = Hashtbl.create 17 in

  List.iter (fun package ->
    Hashtbl.add dep_table package.package_name package
  ) packages;

  let get_deps (package_name : package_name) : package_name list =
    try
  	  let package = Hashtbl.find dep_table package_name
      in
      BatList.unique (List.flatten package.package_depend)
    with Not_found -> []
  
  in

  let in_the_closure = ref package_names in
  let to_do          = ref package_names in

  while not (!to_do = []) do
    let current      = List.hd !to_do in
    let current_deps = get_deps current
    in
    to_do := List.tl !to_do;
    List.iter (fun package_name -> 
      if (not (List.mem package_name !to_do))
      then to_do := package_name :: !to_do;
    ) current_deps;
    in_the_closure := BatList.unique (current_deps @ !in_the_closure)
  done;

  List.filter (fun package ->
  	List.mem package.package_name !in_the_closure
  ) packages

let trim universe specification =
  
  let trimmed_component_types =
    universe.universe_component_types
  in

  let trimmed_implementation =
    universe.universe_implementation
  in

  let implementing_packages =
    BatList.unique ( List.flatten (
      List.map ( fun (component_type_name, package_names) ->
        package_names
      ) trimmed_implementation
    ))
  in

  let trimmed_repositories =
    List.map (fun repository -> {
    	repository_name     = repository.repository_name;
    	repository_packages = dependency_transitive_closure implementing_packages repository.repository_packages;
      }
    ) universe.universe_repositories

  in
  {
    universe_component_types = trimmed_component_types;
    universe_implementation  = trimmed_implementation;
    universe_repositories    = trimmed_repositories;
  }

