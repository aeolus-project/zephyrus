
module X = At1

open Aeolus_types_t

let component_type_name_translate component_type_name = component_type_name
let port_name_translate port_name = port_name
let package_name_translate package_name = package_name
let repository_name_translate repository_name = repository_name
let resource_name_translate resource_name = resource_name

let provide_arity_translate provide_arity =
  match provide_arity with
  | `InfiniteProvide -> X.InfiniteProvide
  | `FiniteProvide i -> X.FiniteProvide i

let require_arity_translate require_arity = require_arity
let resource_consumption_translate resource_consumption = resource_consumption
let resource_provide_arity_translate resource_provide_arity = resource_provide_arity

module SetOfList =
  functor (S : Set.S) ->
  struct
    let translate el_translate l =
      List.fold_left (fun set el ->
        S.add (el_translate el) set
      ) S.empty l
  end

module MapOfList =
  functor (M : Map.S) ->
  struct
    let translate key_translate value_translate l =
      List.fold_left (fun map (key, value) ->
        M.add (key_translate key) (value_translate value) map
      ) M.empty l

  end

let component_type_translate component_type = {
  X.component_type_name     = component_type_name_translate component_type.component_type_name;

  X.component_type_provide  = 
    List.fold_left (fun map (key, value) ->
      X.PortNameMap.add (port_name_translate key) (provide_arity_translate value) map
    ) X.PortNameMap.empty component_type.component_type_provide;
  
  X.component_type_require  = 
    List.fold_left (fun map (key, value) ->
      X.PortNameMap.add (port_name_translate key) (require_arity_translate value) map
    ) X.PortNameMap.empty component_type.component_type_require;
  
  X.component_type_conflict = (
    let module T = SetOfList(X.PortNameSet) in
    T.translate port_name_translate component_type.component_type_conflict);
  
  X.component_type_consume  = 
    List.fold_left (fun map (key, value) ->
      X.ResourceNameMap.add (resource_name_translate key) (resource_consumption_translate value) map
    ) X.ResourceNameMap.empty component_type.component_type_consume
}

let package_translate package = {
  X.package_name     = package_name_translate package.package_name;

  X.package_depend   = List.map (List.map package_name_translate) package.package_depend;

  X.package_conflict = List.map package_name_translate package.package_conflict;

  X.package_consume  = 
    List.fold_left (fun map (key, value) ->
      X.ResourceNameMap.add (resource_name_translate key) (resource_consumption_translate value) map
    ) X.ResourceNameMap.empty package.package_consume
}

let package_names_translate package_names = (
  let module T = SetOfList(X.PackageNameSet) in
  T.translate package_name_translate package_names)

let repository_translate repository = {
  X.repository_name     = repository_name_translate repository.repository_name;

  X.repository_packages = 
    List.fold_left (fun set package_name ->
      X.PackageSet.add (package_translate package_name) set
    ) X.PackageSet.empty repository.repository_packages;
}

let universe_translate universe = {
    X.universe_component_types = 
      List.fold_left (fun set component_type ->
      	X.ComponentTypeSet.add (component_type_translate component_type) set
      ) X.ComponentTypeSet.empty universe.universe_component_types;

    X.universe_implementation = 
      List.fold_left (fun map (key, value) ->
        X.ComponentTypeNameMap.add (component_type_name_translate key) (package_names_translate value) map
      ) X.ComponentTypeNameMap.empty universe.universe_implementation;

    X.universe_repositories = 
      List.fold_left (fun set repository ->
      	X.RepositorySet.add (repository_translate repository) set
      ) X.RepositorySet.empty universe.universe_repositories;
  }
