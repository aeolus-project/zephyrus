
open Helpers

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

exception DoubleProvide  of component_type_name * port_name
exception DoubleRequire  of component_type_name * port_name
exception DoubleConflict of component_type_name * port_name
exception DoubleConsume  of component_type_name * resource_name

let component_type_translate component_type = {
  X.component_type_name = component_type_name_translate component_type.component_type_name;

  X.component_type_provide = (
    let module T = MapOfAssocList(X.PortNameMap) in
    try
      T.translate port_name_translate provide_arity_translate component_type.component_type_provide
    with
    | T.DoubleKey port_name -> raise (DoubleProvide (component_type.component_type_name, port_name)) );
  
  X.component_type_require = (
    let module T = MapOfAssocList(X.PortNameMap) in
    T.translate port_name_translate require_arity_translate component_type.component_type_require);
  
  X.component_type_conflict = (
    let module T = SetOfList(X.PortNameSet) in
    T.translate port_name_translate component_type.component_type_conflict);
  
  X.component_type_consume = (
    let module T = MapOfAssocList(X.ResourceNameMap) in
    T.translate resource_name_translate resource_consumption_translate component_type.component_type_consume);
}

exception DoublePackageConsume of package_name * resource_name

let package_translate package = {
  X.package_name     = package_name_translate package.package_name;

  X.package_depend   = List.map (List.map package_name_translate) package.package_depend;

  X.package_conflict = List.map package_name_translate package.package_conflict;

  X.package_consume  = (
    let module T = MapOfAssocList(X.ResourceNameMap) in
    T.translate resource_name_translate resource_consumption_translate package.package_consume);
}

exception DoublePackageNameInRepository of repository_name * package_name

let repository_translate repository = {
  X.repository_name     = repository_name_translate repository.repository_name;

  X.repository_packages = (
    let module T = MapOfList(X.PackageNameMap) in
    T.translate (fun package -> package.package_name) package_translate repository.repository_packages);
}

exception DoubleImplementation of component_type_name
exception DoublePackageNameInImplementation of component_type_name * package_name
exception DoubleRepositoryNameInUniverse of repository_name

let universe_translate universe = {
  X.universe_component_types = (
    let module T = MapOfList(X.ComponentTypeNameMap) in
    T.translate (fun component_type -> component_type.component_type_name) component_type_translate universe.universe_component_types);

  X.universe_implementation = (
    let package_names_translate package_names =
      let module T = SetOfList(X.PackageNameSet) in
      T.translate package_name_translate package_names
    in
      let module T = MapOfAssocList(X.ComponentTypeNameMap) in
      T.translate component_type_name_translate package_names_translate universe.universe_implementation);

  X.universe_repositories = (
    let module T = MapOfList(X.RepositoryNameMap) in
    T.translate (fun repository -> repository.repository_name) repository_translate universe.universe_repositories);
}

let location_name_translate location_name = location_name
let component_name_translate component_name = component_name

let location_translate location = {
  X.location_name = location_name_translate location.location_name;

  X.location_provide_resources  = (
    let module T = MapOfAssocList(X.ResourceNameMap) in
    T.translate resource_name_translate resource_provide_arity_translate location.location_provide_resources);

  X.location_repository = repository_name_translate location.location_repository;

  X.location_packages_installed = (
    let module T = SetOfList(X.PackageNameSet) in
    T.translate package_name_translate location.location_packages_installed);
}

let component_translate component = {
  X.component_name     = component_name_translate      component.component_name;
  X.component_type     = component_type_name_translate component.component_type;
  X.component_location = location_name_translate       component.component_location;
}

let binding_translate binding = {
  X.binding_port     = port_name_translate      binding.binding_port;
  X.binding_requirer = component_name_translate binding.binding_requirer;
  X.binding_provider = component_name_translate binding.binding_provider;
}

let configuration_translate configuration = {
  X.configuration_locations  = (
    let module T = MapOfList(X.LocationNameMap) in
    T.translate (fun location -> location.location_name) location_translate configuration.configuration_locations);

  X.configuration_components = (
    let module T = MapOfList(X.ComponentNameMap) in
    T.translate (fun component -> component.component_name) component_translate configuration.configuration_components);

  X.configuration_bindings = (
    let module T = SetOfList(X.BindingSet) in
    T.translate binding_translate configuration.configuration_bindings);
}
