
open Helpers

module X = At1
module Y = Aeolus_types_t

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

exception DoubleProvide  of Y.component_type_name * Y.port_name
exception DoubleRequire  of Y.component_type_name * Y.port_name
exception DoubleConflict of Y.component_type_name * Y.port_name
exception DoubleConsume  of Y.component_type_name * Y.resource_name

let component_type_translate component_type = {
  X.component_type_name = component_type_name_translate component_type.Y.component_type_name;

  X.component_type_provide = (
    let module T = MapOfAssocList(X.PortNameMap) in
    try
      T.translate port_name_translate provide_arity_translate component_type.Y.component_type_provide
    with
      T.DoubleKey port_name -> raise (DoubleProvide (component_type.Y.component_type_name, port_name)) );
  
  X.component_type_require = (
    let module T = MapOfAssocList(X.PortNameMap) in
    try
      T.translate port_name_translate require_arity_translate component_type.Y.component_type_require
    with
      T.DoubleKey port_name -> raise (DoubleRequire (component_type.Y.component_type_name, port_name)) );
  
  X.component_type_conflict = (
    let module T = SetOfList(X.PortNameSet) in
    try
      T.translate port_name_translate component_type.Y.component_type_conflict
    with 
      T.DoubleElement port_name -> raise (DoubleConflict (component_type.Y.component_type_name, port_name)) );
  
  X.component_type_consume = (
    let module T = MapOfAssocList(X.ResourceNameMap) in
    try
      T.translate resource_name_translate resource_consumption_translate component_type.Y.component_type_consume
    with
      T.DoubleKey resource_name -> raise (DoubleConsume (component_type.Y.component_type_name, resource_name)) );
}

exception DoublePackageConsume of Y.package_name * Y.resource_name

let package_translate package = {
  X.package_name     = package_name_translate package.Y.package_name;

  X.package_depend   = List.map (List.map package_name_translate) package.Y.package_depend;

  X.package_conflict = List.map package_name_translate package.Y.package_conflict;

  X.package_consume  = (
    let module T = MapOfAssocList(X.ResourceNameMap) in
    try
      T.translate resource_name_translate resource_consumption_translate package.Y.package_consume
    with
      T.DoubleKey resource_name -> raise (DoublePackageConsume (package.Y.package_name, resource_name)) );
}

exception DoublePackageNameInRepository of Y.repository_name * Y.package_name

let repository_translate repository = {
  X.repository_name     = repository_name_translate repository.Y.repository_name;

  X.repository_packages = (
    let module T = MapOfList(X.PackageNameMap) in
    try
      T.translate (fun package -> package.Y.package_name) package_translate repository.Y.repository_packages
    with
      T.DoubleKey package_name -> raise (DoublePackageNameInRepository (repository.Y.repository_name, package_name)) );
}

exception DoubleComponentTypeNameInUniverse of Y.component_type_name
exception DoubleImplementationOfAComponentInUniverse of Y.component_type_name
exception DoublePackageName of Y.package_name
exception DoublePackageNameInImplementation of Y.component_type_name * Y.package_name (* TODO: There is no way to produce this exception, because when we want to throw it we have no information about the component type name... *)
exception DoubleRepositoryNameInUniverse of Y.repository_name

let universe_translate universe = {
  X.universe_component_types = (
    let module T = MapOfList(X.ComponentTypeNameMap) in
    try
      T.translate (fun component_type -> component_type.Y.component_type_name) component_type_translate universe.Y.universe_component_types
    with
      T.DoubleKey component_type_name -> raise (DoubleComponentTypeNameInUniverse component_type_name) );

  X.universe_implementation = (
    let package_names_translate package_names =
      let module T = SetOfList(X.PackageNameSet) in
      try
        T.translate package_name_translate package_names
      with
        T.DoubleElement package_name -> raise (DoublePackageName package_name)
    in
      let module T = MapOfAssocList(X.ComponentTypeNameMap) in
      try
        T.translate component_type_name_translate package_names_translate universe.Y.universe_implementation
      with
        T.DoubleKey component_type_name -> raise (DoubleImplementationOfAComponentInUniverse (component_type_name)) );

  X.universe_repositories = (
    let module T = MapOfList(X.RepositoryNameMap) in
    try
      T.translate (fun repository -> repository.Y.repository_name) repository_translate universe.Y.universe_repositories
    with
      T.DoubleKey repository_name -> raise (DoubleRepositoryNameInUniverse repository_name) );
}

let location_name_translate location_name = location_name
let component_name_translate component_name = component_name

exception DoubleLocationResourceProvide of Y.location_name * Y.resource_name
exception DoubleLocationPackageInstalled of Y.location_name * Y.package_name

let location_translate location = {
  X.location_name = location_name_translate location.Y.location_name;

  X.location_provide_resources  = (
    let module T = MapOfAssocList(X.ResourceNameMap) in
    try
      T.translate resource_name_translate resource_provide_arity_translate location.Y.location_provide_resources
    with
      T.DoubleKey resource_name -> raise (DoubleLocationResourceProvide (location.Y.location_name, resource_name)) );

  X.location_repository = repository_name_translate location.Y.location_repository;

  X.location_packages_installed = (
    let module T = SetOfList(X.PackageNameSet) in
    try
      T.translate package_name_translate location.Y.location_packages_installed
    with
      T.DoubleElement package_name -> raise (DoubleLocationPackageInstalled (location.Y.location_name, package_name)) );
}

let component_translate component = {
  X.component_name     = component_name_translate      component.Y.component_name;
  X.component_type     = component_type_name_translate component.Y.component_type;
  X.component_location = location_name_translate       component.Y.component_location;
}

let binding_translate binding = {
  X.binding_port     = port_name_translate      binding.Y.binding_port;
  X.binding_requirer = component_name_translate binding.Y.binding_requirer;
  X.binding_provider = component_name_translate binding.Y.binding_provider;
}

exception DoubleLocationNameInConfiguration of X.location_name
exception DoubleComponentNameInConfiguration of X.component_name
exception DoubleBindingInConfiguration of X.binding

let configuration_translate configuration = {
  X.configuration_locations  = (
    let module T = MapOfList(X.LocationNameMap) in
    try
      T.translate (fun location -> location.Y.location_name) location_translate configuration.Y.configuration_locations
    with
      T.DoubleKey location_name -> raise (DoubleLocationNameInConfiguration location_name) );

  X.configuration_components = (
    let module T = MapOfList(X.ComponentNameMap) in
    try
      T.translate (fun component -> component.Y.component_name) component_translate configuration.Y.configuration_components
    with
      T.DoubleKey component_name -> raise (DoubleComponentNameInConfiguration component_name) );

  X.configuration_bindings = (
    let module T = SetOfList(X.BindingSet) in
    try
      T.translate binding_translate configuration.Y.configuration_bindings
    with
      T.DoubleElement binding -> raise (DoubleBindingInConfiguration binding));
}
