(* Auto-generated from "json_v1.atd" *)


(** Type definition for syntax version. *)

(** Type definitions for naming. *)
type version = Json_versions_t.version

type component_type_name = Json_v1_t.component_type_name

type port_name = Json_v1_t.port_name

type component_name = Json_v1_t.component_name

type package_name = Json_v1_t.package_name

type repository_name = Json_v1_t.repository_name

type location_name = Json_v1_t.location_name

(** Type definitions for Universe. *)
type resource_name = Json_v1_t.resource_name

type provide_arity = Json_v1_t.provide_arity

type require_arity = Json_v1_t.require_arity

type resource_consumption = Json_v1_t.resource_consumption

type resource_provide_arity = Json_v1_t.resource_provide_arity

type component_type = Json_v1_t.component_type = {
  component_type_name (*atd name *): component_type_name;
  component_type_provide (*atd provide *): (port_name * provide_arity) list;
  component_type_require (*atd require *): (port_name * require_arity) list;
  component_type_conflict (*atd conflict *): port_name list;
  component_type_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_types = Json_v1_t.component_types

type package = Json_v1_t.package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): (package_name list) list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type packages = Json_v1_t.packages

type repository = Json_v1_t.repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = Json_v1_t.repositories

type package_names = Json_v1_t.package_names

(** Type definitions for Configuration. *)
type universe = Json_v1_t.universe = {
  universe_version (*atd version *): version;
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * package_names) list;
  universe_repositories (*atd repositories *): repositories
}

type resources_provided = Json_v1_t.resources_provided

type location_cost = Json_v1_t.location_cost

type location = Json_v1_t.location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component = Json_v1_t.component = {
  component_name (*atd name *): component_name;
  component_type (*atd component_type_workaround *): component_type_name;
  component_location (*atd location *): location_name
}

type binding = Json_v1_t.binding = {
  binding_port (*atd port *): port_name;
  binding_requirer (*atd requirer *): component_name;
  binding_provider (*atd provider *): component_name
}

type configuration = Json_v1_t.configuration = {
  configuration_version (*atd version *): version;
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}

let validate_version = (
  Json_versions_v.validate_version
)
let validate_component_type_name = (
  (fun _ _ -> None)
)
let validate_port_name = (
  (fun _ _ -> None)
)
let validate_component_name = (
  (fun _ _ -> None)
)
let validate_package_name = (
  (fun _ _ -> None)
)
let validate_repository_name = (
  (fun _ _ -> None)
)
let validate_location_name = (
  (fun _ _ -> None)
)
let validate_resource_name = (
  (fun _ _ -> None)
)
let validate_provide_arity = (
  (fun _ _ -> None)
)
let validate_require_arity = (
  (fun _ _ -> None)
)
let validate_resource_consumption = (
  (fun _ _ -> None)
)
let validate_resource_provide_arity = (
  (fun _ _ -> None)
)
let validate__1 = (
  fun _ _ -> None
)
let validate__2 = (
  fun _ _ -> None
)
let validate__3 = (
  fun _ _ -> None
)
let validate__4 = (
  fun _ _ -> None
)
let validate_component_type : _ -> component_type -> _ = (
  fun _ _ -> None
)
let validate__5 = (
  fun _ _ -> None
)
let validate_component_types = (
  validate__5
)
let validate__6 = (
  fun _ _ -> None
)
let validate__7 = (
  fun _ _ -> None
)
let validate_package : _ -> package -> _ = (
  fun _ _ -> None
)
let validate__8 = (
  fun _ _ -> None
)
let validate_packages = (
  validate__8
)
let validate_repository : _ -> repository -> _ = (
  fun _ _ -> None
)
let validate__9 = (
  fun _ _ -> None
)
let validate_repositories = (
  validate__9
)
let validate__10 = (
  fun _ _ -> None
)
let validate_package_names = (
  validate__10
)
let validate__11 = (
  fun _ _ -> None
)
let validate_universe : _ -> universe -> _ = (
  fun path x ->
    (
      validate_version
    ) (`Field "universe_version" :: path) x.universe_version
)
let validate__12 = (
  fun _ _ -> None
)
let validate_resources_provided = (
  validate__12
)
let validate_location_cost = (
  (fun _ _ -> None)
)
let validate_location : _ -> location -> _ = (
  fun _ _ -> None
)
let validate_component : _ -> component -> _ = (
  fun _ _ -> None
)
let validate_binding : _ -> binding -> _ = (
  fun _ _ -> None
)
let validate__13 = (
  fun _ _ -> None
)
let validate__14 = (
  fun _ _ -> None
)
let validate__15 = (
  fun _ _ -> None
)
let validate_configuration : _ -> configuration -> _ = (
  fun path x ->
    (
      validate_version
    ) (`Field "configuration_version" :: path) x.configuration_version
)
let create_component_type 
  ~component_type_name
  ?(component_type_provide = [])
  ?(component_type_require = [])
  ?(component_type_conflict = [])
  ?(component_type_consume = [])
  () : component_type =
  {
    component_type_name = component_type_name;
    component_type_provide = component_type_provide;
    component_type_require = component_type_require;
    component_type_conflict = component_type_conflict;
    component_type_consume = component_type_consume;
  }
let create_package 
  ~package_name
  ?(package_depend = [])
  ?(package_conflict = [])
  ?(package_consume = [])
  () : package =
  {
    package_name = package_name;
    package_depend = package_depend;
    package_conflict = package_conflict;
    package_consume = package_consume;
  }
let create_repository 
  ~repository_name
  ?(repository_packages = [])
  () : repository =
  {
    repository_name = repository_name;
    repository_packages = repository_packages;
  }
let create_universe 
  ~universe_version
  ?(universe_component_types = [])
  ?(universe_implementation = [])
  ?(universe_repositories = [])
  () : universe =
  {
    universe_version = universe_version;
    universe_component_types = universe_component_types;
    universe_implementation = universe_implementation;
    universe_repositories = universe_repositories;
  }
let create_location 
  ~location_name
  ?(location_provide_resources = [])
  ~location_repository
  ?(location_packages_installed = [])
  ?(location_cost = 1)
  () : location =
  {
    location_name = location_name;
    location_provide_resources = location_provide_resources;
    location_repository = location_repository;
    location_packages_installed = location_packages_installed;
    location_cost = location_cost;
  }
let create_component 
  ~component_name
  ~component_type
  ~component_location
  () : component =
  {
    component_name = component_name;
    component_type = component_type;
    component_location = component_location;
  }
let create_binding 
  ~binding_port
  ~binding_requirer
  ~binding_provider
  () : binding =
  {
    binding_port = binding_port;
    binding_requirer = binding_requirer;
    binding_provider = binding_provider;
  }
let create_configuration 
  ~configuration_version
  ?(configuration_locations = [])
  ?(configuration_components = [])
  ?(configuration_bindings = [])
  () : configuration =
  {
    configuration_version = configuration_version;
    configuration_locations = configuration_locations;
    configuration_components = configuration_components;
    configuration_bindings = configuration_bindings;
  }
