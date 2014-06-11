(* Auto-generated from "stateful_json_v1.atd" *)


(** Type definition for stateful syntax version. *)

(** Type definitions for naming. *)
type version = Json_versions_t.version

type component_type_name = Stateful_json_v1_t.component_type_name

type state_name = Stateful_json_v1_t.state_name

type port_name = Stateful_json_v1_t.port_name

type component_name = Stateful_json_v1_t.component_name

type package_name = Stateful_json_v1_t.package_name

type repository_name = Stateful_json_v1_t.repository_name

type location_name = Stateful_json_v1_t.location_name

(** Type definitions for Universe. *)
type resource_name = Stateful_json_v1_t.resource_name

type provide_arity = Stateful_json_v1_t.provide_arity

type require_arity = Stateful_json_v1_t.require_arity

type resource_consumption = Stateful_json_v1_t.resource_consumption

type resource_provide_arity = Stateful_json_v1_t.resource_provide_arity

type state = Stateful_json_v1_t.state = {
  state_name (*atd name *): state_name;
  state_initial (*atd initial *): bool;
  state_final (*atd final *): bool;
  state_provide (*atd provide *): (port_name * provide_arity) list;
  state_require (*atd require *): (port_name * require_arity) list;
  state_conflict (*atd conflict *): port_name list;
  state_successors (*atd successors *): state_name list
}

type component_type = Stateful_json_v1_t.component_type = {
  component_type_name (*atd name *): component_type_name;
  component_type_states (*atd states *): state list;
  component_type_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_types = Stateful_json_v1_t.component_types

type package = Stateful_json_v1_t.package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): package_name list list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type packages = Stateful_json_v1_t.packages

type repository = Stateful_json_v1_t.repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = Stateful_json_v1_t.repositories

type implementation_package = Stateful_json_v1_t.implementation_package = {
  implementation_package_repository (*atd repository *): repository_name;
  implementation_package_package (*atd package *): package_name
}

type implementation_packages = Stateful_json_v1_t.implementation_packages

(** Type definitions for Configuration. *)
type universe = Stateful_json_v1_t.universe = {
  universe_version (*atd version *): version;
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * implementation_packages) list;
  universe_repositories (*atd repositories *): repositories
}

type resources_provided = Stateful_json_v1_t.resources_provided

type location_cost = Stateful_json_v1_t.location_cost

type location = Stateful_json_v1_t.location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component = Stateful_json_v1_t.component = {
  component_name (*atd name *): component_name;
  component_type (*atd component_type_workaround *): component_type_name;
  component_state (*atd state *): state_name;
  component_location (*atd location *): location_name
}

type binding = Stateful_json_v1_t.binding = {
  binding_port (*atd port *): port_name;
  binding_requirer (*atd requirer *): component_name;
  binding_provider (*atd provider *): component_name
}

type configuration = Stateful_json_v1_t.configuration = {
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
let validate_state_name = (
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
let validate_state = (
  fun _ _ -> None
)
let validate__5 = (
  fun _ _ -> None
)
let validate__6 = (
  fun _ _ -> None
)
let validate_component_type = (
  fun _ _ -> None
)
let validate__7 = (
  fun _ _ -> None
)
let validate_component_types = (
  validate__7
)
let validate__8 = (
  fun _ _ -> None
)
let validate__9 = (
  fun _ _ -> None
)
let validate_package = (
  fun _ _ -> None
)
let validate__10 = (
  fun _ _ -> None
)
let validate_packages = (
  validate__10
)
let validate_repository = (
  fun _ _ -> None
)
let validate__11 = (
  fun _ _ -> None
)
let validate_repositories = (
  validate__11
)
let validate_implementation_package = (
  fun _ _ -> None
)
let validate__12 = (
  fun _ _ -> None
)
let validate_implementation_packages = (
  validate__12
)
let validate__13 = (
  fun _ _ -> None
)
let validate_universe = (
  fun path x ->
    (
      validate_version
    ) (`Field "universe_version" :: path) x.universe_version
)
let validate__14 = (
  fun _ _ -> None
)
let validate_resources_provided = (
  validate__14
)
let validate_location_cost = (
  (fun _ _ -> None)
)
let validate_location = (
  fun _ _ -> None
)
let validate_component = (
  fun _ _ -> None
)
let validate_binding = (
  fun _ _ -> None
)
let validate__15 = (
  fun _ _ -> None
)
let validate__16 = (
  fun _ _ -> None
)
let validate__17 = (
  fun _ _ -> None
)
let validate_configuration = (
  fun path x ->
    (
      validate_version
    ) (`Field "configuration_version" :: path) x.configuration_version
)
let create_state 
  ~state_name
  ?(state_initial = false)
  ?(state_final = false)
  ?(state_provide = [])
  ?(state_require = [])
  ?(state_conflict = [])
  ?(state_successors = [])
  () =
  {
    state_name = state_name;
    state_initial = state_initial;
    state_final = state_final;
    state_provide = state_provide;
    state_require = state_require;
    state_conflict = state_conflict;
    state_successors = state_successors;
  }
let create_component_type 
  ~component_type_name
  ~component_type_states
  ?(component_type_consume = [])
  () =
  {
    component_type_name = component_type_name;
    component_type_states = component_type_states;
    component_type_consume = component_type_consume;
  }
let create_package 
  ~package_name
  ?(package_depend = [])
  ?(package_conflict = [])
  ?(package_consume = [])
  () =
  {
    package_name = package_name;
    package_depend = package_depend;
    package_conflict = package_conflict;
    package_consume = package_consume;
  }
let create_repository 
  ~repository_name
  ?(repository_packages = [])
  () =
  {
    repository_name = repository_name;
    repository_packages = repository_packages;
  }
let create_implementation_package 
  ~implementation_package_repository
  ~implementation_package_package
  () =
  {
    implementation_package_repository = implementation_package_repository;
    implementation_package_package = implementation_package_package;
  }
let create_universe 
  ~universe_version
  ?(universe_component_types = [])
  ?(universe_implementation = [])
  ?(universe_repositories = [])
  () =
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
  () =
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
  ~component_state
  ~component_location
  () =
  {
    component_name = component_name;
    component_type = component_type;
    component_state = component_state;
    component_location = component_location;
  }
let create_binding 
  ~binding_port
  ~binding_requirer
  ~binding_provider
  () =
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
  () =
  {
    configuration_version = configuration_version;
    configuration_locations = configuration_locations;
    configuration_components = configuration_components;
    configuration_bindings = configuration_bindings;
  }
