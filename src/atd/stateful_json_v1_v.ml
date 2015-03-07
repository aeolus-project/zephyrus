(* Auto-generated from "stateful_json_v1.atd" *)


(** Type definition for stateful syntax version. *)

(** Type definitions for naming. *)
type version = Json_versions_t.version

type state_name = Stateful_json_v1_t.state_name

type require_arity = Stateful_json_v1_t.require_arity

type provide_arity = Stateful_json_v1_t.provide_arity

type port_name = Stateful_json_v1_t.port_name

type state = Stateful_json_v1_t.state = {
  state_name (*atd name *): state_name;
  state_initial (*atd initial *): bool;
  state_final (*atd final *): bool;
  state_provide (*atd provide *): (port_name * provide_arity) list;
  state_require (*atd require *): (port_name * require_arity) list;
  state_conflict (*atd conflict *): port_name list;
  state_successors (*atd successors *): state_name list
}

(** Type definitions for Universe. *)
type resource_name = Stateful_json_v1_t.resource_name

type resource_consumption = Stateful_json_v1_t.resource_consumption

type repository_name = Stateful_json_v1_t.repository_name

type package_name = Stateful_json_v1_t.package_name

type package = Stateful_json_v1_t.package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): package_name list list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

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

type component_type_name = Stateful_json_v1_t.component_type_name

type component_type_stateful = Stateful_json_v1_t.component_type_stateful = {
  component_type_stateful_name (*atd name *): component_type_name;
  component_type_stateful_states (*atd states *): state list;
  component_type_stateful_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_type = Stateful_json_v1_t.component_type

type component_types = Stateful_json_v1_t.component_types

(** Type definitions for Configuration. *)
type universe = Stateful_json_v1_t.universe = {
  universe_version (*atd version *): version;
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * implementation_packages) list;
  universe_repositories (*atd repositories *): repositories;
  universe_port_hierarchy (*atd port_hierarchy *):
    (port_name * (port_name list)) list
}

type resource_provide_arity = Stateful_json_v1_t.resource_provide_arity

type resources_provided = Stateful_json_v1_t.resources_provided

type port_hierarchy = Stateful_json_v1_t.port_hierarchy = {
  port_hierarchy_port (*atd port *): port_name;
  port_hierarchy_subports (*atd subports *): port_name list
}

type packages = Stateful_json_v1_t.packages

type location_name = Stateful_json_v1_t.location_name

type location_cost = Stateful_json_v1_t.location_cost

type location = Stateful_json_v1_t.location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component_name = Stateful_json_v1_t.component_name

type component_stateful = Stateful_json_v1_t.component_stateful = {
  component_stateful_name (*atd name *): component_name;
  component_stateful_type (*atd component_type_workaround *):
    component_type_name;
  component_stateful_state (*atd state *): state_name;
  component_stateful_location (*atd location *): location_name
}

type component_simple = Stateful_json_v1_t.component_simple = {
  component_simple_name (*atd name *): component_name;
  component_simple_type (*atd component_type_workaround *):
    component_type_name;
  component_simple_location (*atd location *): location_name
}

type component = Stateful_json_v1_t.component

type binding_simple = Stateful_json_v1_t.binding_simple = {
  binding_simple_port (*atd port *): port_name;
  binding_simple_requirer (*atd requirer *): component_name;
  binding_simple_provider (*atd provider *): component_name
}

type binding_hierarchical = Stateful_json_v1_t.binding_hierarchical = {
  binding_hierarchical_port_required (*atd port_required *): port_name;
  binding_hierarchical_port_provided (*atd port_provided *): port_name;
  binding_hierarchical_requirer (*atd requirer *): component_name;
  binding_hierarchical_provider (*atd provider *): component_name
}

type binding = Stateful_json_v1_t.binding

type configuration = Stateful_json_v1_t.configuration = {
  configuration_version (*atd version *): version;
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}

type component_type_simple = Stateful_json_v1_t.component_type_simple = {
  component_type_simple_name (*atd name *): component_type_name;
  component_type_simple_provide (*atd provide *):
    (port_name * provide_arity) list;
  component_type_simple_require (*atd require *):
    (port_name * require_arity) list;
  component_type_simple_conflict (*atd conflict *): port_name list;
  component_type_simple_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

let validate_version = (
  Json_versions_v.validate_version
)
let validate_state_name = (
  (fun _ _ -> None)
)
let validate_require_arity = (
  (fun _ _ -> None)
)
let validate_provide_arity = (
  (fun _ _ -> None)
)
let validate_port_name = (
  (fun _ _ -> None)
)
let validate__4 = (
  fun _ _ -> None
)
let validate__3 = (
  fun _ _ -> None
)
let validate__2 = (
  fun _ _ -> None
)
let validate__1 = (
  fun _ _ -> None
)
let validate_state : _ -> state -> _ = (
  fun _ _ -> None
)
let validate_resource_name = (
  (fun _ _ -> None)
)
let validate_resource_consumption = (
  (fun _ _ -> None)
)
let validate_repository_name = (
  (fun _ _ -> None)
)
let validate_package_name = (
  (fun _ _ -> None)
)
let validate__8 = (
  fun _ _ -> None
)
let validate__9 = (
  fun _ _ -> None
)
let validate__6 = (
  fun _ _ -> None
)
let validate_package : _ -> package -> _ = (
  fun _ _ -> None
)
let validate__10 = (
  fun _ _ -> None
)
let validate_repository : _ -> repository -> _ = (
  fun _ _ -> None
)
let validate__11 = (
  fun _ _ -> None
)
let validate_repositories = (
  validate__11
)
let validate_implementation_package : _ -> implementation_package -> _ = (
  fun _ _ -> None
)
let validate__12 = (
  fun _ _ -> None
)
let validate_implementation_packages = (
  validate__12
)
let validate_component_type_name = (
  (fun _ _ -> None)
)
let validate__5 = (
  fun _ _ -> None
)
let validate_component_type_stateful : _ -> component_type_stateful -> _ = (
  fun _ _ -> None
)
let validate_component_type = (
  validate_component_type_stateful
)
let validate__7 = (
  fun _ _ -> None
)
let validate_component_types = (
  validate__7
)
let validate__14 = (
  fun _ _ -> None
)
let validate__13 = (
  fun _ _ -> None
)
let validate_universe : _ -> universe -> _ = (
  fun path x ->
    (
      validate_version
    ) (`Field "universe_version" :: path) x.universe_version
)
let validate_resource_provide_arity = (
  (fun _ _ -> None)
)
let validate__15 = (
  fun _ _ -> None
)
let validate_resources_provided = (
  validate__15
)
let validate_port_hierarchy : _ -> port_hierarchy -> _ = (
  fun _ _ -> None
)
let validate_packages = (
  validate__10
)
let validate_location_name = (
  (fun _ _ -> None)
)
let validate_location_cost = (
  (fun _ _ -> None)
)
let validate_location : _ -> location -> _ = (
  fun _ _ -> None
)
let validate_component_name = (
  (fun _ _ -> None)
)
let validate_component_stateful : _ -> component_stateful -> _ = (
  fun _ _ -> None
)
let validate_component_simple : _ -> component_simple -> _ = (
  fun _ _ -> None
)
let validate_component = (
  fun _ _ -> None
)
let validate_binding_simple : _ -> binding_simple -> _ = (
  fun _ _ -> None
)
let validate_binding_hierarchical : _ -> binding_hierarchical -> _ = (
  fun _ _ -> None
)
let validate_binding = (
  fun _ _ -> None
)
let validate__18 = (
  fun _ _ -> None
)
let validate__17 = (
  fun _ _ -> None
)
let validate__16 = (
  fun _ _ -> None
)
let validate_configuration : _ -> configuration -> _ = (
  fun path x ->
    (
      validate_version
    ) (`Field "configuration_version" :: path) x.configuration_version
)
let validate_component_type_simple : _ -> component_type_simple -> _ = (
  fun _ _ -> None
)
let create_state 
  ~state_name
  ?(state_initial = false)
  ?(state_final = false)
  ?(state_provide = [])
  ?(state_require = [])
  ?(state_conflict = [])
  ?(state_successors = [])
  () : state =
  {
    state_name = state_name;
    state_initial = state_initial;
    state_final = state_final;
    state_provide = state_provide;
    state_require = state_require;
    state_conflict = state_conflict;
    state_successors = state_successors;
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
let create_implementation_package 
  ~implementation_package_repository
  ~implementation_package_package
  () : implementation_package =
  {
    implementation_package_repository = implementation_package_repository;
    implementation_package_package = implementation_package_package;
  }
let create_component_type_stateful 
  ~component_type_stateful_name
  ~component_type_stateful_states
  ?(component_type_stateful_consume = [])
  () : component_type_stateful =
  {
    component_type_stateful_name = component_type_stateful_name;
    component_type_stateful_states = component_type_stateful_states;
    component_type_stateful_consume = component_type_stateful_consume;
  }
let create_universe 
  ~universe_version
  ?(universe_component_types = [])
  ?(universe_implementation = [])
  ?(universe_repositories = [])
  ?(universe_port_hierarchy = [])
  () : universe =
  {
    universe_version = universe_version;
    universe_component_types = universe_component_types;
    universe_implementation = universe_implementation;
    universe_repositories = universe_repositories;
    universe_port_hierarchy = universe_port_hierarchy;
  }
let create_port_hierarchy 
  ~port_hierarchy_port
  ~port_hierarchy_subports
  () : port_hierarchy =
  {
    port_hierarchy_port = port_hierarchy_port;
    port_hierarchy_subports = port_hierarchy_subports;
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
let create_component_stateful 
  ~component_stateful_name
  ~component_stateful_type
  ~component_stateful_state
  ~component_stateful_location
  () : component_stateful =
  {
    component_stateful_name = component_stateful_name;
    component_stateful_type = component_stateful_type;
    component_stateful_state = component_stateful_state;
    component_stateful_location = component_stateful_location;
  }
let create_component_simple 
  ~component_simple_name
  ~component_simple_type
  ~component_simple_location
  () : component_simple =
  {
    component_simple_name = component_simple_name;
    component_simple_type = component_simple_type;
    component_simple_location = component_simple_location;
  }
let create_binding_simple 
  ~binding_simple_port
  ~binding_simple_requirer
  ~binding_simple_provider
  () : binding_simple =
  {
    binding_simple_port = binding_simple_port;
    binding_simple_requirer = binding_simple_requirer;
    binding_simple_provider = binding_simple_provider;
  }
let create_binding_hierarchical 
  ~binding_hierarchical_port_required
  ~binding_hierarchical_port_provided
  ~binding_hierarchical_requirer
  ~binding_hierarchical_provider
  () : binding_hierarchical =
  {
    binding_hierarchical_port_required = binding_hierarchical_port_required;
    binding_hierarchical_port_provided = binding_hierarchical_port_provided;
    binding_hierarchical_requirer = binding_hierarchical_requirer;
    binding_hierarchical_provider = binding_hierarchical_provider;
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
let create_component_type_simple 
  ~component_type_simple_name
  ?(component_type_simple_provide = [])
  ?(component_type_simple_require = [])
  ?(component_type_simple_conflict = [])
  ?(component_type_simple_consume = [])
  () : component_type_simple =
  {
    component_type_simple_name = component_type_simple_name;
    component_type_simple_provide = component_type_simple_provide;
    component_type_simple_require = component_type_simple_require;
    component_type_simple_conflict = component_type_simple_conflict;
    component_type_simple_consume = component_type_simple_consume;
  }
