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

type component = Stateful_json_v1_t.component

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

type component_simple = Stateful_json_v1_t.component_simple = {
  component_simple_name (*atd name *): component_name;
  component_simple_type (*atd component_type_workaround *):
    component_type_name;
  component_simple_location (*atd location *): location_name
}

type binding_simple = Stateful_json_v1_t.binding_simple = {
  binding_simple_port (*atd port *): port_name;
  binding_simple_requirer (*atd requirer *): component_name;
  binding_simple_provider (*atd provider *): component_name
}

val validate_version :
  Ag_util.Validation.path -> version -> Ag_util.Validation.error option
  (** Validate a value of type {!version}. *)

val validate_state_name :
  Ag_util.Validation.path -> state_name -> Ag_util.Validation.error option
  (** Validate a value of type {!state_name}. *)

val validate_require_arity :
  Ag_util.Validation.path -> require_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!require_arity}. *)

val validate_provide_arity :
  Ag_util.Validation.path -> provide_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!provide_arity}. *)

val validate_port_name :
  Ag_util.Validation.path -> port_name -> Ag_util.Validation.error option
  (** Validate a value of type {!port_name}. *)

val create_state :
  state_name: state_name ->
  ?state_initial: bool ->
  ?state_final: bool ->
  ?state_provide: (port_name * provide_arity) list ->
  ?state_require: (port_name * require_arity) list ->
  ?state_conflict: port_name list ->
  ?state_successors: state_name list ->
  unit -> state
  (** Create a record of type {!state}. *)

val validate_state :
  Ag_util.Validation.path -> state -> Ag_util.Validation.error option
  (** Validate a value of type {!state}. *)

val validate_resource_name :
  Ag_util.Validation.path -> resource_name -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_name}. *)

val validate_resource_consumption :
  Ag_util.Validation.path -> resource_consumption -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_consumption}. *)

val validate_repository_name :
  Ag_util.Validation.path -> repository_name -> Ag_util.Validation.error option
  (** Validate a value of type {!repository_name}. *)

val validate_package_name :
  Ag_util.Validation.path -> package_name -> Ag_util.Validation.error option
  (** Validate a value of type {!package_name}. *)

val create_package :
  package_name: package_name ->
  ?package_depend: package_name list list ->
  ?package_conflict: package_name list ->
  ?package_consume: (resource_name * resource_consumption) list ->
  unit -> package
  (** Create a record of type {!package}. *)

val validate_package :
  Ag_util.Validation.path -> package -> Ag_util.Validation.error option
  (** Validate a value of type {!package}. *)

val create_repository :
  repository_name: repository_name ->
  ?repository_packages: package list ->
  unit -> repository
  (** Create a record of type {!repository}. *)

val validate_repository :
  Ag_util.Validation.path -> repository -> Ag_util.Validation.error option
  (** Validate a value of type {!repository}. *)

val validate_repositories :
  Ag_util.Validation.path -> repositories -> Ag_util.Validation.error option
  (** Validate a value of type {!repositories}. *)

val create_implementation_package :
  implementation_package_repository: repository_name ->
  implementation_package_package: package_name ->
  unit -> implementation_package
  (** Create a record of type {!implementation_package}. *)

val validate_implementation_package :
  Ag_util.Validation.path -> implementation_package -> Ag_util.Validation.error option
  (** Validate a value of type {!implementation_package}. *)

val validate_implementation_packages :
  Ag_util.Validation.path -> implementation_packages -> Ag_util.Validation.error option
  (** Validate a value of type {!implementation_packages}. *)

val validate_component_type_name :
  Ag_util.Validation.path -> component_type_name -> Ag_util.Validation.error option
  (** Validate a value of type {!component_type_name}. *)

val create_component_type_stateful :
  component_type_stateful_name: component_type_name ->
  component_type_stateful_states: state list ->
  ?component_type_stateful_consume: (resource_name * resource_consumption) list ->
  unit -> component_type_stateful
  (** Create a record of type {!component_type_stateful}. *)

val validate_component_type_stateful :
  Ag_util.Validation.path -> component_type_stateful -> Ag_util.Validation.error option
  (** Validate a value of type {!component_type_stateful}. *)

val validate_component_type :
  Ag_util.Validation.path -> component_type -> Ag_util.Validation.error option
  (** Validate a value of type {!component_type}. *)

val validate_component_types :
  Ag_util.Validation.path -> component_types -> Ag_util.Validation.error option
  (** Validate a value of type {!component_types}. *)

val create_universe :
  universe_version: version ->
  ?universe_component_types: component_types ->
  ?universe_implementation: (component_type_name * implementation_packages) list ->
  ?universe_repositories: repositories ->
  ?universe_port_hierarchy: (port_name * (port_name list)) list ->
  unit -> universe
  (** Create a record of type {!universe}. *)

val validate_universe :
  Ag_util.Validation.path -> universe -> Ag_util.Validation.error option
  (** Validate a value of type {!universe}. *)

val validate_resource_provide_arity :
  Ag_util.Validation.path -> resource_provide_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_provide_arity}. *)

val validate_resources_provided :
  Ag_util.Validation.path -> resources_provided -> Ag_util.Validation.error option
  (** Validate a value of type {!resources_provided}. *)

val create_port_hierarchy :
  port_hierarchy_port: port_name ->
  port_hierarchy_subports: port_name list ->
  unit -> port_hierarchy
  (** Create a record of type {!port_hierarchy}. *)

val validate_port_hierarchy :
  Ag_util.Validation.path -> port_hierarchy -> Ag_util.Validation.error option
  (** Validate a value of type {!port_hierarchy}. *)

val validate_packages :
  Ag_util.Validation.path -> packages -> Ag_util.Validation.error option
  (** Validate a value of type {!packages}. *)

val validate_location_name :
  Ag_util.Validation.path -> location_name -> Ag_util.Validation.error option
  (** Validate a value of type {!location_name}. *)

val validate_location_cost :
  Ag_util.Validation.path -> location_cost -> Ag_util.Validation.error option
  (** Validate a value of type {!location_cost}. *)

val create_location :
  location_name: location_name ->
  ?location_provide_resources: resources_provided ->
  location_repository: repository_name ->
  ?location_packages_installed: package_name list ->
  ?location_cost: location_cost ->
  unit -> location
  (** Create a record of type {!location}. *)

val validate_location :
  Ag_util.Validation.path -> location -> Ag_util.Validation.error option
  (** Validate a value of type {!location}. *)

val validate_component_name :
  Ag_util.Validation.path -> component_name -> Ag_util.Validation.error option
  (** Validate a value of type {!component_name}. *)

val create_component_stateful :
  component_stateful_name: component_name ->
  component_stateful_type: component_type_name ->
  component_stateful_state: state_name ->
  component_stateful_location: location_name ->
  unit -> component_stateful
  (** Create a record of type {!component_stateful}. *)

val validate_component_stateful :
  Ag_util.Validation.path -> component_stateful -> Ag_util.Validation.error option
  (** Validate a value of type {!component_stateful}. *)

val validate_component :
  Ag_util.Validation.path -> component -> Ag_util.Validation.error option
  (** Validate a value of type {!component}. *)

val create_binding_hierarchical :
  binding_hierarchical_port_required: port_name ->
  binding_hierarchical_port_provided: port_name ->
  binding_hierarchical_requirer: component_name ->
  binding_hierarchical_provider: component_name ->
  unit -> binding_hierarchical
  (** Create a record of type {!binding_hierarchical}. *)

val validate_binding_hierarchical :
  Ag_util.Validation.path -> binding_hierarchical -> Ag_util.Validation.error option
  (** Validate a value of type {!binding_hierarchical}. *)

val validate_binding :
  Ag_util.Validation.path -> binding -> Ag_util.Validation.error option
  (** Validate a value of type {!binding}. *)

val create_configuration :
  configuration_version: version ->
  ?configuration_locations: location list ->
  ?configuration_components: component list ->
  ?configuration_bindings: binding list ->
  unit -> configuration
  (** Create a record of type {!configuration}. *)

val validate_configuration :
  Ag_util.Validation.path -> configuration -> Ag_util.Validation.error option
  (** Validate a value of type {!configuration}. *)

val create_component_type_simple :
  component_type_simple_name: component_type_name ->
  ?component_type_simple_provide: (port_name * provide_arity) list ->
  ?component_type_simple_require: (port_name * require_arity) list ->
  ?component_type_simple_conflict: port_name list ->
  ?component_type_simple_consume: (resource_name * resource_consumption) list ->
  unit -> component_type_simple
  (** Create a record of type {!component_type_simple}. *)

val validate_component_type_simple :
  Ag_util.Validation.path -> component_type_simple -> Ag_util.Validation.error option
  (** Validate a value of type {!component_type_simple}. *)

val create_component_simple :
  component_simple_name: component_name ->
  component_simple_type: component_type_name ->
  component_simple_location: location_name ->
  unit -> component_simple
  (** Create a record of type {!component_simple}. *)

val validate_component_simple :
  Ag_util.Validation.path -> component_simple -> Ag_util.Validation.error option
  (** Validate a value of type {!component_simple}. *)

val create_binding_simple :
  binding_simple_port: port_name ->
  binding_simple_requirer: component_name ->
  binding_simple_provider: component_name ->
  unit -> binding_simple
  (** Create a record of type {!binding_simple}. *)

val validate_binding_simple :
  Ag_util.Validation.path -> binding_simple -> Ag_util.Validation.error option
  (** Validate a value of type {!binding_simple}. *)

