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

val validate_version :
  Ag_util.Validation.path -> version -> Ag_util.Validation.error option
  (** Validate a value of type {!version}. *)

val validate_component_type_name :
  Ag_util.Validation.path -> component_type_name -> Ag_util.Validation.error option
  (** Validate a value of type {!component_type_name}. *)

val validate_state_name :
  Ag_util.Validation.path -> state_name -> Ag_util.Validation.error option
  (** Validate a value of type {!state_name}. *)

val validate_port_name :
  Ag_util.Validation.path -> port_name -> Ag_util.Validation.error option
  (** Validate a value of type {!port_name}. *)

val validate_component_name :
  Ag_util.Validation.path -> component_name -> Ag_util.Validation.error option
  (** Validate a value of type {!component_name}. *)

val validate_package_name :
  Ag_util.Validation.path -> package_name -> Ag_util.Validation.error option
  (** Validate a value of type {!package_name}. *)

val validate_repository_name :
  Ag_util.Validation.path -> repository_name -> Ag_util.Validation.error option
  (** Validate a value of type {!repository_name}. *)

val validate_location_name :
  Ag_util.Validation.path -> location_name -> Ag_util.Validation.error option
  (** Validate a value of type {!location_name}. *)

val validate_resource_name :
  Ag_util.Validation.path -> resource_name -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_name}. *)

val validate_provide_arity :
  Ag_util.Validation.path -> provide_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!provide_arity}. *)

val validate_require_arity :
  Ag_util.Validation.path -> require_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!require_arity}. *)

val validate_resource_consumption :
  Ag_util.Validation.path -> resource_consumption -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_consumption}. *)

val validate_resource_provide_arity :
  Ag_util.Validation.path -> resource_provide_arity -> Ag_util.Validation.error option
  (** Validate a value of type {!resource_provide_arity}. *)

val create_state :
  state_name: state_name ->
  ?state_initial: bool ->
  ?state_provide: (port_name * provide_arity) list ->
  ?state_require: (port_name * require_arity) list ->
  ?state_conflict: port_name list ->
  ?state_successors: state_name list ->
  unit -> state
  (** Create a record of type {!state}. *)

val validate_state :
  Ag_util.Validation.path -> state -> Ag_util.Validation.error option
  (** Validate a value of type {!state}. *)

val create_component_type :
  component_type_name: component_type_name ->
  component_type_states: state list ->
  ?component_type_consume: (resource_name * resource_consumption) list ->
  unit -> component_type
  (** Create a record of type {!component_type}. *)

val validate_component_type :
  Ag_util.Validation.path -> component_type -> Ag_util.Validation.error option
  (** Validate a value of type {!component_type}. *)

val validate_component_types :
  Ag_util.Validation.path -> component_types -> Ag_util.Validation.error option
  (** Validate a value of type {!component_types}. *)

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

val validate_packages :
  Ag_util.Validation.path -> packages -> Ag_util.Validation.error option
  (** Validate a value of type {!packages}. *)

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

val create_universe :
  universe_version: version ->
  ?universe_component_types: component_types ->
  ?universe_implementation: (component_type_name * implementation_packages) list ->
  ?universe_repositories: repositories ->
  unit -> universe
  (** Create a record of type {!universe}. *)

val validate_universe :
  Ag_util.Validation.path -> universe -> Ag_util.Validation.error option
  (** Validate a value of type {!universe}. *)

val validate_resources_provided :
  Ag_util.Validation.path -> resources_provided -> Ag_util.Validation.error option
  (** Validate a value of type {!resources_provided}. *)

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

val create_component :
  component_name: component_name ->
  component_type: component_type_name ->
  component_state: state_name ->
  component_location: location_name ->
  unit -> component
  (** Create a record of type {!component}. *)

val validate_component :
  Ag_util.Validation.path -> component -> Ag_util.Validation.error option
  (** Validate a value of type {!component}. *)

val create_binding :
  binding_port: port_name ->
  binding_requirer: component_name ->
  binding_provider: component_name ->
  unit -> binding
  (** Create a record of type {!binding}. *)

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

