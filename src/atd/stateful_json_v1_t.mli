(* Auto-generated from "stateful_json_v1.atd" *)


(** Type definition for stateful syntax version. *)

(** Type definitions for naming. *)
type version = Json_versions_t.version

type state_name = string

type require_arity = int

type provide_arity = string

type port_name = string

type state = {
  state_name (*atd name *): state_name;
  state_initial (*atd initial *): bool;
  state_final (*atd final *): bool;
  state_provide (*atd provide *): (port_name * provide_arity) list;
  state_require (*atd require *): (port_name * require_arity) list;
  state_conflict (*atd conflict *): port_name list;
  state_successors (*atd successors *): state_name list
}

(** Type definitions for Universe. *)
type resource_name = string

type resource_consumption = int

type repository_name = string

type package_name = string

type package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): package_name list list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = repository list

type implementation_package = {
  implementation_package_repository (*atd repository *): repository_name;
  implementation_package_package (*atd package *): package_name
}

type implementation_packages = implementation_package list

type component_type_name = string

type component_type_stateful = {
  component_type_stateful_name (*atd name *): component_type_name;
  component_type_stateful_states (*atd states *): state list;
  component_type_stateful_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_type_simple = {
  component_type_simple_name (*atd name *): component_type_name;
  component_type_simple_provide (*atd provide *):
    (port_name * provide_arity) list;
  component_type_simple_require (*atd require *):
    (port_name * require_arity) list;
  component_type_simple_conflict (*atd conflict *): port_name list;
  component_type_simple_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type component_type = [
    `Component_type_simple of component_type_simple
  | `Component_type_stateful of component_type_stateful
]

type component_types = component_type list

(** Type definitions for Configuration. *)
type universe = {
  universe_version (*atd version *): version;
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * implementation_packages) list;
  universe_repositories (*atd repositories *): repositories;
  universe_port_hierarchy (*atd port_hierarchy *):
    (port_name * (port_name list)) list
}

type resource_provide_arity = int

type resources_provided = (resource_name * resource_provide_arity) list

type port_hierarchy = {
  port_hierarchy_port (*atd port *): port_name;
  port_hierarchy_subports (*atd subports *): port_name list
}

type packages = package list

type location_name = string

type location_cost = int

type location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component_name = string

type component_stateful = {
  component_stateful_name (*atd name *): component_name;
  component_type (*atd component_type_workaround *): component_type_name;
  component_stateful_state (*atd state *): state_name;
  component_stateful_location (*atd location *): location_name
}

type component_simple = {
  component_simple_name (*atd name *): component_name;
  component_type (*atd component_type_workaround *): component_type_name;
  component_simple_location (*atd location *): location_name
}

type component = [
    `Component_simple of component_simple
  | `Component_stateful of component_stateful
]

type binding_simple = {
  binding_simple_port (*atd port *): port_name;
  binding_simple_requirer (*atd requirer *): component_name;
  binding_simple_provider (*atd provider *): component_name
}

type binding_hierarchical = {
  binding_hierarchical_port_required (*atd port_required *): port_name;
  binding_hierarchical_port_provided (*atd port_provided *): port_name;
  binding_hierarchical_requirer (*atd requirer *): component_name;
  binding_hierarchical_provider (*atd provider *): component_name
}

type binding = [
    `Binding_hierarchical of binding_hierarchical
  | `Binding_simple of binding_simple
]

type configuration = {
  configuration_version (*atd version *): version;
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}
