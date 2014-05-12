(* Auto-generated from "json_v0.atd" *)


(** Type definitions for naming. *)

type component_type_name = string

type port_name = string

type component_name = string

type package_name = string

type repository_name = string

type location_name = string

(** Type definitions for Component Type. *)
type resource_name = string

type provide_arity =  InfiniteProvide | FiniteProvide of int 

type require_arity = int

type resource_consumption = int

type resource_provide_arity = int

type component_type = {
  component_type_name (*atd name *): component_type_name;
  component_type_provide (*atd provide *): (port_name * provide_arity) list;
  component_type_require (*atd require *): (port_name * require_arity) list;
  component_type_conflict (*atd conflict *): port_name list;
  component_type_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

(** Type definitions for Universe. *)
type component_types = component_type list

type package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): (package_name list) list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type packages = package list

type repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = repository list

type package_names = (repository_name * package_name) list

(** Type definitions for Configuration. *)
type universe = {
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * package_names) list;
  universe_repositories (*atd repositories *): repositories
}

type resources_provided = (resource_name * resource_provide_arity) list

type location_cost = int

type location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component = {
  component_name: component_name;
  component_type: component_type_name;
  component_location: location_name
}

type binding = {
  binding_port (*atd port *): port_name;
  binding_requirer (*atd requirer *): component_name;
  binding_provider (*atd provider *): component_name
}

type configuration = {
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}
