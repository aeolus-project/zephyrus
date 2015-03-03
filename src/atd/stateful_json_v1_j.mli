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

val write_version :
  Bi_outbuf.t -> version -> unit
  (** Output a JSON value of type {!version}. *)

val string_of_version :
  ?len:int -> version -> string
  (** Serialize a value of type {!version}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_version :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> version
  (** Input JSON data of type {!version}. *)

val version_of_string :
  string -> version
  (** Deserialize JSON data of type {!version}. *)

val write_state_name :
  Bi_outbuf.t -> state_name -> unit
  (** Output a JSON value of type {!state_name}. *)

val string_of_state_name :
  ?len:int -> state_name -> string
  (** Serialize a value of type {!state_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_state_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> state_name
  (** Input JSON data of type {!state_name}. *)

val state_name_of_string :
  string -> state_name
  (** Deserialize JSON data of type {!state_name}. *)

val write_require_arity :
  Bi_outbuf.t -> require_arity -> unit
  (** Output a JSON value of type {!require_arity}. *)

val string_of_require_arity :
  ?len:int -> require_arity -> string
  (** Serialize a value of type {!require_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_require_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> require_arity
  (** Input JSON data of type {!require_arity}. *)

val require_arity_of_string :
  string -> require_arity
  (** Deserialize JSON data of type {!require_arity}. *)

val write_provide_arity :
  Bi_outbuf.t -> provide_arity -> unit
  (** Output a JSON value of type {!provide_arity}. *)

val string_of_provide_arity :
  ?len:int -> provide_arity -> string
  (** Serialize a value of type {!provide_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_provide_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> provide_arity
  (** Input JSON data of type {!provide_arity}. *)

val provide_arity_of_string :
  string -> provide_arity
  (** Deserialize JSON data of type {!provide_arity}. *)

val write_port_name :
  Bi_outbuf.t -> port_name -> unit
  (** Output a JSON value of type {!port_name}. *)

val string_of_port_name :
  ?len:int -> port_name -> string
  (** Serialize a value of type {!port_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_port_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> port_name
  (** Input JSON data of type {!port_name}. *)

val port_name_of_string :
  string -> port_name
  (** Deserialize JSON data of type {!port_name}. *)

val write_state :
  Bi_outbuf.t -> state -> unit
  (** Output a JSON value of type {!state}. *)

val string_of_state :
  ?len:int -> state -> string
  (** Serialize a value of type {!state}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_state :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> state
  (** Input JSON data of type {!state}. *)

val state_of_string :
  string -> state
  (** Deserialize JSON data of type {!state}. *)

val write_resource_name :
  Bi_outbuf.t -> resource_name -> unit
  (** Output a JSON value of type {!resource_name}. *)

val string_of_resource_name :
  ?len:int -> resource_name -> string
  (** Serialize a value of type {!resource_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_name
  (** Input JSON data of type {!resource_name}. *)

val resource_name_of_string :
  string -> resource_name
  (** Deserialize JSON data of type {!resource_name}. *)

val write_resource_consumption :
  Bi_outbuf.t -> resource_consumption -> unit
  (** Output a JSON value of type {!resource_consumption}. *)

val string_of_resource_consumption :
  ?len:int -> resource_consumption -> string
  (** Serialize a value of type {!resource_consumption}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_consumption :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_consumption
  (** Input JSON data of type {!resource_consumption}. *)

val resource_consumption_of_string :
  string -> resource_consumption
  (** Deserialize JSON data of type {!resource_consumption}. *)

val write_repository_name :
  Bi_outbuf.t -> repository_name -> unit
  (** Output a JSON value of type {!repository_name}. *)

val string_of_repository_name :
  ?len:int -> repository_name -> string
  (** Serialize a value of type {!repository_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository_name
  (** Input JSON data of type {!repository_name}. *)

val repository_name_of_string :
  string -> repository_name
  (** Deserialize JSON data of type {!repository_name}. *)

val write_package_name :
  Bi_outbuf.t -> package_name -> unit
  (** Output a JSON value of type {!package_name}. *)

val string_of_package_name :
  ?len:int -> package_name -> string
  (** Serialize a value of type {!package_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_package_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> package_name
  (** Input JSON data of type {!package_name}. *)

val package_name_of_string :
  string -> package_name
  (** Deserialize JSON data of type {!package_name}. *)

val write_package :
  Bi_outbuf.t -> package -> unit
  (** Output a JSON value of type {!package}. *)

val string_of_package :
  ?len:int -> package -> string
  (** Serialize a value of type {!package}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_package :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> package
  (** Input JSON data of type {!package}. *)

val package_of_string :
  string -> package
  (** Deserialize JSON data of type {!package}. *)

val write_repository :
  Bi_outbuf.t -> repository -> unit
  (** Output a JSON value of type {!repository}. *)

val string_of_repository :
  ?len:int -> repository -> string
  (** Serialize a value of type {!repository}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository
  (** Input JSON data of type {!repository}. *)

val repository_of_string :
  string -> repository
  (** Deserialize JSON data of type {!repository}. *)

val write_repositories :
  Bi_outbuf.t -> repositories -> unit
  (** Output a JSON value of type {!repositories}. *)

val string_of_repositories :
  ?len:int -> repositories -> string
  (** Serialize a value of type {!repositories}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repositories :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repositories
  (** Input JSON data of type {!repositories}. *)

val repositories_of_string :
  string -> repositories
  (** Deserialize JSON data of type {!repositories}. *)

val write_implementation_package :
  Bi_outbuf.t -> implementation_package -> unit
  (** Output a JSON value of type {!implementation_package}. *)

val string_of_implementation_package :
  ?len:int -> implementation_package -> string
  (** Serialize a value of type {!implementation_package}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_implementation_package :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> implementation_package
  (** Input JSON data of type {!implementation_package}. *)

val implementation_package_of_string :
  string -> implementation_package
  (** Deserialize JSON data of type {!implementation_package}. *)

val write_implementation_packages :
  Bi_outbuf.t -> implementation_packages -> unit
  (** Output a JSON value of type {!implementation_packages}. *)

val string_of_implementation_packages :
  ?len:int -> implementation_packages -> string
  (** Serialize a value of type {!implementation_packages}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_implementation_packages :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> implementation_packages
  (** Input JSON data of type {!implementation_packages}. *)

val implementation_packages_of_string :
  string -> implementation_packages
  (** Deserialize JSON data of type {!implementation_packages}. *)

val write_component_type_name :
  Bi_outbuf.t -> component_type_name -> unit
  (** Output a JSON value of type {!component_type_name}. *)

val string_of_component_type_name :
  ?len:int -> component_type_name -> string
  (** Serialize a value of type {!component_type_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_type_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_type_name
  (** Input JSON data of type {!component_type_name}. *)

val component_type_name_of_string :
  string -> component_type_name
  (** Deserialize JSON data of type {!component_type_name}. *)

val write_component_type_stateful :
  Bi_outbuf.t -> component_type_stateful -> unit
  (** Output a JSON value of type {!component_type_stateful}. *)

val string_of_component_type_stateful :
  ?len:int -> component_type_stateful -> string
  (** Serialize a value of type {!component_type_stateful}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_type_stateful :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_type_stateful
  (** Input JSON data of type {!component_type_stateful}. *)

val component_type_stateful_of_string :
  string -> component_type_stateful
  (** Deserialize JSON data of type {!component_type_stateful}. *)

val write_component_type_simple :
  Bi_outbuf.t -> component_type_simple -> unit
  (** Output a JSON value of type {!component_type_simple}. *)

val string_of_component_type_simple :
  ?len:int -> component_type_simple -> string
  (** Serialize a value of type {!component_type_simple}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_type_simple :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_type_simple
  (** Input JSON data of type {!component_type_simple}. *)

val component_type_simple_of_string :
  string -> component_type_simple
  (** Deserialize JSON data of type {!component_type_simple}. *)

val write_component_type :
  Bi_outbuf.t -> component_type -> unit
  (** Output a JSON value of type {!component_type}. *)

val string_of_component_type :
  ?len:int -> component_type -> string
  (** Serialize a value of type {!component_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_type
  (** Input JSON data of type {!component_type}. *)

val component_type_of_string :
  string -> component_type
  (** Deserialize JSON data of type {!component_type}. *)

val write_component_types :
  Bi_outbuf.t -> component_types -> unit
  (** Output a JSON value of type {!component_types}. *)

val string_of_component_types :
  ?len:int -> component_types -> string
  (** Serialize a value of type {!component_types}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_types :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_types
  (** Input JSON data of type {!component_types}. *)

val component_types_of_string :
  string -> component_types
  (** Deserialize JSON data of type {!component_types}. *)

val write_universe :
  Bi_outbuf.t -> universe -> unit
  (** Output a JSON value of type {!universe}. *)

val string_of_universe :
  ?len:int -> universe -> string
  (** Serialize a value of type {!universe}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_universe :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> universe
  (** Input JSON data of type {!universe}. *)

val universe_of_string :
  string -> universe
  (** Deserialize JSON data of type {!universe}. *)

val write_resource_provide_arity :
  Bi_outbuf.t -> resource_provide_arity -> unit
  (** Output a JSON value of type {!resource_provide_arity}. *)

val string_of_resource_provide_arity :
  ?len:int -> resource_provide_arity -> string
  (** Serialize a value of type {!resource_provide_arity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_provide_arity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_provide_arity
  (** Input JSON data of type {!resource_provide_arity}. *)

val resource_provide_arity_of_string :
  string -> resource_provide_arity
  (** Deserialize JSON data of type {!resource_provide_arity}. *)

val write_resources_provided :
  Bi_outbuf.t -> resources_provided -> unit
  (** Output a JSON value of type {!resources_provided}. *)

val string_of_resources_provided :
  ?len:int -> resources_provided -> string
  (** Serialize a value of type {!resources_provided}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resources_provided :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resources_provided
  (** Input JSON data of type {!resources_provided}. *)

val resources_provided_of_string :
  string -> resources_provided
  (** Deserialize JSON data of type {!resources_provided}. *)

val write_port_hierarchy :
  Bi_outbuf.t -> port_hierarchy -> unit
  (** Output a JSON value of type {!port_hierarchy}. *)

val string_of_port_hierarchy :
  ?len:int -> port_hierarchy -> string
  (** Serialize a value of type {!port_hierarchy}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_port_hierarchy :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> port_hierarchy
  (** Input JSON data of type {!port_hierarchy}. *)

val port_hierarchy_of_string :
  string -> port_hierarchy
  (** Deserialize JSON data of type {!port_hierarchy}. *)

val write_packages :
  Bi_outbuf.t -> packages -> unit
  (** Output a JSON value of type {!packages}. *)

val string_of_packages :
  ?len:int -> packages -> string
  (** Serialize a value of type {!packages}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_packages :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> packages
  (** Input JSON data of type {!packages}. *)

val packages_of_string :
  string -> packages
  (** Deserialize JSON data of type {!packages}. *)

val write_location_name :
  Bi_outbuf.t -> location_name -> unit
  (** Output a JSON value of type {!location_name}. *)

val string_of_location_name :
  ?len:int -> location_name -> string
  (** Serialize a value of type {!location_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_location_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> location_name
  (** Input JSON data of type {!location_name}. *)

val location_name_of_string :
  string -> location_name
  (** Deserialize JSON data of type {!location_name}. *)

val write_location_cost :
  Bi_outbuf.t -> location_cost -> unit
  (** Output a JSON value of type {!location_cost}. *)

val string_of_location_cost :
  ?len:int -> location_cost -> string
  (** Serialize a value of type {!location_cost}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_location_cost :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> location_cost
  (** Input JSON data of type {!location_cost}. *)

val location_cost_of_string :
  string -> location_cost
  (** Deserialize JSON data of type {!location_cost}. *)

val write_location :
  Bi_outbuf.t -> location -> unit
  (** Output a JSON value of type {!location}. *)

val string_of_location :
  ?len:int -> location -> string
  (** Serialize a value of type {!location}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_location :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> location
  (** Input JSON data of type {!location}. *)

val location_of_string :
  string -> location
  (** Deserialize JSON data of type {!location}. *)

val write_component_name :
  Bi_outbuf.t -> component_name -> unit
  (** Output a JSON value of type {!component_name}. *)

val string_of_component_name :
  ?len:int -> component_name -> string
  (** Serialize a value of type {!component_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_name
  (** Input JSON data of type {!component_name}. *)

val component_name_of_string :
  string -> component_name
  (** Deserialize JSON data of type {!component_name}. *)

val write_component_stateful :
  Bi_outbuf.t -> component_stateful -> unit
  (** Output a JSON value of type {!component_stateful}. *)

val string_of_component_stateful :
  ?len:int -> component_stateful -> string
  (** Serialize a value of type {!component_stateful}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_stateful :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_stateful
  (** Input JSON data of type {!component_stateful}. *)

val component_stateful_of_string :
  string -> component_stateful
  (** Deserialize JSON data of type {!component_stateful}. *)

val write_component_simple :
  Bi_outbuf.t -> component_simple -> unit
  (** Output a JSON value of type {!component_simple}. *)

val string_of_component_simple :
  ?len:int -> component_simple -> string
  (** Serialize a value of type {!component_simple}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component_simple :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component_simple
  (** Input JSON data of type {!component_simple}. *)

val component_simple_of_string :
  string -> component_simple
  (** Deserialize JSON data of type {!component_simple}. *)

val write_component :
  Bi_outbuf.t -> component -> unit
  (** Output a JSON value of type {!component}. *)

val string_of_component :
  ?len:int -> component -> string
  (** Serialize a value of type {!component}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_component :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> component
  (** Input JSON data of type {!component}. *)

val component_of_string :
  string -> component
  (** Deserialize JSON data of type {!component}. *)

val write_binding_simple :
  Bi_outbuf.t -> binding_simple -> unit
  (** Output a JSON value of type {!binding_simple}. *)

val string_of_binding_simple :
  ?len:int -> binding_simple -> string
  (** Serialize a value of type {!binding_simple}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_binding_simple :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> binding_simple
  (** Input JSON data of type {!binding_simple}. *)

val binding_simple_of_string :
  string -> binding_simple
  (** Deserialize JSON data of type {!binding_simple}. *)

val write_binding_hierarchical :
  Bi_outbuf.t -> binding_hierarchical -> unit
  (** Output a JSON value of type {!binding_hierarchical}. *)

val string_of_binding_hierarchical :
  ?len:int -> binding_hierarchical -> string
  (** Serialize a value of type {!binding_hierarchical}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_binding_hierarchical :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> binding_hierarchical
  (** Input JSON data of type {!binding_hierarchical}. *)

val binding_hierarchical_of_string :
  string -> binding_hierarchical
  (** Deserialize JSON data of type {!binding_hierarchical}. *)

val write_binding :
  Bi_outbuf.t -> binding -> unit
  (** Output a JSON value of type {!binding}. *)

val string_of_binding :
  ?len:int -> binding -> string
  (** Serialize a value of type {!binding}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_binding :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> binding
  (** Input JSON data of type {!binding}. *)

val binding_of_string :
  string -> binding
  (** Deserialize JSON data of type {!binding}. *)

val write_configuration :
  Bi_outbuf.t -> configuration -> unit
  (** Output a JSON value of type {!configuration}. *)

val string_of_configuration :
  ?len:int -> configuration -> string
  (** Serialize a value of type {!configuration}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_configuration :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> configuration
  (** Input JSON data of type {!configuration}. *)

val configuration_of_string :
  string -> configuration
  (** Deserialize JSON data of type {!configuration}. *)

