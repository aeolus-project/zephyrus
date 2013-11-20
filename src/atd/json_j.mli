(* Auto-generated from "json.atd" *)


(** Type definitions for naming. *)

type component_type_name = Json_t.component_type_name

type port_name = Json_t.port_name

type component_name = Json_t.component_name

type package_name = Json_t.package_name

type repository_name = Json_t.repository_name

type location_name = Json_t.location_name

(** Type definitions for Component Type. *)
type resource_name = Json_t.resource_name

type provide_arity = Json_t.provide_arity

type require_arity = Json_t.require_arity

type resource_consumption = Json_t.resource_consumption

type resource_provide_arity = Json_t.resource_provide_arity

type component_type = Json_t.component_type = {
  component_type_name (*atd name *): component_type_name;
  component_type_provide (*atd provide *): (port_name * provide_arity) list;
  component_type_require (*atd require *): (port_name * require_arity) list;
  component_type_conflict (*atd conflict *): port_name list;
  component_type_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

(** Type definitions for Universe. *)
type component_types = Json_t.component_types

type package = Json_t.package = {
  package_name (*atd name *): package_name;
  package_depend (*atd depend *): package_name list list;
  package_conflict (*atd conflict *): package_name list;
  package_consume (*atd consume *):
    (resource_name * resource_consumption) list
}

type packages = Json_t.packages

type repository = Json_t.repository = {
  repository_name (*atd name *): repository_name;
  repository_packages (*atd packages *): package list
}

type repositories = Json_t.repositories

type package_names = Json_t.package_names

(** Type definitions for Configuration. *)
type universe = Json_t.universe = {
  universe_component_types (*atd component_types *): component_types;
  universe_implementation (*atd implementation *):
    (component_type_name * package_names) list;
  universe_repositories (*atd repositories *): repositories
}

type resources_provided = Json_t.resources_provided

type location_cost = Json_t.location_cost

type location = Json_t.location = {
  location_name (*atd name *): location_name;
  location_provide_resources (*atd provide_resources *): resources_provided;
  location_repository (*atd repository *): repository_name;
  location_packages_installed (*atd packages_installed *): package_name list;
  location_cost (*atd cost *): location_cost
}

type component = Json_t.component = {
  component_name: component_name;
  component_type: component_type_name;
  component_location: location_name
}

type binding = Json_t.binding = {
  binding_port (*atd port *): port_name;
  binding_requirer (*atd requirer *): component_name;
  binding_provider (*atd provider *): component_name
}

(** Type definitions for Specification. *)
type configuration = Json_t.configuration = {
  configuration_locations (*atd locations *): location list;
  configuration_components (*atd components *): component list;
  configuration_bindings (*atd bindings *): binding list
}

type spec_variable_name = Json_t.spec_variable_name

type spec_const = Json_t.spec_const

type spec_local_element = Json_t.spec_local_element

type spec_local_expr = Json_t.spec_local_expr

type spec_op = Json_t.spec_op

type local_specification = Json_t.local_specification

type spec_repository_constraint = Json_t.spec_repository_constraint

type spec_repository_constraints = Json_t.spec_repository_constraints

type spec_resource_constraint = Json_t.spec_resource_constraint

type spec_resource_constraints = Json_t.spec_resource_constraints

type spec_element = Json_t.spec_element

type spec_expr = Json_t.spec_expr

type specification = Json_t.specification

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

val write_package_names :
  Bi_outbuf.t -> package_names -> unit
  (** Output a JSON value of type {!package_names}. *)

val string_of_package_names :
  ?len:int -> package_names -> string
  (** Serialize a value of type {!package_names}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_package_names :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> package_names
  (** Input JSON data of type {!package_names}. *)

val package_names_of_string :
  string -> package_names
  (** Deserialize JSON data of type {!package_names}. *)

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

val write_spec_variable_name :
  Bi_outbuf.t -> spec_variable_name -> unit
  (** Output a JSON value of type {!spec_variable_name}. *)

val string_of_spec_variable_name :
  ?len:int -> spec_variable_name -> string
  (** Serialize a value of type {!spec_variable_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_variable_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_variable_name
  (** Input JSON data of type {!spec_variable_name}. *)

val spec_variable_name_of_string :
  string -> spec_variable_name
  (** Deserialize JSON data of type {!spec_variable_name}. *)

val write_spec_const :
  Bi_outbuf.t -> spec_const -> unit
  (** Output a JSON value of type {!spec_const}. *)

val string_of_spec_const :
  ?len:int -> spec_const -> string
  (** Serialize a value of type {!spec_const}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_const :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_const
  (** Input JSON data of type {!spec_const}. *)

val spec_const_of_string :
  string -> spec_const
  (** Deserialize JSON data of type {!spec_const}. *)

val write_spec_local_element :
  Bi_outbuf.t -> spec_local_element -> unit
  (** Output a JSON value of type {!spec_local_element}. *)

val string_of_spec_local_element :
  ?len:int -> spec_local_element -> string
  (** Serialize a value of type {!spec_local_element}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_local_element :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_local_element
  (** Input JSON data of type {!spec_local_element}. *)

val spec_local_element_of_string :
  string -> spec_local_element
  (** Deserialize JSON data of type {!spec_local_element}. *)

val write_spec_local_expr :
  Bi_outbuf.t -> spec_local_expr -> unit
  (** Output a JSON value of type {!spec_local_expr}. *)

val string_of_spec_local_expr :
  ?len:int -> spec_local_expr -> string
  (** Serialize a value of type {!spec_local_expr}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_local_expr :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_local_expr
  (** Input JSON data of type {!spec_local_expr}. *)

val spec_local_expr_of_string :
  string -> spec_local_expr
  (** Deserialize JSON data of type {!spec_local_expr}. *)

val write_spec_op :
  Bi_outbuf.t -> spec_op -> unit
  (** Output a JSON value of type {!spec_op}. *)

val string_of_spec_op :
  ?len:int -> spec_op -> string
  (** Serialize a value of type {!spec_op}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_op :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_op
  (** Input JSON data of type {!spec_op}. *)

val spec_op_of_string :
  string -> spec_op
  (** Deserialize JSON data of type {!spec_op}. *)

val write_local_specification :
  Bi_outbuf.t -> local_specification -> unit
  (** Output a JSON value of type {!local_specification}. *)

val string_of_local_specification :
  ?len:int -> local_specification -> string
  (** Serialize a value of type {!local_specification}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_local_specification :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> local_specification
  (** Input JSON data of type {!local_specification}. *)

val local_specification_of_string :
  string -> local_specification
  (** Deserialize JSON data of type {!local_specification}. *)

val write_spec_repository_constraint :
  Bi_outbuf.t -> spec_repository_constraint -> unit
  (** Output a JSON value of type {!spec_repository_constraint}. *)

val string_of_spec_repository_constraint :
  ?len:int -> spec_repository_constraint -> string
  (** Serialize a value of type {!spec_repository_constraint}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_repository_constraint :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_repository_constraint
  (** Input JSON data of type {!spec_repository_constraint}. *)

val spec_repository_constraint_of_string :
  string -> spec_repository_constraint
  (** Deserialize JSON data of type {!spec_repository_constraint}. *)

val write_spec_repository_constraints :
  Bi_outbuf.t -> spec_repository_constraints -> unit
  (** Output a JSON value of type {!spec_repository_constraints}. *)

val string_of_spec_repository_constraints :
  ?len:int -> spec_repository_constraints -> string
  (** Serialize a value of type {!spec_repository_constraints}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_repository_constraints :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_repository_constraints
  (** Input JSON data of type {!spec_repository_constraints}. *)

val spec_repository_constraints_of_string :
  string -> spec_repository_constraints
  (** Deserialize JSON data of type {!spec_repository_constraints}. *)

val write_spec_resource_constraint :
  Bi_outbuf.t -> spec_resource_constraint -> unit
  (** Output a JSON value of type {!spec_resource_constraint}. *)

val string_of_spec_resource_constraint :
  ?len:int -> spec_resource_constraint -> string
  (** Serialize a value of type {!spec_resource_constraint}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_resource_constraint :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_resource_constraint
  (** Input JSON data of type {!spec_resource_constraint}. *)

val spec_resource_constraint_of_string :
  string -> spec_resource_constraint
  (** Deserialize JSON data of type {!spec_resource_constraint}. *)

val write_spec_resource_constraints :
  Bi_outbuf.t -> spec_resource_constraints -> unit
  (** Output a JSON value of type {!spec_resource_constraints}. *)

val string_of_spec_resource_constraints :
  ?len:int -> spec_resource_constraints -> string
  (** Serialize a value of type {!spec_resource_constraints}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_resource_constraints :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_resource_constraints
  (** Input JSON data of type {!spec_resource_constraints}. *)

val spec_resource_constraints_of_string :
  string -> spec_resource_constraints
  (** Deserialize JSON data of type {!spec_resource_constraints}. *)

val write_spec_element :
  Bi_outbuf.t -> spec_element -> unit
  (** Output a JSON value of type {!spec_element}. *)

val string_of_spec_element :
  ?len:int -> spec_element -> string
  (** Serialize a value of type {!spec_element}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_element :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_element
  (** Input JSON data of type {!spec_element}. *)

val spec_element_of_string :
  string -> spec_element
  (** Deserialize JSON data of type {!spec_element}. *)

val write_spec_expr :
  Bi_outbuf.t -> spec_expr -> unit
  (** Output a JSON value of type {!spec_expr}. *)

val string_of_spec_expr :
  ?len:int -> spec_expr -> string
  (** Serialize a value of type {!spec_expr}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_spec_expr :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> spec_expr
  (** Input JSON data of type {!spec_expr}. *)

val spec_expr_of_string :
  string -> spec_expr
  (** Deserialize JSON data of type {!spec_expr}. *)

val write_specification :
  Bi_outbuf.t -> specification -> unit
  (** Output a JSON value of type {!specification}. *)

val string_of_specification :
  ?len:int -> specification -> string
  (** Serialize a value of type {!specification}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_specification :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> specification
  (** Input JSON data of type {!specification}. *)

val specification_of_string :
  string -> specification
  (** Deserialize JSON data of type {!specification}. *)

