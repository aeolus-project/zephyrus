
open Aeolus_types_t

(* Global variables *)

type component_variable_key = component_type_name
val string_of_component_variable_key : component_variable_key -> string

type binding_variable_key = port_name * component_type_name * component_type_name
val string_of_binding_variable_key : binding_variable_key -> string


(* Location variables *)

type location_component_variable_key = component_type_name * location_name
val string_of_location_component_variable_key : location_component_variable_key -> string

type location_package_variable_key = package_name * location_name
val string_of_location_package_variable_key : location_package_variable_key -> string

type location_repository_variable_key = repository_name * location_name
val string_of_location_repository_variable_key : location_repository_variable_key -> string


type variable_key =
  (* Global variables *)
  | ComponentVariable of component_type_name
  | BindingVariable   of port_name * component_type_name * component_type_name
  (* Location variables *)
  | LocationComponentVariable  of component_type_name * location_name
  | LocationPackageVariable    of package_name        * location_name
  | LocationRepositoryVariable of repository_name     * location_name

val descr_of_variable_key  : variable_key -> string
val string_of_variable_key : variable_key -> string
