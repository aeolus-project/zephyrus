
open Aeolus_types_j
open Aeolus_types_output_facade
open Helpers

(* Global variables *)

type component_variable_key = component_type_name

let string_of_component_variable_key component_variable_key =
  Printf.sprintf "N%s" (string_of_component_type_name component_variable_key)

type binding_variable_key = port_name * component_type_name * component_type_name

let string_of_binding_variable_key =
  fun (port_name, providing_component_type_name, requiring_component_type_name) ->
  Printf.sprintf 
    "B(%s,%s,%s)"
    (string_of_port_name           port_name)
    (string_of_component_type_name providing_component_type_name)
    (string_of_component_type_name requiring_component_type_name)

(* Location variables *)

type location_component_variable_key = component_type_name * location_name

let string_of_location_component_variable_key =
  fun (component_type_name, location_name) ->
    Printf.sprintf 
      "Nc(%s,%s)"
      (string_of_component_type_name component_type_name)
      (string_of_location_name       location_name)

type location_package_variable_key = package_name * location_name

let string_of_location_package_variable_key =
  fun (package_name, location_name) ->
    Printf.sprintf 
      "Np(%s,%s)"
      (string_of_package_name  package_name)
      (string_of_location_name location_name)

type location_repository_variable_key = repository_name * location_name

let string_of_location_repository_variable_key =
  fun (repository_name, location_name) ->
    Printf.sprintf 
      "R(%s,%s)"
      (string_of_repository_name repository_name)
      (string_of_location_name   location_name)


type variable_key =
  (* Global variables *)
  | ComponentVariable of component_type_name
  | BindingVariable   of port_name * component_type_name * component_type_name
  (* Location variables *)
  | LocationComponentVariable  of component_type_name * location_name
  | LocationPackageVariable    of package_name        * location_name
  | LocationRepositoryVariable of repository_name     * location_name


let descr_of_variable_key variable_key =
  match variable_key with
  (* Global variables *)
  | ComponentVariable (component_type_name)                                                     -> "component variable"
  | BindingVariable   (port_name, providing_component_type_name, requiring_component_type_name) -> "binding variable"
  (* Location variables *)
  | LocationComponentVariable  (component_type_name, location_name) -> "location component variable" 
  | LocationPackageVariable    (package_name,        location_name) -> "location package variable"
  | LocationRepositoryVariable (repository_name,     location_name) -> "location repository variable"


let string_of_variable_key variable_key =
  match variable_key with
  (* Global variables *)
  | ComponentVariable (component_type_name) -> 
      string_of_component_variable_key (component_type_name)

  | BindingVariable   (port_name, providing_component_type_name, requiring_component_type_name) -> 
      string_of_binding_variable_key (port_name, providing_component_type_name, requiring_component_type_name)

  (* Location variables *)
  | LocationComponentVariable  (component_type_name, location_name) -> 
      string_of_location_component_variable_key  (component_type_name, location_name)

  | LocationPackageVariable    (package_name,        location_name) -> 
      string_of_location_package_variable_key (package_name, location_name)

  | LocationRepositoryVariable (repository_name,     location_name) -> 
      string_of_location_repository_variable_key (repository_name, location_name)
