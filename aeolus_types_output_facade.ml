
module Aeolus_types_plain_output =
  struct

    open Aeolus_types_j

    (* Here we use custom functions in order to avoid having quotes everywhere. *)
    let string_of_component_type_name component_type_name = component_type_name
    let string_of_port_name port_name = port_name
    let string_of_component_name component_name = component_name
    let string_of_package_name package_name = package_name
    let string_of_repository_name repository_name = repository_name
    let string_of_location_name location_name = location_name
    let string_of_resource_name resource_name = resource_name
    let string_of_provide_arity provide_arity = Printf.sprintf "%d" provide_arity
    let string_of_require_arity require_arity = Printf.sprintf "%d" require_arity
    let string_of_resource_consumption resource_consumption = Printf.sprintf "%d" resource_consumption
    let string_of_resource_provide_arity resource_provide_arity = Printf.sprintf "%d" resource_provide_arity

    let string_of_spec_variable_name spec_variable_name = Printf.sprintf "%s" spec_variable_name
    let string_of_spec_const spec_const = Printf.sprintf "%d" spec_const


    (* From this point on we use the ATDgen string_of_* functions *)
    let string_of_component_type component_type =  string_of_component_type
    let string_of_component_types component_types = string_of_component_types
    let string_of_package package = string_of_package
    let string_of_packages packages = string_of_packages
    let string_of_repository repository = string_of_repository
    let string_of_repositories repositories = string_of_repositories
    let string_of_package_names package_names = string_of_package_names
    let string_of_universe universe = string_of_universe
    let string_of_resources_provided resources_provided = string_of_resources_provided
    let string_of_location location = string_of_location
    let string_of_component component = string_of_component
    let string_of_binding binding = string_of_binding
    let string_of_configuration configuration = string_of_configuration

  end