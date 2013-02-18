
open Helpers

(*
module type CONFIGURATION_TYPES =
  sig
    type resource_name
    type resource_type
    type resource
    type typing_entry
    type binding
    type typed_system
  end

module My_resource_types : CONFIGURATION_TYPES = Configuration_generation

*)
module C = Aeolus_types_j

module type CONFIGURATION_OUTPUT =
  (* functor (T : CONFIGURATION_TYPES) -> *)
  sig
    val string_of_component_type_name : C.component_type_name -> string
    val string_of_port_name : C.port_name -> string
    val string_of_component_name : C.component_name -> string
    val string_of_package_name : C.package_name -> string
    val string_of_repository_name : C.repository_name -> string
    val string_of_location_name : C.location_name -> string
    val string_of_resource_name : C.resource_name -> string
    val string_of_resource_provide_arity : C.resource_provide_arity -> string
    val string_of_location : C.location -> string
    val string_of_component : C.component -> string
    val string_of_binding : C.binding -> string
    val string_of_configuration : C.configuration -> string
  end

module Simple_configuration_output : CONFIGURATION_OUTPUT =
  struct

    open Aeolus_types_j

    let string_of_component_type_name (component_type_name : component_type_name) : string =
      component_type_name

    let string_of_port_name (port_name : port_name) : string =
      port_name

    let string_of_component_name (component_name : component_name) : string =
      component_name

    let string_of_package_name (package_name : package_name) : string =
      package_name

    let string_of_repository_name (repository_name : repository_name) : string =
      repository_name

    let string_of_location_name (location_name : location_name) : string =
      location_name

    let string_of_resource_name (resource_name : resource_name) : string =
      resource_name

    let string_of_resource_provide_arity (resource_provide_arity : resource_provide_arity) : string =
      Printf.sprintf "%d" resource_provide_arity

    let string_of_location (location : location) : string =
      Printf.sprintf
        "=== Location [%s] ===\n > Repository : %s\n > Packages installed : %s\n > Resources provided : \n%s"
        (string_of_location_name   location.location_name)
        (string_of_repository_name location.location_repository)
        (String.concat ", " (List.map string_of_package_name location.location_packages_installed))
        (lines_of_strings (List.map (fun (resource_name, resource_provide_arity) -> 
           Printf.sprintf
           "   + %s : %s"
           (string_of_resource_name          resource_name)
           (string_of_resource_provide_arity resource_provide_arity)
        ) location.location_provide_resources))

    let string_of_component (component : component) : string =
      Printf.sprintf
        " > Component [%s] : <type : %s; location : %s>"
        (string_of_component_name      component.component_name)
        (string_of_component_type_name component.component_type)
        (string_of_location_name       component.component_location)

    let string_of_binding (binding : binding) : string = 
      Printf.sprintf
        " > Binding over port [%s] :  ([%s])~~C o~~~~~C o~~([%s])"
        (string_of_port_name      binding.binding_port)
        (string_of_component_name binding.binding_requirer)
        (string_of_component_name binding.binding_provider)

    let string_of_configuration (configuration : configuration) : string =
      Printf.sprintf
        "%s\n\n=== Components ===\n%s\n\n=== Bindings ===\n%s"
        (lines_of_strings (List.map string_of_location  configuration.configuration_locations))
        (lines_of_strings (List.map string_of_component configuration.configuration_components))
        (lines_of_strings (List.map string_of_binding   configuration.configuration_bindings))

  end


module JSON_configuration_output : CONFIGURATION_OUTPUT = 
  struct 

   open Aeolus_types_j

    let string_of_component_type_name (component_type_name : component_type_name) = string_of_component_type_name component_type_name
    let string_of_port_name (port_name : port_name) = string_of_port_name port_name
    let string_of_component_name (component_name : component_name) = string_of_component_name component_name
    let string_of_package_name (package_name : package_name) = string_of_package_name package_name
    let string_of_repository_name (repository_name : repository_name) = string_of_repository_name repository_name
    let string_of_location_name (location_name : location_name) = string_of_location_name location_name
    let string_of_resource_name (resource_name : resource_name) = string_of_resource_name resource_name
    let string_of_resource_provide_arity (resource_provide_arity : resource_provide_arity) = string_of_resource_provide_arity resource_provide_arity
    let string_of_location (location : location) = string_of_location location
    let string_of_component (component : component) = string_of_component component
    let string_of_binding (binding : binding) = string_of_binding binding
    let string_of_configuration (configuration : configuration) = string_of_configuration configuration

  end