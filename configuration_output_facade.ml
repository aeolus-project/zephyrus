
open Helpers

(*
module type CONFIGURATION_TYPES =
  sig
    type component_type_name
    type port_name
    type component_name
    type package_name
    type repository_name
    type location_name
    type resource_name
    type resource_provide_arity
    type location
    type component
    type binding
    type configuration
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

    let string_of_location_repository (location_repository : repository_name) : string =
      Printf.sprintf
        " > Repository : %s"
        (string_of_repository_name location_repository)

    let string_of_location_packages (location_packages_installed : package_name list) : string =
      Printf.sprintf
        " > Packages installed : %s"
        (String.concat ", " (List.map string_of_package_name location_packages_installed))

    let string_of_location_provide_resources (location_provide_resources : (resource_name * resource_provide_arity) list) : string =
      Printf.sprintf
        " > Resources provided : %s"
        (String.concat ""
          (List.map (fun (resource_name, resource_provide_arity) -> 
            Printf.sprintf
            "\n   + %s : %s"
            (string_of_resource_name          resource_name)
            (string_of_resource_provide_arity resource_provide_arity)
          ) location_provide_resources)
        )

    let string_of_location_components (components : component list) : string =
      Printf.sprintf
        " > Components installed : %s"
        (String.concat ""
          (List.map (fun component -> 
            Printf.sprintf
              "\n   + %s"
              (string_of_component_name component.component_name)
          ) components)
        )

    let string_of_location (location : location) : string =
      Printf.sprintf
        "=== Location [%s] ===\n%s\n%s\n%s"
        (string_of_location_name              location.location_name)
        (string_of_location_repository        location.location_repository)
        (string_of_location_packages          location.location_packages_installed)
        (string_of_location_provide_resources location.location_provide_resources)

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

        (lines_of_strings 
          (List.map (fun location -> 
            Printf.sprintf
              "%s\n%s\n\n"
              (string_of_location location)
              (string_of_location_components 
                (List.filter (fun component -> 
                  component.component_location = location.location_name
                ) configuration.configuration_components))
          ) configuration.configuration_locations)
        )
        
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