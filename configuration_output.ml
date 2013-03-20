(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)


open Helpers
open Typing_context
open Aeolus_types_t

module type CONFIGURATION_OUTPUT =
  sig
    val string_of_component_type_name    : component_type_name    -> string
    val string_of_port_name              : port_name              -> string
    val string_of_component_name         : component_name         -> string
    val string_of_package_name           : package_name           -> string
    val string_of_repository_name        : repository_name        -> string
    val string_of_location_name          : location_name          -> string
    val string_of_resource_name          : resource_name          -> string
    val string_of_resource_provide_arity : resource_provide_arity -> string
    val string_of_location               : location               -> string
    val string_of_component              : component              -> string
    val string_of_binding                : binding                -> string
    val string_of_configuration          : configuration          -> string
  end

module Simple : CONFIGURATION_OUTPUT =
  struct

    let string_of_component_type_name    component_type_name    = component_type_name
    let string_of_port_name              port_name              = port_name
    let string_of_component_name         component_name         = component_name
    let string_of_package_name           package_name           = package_name
    let string_of_repository_name        repository_name        = repository_name
    let string_of_location_name          location_name          = location_name
    let string_of_resource_name          resource_name          = resource_name
    let string_of_resource_provide_arity resource_provide_arity = Printf.sprintf "%d" resource_provide_arity

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
              (string_of_location_components (get_location_components configuration location.location_name))
          ) configuration.configuration_locations)
        )
        
        (lines_of_strings (List.map string_of_component configuration.configuration_components))
        (lines_of_strings (List.map string_of_binding   configuration.configuration_bindings))

  end


module JSON : CONFIGURATION_OUTPUT = 
  struct 

    open Aeolus_types_j

    let string_of_component_type_name    component_type_name    = string_of_component_type_name    component_type_name
    let string_of_port_name              port_name              = string_of_port_name              port_name
    let string_of_component_name         component_name         = string_of_component_name         component_name
    let string_of_package_name           package_name           = string_of_package_name           package_name
    let string_of_repository_name        repository_name        = string_of_repository_name        repository_name
    let string_of_location_name          location_name          = string_of_location_name          location_name
    let string_of_resource_name          resource_name          = string_of_resource_name          resource_name
    let string_of_resource_provide_arity resource_provide_arity = string_of_resource_provide_arity resource_provide_arity
    let string_of_location               location               = string_of_location               location
    let string_of_component              component              = string_of_component              component
    let string_of_binding                binding                = string_of_binding                binding
    let string_of_configuration          configuration          = string_of_configuration          configuration

  end

module Graphviz_configuration_output = 
  struct 

    let string_of_component_type_name    = Simple.string_of_component_type_name
    let string_of_port_name              = Simple.string_of_port_name
    let string_of_component_name         = Simple.string_of_component_name
    let string_of_package_name           = Simple.string_of_package_name
    let string_of_repository_name        = Simple.string_of_repository_name
    let string_of_location_name          = Simple.string_of_location_name
    let string_of_resource_name          = Simple.string_of_resource_name
    let string_of_resource_provide_arity = Simple.string_of_resource_provide_arity

    let id_of_name name = 
      BatString.filter_map (fun c ->
        match c with 
        | 'a'..'z' -> Some c
        | 'A'..'Z' -> Some c
        | '0'..'9' -> Some c
        | '@'
        | ' '      -> None
        | _        -> Some '_'
      ) name
    
    let component_id component_name =
      Printf.sprintf "component_%s" (id_of_name component_name)

    let location_id location_name =
      Printf.sprintf "location_%s" (id_of_name location_name)

    let string_of_component (component : component) = 
      let id    = component_id component.component_name
      and label = Printf.sprintf "%s" component.component_name
      in
      Printf.sprintf
        "    %s [shape=box,label=\"%s\"];"
        id
        label
    
    let string_of_location (configuration : configuration) (location : location) =
      let id    = location_id location.location_name
      and label = Printf.sprintf "%s" location.location_name
      in
      let location_component_strings =
        List.map string_of_component (get_location_components configuration location.location_name)
      in
      Printf.sprintf
        "  subgraph cluster_%s {\n    label = \"%s\";\n%s\n  }"
        id
        label
        (lines_of_strings location_component_strings)

    let string_of_binding (binding : binding) = 
      let requirer_id  = component_id binding.binding_requirer
      and provider_id  = component_id binding.binding_provider
      (* and label        = binding.binding_port *)
      in
      (*
      Printf.sprintf
        "  %s -> %s [label=\"%s\"];"
        requirer_id
        provider_id
        label
      *)
      Printf.sprintf
        "  %s -> %s;"
        requirer_id
        provider_id

  end

module Graphviz_configuration_output_aaa = 
  struct 

    let string_of_component_type_name    = Simple.string_of_component_type_name
    let string_of_port_name              = Simple.string_of_port_name
    let string_of_component_name         = Simple.string_of_component_name
    let string_of_package_name           = Simple.string_of_package_name
    let string_of_repository_name        = Simple.string_of_repository_name
    let string_of_location_name          = Simple.string_of_location_name
    let string_of_resource_name          = Simple.string_of_resource_name
    let string_of_resource_provide_arity = Simple.string_of_resource_provide_arity

    let id_of_name name = 
      BatString.filter_map (fun c ->
        match c with 
        | 'a'..'z' -> Some c
        | 'A'..'Z' -> Some c
        | '0'..'9' -> Some c
        | '@'
        | ' '      -> None
        | _        -> Some '_'
      ) name
    
    let component_id component_name =
      Printf.sprintf "component_%s" (id_of_name component_name)

    let location_id location_name =
      Printf.sprintf "location_%s" (id_of_name location_name)

    let package_at_location_id location_name package_name =
      Printf.sprintf "%s_package_%s" (location_id location_name) (id_of_name package_name)

    let port_id port_name =
      Printf.sprintf "port_%s" (id_of_name port_name)

    let required_port_id port_name =
      Printf.sprintf "required_%s" (port_id port_name)

    let provided_port_id port_name =
      Printf.sprintf "provided_%s" (port_id port_name)

    (* "<img src=\"tux.jpg\" scale=\"true\"/>" *)

    let string_of_component (universe : universe) (component : component) = 
      let component_type = get_component_type universe component.component_type
      in

      let required_ports_table =
        let required_ports_strings =
          List.map (fun (port_name, arity) -> Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (required_port_id port_name) (BatString.lchop port_name)) component_type.component_type_require
        in
        if required_ports_strings = []
        then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"red\">%s</table>" (lines_of_strings required_ports_strings)

      and provided_ports_table =
        let provided_ports_strings =
          List.map (fun (port_name, arity) -> Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (provided_port_id port_name) (BatString.lchop port_name)) component_type.component_type_provide
        in
        if provided_ports_strings = []
        then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"green\">%s</table>" (lines_of_strings provided_ports_strings)

      in

      let id    = component_id component.component_name
      and label = Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td colspan=\"2\">%s</td></tr><tr><td>%s</td><td>%s</td></tr></table>" component.component_name provided_ports_table required_ports_table
      in
      Printf.sprintf
        "    %s [label=<%s>];"
        id
        label
    
    let string_of_package_at_location (universe : universe) (location : location) (package_name : package_name)  =
      Printf.sprintf
        "%s [shape=ellipse,label=\"%s\"];"
        (package_at_location_id location.location_name package_name)
        package_name

    let string_of_location (universe : universe) (configuration : configuration) (location : location) =
      let id    = location_id location.location_name
      and label = Printf.sprintf "%s\\n[%s]" location.location_name location.location_repository
      in
      let location_component_strings =
        List.map (string_of_component universe) (get_location_components configuration location.location_name)
      (* and location_package_strings =
        List.map (string_of_package_at_location universe location) (get_location_packages_installed configuration location.location_name) *)
      in
      Printf.sprintf
        "  subgraph cluster_%s {\n    label = \"%s\";\n    node [shape=plaintext];\n%s\n  }"
        id
        label
        (lines_of_strings location_component_strings)


    let string_of_binding (universe : universe) (binding : binding) = 
      let requirer_id  = component_id binding.binding_requirer
      and provider_id  = component_id binding.binding_provider
      (* and label        = binding.binding_port *)
      in
      (*
      Printf.sprintf
        "  %s -> %s [label=\"%s\"];"
        requirer_id
        provider_id
        label
      *)
      Printf.sprintf
        "  %s:%s -> %s:%s;"
        requirer_id
        (required_port_id binding.binding_port)
        provider_id
        (provided_port_id binding.binding_port)

    let string_of_configuration (universe : universe) (configuration : configuration) =
      let first_line = "digraph Configuration {"
      and graph_setting_strings = ["rankdir=LR;"]
      and last_line  = "}"
      in
      let location_strings = List.map (string_of_location universe configuration) configuration.configuration_locations
      and binding_strings  = List.map (string_of_binding  universe) configuration.configuration_bindings
      in
      Printf.sprintf
        "%s\n\n%s\n\n%s\n\n%s\n\n%s"
        first_line
        (lines_of_strings graph_setting_strings)
        (lines_of_strings location_strings)
        (lines_of_strings binding_strings)
        last_line

  end

module Graphviz_configuration_output_bbb = 
  struct 

    let string_of_component_type_name    = Simple.string_of_component_type_name
    let string_of_port_name              = Simple.string_of_port_name
    let string_of_component_name         = Simple.string_of_component_name
    let string_of_package_name           = Simple.string_of_package_name
    let string_of_repository_name        = Simple.string_of_repository_name
    let string_of_location_name          = Simple.string_of_location_name
    let string_of_resource_name          = Simple.string_of_resource_name
    let string_of_resource_provide_arity = Simple.string_of_resource_provide_arity

    let id_of_name name = 
      BatString.filter_map (fun c ->
        match c with 
        | 'a'..'z' -> Some c
        | 'A'..'Z' -> Some c
        | '0'..'9' -> Some c
        | '@'
        | ' '      -> None
        | _        -> Some '_'
      ) name
    
    let component_id component_name =
      Printf.sprintf "component_%s" (id_of_name component_name)

    let location_id location_name =
      Printf.sprintf "location_%s" (id_of_name location_name)

    let package_at_location_id location_name package_name =
      Printf.sprintf "%s_package_%s" (location_id location_name) (id_of_name package_name)

    let port_id port_name =
      Printf.sprintf "port_%s" (id_of_name port_name)

    let required_port_id port_name =
      Printf.sprintf "required_%s" (port_id port_name)

    let provided_port_id port_name =
      Printf.sprintf "provided_%s" (port_id port_name)

    (* "<img src=\"tux.jpg\" scale=\"true\"/>" *)

    let string_of_component (universe : universe) (component : component) = 
      let component_type = get_component_type universe component.component_type
      in

      let required_ports_table =
        let required_ports_strings =
          List.map (fun (port_name, arity) -> Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (required_port_id port_name) (BatString.lchop port_name)) component_type.component_type_require
        in
        if required_ports_strings = []
        then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"red\">%s</table>" (lines_of_strings required_ports_strings)

      and provided_ports_table =
        let provided_ports_strings =
          List.map (fun (port_name, arity) -> Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (provided_port_id port_name) (BatString.lchop port_name)) component_type.component_type_provide
        in
        if provided_ports_strings = []
        then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"green\">%s</table>" (lines_of_strings provided_ports_strings)

      in

      let id    = component_id component.component_name
      and label = Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td colspan=\"2\">%s</td></tr><tr><td>%s</td><td>%s</td></tr></table>" component.component_name provided_ports_table required_ports_table
      in
      Printf.sprintf
        "    %s [label=<%s>];"
        id
        label
    
    let string_of_package_at_location (universe : universe) (location : location) (package_name : package_name)  =
      Printf.sprintf
        "%s [shape=ellipse,label=\"%s\"];"
        (package_at_location_id location.location_name package_name)
        package_name

    let string_of_location (universe : universe) (configuration : configuration) (location : location) =
      let id    = location_id location.location_name
      and label = Printf.sprintf "%s\\n[%s]" location.location_name location.location_repository
      in
      let location_component_strings =
        List.map (string_of_component universe) (get_location_components configuration location.location_name)
      and location_package_strings =
        List.map (string_of_package_at_location universe location) (get_location_packages_installed configuration location.location_name) 
      in
      let location_repository = get_repository universe location.location_repository
      and location_packages_installed = location.location_packages_installed
      in
      let location_package_dependency_strings =
        List.flatten (
          List.map (fun package_name -> 
            let package = get_package location_repository package_name
            in
            let dependencies = 
              List.filter (fun package_name -> List.mem package_name location_packages_installed) (BatList.unique (List.flatten package.package_depend))
            in
            List.map (fun depended_package_name ->
              Printf.sprintf 
                "    %s -> %s;"
                (package_at_location_id location.location_name package_name)
                (package_at_location_id location.location_name depended_package_name)
              ) dependencies
          ) location_packages_installed )

      and location_implementation_strings =
        List.flatten (
          List.map (fun component ->
            let implementing_packages =
              List.filter (fun package_name -> List.mem package_name location_packages_installed) (get_component_type_implementation universe component.component_type)
            in
            List.map (fun depended_package_name ->
              Printf.sprintf 
                "    %s -> %s;"
                (component_id component.component_name)
                (package_at_location_id location.location_name depended_package_name)
              ) implementing_packages
          ) (get_location_components configuration location.location_name) )


      in
      Printf.sprintf
        "  subgraph cluster_%s {\n    label = \"%s\";\n    node [shape=plaintext];\n{ rank = same;\n%s}\n\n%s\n\n%s\n  }"
        id
        label
        (lines_of_strings location_component_strings)
        (lines_of_strings location_package_strings)
        (lines_of_strings (location_package_dependency_strings @ location_implementation_strings))


    let string_of_binding (universe : universe) (binding : binding) = 
      let requirer_id  = component_id binding.binding_requirer
      and provider_id  = component_id binding.binding_provider
      (* and label        = binding.binding_port *)
      in
      (*
      Printf.sprintf
        "  %s -> %s [label=\"%s\"];"
        requirer_id
        provider_id
        label
      *)
      Printf.sprintf
        "  %s:%s -> %s:%s;"
        requirer_id
        (required_port_id binding.binding_port)
        provider_id
        (provided_port_id binding.binding_port)

    let string_of_configuration (universe : universe) (configuration : configuration) =
      let first_line = "digraph Configuration {"
      and graph_setting_strings = ["rankdir=TD;"]
      and last_line  = "}"
      in
      let location_strings = List.map (string_of_location universe configuration) configuration.configuration_locations
      in
      Printf.sprintf
        "%s\n\n%s\n\n%s\n\n%s"
        first_line
        (lines_of_strings graph_setting_strings)
        (lines_of_strings location_strings)
        last_line

  end

module Graphviz_configuration_output_1 = 
  struct
  
    open Graphviz_configuration_output

    let string_of_configuration (configuration : configuration) =
      let first_line = "digraph Configuration {"
      and graph_setting_strings = ["rankdir=LR;"]
      and last_line  = "}"
      in
      let component_strings = List.map string_of_component configuration.configuration_components
      and binding_strings   = List.map string_of_binding   configuration.configuration_bindings
      in
      Printf.sprintf
        "%s\n\n%s\n\n%s\n\n%s\n\n%s"
        first_line
        (lines_of_strings graph_setting_strings)
        (lines_of_strings component_strings)
        (lines_of_strings binding_strings)
        last_line

  end

module Graphviz_configuration_output_2 = 
  struct
  
    open Graphviz_configuration_output

    let string_of_configuration (configuration : configuration) =
      let first_line = "digraph Configuration {"
      and graph_setting_strings = ["rankdir=LR;"]
      and last_line  = "}"
      in
      let location_strings = List.map (string_of_location configuration) configuration.configuration_locations
      and binding_strings  = List.map string_of_binding  configuration.configuration_bindings
      in
      Printf.sprintf
        "%s\n\n%s\n\n%s\n\n%s\n\n%s"
        first_line
        (lines_of_strings graph_setting_strings)
        (lines_of_strings location_strings)
        (lines_of_strings binding_strings)
        last_line

  end