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

open Data_model

type graph_settings = {
  show_components : bool;
  show_ports      : bool;
  show_bindings   : bool;
  show_locations  : bool;
  show_packages   : bool;
}

type graph_type =
  | Deployment_graph
  | Simplified_deployment_graph
  | Components_graph
  | Packages_graph

let graph_settings_of_graph_type = function
  | Deployment_graph ->  {
      show_components = true;
      show_ports      = true;
      show_bindings   = true;
      show_locations  = true;
      show_packages   = false;
    }
  | Simplified_deployment_graph -> 
    {
      show_components = true;
      show_ports      = false;
      show_bindings   = true;
      show_locations  = true;
      show_packages   = false;
    }
  | Components_graph -> 
    {
      show_components = true;
      show_ports      = true;
      show_bindings   = true;
      show_locations  = false;
      show_packages   = false;
    }
  | Packages_graph -> 
    {
      show_components = true;
      show_ports      = false;
      show_bindings   = false;
      show_locations  = true;
      show_packages   = true;
    }


let my_regexp = Str.regexp "[^a-z0-9]"
let sanitize_name name = Str.global_replace my_regexp "_" (String.lowercase name)

let component_id component_name = "component_" ^ (sanitize_name component_name)
let location_id  location_name  = "location_" ^ (sanitize_name location_name)
let package_at_location_id location_name package_id = (location_id location_name) ^ "_package_" ^ (sanitize_name (String_of.package_id package_id))
let port_id          port_name = "port_" ^ (sanitize_name port_name)
let required_port_id port_name = "required_" ^ (port_id port_name)
let provided_port_id port_name = "provided_" ^ (port_id port_name)


(*/************************************************************************\*)
(*| 1. Components                                                         |*)
(*\************************************************************************/*)

let configuration_with_graph_settings (graph_settings : graph_settings) (universe : universe) (configuration : configuration) : string =

  let string_of_component (c_id : component_id) : string =
    let c = configuration#get_component c_id in

    let name = String_of.component_name (Name_of.component_id c_id) in 
    let id   = component_id name in
    
    let string_of_component_without_ports () : string = 
      let label = name in 
      Printf.sprintf "%s [shape=box,label=\"%s\"];" id label in

    let string_of_component_with_ports () : string = 
      let ct = universe#get_component_type c#typ  in

      let required_ports_table : string =
        let required_ports_strings = Port_set.fold (fun p res ->
          let p_name = String_of.port_name (Name_of.port_id p) in
          (Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (required_port_id p_name) p_name)::res
        ) ct#require_domain [] in
        if required_ports_strings = []
        then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"red\">%s</table>" (String.concat "\n" required_ports_strings) in

      let provided_ports_table : string =
        let provided_ports_strings = Port_set.fold (fun p res ->
          let p_name = String_of.port_name (Name_of.port_id p) in 
          (Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (provided_port_id p_name) p_name)::res
        ) ct#provide_domain [] in
        if provided_ports_strings = [] 
        then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"green\">%s</table>" (String.concat "\n" provided_ports_strings) in 

      let label : string = Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td colspan=\"2\">%s</td></tr><tr><td>%s</td><td>%s</td></tr></table>" name provided_ports_table required_ports_table in
      Printf.sprintf "%s [shape=box,label=<%s>];" id label in

    if graph_settings.show_ports 
    then string_of_component_with_ports    ()
    else string_of_component_without_ports ()
  in
      
  let string_of_binding (b : binding) : string = (* could be string *)
    let requirer_id  = component_id (String_of.component_name (Name_of.component_id b#requirer)) in
    let provider_id  = component_id (String_of.component_name (Name_of.component_id b#provider)) in
    
    let string_of_binding_without_ports () : string = 
      Printf.sprintf "%s -> %s;" requirer_id provider_id in
    
    let string_of_binding_with_ports () : string = 
      let p_name = String_of.port_name (Name_of.port_id b#port) in
      Printf.sprintf "%s:%s -> %s:%s" requirer_id (required_port_id p_name) provider_id (provided_port_id p_name) in
    
    if graph_settings.show_ports
    then string_of_binding_with_ports ()
    else string_of_binding_without_ports ()
  in

      
  let strings_of_location (l_id : location_id) : string list =

    let l = configuration#get_location l_id in
    
    let name  = String_of.location_name (Name_of.location_id l_id) in
    let id    = location_id name in
    let label = Printf.sprintf "%s\\n[%s]" name (String_of.repository_name (Name_of.repository_id l#repository)) in
  
    let location_package_strings () : string list =   (* the packages inside the location *)
      let strings_of_packages_at_location () : string list =
        Package_id_set.fold (fun k_id res -> 
          (Printf.sprintf "%s[shape=ellipse,label=\"%s\"];" (package_at_location_id (String_of.location_name (Name_of.location_id l_id)) k_id) (String_of.package_id k_id))::res
        ) l#packages_installed [] in
          
      let strings_of_package_dependency_at_location () : string list =
        let name  = String_of.location_name (Name_of.location_id l_id) in
        let location_packages_installed = l#packages_installed in
        Package_id_set.fold (fun k_id res -> 
          let dependencies = Package_id_set_set.choose (Package_id_set_set.filter (fun s -> Package_id_set.subset s location_packages_installed) (universe#get_package k_id)#depend) in
          Package_id_set.fold (fun k_id' res' ->
            (Printf.sprintf "%s -> %s" (package_at_location_id name k_id) (package_at_location_id name k_id'))::res'
          ) dependencies res
        ) location_packages_installed [] in
          
      let strings_of_package_implementation_at_location () : string list =
        let name  = String_of.location_name (Name_of.location_id l_id) in
        let location_packages_installed = l#packages_installed in
        Component_id_set.fold (fun c_id res ->
          let c = configuration#get_component c_id in
          let k_id = Package_id_set.choose (Package_id_set.inter location_packages_installed (universe#get_implementation c#typ)) in
            (Printf.sprintf "%s -> %s" (component_id (String_of.component_name (Name_of.component_id c_id))) (package_at_location_id name k_id))::res
        ) (Component_id_set.filter (fun c -> (configuration#get_component c)#location = l_id) configuration#get_component_ids) [] in

      if graph_settings.show_packages
      then
        (strings_of_packages_at_location           ()) @
        (strings_of_package_dependency_at_location ()) @
        (if graph_settings.show_components 
         then strings_of_package_implementation_at_location () 
         else [])
      else [] in
    
    let location_component_strings () : string list = (* the components inside the location *)
      if graph_settings.show_components
      then (List.map string_of_component (Component_id_set.elements (Component_id_set.filter (fun c -> (configuration#get_component c)#location = l_id) configuration#get_component_ids)))
      else [] in
    
    if graph_settings.show_locations then
      let before  = [Printf.sprintf "subgraph cluster_%s {" id] in
      let content = [Printf.sprintf "label = \"%s\";" label] @ ( (location_component_strings ()) @ (location_package_strings ())) in
      let after   = ["}"] in 
      before @ ((List.map (fun s -> Printf.sprintf "  %s" s) content) @ after)
    else 
      (location_component_strings ()) @
      (location_package_strings   ())
  in

  let location_strings : string list = List.flatten (List.map strings_of_location (Location_id_set.elements configuration#get_location_ids)) in
  let binding_strings  : string list = if graph_settings.show_bindings then List.map string_of_binding (Binding_set.elements configuration#get_bindings) else [] in

  let rankdir_value = if graph_settings.show_packages then "TB" else "LR" in

  let before  = ["digraph Configuration {" ] in
  let content = ("rankdir=" ^ rankdir_value ^ ";"):: (location_strings @ binding_strings) in
  let after   = [ "}" ] in
  String.concat "\n" (before @ (List.map (fun s -> Printf.sprintf "  %s" s) content) @ after)


let configuration (graph_type : graph_type) : universe -> configuration -> string =
  configuration_with_graph_settings (graph_settings_of_graph_type graph_type)