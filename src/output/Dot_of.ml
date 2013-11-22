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

let settings_of = function
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

(*

let strings_of_component c : component -> string list =
  let name = String_of.component_name c#name in
  let component_core = (component_id name) ^ " [shape=box,label=\"" ^ name ^ "\"];" in (* strings_of_component_without_ports *)

  let strings_of_component_without_ports (component : component) : string list = 
    let id    =  in
    let label = String_of.component_name c#name in
          [Printf.sprintf "%s [shape=box,label=\"%s\"];" id label]

  and strings_of_component_with_ports (component : component) : string list = 
          let component_type = get_component_type universe component.component_type
          in

          let required_ports_table =
            let required_ports_strings =
              List.map (fun (port_name, arity) -> Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (required_port_id port_name) (String.lchop port_name)) component_type.component_type_require
            in
            if required_ports_strings = [] then " "
            else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"red\">%s</table>" (lines_of_strings required_ports_strings)

          and provided_ports_table =
            let provided_ports_strings =
              List.map (fun (port_name, arity) -> Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (provided_port_id port_name) (String.lchop port_name)) component_type.component_type_provide
            in
            if provided_ports_strings = [] then " "
            else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"green\">%s</table>" (lines_of_strings provided_ports_strings)

          in
          let id    = component_id component.component_name
          and label = Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td colspan=\"2\">%s</td></tr><tr><td>%s</td><td>%s</td></tr></table>" component.component_name provided_ports_table required_ports_table
          in
          [Printf.sprintf "%s [shape=plaintext,label=<%s>];" id label]


*)

let configuration (graph_settings : graph_settings) (universe : universe) (configuration : configuration) : string =

  let strings_of_component (c: component) : string = (* could be string *)
    let name = String_of.component_name (Name_of.component_id c#id) in 
    let id    = component_id name in
    
    let strings_of_component_without_ports (component : component) : string = 
       let label = name in (id ^ " [shape=box,label=\"" ^ label ^ "\"];") in

    let strings_of_component_with_ports (component : component) : string = 
      let t = universe#get_component_type component#typ  in
      let required_ports_table =
        let inner p res = let p_name = String_of.port_name (Name_of.port_id p) in (Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (required_port_id p_name) p_name)::res in
        let required_ports_strings = Port_set.fold inner t#require_domain [] in
        if required_ports_strings = [] then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"red\">%s</table>" (String.concat "\n" required_ports_strings) in
      let provided_ports_table =
        let inner p res = let p_name = String_of.port_name (Name_of.port_id p) in (Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (provided_port_id p_name) p_name)::res in
        let provided_ports_strings = Port_set.fold inner t#provide_domain [] in
        if provided_ports_strings = [] then " "
        else Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" bgcolor=\"green\">%s</table>" (String.concat "\n" provided_ports_strings)
      in let label = Printf.sprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td colspan=\"2\">%s</td></tr><tr><td>%s</td><td>%s</td></tr></table>"
          name provided_ports_table required_ports_table
      in (id ^ " [shape=box,label=<" ^ label ^ ">];") in
    if graph_settings.show_ports then strings_of_component_with_ports c else strings_of_component_without_ports c
  in
      
  let strings_of_binding (b: binding) : string list = (* could be string *)
    let requirer_id  = component_id (String_of.component_name (Name_of.component_id b#requirer)) in
    let provider_id  = component_id (String_of.component_name (Name_of.component_id b#provider)) in
    let strings_of_binding_without_ports (b : binding) =  [requirer_id ^ " -> " ^ provider_id ^ ";"] in
    let strings_of_binding_with_ports (binding : binding) = let p_name = String_of.port_name (Name_of.port_id b#port) in
      [ requirer_id ^ ":" ^ (required_port_id p_name) ^ " -> " ^ provider_id ^ ":" ^ (provided_port_id p_name) ]  in
    if graph_settings.show_ports then strings_of_binding_with_ports b else strings_of_binding_without_ports b
  in

  let strings_of_packages_at_location (l : location) : string list =
    let inner k_id res = ((package_at_location_id (String_of.location_name (Name_of.location_id l#id)) k_id) ^ "[shape=ellipse,label=\"" ^ (String_of.package_id k_id) ^ "\"];")::res in
     Package_id_set.fold inner l#packages_installed []
  in
      
  let strings_of_package_dependency_at_location (location : location) : string list =
    let name  = String_of.location_name (Name_of.location_id location#id) in
    let location_packages_installed = location#packages_installed in
    Package_id_set.fold (fun k_id res -> 
        let dependencies = Package_id_set_set.choose (Package_id_set_set.filter (fun s -> Package_id_set.subset s location_packages_installed) (universe#get_package k_id)#depend) in
        Package_id_set.fold (fun k_id' res' -> ((package_at_location_id name k_id) ^ " -> " ^ (package_at_location_id name k_id'))::res') dependencies res)
      location_packages_installed []
  in
      
  let strings_of_package_implementation_at_location (location : location) : string list =
    let name  = String_of.location_name (Name_of.location_id location#id) in
    let location_packages_installed = location#packages_installed in
    Component_id_set.fold (fun c_id res ->
        let c = configuration#get_component c_id in
        let k_id = Package_id_set.choose (Package_id_set.inter location_packages_installed (universe#get_implementation c#typ)) in
          ((component_id (String_of.component_name (Name_of.component_id c#id))) ^ " -> " ^ (package_at_location_id name k_id))::res)
      (Component_id_set.filter (fun c -> (configuration#get_component c)#location = location#id) configuration#get_component_ids) []
  in
      
  let strings_of_location (location : location) : string list =
    let name  = String_of.location_name (Name_of.location_id location#id) in
    let id    = location_id name in
    let label = Printf.sprintf "%s\\n[%s]" name (String_of.repository_name (Name_of.repository_id location#repository)) in
    let location_component_strings : string list = (* the components inside the location *)
      if graph_settings.show_components
      then (List.map (fun c -> strings_of_component (configuration#get_component c)) (Component_id_set.elements (Component_id_set.filter (fun c -> (configuration#get_component c)#location = location#id) configuration#get_component_ids)))
      else [] in
    let location_package_strings : string list =   (* the packages inside the location *)
      if graph_settings.show_packages
      then
          (strings_of_packages_at_location           location) 
        @ (strings_of_package_dependency_at_location location)
        @ (if graph_settings.show_components then strings_of_package_implementation_at_location location else [])
      else [] in
    if graph_settings.show_locations then
      let before  = ("subgraph cluster_" ^ id ^ " {") in
      let content = (Printf.sprintf "label = \"%s\";" label)::(location_component_strings @ location_package_strings) in
      let after = ["}"] in before::((List.map (fun s -> "  " ^ s) content) @ after)
    else location_component_strings @ location_package_strings
  in

  let location_strings : string list = List.flatten (List.map (fun l -> strings_of_location (configuration#get_location l)) (Location_id_set.elements configuration#get_location_ids)) in
  let binding_strings  : string list = if graph_settings.show_bindings then List.flatten (List.map strings_of_binding (Binding_set.elements configuration#get_bindings)) else [] in

  let rankdir_value = if graph_settings.show_packages then "TB" else "LR" in

  let before  = ["digraph Configuration {" ] in
  let content = ("rankdir=" ^ rankdir_value ^ ";"):: (location_strings @ binding_strings) in
  let after   = [ "}" ] in
  String.concat "\n" (before @ (List.map (fun s -> "  " ^ s) content) @ after)



