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


(* Depends on
    - datatypes/Data_model
    - datatypes/Data_constraint
    - datatypes/Data_helper
    - atd/Json_j
*)

(* 0. Name Conversion *)



(*/**********************************************\*)
(* 1. Translation to Json internal representation *)
(*\**********************************************/*)

open Data_model

let convert_resource_name          x = x
let convert_resource_consume_arity x = x
let convert_resource_provide_arity x = x
let convert_port_name              x = x
let convert_component_type_name    x = x
let convert_provide_arity          x = match x with | Infinite_provide -> `InfiniteProvide | Finite_provide(i) -> `FiniteProvide(i)
let convert_require_arity          x = x
let convert_package_name           x = x
let convert_repository_name        x = x
let convert_location_name          x = x
let convert_component_name         x = x

(* universe *)
let convert_component_type c resources port_get_name resource_get_name = {
    Json_j.component_type_name = convert_component_type_name (c#name);
    Json_j.component_type_provide  = List.map (fun p -> (port_get_name p, convert_provide_arity (c#provide p))) (Port_id_set.elements c#provide_domain);
    Json_j.component_type_require  = List.map (fun p -> (port_get_name p, convert_require_arity (c#require p))) (Port_id_set.elements c#require_domain);
    Json_j.component_type_conflict = List.map port_get_name (Port_id_set.elements (c#conflict));
    Json_j.component_type_consume  = List.map (fun r -> (resource_get_name r, convert_resource_consume_arity (c#consume r))) resources
}

let convert_package k resources package_get_name resource_get_name = {
    Json_j.package_name     = convert_package_name (k#name);
    Json_j.package_depend   = List.map (fun ks -> List.map package_get_name (Package_id_set.elements ks)) (Package_id_set_set.elements k#depend);
    Json_j.package_conflict = List.map package_get_name (Package_id_set.elements k#conflict);
    Json_j.package_consume  = List.map (fun r -> (resource_get_name r, convert_resource_consume_arity (k#consume r))) resources
}

let convert_repository r resources package_get_name resource_get_name = {
    Json_j.repository_name     = convert_repository_name (r#name);
    Json_j.repository_packages = List.map (fun k -> convert_package k resources package_get_name resource_get_name) (Package_set.elements (r#packages))
}

let convert_universe_tmp u resources resource_get_name =
  let port_get_name p = convert_port_name (u#get_port_name p) in
  let component_type_get_name c = convert_component_type_name (u#get_component_type_name c) in
  let repository_get_name r = convert_repository_name (u#get_repository_name r) in
  let package_get_name_full k = (repository_get_name (u#repository_of_package k), convert_package_name (u#get_package_name k)) in
  let package_get_name k = convert_package_name (u#get_package_name k) in {
    Json_j.universe_component_types =
      List.map (fun c -> convert_component_type c resources port_get_name resource_get_name) (Component_type_set.elements u#get_component_types);
    Json_j.universe_implementation = List.map (fun c -> (component_type_get_name c, List.map package_get_name_full (Package_id_set.elements (u#get_implementation c))))
        (Component_type_id_set.elements u#get_component_type_ids);
    Json_j.universe_repositories = List.map (fun r -> convert_repository r resources package_get_name resource_get_name) (Repository_set.elements (u#get_repositories))
}

let convert_universe u resources = convert_universe_tmp u (Resource_id_set.elements resources#resource_ids) resources#get_name


(* configuration *)
let convert_location l resources repository_get_name package_get_name component_get_name resource_get_name = {
    Json_j.location_name = convert_location_name l#name;
    Json_j.location_repository = repository_get_name l#repository;
    Json_j.location_packages_installed = List.map package_get_name (Package_id_set.elements l#packages_installed);
    Json_j.location_provide_resources = List.map (fun r -> (resource_get_name r, convert_resource_provide_arity (l#provide_resources r))) resources
}

let convert_component c resources component_type_get_name location_get_name = {
    Json_j.component_name = convert_component_name c#name;
    Json_j.component_type = component_type_get_name c#typ;
    Json_j.component_location = location_get_name c#location
}

let convert_binding b port_get_name component_get_name = {
    Json_j.binding_port     = port_get_name b#port;
    Json_j.binding_requirer = component_get_name b#requirer;
    Json_j.binding_provider = component_get_name b#provider
}

let convert_configuration_tmp resources c u resource_get_name =
  let repository_get_name = u#get_repository_name in
  let port_get_name = u#get_port_name in
  let component_type_get_name c = convert_component_type_name (u#get_component_type_name c) in
  let package_get_name = u#get_package_name in
  let location_get_name = c#get_location_name in
  let component_get_name = c#get_component_name in {
    Json_j.configuration_locations = List.map (fun l -> convert_location l resources repository_get_name package_get_name component_get_name resource_get_name)
        (Location_set.elements c#get_locations);
    Json_j.configuration_components = List.map (fun c -> convert_component c resources component_type_get_name location_get_name) (Component_set.elements c#get_components);
    Json_j.configuration_bindings = List.map (fun b -> convert_binding b port_get_name component_get_name) (Binding_set.elements c#get_bindings)
}

let convert_configuration c u resources = convert_configuration_tmp (Resource_id_set.elements resources#resource_ids) c u resources#get_name

(* Specification *)



(*/**********************************************\*)
(* 2. Print everything                            *)
(*\**********************************************/*)


let universe u resources channel = Json_j.write_universe (Bi_outbuf.create_channel_writer channel) (convert_universe u resources)
let configuration c u resources channel = Json_j.write_configuration (Bi_outbuf.create_channel_writer channel) (convert_configuration c u resources)


let universe_string u resources = Yojson.Safe.prettify (Json_j.string_of_universe (convert_universe u resources))
let configuration_string c u resources = Yojson.Safe.prettify (Json_j.string_of_configuration (convert_configuration c u resources))



