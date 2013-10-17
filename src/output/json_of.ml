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

let convert_location_cost          x = x

(* universe *)
let convert_component_type t resource_id_list = 
(* DEBUG **************************)

   print_string( " => component \"" ^ (Name_of.component_type_id t#id) ^ "\" provides " ^ (String_of.port_id_set t#provide_domain) ^ "\n"); `
   print_string( " => component \"" ^ (Name_of.component_type_id t#id) ^ "\" requires " ^ (String_of.port_id_set t#require_domain) ^ "\n");

(* DEBUG **************************) {
    Json_j.component_type_name = Name_of.component_type_id t#id;
    Json_j.component_type_provide  = List.map (fun p -> (Name_of.port_id p, convert_provide_arity (t#provide p))) (Port_id_set.elements t#provide_domain);
    Json_j.component_type_require  = List.map (fun p -> (Name_of.port_id p, convert_require_arity (t#require p))) (Port_id_set.elements t#require_domain);
    Json_j.component_type_conflict = List.map Name_of.port_id (Port_id_set.elements (t#conflict));
    Json_j.component_type_consume  = List.map (fun r -> (Name_of.resource_id r, convert_resource_consume_arity (t#consume r))) resource_id_list
}

let convert_package k resource_id_list = {
    Json_j.package_name     = Name_of.package_id k#id;
    Json_j.package_depend   = List.map (fun ks -> List.map Name_of.package_id (Package_id_set.elements ks)) (Package_id_set_set.elements k#depend);
    Json_j.package_conflict = List.map Name_of.package_id (Package_id_set.elements k#conflict);
    Json_j.package_consume  = List.map (fun r -> (Name_of.resource_id r, convert_resource_consume_arity (k#consume r))) resource_id_list
}

let convert_repository r resource_id_list = {
    Json_j.repository_name     = Name_of.resource_id r#id;
    Json_j.repository_packages = List.map (fun k -> convert_package k resource_id_list) (Package_set.elements (r#packages))
}

let convert_universe_tmp u resource_id_list =
  let package_get_name_full   k = (Name_of.repository_id (u#repository_of_package k), Name_of.package_id k) in {
    Json_j.universe_component_types =
      List.map (fun c -> convert_component_type c resource_id_list) (Component_type_set.elements u#get_component_types);
    Json_j.universe_implementation = List.map (fun c -> (Name_of.component_type_id c, List.map package_get_name_full (Package_id_set.elements (u#get_implementation c))))
        (Component_type_id_set.elements u#get_component_type_ids);
    Json_j.universe_repositories = List.map (fun r -> convert_repository r resource_id_list) (Repository_set.elements (u#get_repositories))
}

let convert_universe u = convert_universe_tmp u (Resource_id_set.elements u#get_resource_ids)


(* configuration *)
let convert_location l resource_id_list = {
    Json_j.location_name = Name_of.location_id l#id;
    Json_j.location_repository = Name_of.repository_id l#repository;
    Json_j.location_packages_installed = List.map Name_of.package_id (Package_id_set.elements l#packages_installed);
    Json_j.location_provide_resources = List.map (fun r -> (Name_of.resource_id r, convert_resource_provide_arity (l#provide_resources r))) resource_id_list;
    Json_j.location_cost = convert_location_cost l#cost
}

let convert_component c = {
    Json_j.component_name     = Name_of.component_id      c#id;
    Json_j.component_type     = Name_of.component_type_id c#typ;
    Json_j.component_location = Name_of.location_id       c#location
}

let convert_binding b = {
    Json_j.binding_port     = Name_of.port_id      b#port;
    Json_j.binding_requirer = Name_of.component_id b#requirer;
    Json_j.binding_provider = Name_of.component_id b#provider
}

let convert_configuration_tmp c u resource_id_list = {
    Json_j.configuration_locations  = List.map (fun l -> convert_location l resource_id_list) (Location_set.elements c#get_locations);
    Json_j.configuration_components = List.map (fun c -> convert_component c) (Component_set.elements c#get_components);
    Json_j.configuration_bindings   = List.map (fun b -> convert_binding b) (Binding_set.elements c#get_bindings)
}

let convert_configuration c u = convert_configuration_tmp c u (Resource_id_set.elements u#get_resource_ids)

(* Specification *)



(*/**********************************************\*)
(* 2. Print everything                            *)
(*\**********************************************/*)


let universe u channel = Json_j.write_universe (Bi_outbuf.create_channel_writer channel) (convert_universe u)
let configuration c u channel = Json_j.write_configuration (Bi_outbuf.create_channel_writer channel) (convert_configuration c u)


let universe_string u = Yojson.Safe.prettify (Json_j.string_of_universe (convert_universe u))
let configuration_string c u = Yojson.Safe.prettify (Json_j.string_of_configuration (convert_configuration c u))



