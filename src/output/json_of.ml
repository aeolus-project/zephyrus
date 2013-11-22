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
let convert_universe (u : universe) = 
    
  (* Prepare the available resources in form of a list. *)
  let resource_id_list = Resource_id_set.elements u#get_resource_ids in
    
  (* Component types. *)
  let convert_component_type ct_id = 
    let ct = u#get_component_type ct_id in {
      Json_j.component_type_name     = Name_of.component_type_id ct_id;
      Json_j.component_type_provide  = List.map (fun p -> (Name_of.port_id p, convert_provide_arity (ct#provide p))) (Port_id_set.elements ct#provide_domain);
      Json_j.component_type_require  = List.map (fun p -> (Name_of.port_id p, convert_require_arity (ct#require p))) (Port_id_set.elements ct#require_domain);
      Json_j.component_type_conflict = List.map Name_of.port_id                                                      (Port_id_set.elements (ct#conflict));
      Json_j.component_type_consume  = List.map (fun r -> (Name_of.resource_id r, convert_resource_consume_arity (ct#consume r))) resource_id_list
    } in

  (* Packages. *)
  let convert_package k_id = 
    let k = u#get_package k_id in {
      Json_j.package_name     = Name_of.package_id k_id;
      Json_j.package_depend   = List.map (fun k_ids -> List.map Name_of.package_id (Package_id_set.elements k_ids)) (Package_id_set_set.elements k#depend);
      Json_j.package_conflict = List.map Name_of.package_id                                                         (Package_id_set.elements k#conflict);
      Json_j.package_consume  = List.map (fun r -> (Name_of.resource_id r, convert_resource_consume_arity (k#consume r))) resource_id_list;
    } in

  (* Repositories. *)
  let convert_repository r_id = 
    let r = u#get_repository r_id in {
      Json_j.repository_name     = Name_of.repository_id r_id;
      Json_j.repository_packages = List.map convert_package (Package_id_set.elements (r#package_ids));
    } in

  (* Implementation. *)
  let convert_implementation_of_a_component_type ct_id =
    let package_get_name_full k_id = (Name_of.repository_id (u#repository_of_package k_id), Name_of.package_id k_id) in 
    (Name_of.component_type_id ct_id, List.map package_get_name_full (Package_id_set.elements (u#get_implementation ct_id))) in

  (* The universe: *)
  {
    Json_j.universe_component_types = List.map convert_component_type                     (Component_type_id_set.elements u#get_component_type_ids);
    Json_j.universe_implementation  = List.map convert_implementation_of_a_component_type (Component_type_id_set.elements u#get_component_type_ids);
    Json_j.universe_repositories    = List.map convert_repository                         (Repository_id_set.elements u#get_repository_ids);
  }


(* configuration *)
let convert_configuration (u : universe) (c : configuration) =

  (* Prepare the available resources in form of a list. *)
  let resource_id_list = Resource_id_set.elements u#get_resource_ids in

  (* Locations. *)
  let convert_location l_id = 
    let l = c#get_location l_id in {
      Json_j.location_name               = Name_of.location_id l_id;
      Json_j.location_repository         = Name_of.repository_id l#repository;
      Json_j.location_packages_installed = List.map Name_of.package_id (Package_id_set.elements l#packages_installed);
      Json_j.location_provide_resources  = List.map (fun r -> (Name_of.resource_id r, convert_resource_provide_arity (l#provide_resources r))) resource_id_list;
      Json_j.location_cost               = convert_location_cost l#cost
    } in

  (* Components. *)
  let convert_component c_id = 
    let c = c#get_component c_id in {
      Json_j.component_name     = Name_of.component_id      c_id;
      Json_j.component_type     = Name_of.component_type_id c#typ;
      Json_j.component_location = Name_of.location_id       c#location
    } in

  (* Bindings. *)
  let convert_binding b = {
      Json_j.binding_port     = Name_of.port_id      b#port;
      Json_j.binding_requirer = Name_of.component_id b#requirer;
      Json_j.binding_provider = Name_of.component_id b#provider
  } in

  (* The configuration: *)
  {
    Json_j.configuration_locations  = List.map convert_location  (Location_id_set.elements  c#get_location_ids);
    Json_j.configuration_components = List.map convert_component (Component_id_set.elements c#get_component_ids);
    Json_j.configuration_bindings   = List.map convert_binding   (Binding_set.elements      c#get_bindings)
  }


(*/**********************************************\*)
(* 2. Print everything                            *)
(*\**********************************************/*)


let universe      u   channel = Json_j.write_universe      (Bi_outbuf.create_channel_writer channel) (convert_universe      u  )
let configuration u c channel = Json_j.write_configuration (Bi_outbuf.create_channel_writer channel) (convert_configuration u c)

let universe_string      (u : universe)                     = Yojson.Safe.prettify (Json_j.string_of_universe      (convert_universe      u  ))
let configuration_string (u : universe) (c : configuration) = Yojson.Safe.prettify (Json_j.string_of_configuration (convert_configuration u c))



