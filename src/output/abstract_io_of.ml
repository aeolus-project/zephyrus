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
    - datatypes/Abstract_io
*)

open Data_model


let component_type_id = Data_state.component_type
let port_id           = Data_state.port
let resource_id       = Data_state.resource
let package_id        = Data_state.package
let repository_id     = Data_state.repository
let location_id       = Data_state.location
let component_id      = Data_state.component

let provide_arity          x = match x with Infinite_provide -> Abstract_io.InfiniteProvide | Finite_provide(i) -> Abstract_io.FiniteProvide(i)
let require_arity          x = x

let resource_consume_arity x = x
let resource_provide_arity x = x

let location_cost          x = x

(* universe *)
let universe (u : universe) = 
    
  (* Prepare the available resources in form of a list. *)
  let resource_id_list = Resource_id_set.elements u#get_resource_ids in
    
  (* Component types. *)
  let component_type ct_id = (* TODO: fix the generation of that component type
    let ct = u#get_component_type ct_id in {
      Abstract_io.component_type_name     = component_type_id ct_id;
      Abstract_io.component_type_provide  = List.map (fun p -> (port_id p, provide_arity (ct#provide p))) (Port_id_set.elements ct#provide_domain);
      Abstract_io.component_type_require  = List.map (fun p -> (port_id p, require_arity (ct#require p))) (Port_id_set.elements ct#require_domain);
      Abstract_io.component_type_conflict = List.map port_id                                              (Port_id_set.elements (ct#conflict));
      Abstract_io.component_type_consume  = List.map (fun r -> (resource_id r, resource_consume_arity (ct#consume r))) resource_id_list
    } *) {
      Abstract_io.component_type_name     = "toto";
      Abstract_io.component_type_states = Abstract_io.Without_state ({
        Abstract_io.provide = [];
        Abstract_io.require = [];
        Abstract_io.conflict = []
      });
      Abstract_io.component_type_consume = []
   } in

  (* Packages. *)
  let package k_id = 
    let k = u#get_package k_id in {
      Abstract_io.package_name     = package_id k_id;
      Abstract_io.package_depend   = List.map (fun k_ids -> List.map package_id (Package_id_set.elements k_ids)) (Package_id_set_set.elements k#depend);
      Abstract_io.package_conflict = List.map package_id                                                         (Package_id_set    .elements k#conflict);
      Abstract_io.package_consume  = List.map (fun r -> (resource_id r, resource_consume_arity (k#consume r))) resource_id_list;
    } in

  (* Repositories. *)
  let repository r_id = 
    let r = u#get_repository r_id in {
      Abstract_io.repository_name     = repository_id r_id;
      Abstract_io.repository_packages = List.map package (Package_id_set.elements (r#package_ids));
    } in

  (* Implementation. *)
  let implementation_of_a_component_type ct_id =
    let package_get_name_full k_id = (repository_id (u#repository_of_package k_id), package_id k_id) in 
    (component_type_id ct_id, List.map package_get_name_full (Package_id_set.elements (u#get_implementation ct_id))) in

  (* The universe: *)
  {
    Abstract_io.universe_component_types = List.map component_type                     (Component_type_id_set.elements u#get_component_type_ids);
    Abstract_io.universe_implementation  = [](*List.map implementation_of_a_component_type (Component_type_id_set.elements u#get_component_type_ids)*);
    Abstract_io.universe_repositories    = List.map repository                         (Repository_id_set    .elements u#get_repository_ids);
    Abstract_io.universe_port_hierarchy  = []; (* TODO *)
  }


(* configuration *)
let configuration (u : universe) (c : configuration) =

  (* Prepare the available resources in form of a list. *)
  let resource_id_list = Resource_id_set.elements u#get_resource_ids in

  (* Locations. *)
  let location l_id = 
    let l = c#get_location l_id in {
      Abstract_io.location_name               = location_id l_id;
      Abstract_io.location_repository         = repository_id l#repository;
      Abstract_io.location_packages_installed = List.map package_id (Package_id_set.elements l#packages_installed);
      Abstract_io.location_provide_resources  = List.map (fun r -> (resource_id r, resource_provide_arity (l#provide_resources r))) resource_id_list;
      Abstract_io.location_cost               = location_cost l#cost
    } in

  (* Components. *)
  let component c_id = 
    let c = c#get_component c_id in {
      Abstract_io.component_name     = component_id      c_id;
      Abstract_io.component_type     = component_type_id c#typ;
      Abstract_io.component_location = location_id       c#location
    } in

  (* Bindings. *)
  let binding b = {
      Abstract_io.binding_port_provided  = port_id      b#port_provided;
      Abstract_io.binding_provider = component_id b#provider;
      Abstract_io.binding_port_required  = port_id      b#port_required;
      Abstract_io.binding_requirer = component_id b#requirer
  } in

  (* The configuration: *)
  {
    Abstract_io.configuration_locations  = List.map location  (Location_id_set.elements  c#get_location_ids);
    Abstract_io.configuration_components = List.map component (Component_id_set.elements c#get_component_ids);
    Abstract_io.configuration_bindings   = List.map binding   (Binding_set.elements      c#get_bindings)
  }


