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
    - datatypes/Data_model (obviously)
    - datatypes/Data_state (for storing. Maybe a better aternative can be found)
*)


open Data_model



(** universe *)


(* TODO: inductively compute all data and store it in the Data_state file. Here, we recompute the same things over and over again *)

(** component_type *)

module Component_type_name_map_to_key_set = Component_type_name_map.Set_of_key(Component_type_name_set)
module Component_type_name_map_to_value_component_set = Component_type_name_map.Set_of_value(Component_type_set)

(* TODO: right now, only fetch the full model. I don't know if we should keep the different variant of the universe in Data_state, or replace it with each trim *)
(* TODO: another possibility is to have a class/module/structure that, when being instanciated, automatically generate all data from its universe and config *)
let get_component_type_names universe = match !Data_state.component_type_name_set_full with
  | Some(set) -> set
  | None      -> let res = Component_type_name_map_to_key_set.set_of_key universe.universe_component_types in
    Data_state.component_type_name_set_full := Some(res); res

let get_component_types universe = match !Data_state.component_type_set_full with
  | Some(set) -> set
  | None      -> let res = Component_type_name_map_to_value_component_set.set_of_value universe.universe_component_types in
    Data_state.component_type_set_full := Some(res); res

let get_component_type universe component_type_name = try Component_type_name_map.find component_type_name universe.universe_component_types with
  | Not_found -> Zephyrus_log.log_missing_data "component type" component_type_name "universe"; exit(-1)


(** port *)

module Port_name_map_to_key_set = Port_name_map.Set_of_key(Port_name_set)

let port_is_provide_strict prov = match prov with
  | FiniteProvide i -> i > 0
  | InfiniteProvide -> true

let get_port_names universe =
  let component_types = get_component_types universe in
  Component_type_set.fold (fun t res ->
      Port_name_set.union res (
      Port_name_set.union t.component_type_conflict (
      Port_name_set.union (Port_name_map_to_key_set.set_of_key t.component_type_provide)
        (Port_name_map_to_key_set.set_of_key t.component_type_require)))) component_types Port_name_set.empty

let get_provide_arity component_type port_name =
  try Port_name_map.find port_name component_type.component_type_provide with
  | Not_found -> Zephyrus_log.log_missing_data "provided port" port_name ("component \"" ^ component_type.component_type_name ^ "\""); exit(-1)
(*  | Not_found -> (FiniteProvide 0) *)

let get_require_arity component_type port_name =
  try Port_name_map.find port_name component_type.component_type_require  with
  | Not_found -> Zephyrus_log.log_missing_data "required port" port_name ("component \"" ^ component_type.component_type_name ^ "\""); exit(-1)
(*  | Not_found -> 0 *)

let is_in_conflict component_type port_name = Port_name_set.mem port_name component_type.component_type_conflict

let requirers universe port_name =
  let component_types : Component_type_set.t = get_component_types universe in
  Component_type_set.filter
    (fun t -> if Port_name_map.mem port_name t.component_type_require then get_require_arity t port_name > 0 else false) component_types

let providers universe port_name =
  let component_types : Component_type_set.t = get_component_types universe in
  Component_type_set.filter
    (fun t -> if Port_name_map.mem port_name t.component_type_provide then port_is_provide_strict (get_provide_arity t port_name) else false) component_types

let conflicters universe port_name =
  let component_types : Component_type_set.t = get_component_types universe in
  Component_type_set.filter (fun component_type -> is_in_conflict component_type port_name) component_types



(** repository *)

module Repository_name_map_to_key_set = Repository_name_map.Set_of_key(Repository_name_set)
module Repository_name_map_to_value_repository_set = Repository_name_map.Set_of_value(Repository_set)

let get_repository_names universe = Repository_name_map_to_key_set.set_of_key universe.universe_repositories
let get_repositories universe = Repository_name_map_to_value_repository_set.set_of_value universe.universe_repositories

let get_repository universe repository_name = try Repository_name_map.find repository_name universe.universe_repositories with
  | Not_found -> Zephyrus_log.log_missing_data "repository" repository_name "universe"; exit(-1)


(** package *)

module Package_name_map_to_key_set = Package_name_map.Set_of_key(Package_name_set)
module Package_name_map_to_value_package_set = Package_name_map.Set_of_value(Package_set)

let get_repository_package_names repository = Package_name_map_to_key_set.set_of_key repository.repository_packages
let get_repository_packages repository =  Package_name_map_to_value_package_set.set_of_value repository.repository_packages

let get_package_names universe = Repository_set.fold (fun r res -> Package_name_set.union res (get_repository_package_names r)) (get_repositories universe) Package_name_set.empty
let get_packages universe = Repository_set.fold (fun r res -> Package_set.union (get_repository_packages r) res) (get_repositories universe) Package_set.empty

let is_package_in_repository repository k = Package_name_map.mem k.package_name repository.repository_packages

let get_repository_package repository package_name =
  try Package_name_map.find package_name repository.repository_packages with
  | Not_found -> Zephyrus_log.log_missing_data "package" package_name ("repository \"" ^ repository.repository_name ^ "\""); exit(-1)

let get_component_type_implementation universe component_type_name =
  try Component_type_name_map.find component_type_name universe.universe_implementation with
  (* If this component type is not on the universe implementation list, 
   * this does not mean it does not exist, but that it simply does not
   * need any packages to implement it. *)
  | Not_found -> Package_name_set.empty
  (* Alternative interpretation: *)
  (* does_not_exist (ComponentTypeImplementationInUniverse (component_type_name)) *)




(** resource *)

module Resource_name_map_to_key_set = Resource_name_map.Set_of_key(Resource_name_set)

let get_resource_names universe =
  let tmp = Component_type_set.fold (fun t res -> Resource_name_map_to_key_set.set_of_key t.component_type_consume) (get_component_types universe) Resource_name_set.empty in
  Package_set.fold (fun k res -> Resource_name_map_to_key_set.set_of_key k.package_consume) (get_packages universe) tmp

let get_component_type_resource_consumption component_type resource_name =
  try Resource_name_map.find resource_name component_type.component_type_consume with
  | Not_found -> 0

let get_package_resource_consumption package resource_name =
  try Resource_name_map.find resource_name package.package_consume with
  | Not_found -> 0


(** configuration *)


(** component *)

module Component_name_map_to_value_component_set = Component_name_map.Set_of_value(Component_set)

let get_components configuration = Component_name_map_to_value_component_set.set_of_value configuration.configuration_components

(** location *)

module Location_name_map_to_key_set = Location_name_map.Set_of_key(Location_name_set)
module Location_name_map_to_value_location_set = Location_name_map.Set_of_value(Location_set)

let get_location_names configuration = Location_name_map_to_key_set.set_of_key configuration.configuration_locations
let get_locations configuration =  Location_name_map_to_value_location_set.set_of_value configuration.configuration_locations

let get_location configuration location_name = 
  try Location_name_map.find location_name configuration.configuration_locations with
  | Not_found -> Zephyrus_log.log_missing_data "location" location_name "configuration"; exit(-1)

let get_location_components configuration location_name =
  let components : Component_set.t = get_components configuration in
  Component_set.filter (fun component -> component.component_location = location_name) components

let get_location_packages_installed configuration location_name =
  let location = get_location configuration location_name in location.location_packages_installed

let get_location_resource_provide_arity location resource_name =
  try Resource_name_map.find resource_name location.location_provide_resources with
  | Not_found -> 0




