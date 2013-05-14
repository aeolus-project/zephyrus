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


open Aeolus_types
open Aeolus_types_output_new.Plain

open ExtLib

open Helpers



(** universe *)


(** component_type *)

let get_component_type_names universe =
  let module ComponentTypeNameSetOfMapKeys =
    SetOfMapKeys(ComponentTypeNameMap)(ComponentTypeNameSet)
  in
  ComponentTypeNameSetOfMapKeys.set_of_map_keys universe.universe_component_types

let get_component_types universe = 
  let module ComponentTypeSetOfComponentTypeNameMapValues =
    SetOfMapValues(ComponentTypeNameMap)(ComponentTypeSet)
  in
    ComponentTypeSetOfComponentTypeNameMapValues.set_of_map_values universe.universe_component_types

let get_component_type universe component_type_name =
  try
    ComponentTypeNameMap.find component_type_name universe.universe_component_types
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the component type %s does not exist in this universe" 
        (string_of_component_type_name component_type_name))


(** port *)

let get_port_names universe =
  let component_types : ComponentTypeSet.t = get_component_types universe
  in
  let module PortNameSetSetOfComponentTypeSet =
    SetOfSet(ComponentTypeSet)(PortNameSetSet)
  in
  let port_name_sets : PortNameSetSet.t = PortNameSetSetOfComponentTypeSet.convert (function component_type -> 
    let module PortNameSetOfMapKeys =
      SetOfMapKeys(PortNameMap)(PortNameSet)
    in
    let provide_port_set  : PortNameSet.t = PortNameSetOfMapKeys.set_of_map_keys component_type.component_type_provide
    and require_port_set  : PortNameSet.t = PortNameSetOfMapKeys.set_of_map_keys component_type.component_type_require
    and conflict_port_set : PortNameSet.t = component_type.component_type_conflict
    in
    List.fold_left PortNameSet.union PortNameSet.empty [provide_port_set; require_port_set; conflict_port_set]

  ) component_types
  in
  PortNameSetSet.fold PortNameSet.union port_name_sets PortNameSet.empty

let get_provide_arity component_type port_name =
  try
     PortNameMap.find port_name component_type.component_type_provide
  with
  | Not_found -> (FiniteProvide 0)

let get_require_arity component_type port_name =
  try
    PortNameMap.find port_name component_type.component_type_require
  with
  | Not_found -> 0

let is_in_conflict component_type port_name =
  PortNameSet.mem port_name component_type.component_type_conflict

let requirers universe port_name =
  let component_type_names : ComponentTypeNameSet.t = get_component_type_names universe
  in
  ComponentTypeNameSet.filter (fun component_type_name ->
    let component_type = get_component_type universe component_type_name
    in
    get_require_arity component_type port_name > 0

  ) component_type_names

let providers universe port_name =
  let component_type_names : ComponentTypeNameSet.t = get_component_type_names universe
  in
  ComponentTypeNameSet.filter (fun component_type_name ->
    let component_type = get_component_type universe component_type_name
    in
    match get_provide_arity component_type port_name with
    | FiniteProvide i -> i > 0
    | InfiniteProvide -> true

  ) component_type_names

let conflicters universe port_name =
  let component_type_names : ComponentTypeNameSet.t = get_component_type_names universe
  in
  ComponentTypeNameSet.filter (fun component_type_name ->
    let component_type = get_component_type universe component_type_name
    in
    is_in_conflict component_type port_name

  ) component_type_names


(** repository *)

let get_repository_names universe =
  let module RepositoryNameSetOfMapKeys =
    SetOfMapKeys(RepositoryNameMap)(RepositoryNameSet)
  in
  RepositoryNameSetOfMapKeys.set_of_map_keys universe.universe_repositories

let get_repositories universe =
  let module RepositorySetOfRepositoryNameMapValues =
    SetOfMapValues(RepositoryNameMap)(RepositorySet)
  in
  RepositorySetOfRepositoryNameMapValues.set_of_map_values universe.universe_repositories

let get_repository universe repository_name =
  try
    RepositoryNameMap.find repository_name universe.universe_repositories
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the repository %s does not exist in this universe" 
        (string_of_repository_name repository_name))


(** package *)

let get_repository_package_names repository =
  let module PackageNameSetOfMapKeys =
    SetOfMapKeys(PackageNameMap)(PackageNameSet)
  in
  PackageNameSetOfMapKeys.set_of_map_keys repository.repository_packages

let get_repository_packages repository =
  let module PackageSetOfPackageNameMapValues =
    SetOfMapValues(PackageNameMap)(PackageSet)
  in
  PackageSetOfPackageNameMapValues.set_of_map_values repository.repository_packages

let get_package_names universe =
  let get_all_package_names_from_package package =
    let package_name                   : PackageNameSet.t = PackageNameSet.singleton package.package_name
    and packages_mentioned_in_depend   : PackageNameSet.t = PackageNameSetSet.fold PackageNameSet.union package.package_depend PackageNameSet.empty
    and packages_mentioned_in_conflict : PackageNameSet.t = package.package_conflict
    in
    List.fold_left PackageNameSet.union package_name [packages_mentioned_in_depend; packages_mentioned_in_conflict]
  in
  let repositories : RepositorySet.t = get_repositories universe
  in
  let module PackageNameSetSetOfRepositorySet =
    SetOfSet(RepositorySet)(PackageNameSetSet)
  in
  let package_sets : PackageNameSetSet.t =
    PackageNameSetSetOfRepositorySet.convert (function repository ->
      let packages : PackageSet.t = get_repository_packages repository
      in
      let module PackageNameSetSetOfPackageSet =
        SetOfSet(PackageSet)(PackageNameSetSet)
      in
      let package_sets : PackageNameSetSet.t = 
        PackageNameSetSetOfPackageSet.convert get_all_package_names_from_package packages
      in
      PackageNameSetSet.fold PackageNameSet.union package_sets PackageNameSet.empty

    ) repositories
  in
  PackageNameSetSet.fold PackageNameSet.union package_sets PackageNameSet.empty

let get_packages universe =
  let repositories : RepositorySet.t = get_repositories universe
  in
  let module PackageSetSetOfRepositorySet =
    SetOfSet(RepositorySet)(PackageSetSet)
  in
  let package_sets : PackageSetSet.t = 
    PackageSetSetOfRepositorySet.convert get_repository_packages repositories
  in
  PackageSetSet.fold PackageSet.union package_sets PackageSet.empty

let get_repository_package repository package_name =
  try
    PackageNameMap.find package_name repository.repository_packages
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the package %s does not exist in this repository" 
        (string_of_package_name package_name))

let get_component_type_implementation universe component_type_name =
  try
    ComponentTypeNameMap.find component_type_name universe.universe_implementation
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the component type %s does not exist in this universe" 
        (string_of_component_type_name component_type_name))


(** resource *)

let get_resource_names universe =

  let module ResourceNameSetOfMapKeys =
    SetOfMapKeys(ResourceNameMap)(ResourceNameSet)

  in

  (* Resource names mentioned in all component types. *)
  let resource_names_from_component_types : ResourceNameSet.t = 

    let component_types = get_component_types universe
    in
    let module ResourceNameSetSetOfComponentTypeSet =
      SetOfSet(ComponentTypeSet)(ResourceNameSetSet)
    in
    let resource_name_sets : ResourceNameSetSet.t = 
      ResourceNameSetSetOfComponentTypeSet.convert (fun component_type -> 
        ResourceNameSetOfMapKeys.set_of_map_keys component_type.component_type_consume
      ) component_types
    in
    ResourceNameSetSet.fold ResourceNameSet.union resource_name_sets ResourceNameSet.empty

  (* Resource names mentioned in all packages. *)
  and resource_names_from_packages : ResourceNameSet.t = 

    let packages = get_packages universe
    in
    let module ResourceNameSetSetOfPackageSet =
      SetOfSet(PackageSet)(ResourceNameSetSet)
    in
    let resource_name_sets : ResourceNameSetSet.t = 
      ResourceNameSetSetOfPackageSet.convert (fun package -> 
        ResourceNameSetOfMapKeys.set_of_map_keys package.package_consume
      ) packages
    in
    ResourceNameSetSet.fold ResourceNameSet.union resource_name_sets ResourceNameSet.empty

  in
  ResourceNameSet.union
    resource_names_from_component_types
    resource_names_from_packages

let get_component_type_resource_consumption component_type resource_name =
  try
    ResourceNameMap.find resource_name component_type.component_type_consume
  with
  | Not_found -> 0

let get_package_resource_consumption package resource_name =
  try
    ResourceNameMap.find resource_name package.package_consume
  with
  | Not_found -> 0



(** configuration *)


(** component *)

let get_components configuration =
  let module ComponentSetOfComponentNameMapValues =
    SetOfMapValues(ComponentNameMap)(ComponentSet)
  in
  ComponentSetOfComponentNameMapValues.set_of_map_values configuration.configuration_components

(** location *)

let get_location_names configuration =
  let module LocationNameSetOfMapKeys =
    SetOfMapKeys(LocationNameMap)(LocationNameSet)
  in
  LocationNameSetOfMapKeys.set_of_map_keys configuration.configuration_locations

let get_locations configuration = 
  let module LocationSetOfLocationNameMapValues =
    SetOfMapValues(LocationNameMap)(LocationSet)
  in
  LocationSetOfLocationNameMapValues.set_of_map_values configuration.configuration_locations

let get_location configuration location_name = 
  try
    LocationNameMap.find location_name configuration.configuration_locations
  with
  | Not_found -> 
      failwith 
        (Printf.sprintf 
        "the location %s does not exist in this configuration" 
        (string_of_location_name location_name))

let get_location_components configuration location_name =
  let components : ComponentSet.t = get_components configuration
  in
  ComponentSet.filter (fun component -> 
    component.component_location = location_name
  ) components

let get_location_packages_installed configuration location_name =
  let location = get_location configuration location_name
  in
  location.location_packages_installed

let get_location_resource_provide_arity location resource_name =
  try
    ResourceNameMap.find resource_name location.location_provide_resources
  with
  | Not_found -> 0



(*
let consumers universe resource_name =
  let component_types =
    List.filter_map (fun component_type ->
      if List.exists (fun (consumed_resource_name, resource_consumption) ->
           (consumed_resource_name = resource_name) && (resource_consumption > 0)
         ) component_type.component_type_consume
      then Some (component_type.component_type_name)
      else None
    ) universe.universe_component_types

  and packages =
    List.filter_map (fun component_type ->
      if List.exists (fun (consumed_resource_name, resource_consumption) ->
           (consumed_resource_name = resource_name) && (resource_consumption > 0)
         ) component_type.component_type_consume
      then Some (component_type.component_type_name)
      else None
    ) universe.universe_component_types
*)
