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

open ExtLib

open Helpers


(** handling errors when something demanded does not exist *)
type something_does_not_exist =
  | ComponentTypeInUniverse                of component_type_name
  | ComponentTypeImplementationInUniverse  of component_type_name
  | RepositoryInUniverse                   of repository_name
  | PackageInRepository                    of package_name * repository_name
  | LocationInTheConfiguration             of location_name

let does_not_exist what = 
  let fail_message =
    match what with
    | ComponentTypeInUniverse (component_type_name) -> 
        Printf.sprintf 
          "the component type %s does not exist in this universe" 
          (string_of_component_type_name component_type_name)

    | ComponentTypeImplementationInUniverse (component_type_name) -> 
        Printf.sprintf 
          "the component type %s implementation does not exist in this universe" 
          (string_of_component_type_name component_type_name)

    | RepositoryInUniverse (repository_name) ->
        Printf.sprintf 
          "the repository %s does not exist in this universe" 
          (string_of_repository_name repository_name)

    | PackageInRepository (package_name, repository_name) ->
        Printf.sprintf 
          "the package %s does not exist in the repository %s" 
          (string_of_package_name    package_name)
          (string_of_repository_name repository_name)

    | LocationInTheConfiguration (location_name) ->
        Printf.sprintf 
          "the location %s does not exist in this configuration" 
          (string_of_location_name location_name)
  in
  failwith fail_message



(** universe *)


(** component_type *)

let get_component_type_names universe =
  let module Component_type_name_set_of_map_keys =
    Set_of_map_keys(Component_type_name_map)(Component_type_name_set)
  in
  Component_type_name_set_of_map_keys.set_of_map_keys universe.universe_component_types

let get_component_types universe = 
  let module Component_type_set_of_component_type_name_map_values =
    Set_of_map_values(Component_type_name_map)(Component_type_set)
  in
  Component_type_set_of_component_type_name_map_values.set_of_map_values universe.universe_component_types

let get_component_type universe component_type_name =
  try
    Component_type_name_map.find component_type_name universe.universe_component_types
  with
  | Not_found -> 
      does_not_exist (ComponentTypeInUniverse (component_type_name))


(** port *)

let get_port_names universe =
  let component_types : Component_type_set.t = get_component_types universe
  in
  let module Port_name_set_set_of_component_type_set =
    Set_of_set(Component_type_set)(Port_name_set_set)
  in
  let port_name_sets : Port_name_set_set.t = Port_name_set_set_of_component_type_set.convert (function component_type -> 
    let module Port_name_set_of_map_keys =
      Set_of_map_keys(Port_name_map)(Port_name_set)
    in
    let provide_port_set  : Port_name_set.t = Port_name_set_of_map_keys.set_of_map_keys component_type.component_type_provide
    and require_port_set  : Port_name_set.t = Port_name_set_of_map_keys.set_of_map_keys component_type.component_type_require
    and conflict_port_set : Port_name_set.t = component_type.component_type_conflict
    in
    List.fold_left Port_name_set.union Port_name_set.empty [provide_port_set; require_port_set; conflict_port_set]

  ) component_types
  in
  Port_name_set_set.fold Port_name_set.union port_name_sets Port_name_set.empty

let get_provide_arity component_type port_name =
  try
     Port_name_map.find port_name component_type.component_type_provide
  with
  | Not_found -> (FiniteProvide 0)

let get_require_arity component_type port_name =
  try
    Port_name_map.find port_name component_type.component_type_require
  with
  | Not_found -> 0

let is_in_conflict component_type port_name =
  Port_name_set.mem port_name component_type.component_type_conflict

let requirers universe port_name =
  let component_types : Component_type_set.t = get_component_types universe
  in
  Component_type_set.filter (fun component_type ->
    get_require_arity component_type port_name > 0
  ) component_types

let providers universe port_name =
  let component_types : Component_type_set.t = get_component_types universe
  in
  Component_type_set.filter (fun component_type ->
    match get_provide_arity component_type port_name with
    | FiniteProvide i -> i > 0
    | InfiniteProvide -> true
  ) component_types

let conflicters universe port_name =
  let component_types : Component_type_set.t = get_component_types universe
  in
  Component_type_set.filter (fun component_type ->
    is_in_conflict component_type port_name
  ) component_types


(** repository *)

let get_repository_names universe =
  let module Repository_name_set_of_map_keys =
    Set_of_map_keys(Repository_name_map)(Repository_name_set)
  in
  Repository_name_set_of_map_keys.set_of_map_keys universe.universe_repositories

let get_repositories universe =
  let module Repository_set_of_repository_name_map_values =
    Set_of_map_values(Repository_name_map)(Repository_set)
  in
  Repository_set_of_repository_name_map_values.set_of_map_values universe.universe_repositories

let get_repository universe repository_name =
  try
    Repository_name_map.find repository_name universe.universe_repositories
  with
  | Not_found -> 
      does_not_exist (RepositoryInUniverse (repository_name))


(** package *)

let get_repository_package_names repository =
  let module Package_name_set_of_map_keys =
    Set_of_map_keys(Package_name_map)(Package_name_set)
  in
  Package_name_set_of_map_keys.set_of_map_keys repository.repository_packages

let get_repository_packages repository =
  let module Package_set_of_package_name_map_values =
    Set_of_map_values(Package_name_map)(Package_set)
  in
  Package_set_of_package_name_map_values.set_of_map_values repository.repository_packages

let get_package_names universe =
  let get_all_package_names_from_package package =
    let package_name                   : Package_name_set.t = Package_name_set.singleton package.package_name
    and packages_mentioned_in_depend   : Package_name_set.t = Package_name_set_set.fold Package_name_set.union package.package_depend Package_name_set.empty
    and packages_mentioned_in_conflict : Package_name_set.t = package.package_conflict
    in
    List.fold_left Package_name_set.union package_name [packages_mentioned_in_depend; packages_mentioned_in_conflict]
  in
  let repositories : Repository_set.t = get_repositories universe
  in
  let module Package_name_set_set_of_repository_set =
    Set_of_set(Repository_set)(Package_name_set_set)
  in
  let package_sets : Package_name_set_set.t =
    Package_name_set_set_of_repository_set.convert (function repository ->
      let packages : Package_set.t = get_repository_packages repository
      in
      let module Package_name_set_set_of_package_set =
        Set_of_set(Package_set)(Package_name_set_set)
      in
      let package_sets : Package_name_set_set.t = 
        Package_name_set_set_of_package_set.convert get_all_package_names_from_package packages
      in
      Package_name_set_set.fold Package_name_set.union package_sets Package_name_set.empty

    ) repositories
  in
  Package_name_set_set.fold Package_name_set.union package_sets Package_name_set.empty

let get_packages universe =
  let repositories : Repository_set.t = get_repositories universe
  in
  let module Package_set_set_of_repository_set =
    Set_of_set(Repository_set)(Package_set_set)
  in
  let package_sets : Package_set_set.t = 
    Package_set_set_of_repository_set.convert get_repository_packages repositories
  in
  Package_set_set.fold Package_set.union package_sets Package_set.empty

let get_repository_package repository package_name =
  try
    Package_name_map.find package_name repository.repository_packages
  with
  | Not_found -> 
      does_not_exist (PackageInRepository (package_name, repository.repository_name))

let get_component_type_implementation universe component_type_name =
  try
    Component_type_name_map.find component_type_name universe.universe_implementation
  with
  (* If this component type is not on the universe implementation list, 
   * this does not mean it does not exist, but that it simply does not
   * need any packages to implement it. *)
  | Not_found -> Package_name_set.empty

  (* Alternative interpretation: *)
  (* does_not_exist (ComponentTypeImplementationInUniverse (component_type_name)) *)

(** resource *)

let get_resource_names universe =

  let module Resource_name_set_of_map_keys =
    Set_of_map_keys(Resource_name_map)(Resource_name_set)

  in

  (* Resource names mentioned in all component types. *)
  let resource_names_from_component_types : Resource_name_set.t = 

    let component_types = get_component_types universe
    in
    let module Resource_name_set_set_of_component_type_set =
      Set_of_set(Component_type_set)(Resource_name_set_set)
    in
    let resource_name_sets : Resource_name_set_set.t = 
      Resource_name_set_set_of_component_type_set.convert (fun component_type -> 
        Resource_name_set_of_map_keys.set_of_map_keys component_type.component_type_consume
      ) component_types
    in
    Resource_name_set_set.fold Resource_name_set.union resource_name_sets Resource_name_set.empty

  (* Resource names mentioned in all packages. *)
  and resource_names_from_packages : Resource_name_set.t = 

    let packages = get_packages universe
    in
    let module Resource_name_set_set_of_package_set =
      Set_of_set(Package_set)(Resource_name_set_set)
    in
    let resource_name_sets : Resource_name_set_set.t = 
      Resource_name_set_set_of_package_set.convert (fun package -> 
        Resource_name_set_of_map_keys.set_of_map_keys package.package_consume
      ) packages
    in
    Resource_name_set_set.fold Resource_name_set.union resource_name_sets Resource_name_set.empty

  in
  Resource_name_set.union
    resource_names_from_component_types
    resource_names_from_packages

let get_component_type_resource_consumption component_type resource_name =
  try
    Resource_name_map.find resource_name component_type.component_type_consume
  with
  | Not_found -> 0

let get_package_resource_consumption package resource_name =
  try
    Resource_name_map.find resource_name package.package_consume
  with
  | Not_found -> 0



(** configuration *)


(** component *)

let get_components configuration =
  let module Component_set_of_component_name_map_values =
    Set_of_map_values(Component_name_map)(Component_set)
  in
  Component_set_of_component_name_map_values.set_of_map_values configuration.configuration_components

(** location *)

let get_location_names configuration =
  let module Location_name_set_of_map_keys =
    Set_of_map_keys(Location_name_map)(Location_name_set)
  in
  Location_name_set_of_map_keys.set_of_map_keys configuration.configuration_locations

let get_locations configuration = 
  let module Location_set_of_location_name_map_values =
    Set_of_map_values(Location_name_map)(Location_set)
  in
  Location_set_of_location_name_map_values.set_of_map_values configuration.configuration_locations

let get_location configuration location_name = 
  try
    Location_name_map.find location_name configuration.configuration_locations
  with
  | Not_found -> 
      does_not_exist (LocationInTheConfiguration (location_name))

let get_location_components configuration location_name =
  let components : Component_set.t = get_components configuration
  in
  Component_set.filter (fun component -> 
    component.component_location = location_name
  ) components

let get_location_packages_installed configuration location_name =
  let location = get_location configuration location_name
  in
  location.location_packages_installed

let get_location_resource_provide_arity location resource_name =
  try
    Resource_name_map.find resource_name location.location_provide_resources
  with
  | Not_found -> 0
