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

(****************************************************)
(******************** DEPRECATED ********************)
(****************************************************)

(* Depends on
    - datatypes/Data_model (obviously)
    - datatypes/Data_state (for storing. Maybe a better aternative can be found)
*)

open Data_model


(*  let get_provide_arity component_type port_name =
    try Port_name_map.find port_name component_type.component_type_provide with
    | Not_found -> Zephyrus_log.log_missing_data "provided port" port_name ("component \"" ^ component_type.component_type_name ^ "\""); exit(-1)

  let get_require_arity component_type port_name =
    try Port_name_map.find port_name component_type.component_type_require  with
    | Not_found -> Zephyrus_log.log_missing_data "required port" port_name ("component \"" ^ component_type.component_type_name ^ "\""); exit(-1)

  let is_in_conflict component_type port_name = Port_name_set.mem port_name component_type.component_type_conflict
*)

  let port_is_provide_strict prov = match prov with
    | Finite_provide i -> i > 0
    | Infinite_provide -> true


  let requirers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
    (fun id t -> if Port_id_set.mem port_id t#require_domain then (t#require port_id) > 0 else false) component_types)

  let providers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
    (fun id t -> if Port_id_set.mem port_id t#provide_domain then port_is_provide_strict (t#provide port_id) else false) component_types)

  let conflicters component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
    (fun id t -> Port_id_set.mem port_id (t#conflict)) component_types)


(*


module Core = struct (** Core module with all functionalities partially defined *)
  open Data_model

  (** 1. universe *)

  (** 1.1. component_type *)

  module Component_type_name_map_to_key_set = Component_type_name_map.Set_of_key(Component_type_name_set)
  module Component_type_name_map_to_value_component_set = Component_type_name_map.Set_of_value(Component_type_set)

  let get_component_type_names universe_component_types = Component_type_name_map_to_key_set.set_of_key universe_component_types
  let get_component_types universe_component_types = Component_type_name_map_to_value_component_set.set_of_value universe_component_types

  let get_component_type universe_component_types component_type_name = try Component_type_name_map.find component_type_name universe_component_types with
    | Not_found -> Zephyrus_log.log_missing_data "component type" component_type_name "universe"; exit(-1)


  (** 1.2. port *)

  module Port_name_map_to_key_set = Port_name_map.Set_of_key(Port_name_set)

  let port_is_provide_strict prov = match prov with
    | Finite_provide i -> i > 0
    | Infinite_provide -> true

  let get_port_names component_types =
    Component_type_set.fold (fun t res ->
        Port_name_set.union res (
        Port_name_set.union t.component_type_conflict (
        Port_name_set.union (Port_name_map_to_key_set.set_of_key t.component_type_provide)
          (Port_name_map_to_key_set.set_of_key t.component_type_require)))) component_types Port_name_set.empty

  let get_provide_arity component_type port_name =
    try Port_name_map.find port_name component_type.component_type_provide with
    | Not_found -> Zephyrus_log.log_missing_data "provided port" port_name ("component \"" ^ component_type.component_type_name ^ "\""); exit(-1)

  let get_require_arity component_type port_name =
    try Port_name_map.find port_name component_type.component_type_require  with
    | Not_found -> Zephyrus_log.log_missing_data "required port" port_name ("component \"" ^ component_type.component_type_name ^ "\""); exit(-1)

  let is_in_conflict component_type port_name = Port_name_set.mem port_name component_type.component_type_conflict

  let requirers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
    (fun id t -> if Port_id_map.mem port_id t.component_type_require then get_require_arity t port_id > 0 else false) component_types)

  let providers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
    (fun id t -> if Port_id_map.mem port_id t.component_type_provide then port_is_provide_strict (get_provide_id t port_name) else false) component_types)

  let conflicters component_types port_name = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
    (fun id t -> is_in_conflict component_type port_id) component_types)



  (** 1.3. repository *)

  module Repository_name_map_to_key_set = Repository_name_map.Set_of_key(Repository_name_set)
  module Repository_name_map_to_value_repository_set = Repository_name_map.Set_of_value(Repository_set)

  let get_repository_names universe_repositories = Repository_name_map_to_key_set.set_of_key universe_repositories
  let get_repositories universe_repositories = Repository_name_map_to_value_repository_set.set_of_value universe_repositories

  let get_repository universe_repositories repository_name = try Repository_name_map.find repository_name universe_repositories with
    | Not_found -> Zephyrus_log.log_missing_data "repository" repository_name "universe"; exit(-1)


  (** 1.4. package *)

  module Package_name_map_to_key_set = Package_name_map.Set_of_key(Package_name_set)
  module Package_name_map_to_value_package_set = Package_name_map.Set_of_value(Package_set)

  let get_repository_package_names repository = Package_name_map_to_key_set.set_of_key repository.repository_packages
  let get_repository_packages repository =  Package_name_map_to_value_package_set.set_of_value repository.repository_packages

  let get_package_names get_repositories_universe = Repository_set.fold
    (fun r res -> Package_name_set.union res (get_repository_package_names r)) get_repositories_universe Package_name_set.empty
  let get_packages get_repositories_universe = Repository_set.fold
    (fun r res -> Package_set.union (get_repository_packages r) res) get_repositories_universe Package_set.empty

  let is_package_in_repository repository k = Package_name_map.mem k.package_name repository.repository_packages

  let get_repository_package repository package_name =
    try Package_name_map.find package_name repository.repository_packages with
    | Not_found -> Zephyrus_log.log_missing_data "package" package_name ("repository \"" ^ repository.repository_name ^ "\""); exit(-1)

  let get_component_type_implementation universe_implementation component_type_name =
    try Component_type_name_map.find component_type_name universe_implementation with
    (* If this component type is not on the universe implementation list, 
     * this does not mean it does not exist, but that it simply does not
     * need any packages to implement it. *)
    | Not_found -> Package_name_set.empty
    (* Alternative interpretation: *)
    (* does_not_exist (ComponentTypeImplementationInUniverse (component_type_name)) *)


  (** 1.5. resource *)

  module Resource_name_map_to_key_set = Resource_name_map.Set_of_key(Resource_name_set)

  let get_resource_names get_component_types_universe get_packages_universe =
    let tmp = Component_type_set.fold (fun t res -> Resource_name_map_to_key_set.set_of_key t.component_type_consume) (get_component_types_universe) Resource_name_set.empty in
    Package_set.fold (fun k res -> Resource_name_map_to_key_set.set_of_key k.package_consume) (get_packages_universe) tmp

  let get_component_type_resource_consumption component_type resource_name =
    try Resource_name_map.find resource_name component_type.component_type_consume with
    | Not_found -> 0

  let get_package_resource_consumption package resource_name =
    try Resource_name_map.find resource_name package.package_consume with
    | Not_found -> 0


  (** 2. configuration *)


  (** 2.1. component *)

  module Component_name_map_to_key_set = Component_name_map.Set_of_key(Component_name_set)
  module Component_name_map_to_value_component_set = Component_name_map.Set_of_value(Component_set)

  let get_component_names configuration_components = Component_name_map_to_key_set.set_of_key configuration_components
  let get_components configuration_components = Component_name_map_to_value_component_set.set_of_value configuration_components

  let get_local_component l t map = 
    let tmp = Component_name_map.filter (fun _ c -> (c.component_location = l) & (c.component_type = t)) map in
    Component_name_map_to_key_set.set_of_key tmp

  (** 2.2. location *)

  module Location_name_map_to_key_set = Location_name_map.Set_of_key(Location_name_set)
  module Location_name_map_to_value_location_set = Location_name_map.Set_of_value(Location_set)

  let get_location_names configuration_locations = Location_name_map_to_key_set.set_of_key configuration_locations
  let get_locations configuration_locations =  Location_name_map_to_value_location_set.set_of_value configuration_locations

  let get_location configuration_locations location_name = 
    try Location_name_map.find location_name configuration_locations with
    | Not_found -> Zephyrus_log.log_missing_data "location" location_name "configuration"; exit(-1)

  let get_location_components components location_name = Component_set.filter (fun component -> component.component_location = location_name) components

  let get_location_packages_installed location = location.location_packages_installed

  let get_location_resource_provide_arity location resource_name =
    try Resource_name_map.find resource_name location.location_provide_resources with
    | Not_found -> 0

  let get_local_package l k map = try let l' = Location_name_map.find l map in Package_name_set.mem k l'.location_packages_installed with
   | Not_found -> false
     

end


module Plain = struct
  open Data_model

  (** 1. universe *)
  (** 1.1. Component types *)
  let get_component_type_names universe = Core.get_component_type_names universe.universe_component_types
  let get_component_types universe = Core.get_component_types universe.universe_component_types
  let get_component_type universe component_type_name = Core.get_component_type universe.universe_component_types component_type_name

  (** 1.2. ports *)
  let get_port_names universe = Core.get_port_names (get_component_types universe)
  let get_provide_arity = Core.get_provide_arity
  let get_require_arity = Core.get_require_arity
  let is_in_conflict = Core.is_in_conflict

  let requirers universe port_name = Core.requirers (get_component_types universe) port_name
  let providers universe port_name = Core.providers (get_component_types universe) port_name
  let conflicters universe port_name = Core.conflicters (get_component_types universe) port_name

  (** 1.3. repository *)
  let get_repository_names universe = Core.get_repository_names universe.universe_repositories
  let get_repositories universe = Core.get_repositories universe.universe_repositories
  let get_repository universe repository_name = Core.get_repository (universe.universe_repositories) repository_name

  (** 1.4. package *)
  let get_repository_package_names = Core.get_repository_package_names
  let get_repository_packages = Core.get_repository_packages

  let get_package_names universe = Core.get_package_names (get_repositories universe)
  let get_packages universe = Core.get_packages (get_repositories universe)

  let is_package_in_repository = Core.is_package_in_repository
  let get_repository_package = Core.get_repository_package

  let get_component_type_implementation universe component_type_name = Core.get_component_type_implementation universe.universe_implementation component_type_name

  (** 1.5. resource *)
  let get_resource_names universe = Core.get_resource_names (get_component_types universe) (get_packages universe)
  let get_component_type_resource_consumption = Core.get_component_type_resource_consumption
  let get_package_resource_consumption = Core.get_package_resource_consumption

  (** 2. configuration *)
  (** 2.1. component *)
  let get_components configuration = Core.get_components configuration.configuration_components

  (** 2.2. location *)
  let get_location_names configuration = Core.get_location_names configuration.configuration_locations
  let get_locations configuration =  Core.get_locations configuration.configuration_locations

  let get_location configuration location_name = Core.get_location configuration.configuration_locations location_name
  let get_location_components configuration location_name = Core.get_location_components (get_components configuration) location_name
  let get_location_packages_installed configuration location_name = Core.get_location_packages_installed (get_location configuration location_name)

  let get_location_resource_provide_arity = Core.get_location_resource_provide_arity

end



module Improved = struct

  module Component_type_names_of_component_types = Data_common.Set.Convert(Data_model.Component_type_set)(Data_model.Component_type_name_set)
  let component_type_names_of_component_types set = Component_type_names_of_component_types.convert (fun c -> c.Data_model.component_type_name) set

  class universe u = object(res)
    (* private *)
    val mutable implem_u_dt = None;
    val mutable implem_u_dp = None;
    val mutable implem_u_dr = None;
    val mutable implem_u_dk = None;
    
    val mutable implem_u_i = Data_model.Component_type_name_map.empty;
    val mutable implem_u_w = Data_model.Package_name_map.empty;

    val mutable implem_ur = Data_model.Port_name_map.empty;
    val mutable implem_up = Data_model.Port_name_map.empty;
    val mutable implem_uc = Data_model.Port_name_map.empty;

    val mutable implem_get_component_types = None;
    val mutable implem_get_repositories    = None;

    (* methods *)
    method universe_component_types = u.Data_model.universe_component_types;
    method universe_implementation  = u.Data_model.universe_implementation;
    method universe_repositories    = u.Data_model.universe_repositories;

    method get_component_types = (fun () -> match implem_get_component_types with | Some(set) -> set
             | None -> let tmp = Core.get_component_types res#universe_component_types in implem_get_component_types <- Some(tmp); tmp);
    method get_repositories    = (fun () -> match implem_get_repositories with | Some(set) -> set
             | None -> let tmp = Core.get_repositories res#universe_repositories in implem_get_repositories <- Some(tmp); tmp);

    method u_dt = (fun () -> match implem_u_dt with | Some(set) -> set
             | None -> let tmp = Core.get_component_type_names res#universe_component_types in implem_u_dt <- Some(tmp); tmp);
    method u_dp = (fun () -> match implem_u_dp with | Some(set) -> set
             | None -> let tmp = Core.get_port_names (res#get_component_types ()) in implem_u_dp <- Some(tmp); tmp);
    method u_dr = (fun () -> match implem_u_dr with | Some(set) -> set
             | None -> let tmp = Core.get_repository_names res#universe_repositories in implem_u_dr <- Some(tmp); tmp);
    method u_dk = (fun () -> match implem_u_dk with | Some(set) -> set
             | None -> let tmp = Core.get_package_names (res#get_repositories ()) in implem_u_dk <- Some(tmp); tmp);

    method u_i = (fun t -> try Data_model.Component_type_name_map.find t implem_u_i with
             | Not_found -> let tmp = Core.get_component_type_implementation res#universe_implementation t in 
               implem_u_i <- Data_model.Component_type_name_map.add t tmp implem_u_i; tmp);
    method u_w = (fun k -> (if implem_u_w = Data_model.Package_name_map.empty then (
       let add_repository r =
         let merge_function n k1 k2 = match (k1,k2) with
           | (Some(k1'), Some(k2')) -> Some(k1')
           | (Some(k1'), None     ) -> Some(k1')
           | (None     , Some(k2')) -> Some(k2')
           | (None     , None     ) -> None in implem_u_w <- Data_model.Package_name_map.merge merge_function implem_u_w r.Data_model.repository_packages in
        Data_model.Repository_set.iter add_repository (res#get_repositories ())));
            try Data_model.Package_name_map.find k implem_u_w with
             | Not_found -> Zephyrus_log.log_missing_data "package" k "universe"; exit(-1));

    method ur = (fun p -> try Data_model.Port_name_map.find p implem_ur with
             | Not_found -> let tmp = component_type_names_of_component_types (Core.requirers (res#get_component_types ()) p) in
               implem_ur <- Data_model.Port_name_map.add p tmp implem_ur; tmp);

    method up = (fun p -> try Data_model.Port_name_map.find p implem_up with
             | Not_found -> let tmp = component_type_names_of_component_types (Core.providers (res#get_component_types ()) p) in
               implem_up <- Data_model.Port_name_map.add p tmp implem_up; tmp);

    method uc = (fun p -> try Data_model.Port_name_map.find p implem_uc with
             | Not_found -> let tmp = component_type_names_of_component_types (Core.conflicters (res#get_component_types ()) p) in
               implem_uc <- Data_model.Port_name_map.add p tmp implem_uc; tmp);

    method get_component_type = (fun n -> Core.get_component_type res#universe_component_types n);
    method get_repository = (fun n -> Core.get_repository res#universe_repositories n);
    method get_package = res#u_w;
  end

(*
  (* 1. types extended with extra state to store important data, and functions to directly get them *)
  type universe = {
    universe_component_types : Data_model.component_type Data_model.Component_type_name_map.t;     (** Component types available in this universe. *)
    universe_implementation  : Data_model.Package_name_set.t Data_model.Component_type_name_map.t; (** Which packages can implement the component types of this universe. *)
    universe_repositories    : Data_model.repository Data_model.Repository_name_map.t;             (** Package repositories available in this universe. *)

    u_dt : unit -> Data_model.Component_type_name_set.t;
    u_dp : unit -> Data_model.Port_name_set.t;
    u_dr : unit -> Data_model.Repository_name_set.t;
    u_dk : unit -> Data_model.Package_name_set.t;
    
    u_i : Data_model.component_type_name -> Data_model.Package_name_set.t;
    u_w : Data_model.package_name -> Data_model.package;

    ur : Data_model.port_name -> Data_model.Component_type_name_set.t;
    up : Data_model.port_name -> Data_model.Component_type_name_set.t;
    uc : Data_model.port_name -> Data_model.Component_type_name_set.t;

    get_component_types : unit -> Data_model.Component_type_set.t;
    get_repositories    : unit -> Data_model.Repository_set.t;

    get_component_type : Data_model.component_type_name -> Data_model.component_type;
    get_repository     : Data_model.repository_name -> Data_model.repository;
    get_package        : Data_model.package_name -> Data_model.package;

    (* private *)
    mutable implem_u_dt : Data_model.Component_type_name_set.t option;
    mutable implem_u_dp : Data_model.Port_name_set.t option;
    mutable implem_u_dr : Data_model.Repository_name_set.t option;
    mutable implem_u_dk : Data_model.Package_name_set.t option;
    
    mutable implem_u_i : Data_model.Package_name_set.t Data_model.Component_type_name_map.t;
    mutable implem_u_w : Data_model.package Data_model.Package_name_map.t;

    mutable implem_ur : Data_model.Component_type_name_set.t Data_model.Port_name_map.t;
    mutable implem_up : Data_model.Component_type_name_set.t Data_model.Port_name_map.t;
    mutable implem_uc : Data_model.Component_type_name_set.t Data_model.Port_name_map.t;

    mutable implem_get_component_types : Data_model.Component_type_set.t option;
    mutable implem_get_repositories    : Data_model.Repository_set.t option;
  }

  module Location_component_type = struct
    type t = Data_model.location_name * Data_model.component_type_name
    let compare (l,t) (l',t') = let tmp = Data_model.Location_name.compare l l' in
      if tmp = 0 then Data_model.Component_type_name.compare t t' else tmp
  end module Location_component_type_map = Data_common.Map.Make(Location_component_type)

  module Location_package = struct
    type t = Data_model.location_name * Data_model.package_name
    let compare (l,t) (l',t') = let tmp = Data_model.Location_name.compare l l' in
      if tmp = 0 then Data_model.Package_name.compare t t' else tmp
  end module Location_package_map = Data_common.Map.Make(Location_package)
  

  type configuration = {
    configuration_locations  : Data_model.location Data_model.Location_name_map.t;
    configuration_components : Data_model.component Data_model.Component_name_map.t;
    configuration_bindings   : Data_model.Binding_set.t;

    c_l : unit -> Data_model.Location_name_set.t;
    c_c : unit -> Data_model.Component_name_set.t;
    c_type : Data_model.component_name -> Data_model.component_type_name;

    get_local_component : Data_model.location_name -> Data_model.component_type_name -> Data_model.Component_name_set.t;
    get_local_package : Data_model.location_name -> Data_model.package_name -> bool;

    (*  private *)
    mutable implem_c_l : Data_model.Location_name_set.t option;
    mutable implem_c_c : Data_model.Component_name_set.t option;

    mutable implem_get_local_component : Data_model.Component_name_set.t Location_component_type_map.t;
    mutable implem_get_local_package : bool Location_package_map.t
  }

  (* 2. First basic conversion functions *)
  let universe_plain_of_extended u = {
    Data_model.universe_component_types = u.universe_component_types;
    Data_model.universe_implementation  = u.universe_implementation;
    Data_model.universe_repositories    = u.universe_repositories
  }

  let configuration_plain_of_extended c = {
    Data_model.configuration_locations  = c.configuration_locations;
    Data_model.configuration_components = c.configuration_components;
    Data_model.configuration_bindings   = c.configuration_bindings
  }


  (* 3. Second less basic conversion functions *)

  let set_package_set res =
    let add_repository r =
     let merge_function n k1 k2 = match (k1,k2) with
       | (Some(k1'), Some(k2')) -> Some(k1')
       | (Some(k1'), None     ) -> Some(k1')
       | (None     , Some(k2')) -> Some(k2')
       | (None     , None     ) -> None in res.implem_u_w <- Data_model.Package_name_map.merge merge_function res.implem_u_w r.Data_model.repository_packages in
    Data_model.Repository_set.iter add_repository (res.get_repositories ())

  module Component_type_names_of_component_types = Data_common.Set.Convert(Data_model.Component_type_set)(Data_model.Component_type_name_set)
  let component_type_names_of_component_types set = Component_type_names_of_component_types.convert (fun c -> c.Data_model.component_type_name) set
    

  let universe_extended_of_plain u = let rec res = {
    universe_component_types = u.Data_model.universe_component_types;
    universe_implementation  = u.Data_model.universe_implementation;
    universe_repositories    = u.Data_model.universe_repositories;

    u_dt = (fun _ -> match res.implem_u_dt with | Some(set) -> set
             | None -> let tmp = Core.get_component_type_names res.universe_component_types in res.implem_u_dt <- Some(tmp); tmp);
    u_dp = (fun _ -> match res.implem_u_dp with | Some(set) -> set
             | None -> let tmp = Core.get_port_names (res.get_component_types ()) in res.implem_u_dp <- Some(tmp); tmp);
    u_dr = (fun _ -> match res.implem_u_dr with | Some(set) -> set
             | None -> let tmp = Core.get_repository_names res.universe_repositories in res.implem_u_dr <- Some(tmp); tmp);
    u_dk = (fun _ -> match res.implem_u_dk with | Some(set) -> set
             | None -> let tmp = Core.get_package_names (res.get_repositories ()) in res.implem_u_dk <- Some(tmp); tmp);

    u_i = (fun t -> try Data_model.Component_type_name_map.find t res.implem_u_i with
             | Not_found -> let tmp = Core.get_component_type_implementation res.universe_implementation t in 
               res.implem_u_i <- Data_model.Component_type_name_map.add t tmp res.implem_u_i; tmp);
    u_w = (fun k -> (if res.implem_u_w = Data_model.Package_name_map.empty then set_package_set res);
            try Data_model.Package_name_map.find k res.implem_u_w with
             | Not_found -> Zephyrus_log.log_missing_data "package" k "universe"; exit(-1));

    ur = (fun p -> try Data_model.Port_name_map.find p res.implem_ur with
             | Not_found -> let tmp = component_type_names_of_component_types (Core.requirers (res.get_component_types ()) p) in
               res.implem_ur <- Data_model.Port_name_map.add p tmp res.implem_ur; tmp);

    up = (fun p -> try Data_model.Port_name_map.find p res.implem_up with
             | Not_found -> let tmp = component_type_names_of_component_types (Core.providers (res.get_component_types ()) p) in
               res.implem_up <- Data_model.Port_name_map.add p tmp res.implem_up; tmp);

    uc = (fun p -> try Data_model.Port_name_map.find p res.implem_uc with
             | Not_found -> let tmp = component_type_names_of_component_types (Core.conflicters (res.get_component_types ()) p) in
               res.implem_uc <- Data_model.Port_name_map.add p tmp res.implem_uc; tmp);


    get_component_types = (fun _ -> match res.implem_get_component_types with | Some(set) -> set
             | None -> let tmp = Core.get_component_types res.universe_component_types in res.implem_get_component_types <- Some(tmp); tmp);
    get_repositories    = (fun _ -> match res.implem_get_repositories with | Some(set) -> set
             | None -> let tmp = Core.get_repositories res.universe_repositories in res.implem_get_repositories <- Some(tmp); tmp);

    get_component_type = (fun n -> Core.get_component_type res.universe_component_types n);
    get_repository = (fun n -> Core.get_repository res.universe_repositories n);
    get_package = res.u_w;

    (* private *)
    implem_u_dt = None;
    implem_u_dp = None;
    implem_u_dr = None;
    implem_u_dk = None;
    
    implem_u_i = Data_model.Component_type_name_map.empty;
    implem_u_w = Data_model.Package_name_map.empty;

    implem_ur = Data_model.Port_name_map.empty;
    implem_up = Data_model.Port_name_map.empty;
    implem_uc = Data_model.Port_name_map.empty;

    implem_get_component_types = None;
    implem_get_repositories    = None;
  } in res


  let configuration_extended_of_plain c = ()

*)



end
*)
