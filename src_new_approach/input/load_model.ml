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
(*    along with Zephyrus.  If not, see <http=//www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(* Depends on
    - List (standard library)
    - datatypes/Data_model (all universe, configuration, specification)
    - datatypes/Data_state (to set data)
    - input/Settings (to get files and optimization function)
    - atd/json.atd | _build/atd/json_t (json parsing: type  module)
    - atd/json.atd | _build/atd/json_j (json parsing: of_string  module)
    - input/Input_helper (for input file manipulation)
*)

open Data_model


(* 0. Name Conversion *)

let convert_resource_name          x = x
let convert_resource_consume_arity x = x
let convert_resource_provide_arity x = x
let convert_port_name              x = x
let convert_component_type_name    x = x
let convert_provide_arity          x = match x with | `InfiniteProvide -> Infinite_provide | `FiniteProvide(i) -> Finite_provide(i)
let convert_require_arity          x = x
let convert_package_name         r x = if Settings.get_bool_basic Settings.data_package_name_extended then r ^ x else x
let convert_repository_name        x = x
let convert_location_name          x = x
let convert_component_name         x = x

(*****************************************************)
(** 1. Resource Names and Ids Extractions            *)
(*****************************************************)
(*

class get_resources u k c s =
  let resource_names : Resource_name_set.t ref = ref Resource_name_set.empty in
  let get_resource_names_of_list l = List.fold_iter (fun (r,_) -> resource_names := Resource_name_set.add r !resource_names) l in
  let parse_component_type c = get_resource_names_of_list c.Json_j.component_type_consume in
  let parse_package k = get_resource_names_of_list k.Json_j.package_consume in
  let parse_repository r = List.iter parse_package r.Json_j.repository_packages in
  let parse_universe u = List.iter parse_component_type u.Json_j.universe_component_types; List.iter parse_repository u.Json_j.universe_repositories in
  let parse_location l = get_resource_names_of_list l.Json_j.location_provide_resources in
  let parse_configuration c = List.iter parse_location c.Json_j.configuration_locations in
  let parse_el e = match e with `SpecElementLocalisation(rs,_,_) -> | _ -> ()
*)

(*****************************************************)
(** 2. Universe Conversion                           *)
(*****************************************************)

module Repository_id_Package_name = struct
  type t = repository_id * package_name
  let compare v1 v2 =
    let r1 = Repository_id.compare (fst v1) (fst v2) in if r1 = 0 then
      Package_name.compare (snd v1) (snd v2)
    else r1
end module Repository_id_Package_name_map = Data_common.Map.Make(Repository_id_Package_name)

let port_is_provide_strict prov = match prov with | Finite_provide i -> i > 0 | Infinite_provide -> true

let requirers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
  (fun id t -> if Port_id_set.mem port_id t#require_domain then (t#require port_id) > 0 else false) component_types)
let providers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
  (fun id t -> if Port_id_set.mem port_id t#provide_domain then port_is_provide_strict (t#provide port_id) else false) component_types)
let conflicters component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
  (fun id t -> Port_id_set.mem port_id (t#conflict)) component_types)


(*module Package_key_conversion = Data_common.Map.Convert(Repository_id_Package_name_map)(Package_id_map)*)

class convert_universe get_resource_id get_resource_name input_repositories u = 
  (* 1. Data Storage *)
    (* ports *)
  let port_names    : Port_name_set.t ref = ref Port_name_set.empty in             (* all port names *)
  let port_ids      : Port_id_set.t ref   = ref Port_id_set.empty in               (* all port ids *)
  let port_local_provide : Port_id_set.t ref   = ref Port_id_set.empty in          (* all port ids provided by the current component *)
  let port_local_require : Port_id_set.t ref   = ref Port_id_set.empty in          (* all port ids required by the current component *)
  let port_get_id   : (port_id Port_name_map.t) ref = ref Port_name_map.empty in   (* mapping from port names to port ids *)
  let port_get_name : (port_name Port_id_map.t) ref = ref Port_id_map.empty in     (* mapping from port ids to port names *)
  let add_port name id = 
    port_names    := Port_name_set.add name (!port_names);
    port_ids      := Port_id_set.add id (!port_ids);
    port_get_id   := Port_name_map.add name id (!port_get_id);
    port_get_name := Port_id_map.add id name (!port_get_name) in
  let add_port_provided id = port_local_provide := Port_id_set.add id (!port_local_provide) in
  let add_port_required id = port_local_require := Port_id_set.add id (!port_local_require) in
  let port_current_id = ref 0 in
  let find_port name = try Port_name_map.find name !port_get_id with
    | Not_found -> let id = !port_current_id in port_current_id := (!port_current_id) + 1; add_port name id; id in

    (* component types *)
  let component_types         : Component_type_set.t ref = ref Component_type_set.empty in                                (* all component types *)
  let component_type_names    : Component_type_name_set.t ref = ref Component_type_name_set.empty in                      (* all component type names *)
  let component_type_ids      : Component_type_id_set.t ref = ref Component_type_id_set.empty in                        (* all component type ids *)
  let component_type_get      : (component_type Component_type_id_map.t) ref = ref Component_type_id_map.empty in         (* mapping from ids to component types *)
  let component_type_get_id   : (component_type_id Component_type_name_map.t) ref = ref Component_type_name_map.empty in  (* mapping from names to ids *)
  let component_type_get_name : (component_type_name Component_type_id_map.t) ref = ref Component_type_id_map.empty in    (* mapping from ids to name *)
  let add_component_type t id = 
    component_types         := Component_type_set.add t (!component_types);
    component_type_names    := Component_type_name_set.add t#name (!component_type_names);
    component_type_ids      := Component_type_id_set.add id (!component_type_ids);
    component_type_get      := Component_type_id_map.add id t (!component_type_get);
    component_type_get_id   := Component_type_name_map.add t#name id (!component_type_get_id);
    component_type_get_name := Component_type_id_map.add id t#name (!component_type_get_name) in
  let component_type_current_id = ref 0 in
  let new_component_type t = 
    let id = !component_type_current_id in component_type_current_id := (!component_type_current_id) + 1; add_component_type t id in
  let find_component_type_id name = try Component_type_name_map.find name !component_type_get_id with
    | Not_found ->  (* erroneous case, but well, for sake of consistency, we pospone error message in the check phase *)
      let id = !component_type_current_id in
        component_type_current_id := (!component_type_current_id) + 1;
        component_type_names    := Component_type_name_set.add name (!component_type_names);
        component_type_ids      := Component_type_id_set.add id (!component_type_ids);
        component_type_get_id   := Component_type_name_map.add name id (!component_type_get_id);
        component_type_get_name := Component_type_id_map.add id name (!component_type_get_name); id in

    (* package *)
  let packages          : Package_set.t ref      = ref Package_set.empty in
  let package_names     : Package_name_set.t ref = ref Package_name_set.empty in
  let package_ids       : Package_id_set.t ref   = ref Package_id_set.empty in
  let package_get       : (package Package_id_map.t) ref                    = ref Package_id_map.empty in
  let package_get_id    : (package_id Repository_id_Package_name_map.t) ref = ref Repository_id_Package_name_map.empty in
  let package_get_name  : (package_name Package_id_map.t) ref               = ref Package_id_map.empty in
  let repos_of_packages : (repository_id Package_id_map.t) ref  = ref Package_id_map.empty in
  let local_packages    : Package_set.t ref      = ref Package_set.empty in
  let local_package_ids : Package_id_set.t ref   = ref Package_id_set.empty in
  let add_package r name id = 
    repos_of_packages := Package_id_map.add id r !repos_of_packages;
    package_names     := Package_name_set.add name (!package_names);
    package_ids       := Package_id_set.add id (!package_ids);
    local_package_ids := Package_id_set.add id (!package_ids);
    package_get_id    := Repository_id_Package_name_map.add (r,name) id (!package_get_id);
    package_get_name  := Package_id_map.add id name (!package_get_name) in
  let package_current_id = ref 0 in
  let find_package r name = try Repository_id_Package_name_map.find (r,name) !package_get_id with
    | Not_found -> let id = !package_current_id in package_current_id := (!package_current_id) + 1; add_package r name id; id in
  let new_package r k = let id = find_package r k#name in
    packages       := Package_set.add k (!packages);
    local_packages := Package_set.add k (!packages);
    package_get    := Package_id_map.add id k (!package_get); (id,k) in

    (* repositories *)
  let repositories        : Repository_set.t ref      = ref Repository_set.empty in
  let repository_names    : Repository_name_set.t ref = ref Repository_name_set.empty in
  let repository_ids      : Repository_id_set.t ref   = ref Repository_id_set.empty in
  let repository_get      : (repository Repository_id_map.t) ref      = ref Repository_id_map.empty in
  let repository_get_id   : (repository_id Repository_name_map.t) ref = ref Repository_name_map.empty in
  let repository_get_name : (repository_name Repository_id_map.t) ref = ref Repository_id_map.empty in
  let add_repository r id = 
    repositories        := Repository_set.add r (!repositories);
    repository_names    := Repository_name_set.add r#name (!repository_names);
    repository_ids      := Repository_id_set.add id (!repository_ids);
    repository_get      := Repository_id_map.add id r (!repository_get);
    repository_get_id   := Repository_name_map.add r#name id (!repository_get_id);
    repository_get_name := Repository_id_map.add id r#name (!repository_get_name) in
  let repository_current_id = ref 0 in
  let get_current_repository_id () = !repository_current_id in
  let new_repository t = 
    let id = !repository_current_id in repository_current_id := (!repository_current_id) + 1; add_repository t id in
  let find_repository r = try Repository_name_map.find r !repository_get_id with
    | Not_found -> Zephyrus_log.log_missing_data "repository" r "repository declaration" in

  (* 2. Conversion *)
    (* component types *)
  let convert_component_type t =
    port_local_provide := Port_id_set.empty;
    port_local_require := Port_id_set.empty;
    let implem_provide  = Port_id_map.map_of_list
      (fun (p,n) -> (let id = find_port (convert_port_name p) in add_port_provided id; id, convert_provide_arity n)) t.Json_t.component_type_provide in
    let implem_require  = Port_id_map.map_of_list
      (fun (p,n) -> (let id = find_port (convert_port_name p) in add_port_required id; id, convert_require_arity n)) t.Json_t.component_type_require in
    let implem_conflict = Port_id_set.set_of_list (fun p -> find_port (convert_port_name p)) t.Json_t.component_type_conflict in
    let implem_consume = Resource_id_map.map_of_list
      (fun (r,n) -> (get_resource_id (convert_resource_name r), convert_resource_consume_arity n)) t.Json_t.component_type_consume in
    new_component_type (object(self)
      method name           = convert_component_type_name t.Json_t.component_type_name
      method provide      p = try Port_id_map.find p implem_provide with
        | Not_found -> let port_desc = "(" ^ (string_of_int p) ^ "," ^ (try Port_id_map.find p (!port_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "port" port_desc ("provides of the component type \"" ^ (self#name) ^ "\"")
      method provide_domain = !port_local_provide
      method require      p = try Port_id_map.find p implem_require with
        | Not_found -> let port_desc = "(" ^ (string_of_int p) ^ "," ^ (try Port_id_map.find p (!port_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "port" port_desc ("requires of the component type \"" ^ (self#name) ^ "\"")
      method require_domain = !port_local_require
      method conflict       = implem_conflict
      method consume      r = try Resource_id_map.find r implem_consume with Not_found -> 0
    end) in
    (* packages *)
  let convert_package r r_name k =
    let implem_depend = Package_id_set_set.set_of_list
      (fun s -> Package_id_set.set_of_list (fun n -> find_package r (convert_package_name r_name n)) s) k.Json_t.package_depend in
    let implem_conflict = Package_id_set.set_of_list (fun n -> find_package r (convert_package_name r_name n)) k.Json_t.package_conflict in
    let implem_consume = Resource_id_map.map_of_list
      (fun (r,n) -> (get_resource_id (convert_resource_name r), convert_resource_consume_arity n)) k.Json_t.package_consume in
    new_package r (object
      method name      = convert_package_name r_name k.Json_t.package_name
      method depend    = implem_depend
      method conflict  = implem_conflict
      method consume r = try Resource_id_map.find r implem_consume with Not_found -> 0
    end) in

    (* repositories *)
  let convert_repository r = let id = get_current_repository_id () in
    local_package_ids := Package_id_set.empty; local_packages := Package_set.empty;
    let implem_name = convert_repository_name r.Json_t.repository_name in
    let implem_packages = Package_id_map.map_of_list
      (fun k -> convert_package id implem_name k) r.Json_t.repository_packages in
    new_repository (object(self) 
      method name          = implem_name
      method get_package k = try Package_id_map.find k implem_packages with
        | Not_found -> let package_desc = "(" ^ (string_of_int k) ^ "," ^ (try Package_id_map.find k (!package_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "package" package_desc ("package of the repository \"" ^ (self#name) ^ "\"")
      method packages = !local_packages
      method package_ids = !local_package_ids
    end) in

    (* universe *)
    let _ = List.iter convert_repository u.Json_t.universe_repositories        (* fill the repository and package tables 1/2 *) in   
    let _ = List.iter convert_repository input_repositories                    (* fill the repository and package tables 2/2 *) in   
    let _ = List.iter convert_component_type u.Json_t.universe_component_types (* fill the component type table *) in
    let implementation = Component_type_id_map.map_of_list                     (* may add erroneous packages and component types in the table *)
      (fun (t,ks) -> (find_component_type_id (convert_component_type_name t), Package_id_set.set_of_list
        (fun (r,k) -> let r_name = convert_repository_name r in find_package (find_repository r_name) (convert_package_name r_name k)) ks)) u.Json_t.universe_implementation in

object(self)
(* private *)
  val implem_get_component_type = !component_type_get
  val implem_get_implementation = implementation
  val implem_get_repository     = !repository_get
  val implem_get_package        = !package_get

  val mutable implem_ur = Port_id_map.empty;
  val mutable implem_up = Port_id_map.empty;
  val mutable implem_uc = Port_id_map.empty;

  val implem_get_port_id           = !port_get_id
  val implem_get_component_type_id = !component_type_get_id
  val implem_get_repository_id     = !repository_get_id
  val implem_get_package_id        = !package_get_id

  val implem_get_port_name           = !port_get_name
  val implem_get_component_type_name = !component_type_get_name
  val implem_get_repository_name     = !repository_get_name
  val implem_get_package_name        = !package_get_name

  (* methods *)
  method get_component_type id = Component_type_id_map.find id implem_get_component_type
  method get_implementation id = try Component_type_id_map.find id implem_get_implementation with | Not_found -> Package_id_set.empty
  method get_repository     id = Repository_id_map.find id implem_get_repository
  method get_package        id = Package_id_map.find id implem_get_package

  method repository_of_package id = Package_id_map.find id !repos_of_packages

  method get_component_types = !component_types
  method get_repositories    = !repositories
  method get_packages        = !packages

  method get_port_ids           = !port_ids
  method get_component_type_ids = !component_type_ids
  method get_repository_ids     = !repository_ids
  method get_package_ids        = !package_ids

  method get_port_names           = !port_names
  method get_component_type_names = !component_type_names
  method get_repository_names     = !repository_names
  method get_package_names        = !package_names

  (* methods coming from the paper. Usually, aliases for well-named functions *)
  method u_dt = self#get_component_type_ids
  method u_dp = self#get_port_ids
  method u_dr = self#get_repository_ids
  method u_dk = self#get_package_ids
    
  method u_i = self#get_implementation
  method u_w = self#get_package

  method ur p = (try Port_id_map.find p implem_ur with
           | Not_found -> let tmp = (requirers implem_get_component_type p) in implem_ur <- Port_id_map.add p tmp implem_ur; tmp)
  method up p = (try Port_id_map.find p implem_up with
           | Not_found -> let tmp = (providers implem_get_component_type p) in implem_ur <- Port_id_map.add p tmp implem_ur; tmp)
  method uc p = (try Port_id_map.find p implem_ur with
           | Not_found -> let tmp = (conflicters implem_get_component_type p) in implem_ur <- Port_id_map.add p tmp implem_ur; tmp)

  (* methods for naming *)
  method get_port_id           n = try Port_name_map.find n implem_get_port_id with Not_found -> deprecated_package_id (* to deal with initial configurations *)
  method get_component_type_id n = try Component_type_name_map.find n implem_get_component_type_id with Not_found -> deprecated_component_type_id (* to deal with initial configurations *)
  method get_repository_id     n = Repository_name_map.find n implem_get_repository_id
  method get_package_id      r n = Repository_id_Package_name_map.find (r,n) implem_get_package_id

  method get_port_name           id = Port_id_map.find id implem_get_port_name
  method get_component_type_name id = if id = deprecated_component_type_id then "!!deprecated_component!!" else Component_type_id_map.find id implem_get_component_type_name
  method get_repository_name     id = Repository_id_map.find id implem_get_repository_name
  method get_package_name        id = if id = deprecated_package_id then "!!deprecated_package!!" else Package_id_map.find id implem_get_package_name
end

(* Possible inconsistencies not detected during generation:
 - package presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
 - component type presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
*)

(*

let convert_universe u = {
  universe_component_types = Component_type_name_map.map_of_list (fun t -> let t' = convert_component_type t in (t'.component_type_name, t')) u.Json_t.universe_component_types;
  universe_implementation  = Component_type_name_map.map_of_list (fun (t,ks) -> (convert_component_name t, Package_name_set.set_of_direct_list ks))  u.Json_t.universe_implementation;
  universe_repositories    = Repository_name_map.map_of_list (fun r -> let r' = convert_repository r in (r'.repository_name, r')) u.Json_t.universe_repositories }
*)


(*****************************************************)
(** 3. Configuration Conversion                      *)
(*****************************************************)


module Location_component_type = struct
  type t = Data_model.location_id * Data_model.component_type_id
  let compare (l,t) (l',t') = let tmp = Data_model.Location_id.compare l l' in
    if tmp = 0 then Data_model.Component_type_id.compare t t' else tmp
end module Location_component_type_map = Data_common.Map.Make(Location_component_type)

module Location_package = struct
  type t = Data_model.location_id * Data_model.package_id
  let compare (l,t) (l',t') = let tmp = Data_model.Location_id.compare l l' in
    if tmp = 0 then Data_model.Package_id.compare t t' else tmp
end module Location_package_map = Data_common.Map.Make(Location_package)

let get_local_component l t map = Component_id_map_extract_key.set_of_keys (Component_id_map.filter (fun _ c -> (c#location = l) & (c#typ = t)) map)
let get_local_package l k map = try let l' = Location_id_map.find l map in Package_id_set.mem k l'#packages_installed with Not_found -> false


class convert_configuration get_resource_id get_port_id get_component_type_id get_package_id get_repository_id c =
  (* 1. Data storage *)
    (* locations *)
  let locations         : Location_set.t ref = ref Location_set.empty in
  let location_names    : Location_name_set.t ref = ref Location_name_set.empty in
  let location_ids      : Location_id_set.t ref = ref Location_id_set.empty in
  let location_get      : (location Location_id_map.t) ref = ref Location_id_map.empty in
  let location_get_id   : (location_id Location_name_map.t) ref = ref Location_name_map.empty in
  let location_get_name : (location_name Location_id_map.t) ref = ref Location_id_map.empty in
  let add_location r id = 
    locations         := Location_set.add r (!locations);
    location_names    := Location_name_set.add r#name (!location_names);
    location_ids      := Location_id_set.add id (!location_ids);
    location_get      := Location_id_map.add id r (!location_get);
    location_get_id   := Location_name_map.add r#name id (!location_get_id);
    location_get_name := Location_id_map.add id r#name (!location_get_name) in
  let location_current_id = ref 0 in
  let new_location l = 
    let id = !location_current_id in location_current_id := (!location_current_id) + 1; add_location l id in
  let find_location l = try Location_name_map.find l (!location_get_id) with
    | Not_found -> Zephyrus_log.log_missing_data "location" l "component" in
    (* components *)
  let components         : Component_set.t ref = ref Component_set.empty in
  let component_names    : Component_name_set.t ref = ref Component_name_set.empty in
  let component_ids      : Component_id_set.t ref = ref Component_id_set.empty in
  let component_get      : (component Component_id_map.t) ref = ref Component_id_map.empty in
  let component_get_id   : (component_id Component_name_map.t) ref = ref Component_name_map.empty in
  let component_get_name : (component_name Component_id_map.t) ref = ref Component_id_map.empty in
  let add_component r id = 
    components         := Component_set.add r (!components);
    component_names    := Component_name_set.add r#name (!component_names);
    component_ids      := Component_id_set.add id (!component_ids);
    component_get      := Component_id_map.add id r (!component_get);
    component_get_id   := Component_name_map.add r#name id (!component_get_id);
    component_get_name := Component_id_map.add id r#name (!component_get_name) in
  let component_current_id = ref 0 in
  let new_component t = 
    let id = !component_current_id in component_current_id := (!component_current_id) + 1; add_component t id in
  let find_component c = try Component_name_map.find c (!component_get_id) with
    | Not_found -> Zephyrus_log.log_missing_data "component" c "binding" in

  (* 2. Conversion *)
    (* location *)
  let convert_location l =
    let implem_name = convert_location_name l.Json_t.location_name in
    let r_name = convert_repository_name l.Json_t.location_repository in let implem_repository =  get_repository_id r_name in
    let implem_package = Package_id_set.set_of_list (fun k -> get_package_id implem_repository (convert_package_name r_name k)) l.Json_t.location_packages_installed in
    let implem_resource = Resource_id_map.map_of_list
      (fun (r,n) -> (get_resource_id (convert_resource_name r), convert_resource_provide_arity n)) l.Json_t.location_provide_resources in
    new_location (object
    method name                = implem_name
    method repository          = implem_repository
    method packages_installed  = implem_package
    method provide_resources r = try Resource_id_map.find r implem_resource with Not_found -> 0 
  end) in
    (* component *)
  let convert_component c =
    let implem_name = convert_component_name c.Json_t.component_name in
    let implem_type = get_component_type_id (convert_component_type_name c.Json_t.component_type) in
    let implem_location = find_location (convert_location_name c.Json_t.component_location) in
    new_component (object
      method name     = implem_name
      method typ      = implem_type
      method location = implem_location end) in
    (* bindings *)
  let convert_binding b =
    let implem_port     = get_port_id (convert_port_name b.Json_t.binding_port) in
    let implem_requirer = find_component (convert_component_name b.Json_t.binding_requirer) in
    let implem_provider = find_component (convert_component_name b.Json_t.binding_provider) in (object
     method port     = implem_port
     method requirer = implem_requirer
     method provider = implem_provider end) in
  (* configuration *)
  let _ = List.iter convert_location c.Json_t.configuration_locations in
  let _ = List.iter convert_component c.Json_t.configuration_components in
  let implem_bindings = Binding_set.set_of_list (convert_binding) c.Json_t.configuration_bindings in
object(self)
(* private *)
  val mutable implem_get_local_component = Location_component_type_map.empty;
  val mutable implem_get_local_package = Location_package_map.empty;
(* methods *)

  method get_location l = Location_id_map.find l (!location_get)
  method get_component c = Component_id_map.find c (!component_get)

  method get_locations  = !locations
  method get_components = !components
  method get_bindings   = implem_bindings

  method get_location_ids  = !location_ids
  method get_component_ids = !component_ids

  method get_location_names  = !location_names
  method get_component_names = !component_names

  method c_l = self#get_location_ids
  method c_c = self#get_component_ids
  method c_type c = (self#get_component c)#typ

  method get_local_component l t = (try Location_component_type_map.find (l,t) implem_get_local_component with
         | Not_found -> let tmp = get_local_component l t (!component_get) in
           implem_get_local_component <- Location_component_type_map.add (l,t) tmp implem_get_local_component; tmp)
  method get_local_package l k = (try Location_package_map.find (l,k) implem_get_local_package with
         | Not_found -> let tmp = get_local_package l k (!location_get) in
           implem_get_local_package <- Location_package_map.add (l,k) tmp implem_get_local_package; tmp);


  method get_location_id l  = Location_name_map.find l (!location_get_id)
  method get_component_id c = Component_name_map.find c (!component_get_id)

  method get_location_name l = Location_id_map.find l (!location_get_name)
  method get_component_name c = Component_id_map.find c (!component_get_name)
end


(*****************************************************)
(** 4. Specification conversion                      *)
(*****************************************************)


let convert_spec_variable_name = fun x -> x
let convert_spec_const         = fun x -> x
let convert_spec_op o          = match o with
 | `Lt -> Lt | `LEq -> LEq | `Eq -> Eq | `GEq -> GEq | `Gt -> Gt | `NEq -> NEq

let convert_specification get_resource_id get_port_id get_component_type_id get_package_id get_repository_id s =
  let convert_spec_local_element el = match el with
    | `SpecLocalElementPackage(r,k)       -> let r_name = convert_repository_name r in let repo = get_repository_id r_name in
                                               Spec_local_element_package(get_package_id repo (convert_package_name r_name k))
    | `SpecLocalElementComponentType(t) -> Spec_local_element_component_type(get_component_type_id (convert_component_type_name t))
    | `SpecLocalElementPort(p)       -> Spec_local_element_port(get_port_id (convert_port_name p)) in
  let rec convert_spec_local_expr e = match e with
    | `SpecLocalExprVar(v)        -> Spec_local_expr_var(convert_spec_variable_name v)
    | `SpecLocalExprConst(c)      -> Spec_local_expr_const(convert_spec_const c)
    | `SpecLocalExprArity(el)     -> Spec_local_expr_arity(convert_spec_local_element el)
    | `SpecLocalExprAdd(ex1, ex2) -> Spec_local_expr_add(convert_spec_local_expr ex1, convert_spec_local_expr ex2)
    | `SpecLocalExprSub(ex1, ex2) -> Spec_local_expr_sub(convert_spec_local_expr ex1, convert_spec_local_expr ex2)
    | `SpecLocalExprMul(n, ex)    -> Spec_local_expr_mul(convert_spec_const n, convert_spec_local_expr ex) in
  let rec convert_local_specification sl = match sl with 
    | `SpecLocalTrue            -> Spec_local_true
    | `SpecLocalOp(ex1, o, ex2) -> Spec_local_op(convert_spec_local_expr ex1, convert_spec_op o, convert_spec_local_expr ex2)
    | `SpecLocalAnd(sl1, sl2)   -> Spec_local_and(convert_local_specification sl1, convert_local_specification sl2)
    | `SpecLocalOr(sl1, sl2)    -> Spec_local_or(convert_local_specification sl1, convert_local_specification sl2)
    | `SpecLocalImpl(sl1, sl2)  -> Spec_local_impl(convert_local_specification sl1, convert_local_specification sl2)
    | `SpecLocalNot(sl')   -> Spec_local_not(convert_local_specification sl') in

  let convert_spec_repository_constraint x = get_repository_id (convert_repository_name x) in
  let convert_spec_resource_constraint (r, o, c) = (get_resource_id (convert_resource_name r), convert_spec_op o, convert_spec_const c) in

  let convert_spec_element el = match el with
    | `SpecElementPackage(r,k)       -> let r_name = convert_repository_name r in let repo = get_repository_id r_name in
                                               Spec_element_package(get_package_id repo (convert_package_name r_name k))
    | `SpecElementComponentType(t)       -> Spec_element_component_type(get_component_type_id (convert_component_type_name t))
    | `SpecElementPort(p)                -> Spec_element_port(get_port_id (convert_port_name p))
    | `SpecElementLocalisation(phi,r,sl)     -> Spec_element_location(
          List.map convert_spec_resource_constraint phi, List.map convert_spec_repository_constraint r, convert_local_specification sl) in
  let rec convert_spec_expr e = match e with
    | `SpecExprVar(v)        -> Spec_expr_var(convert_spec_variable_name v)
    | `SpecExprConst(c)      -> Spec_expr_const(convert_spec_const c)
    | `SpecExprArity(el)     -> Spec_expr_arity(convert_spec_element el)
    | `SpecExprAdd(ex1, ex2) -> Spec_expr_add(convert_spec_expr ex1, convert_spec_expr ex2)
    | `SpecExprSub(ex1, ex2) -> Spec_expr_sub(convert_spec_expr ex1, convert_spec_expr ex2)
    | `SpecExprMul(n, ex)    -> Spec_expr_mul(convert_spec_const n, convert_spec_expr ex) in
  let rec convert s = match s with 
  | `SpecTrue            -> Spec_true
  | `SpecOp(ex1, o, ex2) -> Spec_op(convert_spec_expr ex1, convert_spec_op o, convert_spec_expr ex2)
  | `SpecAnd(s1, s2)     -> Spec_and(convert s1, convert s2)
  | `SpecOr(s1, s2)      -> Spec_or(convert s1, convert s2)
  | `SpecImpl(s1, s2)    -> Spec_impl(convert s1, convert s2)
  | `SpecNot(s')         -> Spec_not(convert s') in convert s


(** 4.5. Optimization_function  *)

let convert_optimization_function o = match o with
  | Settings.Optim_none         -> Optimization_function_none
  | Settings.Optim_simple       -> Optimization_function_simple
  | Settings.Optim_compact      -> Optimization_function_compact
  | Settings.Optim_spread       -> Optimization_function_spread
  | Settings.Optim_conservative -> Optimization_function_conservative


(*****************************************************)
(** 5. Global Load Functions                         *)
(*****************************************************)



let load_universe () = if Settings.get_bool_basic Settings.data_generation_universe then Input_helper.parse_json Json_j.read_universe Settings.input_file_universe else None

let load_repositories () = if Settings.get_bool_basic Settings.data_generation_repositories then List.fold_left
  (fun res (n,f) -> let r = Input_helper.parse_json Json_j.read_packages (ref (Some (f))) in match r with None -> res
      | Some(ks) -> { Json_j.repository_name = n; Json_j.repository_packages = ks }::res) [] !Settings.input_file_repositories else []

let load_initial_configuration () = if Settings.get_bool_basic Settings.data_generation_initial_configuration then
  Input_helper.parse_json Json_j.read_configuration Settings.input_file_initial_configuration else None

let load_specification () = if Settings.get_bool_basic Settings.data_generation_specification then
  Input_helper.parse_standard Specification_parser.main Specification_lexer.token Settings.input_file_specification else None

let load_optimization_function () = if Settings.get_bool_basic Settings.data_generation_optimization_function then
  (match !Settings.input_optimization_function with None -> None | Some(o) -> Some(convert_optimization_function o)) else None

let load_model () =
  (* 1. data structure to constuct the set of resource names *)
  let resource_names    : Resource_name_set.t ref = ref Resource_name_set.empty in
  let resource_ids      : Resource_id_set.t ref = ref Resource_id_set.empty in
  let resource_get_name : (resource_name Resource_id_map.t) ref = ref Resource_id_map.empty in
  let resource_get_id   : (resource_id Resource_name_map.t) ref = ref Resource_name_map.empty in
  let add_resource name id = 
    resource_names    := Resource_name_set.add name !resource_names;
    resource_ids      := Resource_id_set.add id !resource_ids;
    resource_get_name := Resource_id_map.add id name !resource_get_name;
    resource_get_id   := Resource_name_map.add name id !resource_get_id in
  let resource_current_id = ref 0 in
  let get_resource_id name = try Resource_name_map.find name !resource_get_id with
    | Not_found -> let id = !resource_current_id in resource_current_id := id + 1; add_resource name id; id in
  let get_resource_name id = try Resource_id_map.find id !resource_get_name with
    | Not_found -> Zephyrus_log.log_missing_data "resource id" (String_of.resource_id id) "universe" in
  (* 2. generate the universe (if required) *)
  let universe = match load_universe () with
    | None -> None
    | Some(u) -> Some(new convert_universe get_resource_id get_resource_name (load_repositories ()) u) in
  match universe with 
    | None -> ()  (* no universe, nothing we can do in zephyrus *)
    | Some(u) ->
      let (get_port_id, get_component_type_id, get_package_id, get_repository_id) = (u#get_port_id, u#get_component_type_id, u#get_package_id, u#get_repository_id) in
  (* 3. generate the initial configuration (if required) *)
  let initial_configuration = match load_initial_configuration () with
    | None -> None
    | Some(c) -> Some(new convert_configuration get_resource_id get_port_id get_component_type_id get_package_id get_repository_id c) in
  (* 4. generate the specification (if required) *)
  let specification = match load_specification () with
    | None -> None
    | Some(s) -> Some(convert_specification get_resource_id get_port_id get_component_type_id get_package_id get_repository_id s) in
  (* 5. generation of the resource set *)
  let resources = object
    method resource_names = !resource_names
    method resource_ids   = !resource_ids
    method get_name    id = try Resource_id_map.find id !resource_get_name with Not_found -> Zephyrus_log.log_missing_data "resource id" (String_of.resource_id id) "universe"
    method get_id    name = try Resource_name_map.find name !resource_get_id with
      Not_found -> Zephyrus_log.log_missing_data "resource name" (String_of.resource_name name) "universe"
  end in
  (* 6. generation of the optimization function (if required) *)
  let optimization_function = load_optimization_function () in
  (* 7. set all data in data_state *)
  Data_state.universe_full := universe;
  Data_state.initial_configuration_full := initial_configuration;
  Data_state.specification_full := specification;
  Data_state.resources_full := Some(resources);
  Data_state.optimization_function := optimization_function


