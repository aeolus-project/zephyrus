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


(* 1. Universe Conversion *)


module Repository_id_Package_name = struct
  type t = repository_id * package_name
  let compare v1 v2 =
    let r1 = Repository_id.compare (fst v1) (fst v2) in if r1 = 0 then
      Package_name.compare (snd v1) (snd v2)
    else r1
end module Repository_id_Package_name_map = Data_common.Map.Make(Repository_id_Package_name)

(*module Package_key_conversion = Data_common.Map.Convert(Repository_id_Package_name_map)(Package_id_map)*)

class load_universe get_resource_id get_resource_name u = 
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
  let packages         : Package_set.t ref      = ref Package_set.empty in
  let package_names    : Package_name_set.t ref = ref Package_name_set.empty in
  let package_ids      : Package_id_set.t ref   = ref Package_id_set.empty in
  let package_get      : (package Package_id_map.t) ref                    = ref Package_id_map.empty in
  let package_get_id   : (package_id Repository_id_Package_name_map.t) ref = ref Repository_id_Package_name_map.empty in
  let package_get_name : (package_name Package_id_map.t) ref               = ref Package_id_map.empty in
  let add_package r name id = 
    package_names    := Package_name_set.add name (!package_names);
    package_ids      := Package_id_set.add id (!package_ids);
    package_get_id   := Repository_id_Package_name_map.add (r,name) id (!package_get_id);
    package_get_name := Package_id_map.add id name (!package_get_name) in
  let package_current_id = ref 0 in
  let find_package r name = try Repository_id_Package_name_map.find (r,name) !package_get_id with
    | Not_found -> let id = !package_current_id in package_current_id := (!package_current_id) + 1; add_package r name id; id in
  let new_package r k = let id = find_package r k#name in
    packages    := Package_set.add k (!packages);
    package_get := Package_id_map.add id k (!package_get); (id,k) in

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
    let implem_name = convert_repository_name r.Json_t.repository_name in
    let implem_packages = Package_id_map.map_of_list
      (fun k -> convert_package id implem_name k) r.Json_t.repository_packages in
    new_repository (object(self) 
      method name       = implem_name
      method packages k = try Package_id_map.find k implem_packages with
        | Not_found -> let package_desc = "(" ^ (string_of_int k) ^ "," ^ (try Package_id_map.find k (!package_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "package" package_desc ("package of the repository \"" ^ (self#name) ^ "\"")
    end) in

    (* universe *)
    let _ = List.iter convert_component_type u.Json_t.universe_component_types (* fill the component type table *) in
    let _ = List.iter convert_repository u.Json_t.universe_repositories        (* fill the repository and package tables *) in   
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
  method get_implementation id = Component_type_id_map.find id implem_get_implementation
  method get_repository     id = Repository_id_map.find id implem_get_repository
  method get_package        id = Package_id_map.find id implem_get_package

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
           | Not_found -> let tmp = (Functions_model.requirers implem_get_component_type p) in
             implem_ur <- Port_id_map.add p tmp implem_ur; tmp)
  method up p = (try Port_id_map.find p implem_up with
           | Not_found -> let tmp = (Functions_model.providers implem_get_component_type p) in
             implem_ur <- Port_id_map.add p tmp implem_ur; tmp)
  method uc p = (try Port_id_map.find p implem_ur with
           | Not_found -> let tmp = (Functions_model.conflicters implem_get_component_type p) in
             implem_ur <- Port_id_map.add p tmp implem_ur; tmp)

  (* methods for naming *)
  method get_port_id           n = Port_name_map.find n implem_get_port_id
  method get_component_type_id n = Component_type_name_map.find n implem_get_component_type_id
  method get_repository_id     n = Repository_name_map.find n implem_get_repository_id
  method get_package_id      r n = Repository_id_Package_name_map.find (r,n) implem_get_package_id

  method get_port_name           id = Port_id_map.find id implem_get_port_name
  method get_component_type_name id = Component_type_id_map.find id implem_get_component_type_name
  method get_repository_name     id = Repository_id_map.find id implem_get_repository_name
  method get_package_name        id = Package_id_map.find id implem_get_package_name
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


(* 2. Configuration Conversion *)

(*
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

class load_configuration get_resource_id get_port_id get_component_type_id get_package_id get_repository_id c =
  (* 1. Data storage *)
    (* locations *)
  let locations         : Location_set.t ref = ref Location_set.empty in
  let location_names    : Location_name_set.t ref = ref Location_name_set.empty in
  let location_ids      : Location_name_set.t ref = ref Location_id_set.empty in
  let location_get      : (location Location_id_map.t) ref = ref Location_id_map.empty in
  let location_get_id   : (location_id Location_name_map.t) ref = ref Location_name_map.empty in
  let location_get_name : (location_name Location_id_map.t) ref = ref Location_id_map.empty in
  let add_location r id = 
    locations         := Location_set.add r (!locations);
    location_names    := Location_name_set.add r.location_name (!location_names);
    location_ids      := Location_id_set.add id (!location_ids);
    location_get      := Location_id_map.add id r (!location_get);
    location_get_id   := Location_name_map.add r.location_name id (!location_get_id);
    location_get_name := Location_name_set.add id r.location_name (!location_get_name) in
  let location_current_id = ref 0 in
  let get_current_location_id () = !location_current_id in
  let new_location l = 
    let id = !location_current_id in location_current_id := (!location_current_id) + 1; add_location l id; id in
  let find_location l = try Location_name_map.find l location_get_id with
    | Not_found -> Zephyrus_log.log_panic ("Mentionning the non-existant \"" ^ l ^ "\" location in the implementation relation") in
    (* components *)
  let components         : Component_set.t ref = ref Component_set.empty in
  let component_names    : Component_name_set.t ref = ref Component_name_set.empty in
  let component_ids      : Component_name_set.t ref = ref Component_id_set.empty in
  let component_get      : (component Component_id_map.t) ref = ref Component_id_map.empty in
  let component_get_id   : (component_id Component_name_map.t) ref = ref Component_name_map.empty in
  let component_get_name : (component_name Component_id_map.t) ref = ref Component_id_map.empty in
  let add_component r id = 
    components         := Component_set.add r (!components);
    component_names    := Component_name_set.add r.component_name (!component_names);
    component_ids      := Component_id_set.add id (!component_ids);
    component_get      := Component_id_map.add id r (!component_get);
    component_get_id   := Component_name_map.add r.component_name id (!component_get_id);
    component_get_name := Component_name_set.add id r.component_name (!component_get_name) in
  let component_current_id = ref 0 in
  let get_current_component_id () = !component_current_id in
  let new_component t = 
    let id = !component_current_id in component_current_id := (!component_current_id) + 1; add_component t id; id in
  let find_component l = try Component_name_map.find l component_get_id with
    | Not_found -> Zephyrus_log.log_panic ("Mentionning the non-existant \"" ^ l ^ "\" component in the implementation relation") in

  (* 2. Conversion *)
    (* location *)
  let convert_location l = new_location {
    location_name               = convert_location_name l.Json_t.location_name;
    location_repository         = get_repository_id (convert_repository_name l.Json_t.location_repository);
    location_packages_installed = Package_name_set.set_of_list (convert_package_name) l.Json_t.location_packages_installed;
    location_provide_resources  = Resource_name_map.map_of_list (fun (r,n) -> (convert_resource_name r, convert_resource_provide n)) l.Json_t.location_provide_resources }

let convert_component c = {
  component_name     = convert_component_name c.Json_t.component_name;
  component_type     = convert_component_type_name c.Json_t.component_type;
  component_location = convert_location_name c.Json_t.component_location }

let convert_binding b = {
  binding_port     = convert_port_name b.Json_t.binding_port;
  binding_requirer = convert_component_name b.Json_t.binding_requirer;
  binding_provider = convert_component_name b.Json_t.binding_provider }

let convert_configuration c = {
  configuration_locations  = Location_name_map.map_of_list (fun l -> let l' = convert_location l in (l'.location_name, l')) c.Json_t.configuration_locations;
  configuration_components = Component_name_map.map_of_list (fun c -> let c' = convert_component c in (c'.component_name, c')) c.Json_t.configuration_components;
  configuration_bindings   = Binding_set.set_of_list (convert_binding) c.Json_t.configuration_bindings }

*)

let convert_spec_variable_name = fun x -> x
let convert_spec_const         = fun x -> x
let convert_spec_op o          = match o with | `Lt -> Lt | `LEq -> LEq
   | `Eq -> Eq | `GEq -> GEq | `Gt -> Gt | `NEq -> NEq

let convert_spec_local_element el = match el with
  | `SpecLocalElementPackage(k)       -> SpecLocalElementPackage(k)
  | `SpecLocalElementComponentType(t) -> SpecLocalElementComponentType(t)
  | `SpecLocalElementPort(p)          -> SpecLocalElementPort(p)
let rec convert_spec_local_expr e = match e with
  | `SpecLocalExprVar(v)        -> SpecLocalExprVar(convert_spec_variable_name v)
  | `SpecLocalExprConst(c)      -> SpecLocalExprConst(convert_spec_const c)
  | `SpecLocalExprArity(el)     -> SpecLocalExprArity(convert_spec_local_element el)
  | `SpecLocalExprAdd(ex1, ex2) -> SpecLocalExprAdd(convert_spec_local_expr ex1, convert_spec_local_expr ex2)
  | `SpecLocalExprSub(ex1, ex2) -> SpecLocalExprSub(convert_spec_local_expr ex1, convert_spec_local_expr ex2)
  | `SpecLocalExprMul(n, ex)    -> SpecLocalExprMul(convert_spec_const n, convert_spec_local_expr ex)
let rec convert_local_specification sl = match sl with 
  | `SpecLocalTrue            -> SpecLocalTrue
  | `SpecLocalOp(ex1, o, ex2) -> SpecLocalOp(convert_spec_local_expr ex1, convert_spec_op o, convert_spec_local_expr ex2)
  | `SpecLocalAnd(sl1, sl2)   -> SpecLocalAnd(convert_local_specification sl1, convert_local_specification sl2)
  | `SpecLocalOr(sl1, sl2)    -> SpecLocalOr(convert_local_specification sl1, convert_local_specification sl2)
  | `SpecLocalImpl(sl1, sl2)  -> SpecLocalImpl(convert_local_specification sl1, convert_local_specification sl2)
  | `SpecLocalNot(sl')   -> SpecLocalNot(convert_local_specification sl')

let convert_spec_repository_constraint = fun x -> x
let convert_spec_resource_constraint (r, o, c) = (convert_resource_name r, convert_spec_op o, convert_spec_const c)

let convert_spec_element el = match el with
  | `SpecElementPackage(k)             -> SpecElementPackage(k)
  | `SpecElementComponentType(t)       -> SpecElementComponentType(t)
  | `SpecElementPort(p)                -> SpecElementPort(p)
  | `SpecElementLocalisation(phi,r,sl) -> SpecElementLocalisation(
        List.map convert_spec_resource_constraint phi, List.map convert_spec_repository_constraint r, convert_local_specification sl)
let rec convert_spec_expr e = match e with
  | `SpecExprVar(v)        -> SpecExprVar(convert_spec_variable_name v)
  | `SpecExprConst(c)      -> SpecExprConst(convert_spec_const c)
  | `SpecExprArity(el)     -> SpecExprArity(convert_spec_element el)
  | `SpecExprAdd(ex1, ex2) -> SpecExprAdd(convert_spec_expr ex1, convert_spec_expr ex2)
  | `SpecExprSub(ex1, ex2) -> SpecExprSub(convert_spec_expr ex1, convert_spec_expr ex2)
  | `SpecExprMul(n, ex)    -> SpecExprMul(convert_spec_const n, convert_spec_expr ex)
let rec convert_specification s =  match s with 
  | `SpecTrue            -> SpecTrue
  | `SpecOp(ex1, o, ex2) -> SpecOp(convert_spec_expr ex1, convert_spec_op o, convert_spec_expr ex2)
  | `SpecAnd(s1, s2)   -> SpecAnd(convert_specification s1, convert_specification s2)
  | `SpecOr(s1, s2)    -> SpecOr(convert_specification s1, convert_specification s2)
  | `SpecImpl(s1, s2)  -> SpecImpl(convert_specification s1, convert_specification s2)
  | `SpecNot(s')   -> SpecNot(convert_specification s')

(* 1. Universe *)

let component_type_name_of_string    str = convert_component_type_name (Json_j.component_type_name_of_string str)
let port_name_of_string              str = convert_port_name (Json_j.port_name_of_string str)
let component_name_of_string         str = convert_component_name (Json_j.component_name_of_string str)
let package_name_of_string           str = convert_package_name (Json_j.package_name_of_string str)
let repository_name_of_string        str = convert_repository_name (Json_j.repository_name_of_string str)
let location_name_of_string          str = convert_location_name (Json_j.location_name_of_string str)
let resource_name_of_string          str = convert_resource_name (Json_j.resource_name_of_string str)
let provide_arity_of_string          str = convert_provide_arity (Json_j.provide_arity_of_string str)
let require_arity_of_string          str = convert_require_arity (Json_j.require_arity_of_string str)
let resource_consumption_of_string   str = convert_resource_consumption (Json_j.resource_consumption_of_string str)
let resource_provide_arity_of_string str = convert_resource_provide (Json_j.resource_provide_arity_of_string str)
let component_type_of_string         str = convert_component_type (Json_j.component_type_of_string str)
let component_types_of_string        str = List.map convert_component_type (Json_j.component_types_of_string str)
let package_of_string                str = convert_package (Json_j.package_of_string str)
let packages_of_string               str = List.map convert_package (Json_j.packages_of_string str)
let repository_of_string             str = convert_repository (Json_j.repository_of_string str)
let repositories_of_string           str = List.map convert_repository (Json_j.repositories_of_string str)
let package_names_of_string          str = List.map convert_package_name (Json_j.package_names_of_string str)
let universe_of_string               str = convert_universe (Json_j.universe_of_string str)


(* 2. Configuration *)

let location_of_string      str = convert_location (Json_j.location_of_string str)
let configuration_of_string str = convert_configuration (Json_j.configuration_of_string str)


(* 3. Specification *)

let specification_of_string      str = convert_specification (let lexbuf = Lexing.from_string str in Specification_parser.main Specification_lexer.token lexbuf)
let specification_of_string_json str = convert_specification (Json_j.specification_of_string str)


(* 4. Main load functions *)


let load_universe _ = 
  let repository_name     : string option ref = ref None in
  let repository_filename : string option ref = ref None in
  let repository          : repository option ref = ref None in
  let get_repository name filename = repository_name := Some(name); repository_filename := Some(filename);
    Input_helper.input_and_set ("Repository \"" ^ name ^ "\"") repository_filename repository repository_of_string in
  let repositories = List.map (fun (n,r) -> get_repository n r; (n,!repository)) (!Settings.input_file_repositories) in
  Input_helper.input_and_set "Universe" Settings.input_file_universe Data_state.universe_full universe_of_string;
  match !Data_state.universe_full with
  | None -> ()
  | Some(u) -> Data_state.universe_full := Some ({
      universe_component_types = u.universe_component_types;
      universe_implementation = u.universe_implementation;
      universe_repositories = List.fold_left (fun m (n,r) -> match r with None -> m | Some(r') -> Repository_name_map.add n r' m) u.universe_repositories repositories })


let load_universe_flat _ = () (* TODO: implement that function *)

let load_configuration _ = Input_helper.input_and_set "Initial Configuration"
    Settings.input_file_initial_configuration Data_state.configuration_init_full configuration_of_string

let load_specification _ = Input_helper.input_and_set "Specification"
    Settings.input_file_specification Data_state.specification_full specification_of_string

let load_optimization_function _ = match !Settings.input_optimization_function with
  | None -> Zephyrus_log.log_setting_not_set "Optimization function"
  | Some(opt) -> Data_state.optimization_function := Some( match opt with
    | Settings.Optim_none         -> Optimization_function_none
    | Settings.Optim_simple       -> Optimization_function_simple
    | Settings.Optim_compact      -> Optimization_function_compact
    | Settings.Optim_spread       -> Optimization_function_spread
    | Settings.Optim_conservative -> Optimization_function_conservative)

let load_model _ = 
  load_universe ();
  load_configuration ();
  load_specification ();
  load_optimization_function ()



