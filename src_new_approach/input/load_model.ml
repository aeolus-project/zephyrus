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


(*
  class type name_id_mapping = object
    method ids        : Component_type_id_set.t
    method names      : Component_type_name_set.t
    method name_of_id : component_type_id   -> component_type_name
    method id_of_name : component_type_name -> component_type_id
  end
*)

(*
module Name_id_mapping =
  functor (Id_set   : Set.S) ->
  functor (Name_set : Set.S) ->
  struct

    class type name_id_mapping = object
      method ids        : Id_set.t
      method names      : Name_set.t
      method name_of_id : Id_set.elt   -> Name_set.elt
      method id_of_name : Name_set.elt -> Id_set.elt
    end

  end

module Component_type_name_id_mapping = Name_id_mapping(Component_type_id_set)(Component_type_name_set)

*)

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

(* for unique identifier creation *)
module Incrementing_integer :
sig 
  type t
  val create  : unit -> t
  val current : t -> int
  val next    : t -> int
end = struct
  type t = int ref
  let create () = ref 0
  let current t = !t
  let next t =
    let value = !t in
    t := !t + 1;
    value
end

module Incrementing_id = Incrementing_integer

(* this module is used to get from a package name the right package, by giving in addition in which repository to look in for *)
module Repository_id_package_name = struct
  type t = repository_id * package_name
  let compare v1 v2 =
    let r1 = Repository_id.compare (fst v1) (fst v2) in if r1 = 0 then
      Package_name.compare (snd v1) (snd v2)
    else r1
end 

module Repository_id_package_name_set = Data_common.Set.Make(Repository_id_package_name)
module Repository_id_package_name_map = Data_common.Map.Make(Repository_id_package_name)


class type name_id_mappings = object
  (* component_type *)
  method component_type_ids        : Component_type_id_set.t
  method component_type_names      : Component_type_name_set.t
  method component_type_name_of_id : component_type_id   -> component_type_name
  method component_type_id_of_name : component_type_name -> component_type_id
  (* port *)
  method port_ids                  : Port_id_set.t
  method port_names                : Port_name_set.t
  method port_name_of_id           : port_id   -> port_name
  method port_id_of_name           : port_name -> port_id
  (* repository *)
  method repository_ids            : Repository_id_set.t
  method repository_names          : Repository_name_set.t
  method repository_name_of_id     : repository_id   -> repository_name
  method repository_id_of_name     : repository_name -> repository_id
  (* package *)
  method package_ids               : Package_id_set.t
  method package_names             : Package_name_set.t
  method package_name_of_id        : package_id -> package_name
  method package_id_of_name        : Repository_id_package_name.t -> package_id
  (* resource *)
  method resource_ids              : Resource_id_set.t
  method resource_names            : Resource_name_set.t
  method resource_name_of_id       : resource_id   -> resource_name
  method resource_id_of_name       : resource_name -> resource_id
  (* location *)
  method location_ids              : Location_id_set.t
  method location_names            : Location_name_set.t
  method location_name_of_id       : location_id   -> location_name
  method location_id_of_name       : location_name -> location_id
  (* component *)
  method component_ids             : Component_id_set.t
  method component_names           : Component_name_set.t
  method component_name_of_id      : component_id   -> component_name
  method component_id_of_name      : component_name -> component_id
end

class name_id_mappings_of_json universe initial_configuration specification : name_id_mappings = 

    (* component types *)
  let component_type_names          : Component_type_name_set.t ref                     = ref Component_type_name_set.empty in  (* all component type names *)
  let component_type_ids            : Component_type_id_set.t ref                       = ref Component_type_id_set.empty in    (* all component type ids *)
  let component_type_name_to_id_map : (component_type_id Component_type_name_map.t) ref = ref Component_type_name_map.empty in  (* mapping component type name -> component type id *)
  let component_type_id_to_name_map : (component_type_name Component_type_id_map.t) ref = ref Component_type_id_map.empty in    (* mapping component type id -> component type name *)

  let component_type_id_of_name (name : component_type_name) : component_type_id   = Component_type_name_map.find name !component_type_name_to_id_map in
  let component_type_name_of_id (id   : component_type_id)   : component_type_name = Component_type_id_map.find   id   !component_type_id_to_name_map in

  (* function that adds the relation id <-> name in the maps. *)
  let add_component_type name id =
    component_type_names          := Component_type_name_set.add name    (!component_type_names);
    component_type_ids            := Component_type_id_set.add   id      (!component_type_ids);
    component_type_name_to_id_map := Component_type_name_map.add name id (!component_type_name_to_id_map);
    component_type_id_to_name_map := Component_type_id_map.add   id name (!component_type_id_to_name_map) in

  (* for unique identifier creation *)
  let component_type_current_id = Incrementing_id.create () in

  (* look for the component type in the maps, or create it *)
  let get_or_add_component_type_id name = 
    try component_type_id_of_name name
    with Not_found -> let id = Incrementing_id.next component_type_current_id in add_component_type name id; id in



    (* ports *)
  let port_names          : Port_name_set.t ref           = ref Port_name_set.empty in   (* all port names *)
  let port_ids            : Port_id_set.t ref             = ref Port_id_set.empty in     (* all port ids *)
  let port_name_to_id_map : (port_id Port_name_map.t) ref = ref Port_name_map.empty in   (* mapping port name -> port id *)
  let port_id_to_name_map : (port_name Port_id_map.t) ref = ref Port_id_map.empty in     (* mapping port id -> port name *)

  let port_id_of_name (name : port_name) : port_id   = Port_name_map.find name !port_name_to_id_map in
  let port_name_of_id (id   : port_id)   : port_name = Port_id_map.find   id   !port_id_to_name_map in

  (* function that adds the relation name <-> id in the maps. *)
  let add_port name id = 
    port_names          := Port_name_set.add name (!port_names); 
    port_ids            := Port_id_set.add   id   (!port_ids);
    port_name_to_id_map := Port_name_map.add name id (!port_name_to_id_map);
    port_id_to_name_map := Port_id_map.add   id name (!port_id_to_name_map) in

  (* for unique identifier creation *)
  let port_current_id = Incrementing_id.create () in

  (* look for the port in the maps, or create it. *)
  let get_or_add_port_id name = 
    try port_id_of_name name
    with Not_found -> let id = Incrementing_id.next port_current_id in add_port name id; id in


    (* repositories *)
  let repository_names          : Repository_name_set.t ref                 = ref Repository_name_set.empty in  (* all repository names *)
  let repository_ids            : Repository_id_set.t ref                   = ref Repository_id_set.empty in    (* all repository ids *)
  let repository_name_to_id_map : (repository_id Repository_name_map.t) ref = ref Repository_name_map.empty in  (* mapping repository name -> repository id *)
  let repository_id_to_name_map : (repository_name Repository_id_map.t) ref = ref Repository_id_map.empty in    (* mapping repository id -> repository name *)

  let repository_id_of_name (name : repository_name) : repository_id   = Repository_name_map.find name !repository_name_to_id_map in
  let repository_name_of_id (id   : repository_id)   : repository_name = Repository_id_map.find   id   !repository_id_to_name_map in

  (* function that adds the relation r <-> id <-> r#name in the maps. *)
  let add_repository name id = 
    repository_names          := Repository_name_set.add name    (!repository_names);
    repository_ids            := Repository_id_set.add   id      (!repository_ids);
    repository_name_to_id_map := Repository_name_map.add name id (!repository_name_to_id_map);
    repository_id_to_name_map := Repository_id_map.add   id name (!repository_id_to_name_map) in

  (* for unique identifier creation *)
  let repository_current_id = Incrementing_id.create () in

  (* look for the repository in the maps, or create it. *)
  let get_or_add_repository_id name = 
    try repository_id_of_name name
    with Not_found -> let id = Incrementing_id.next repository_current_id in add_repository name id; id in


    (* packages *)
  let package_names          : Package_name_set.t ref                            = ref Package_name_set.empty in               (* all package names *)
  let package_ids            : Package_id_set.t ref                              = ref Package_id_set.empty in                 (* all package ids *)
  let package_name_to_id_map : (package_id Repository_id_package_name_map.t) ref = ref Repository_id_package_name_map.empty in (* mapping (repository id * package name) -> package id *)
  let package_id_to_name_map : (package_name Package_id_map.t) ref               = ref Package_id_map.empty in                 (* mapping package id -> package name *)
  
  let package_id_of_name (name : Repository_id_package_name.t) : package_id   = Repository_id_package_name_map.find name !package_name_to_id_map in
  let package_name_of_id (id   : package_id)                   : package_name = Package_id_map.find                 id   !package_id_to_name_map in

  (* function that adds the relation (repository_id * package_name) <-> package id in the mapping. *)
  let add_package r_id name package_id = 
    package_names           := Package_name_set.add name (!package_names);
    package_ids             := Package_id_set.add   package_id   (!package_ids);
    package_name_to_id_map  := Repository_id_package_name_map.add (r_id, name) package_id (!package_name_to_id_map);
    package_id_to_name_map  := Package_id_map.add package_id name (!package_id_to_name_map) in

  (* for unique identifier creation *)
  let package_current_id = Incrementing_id.create () in 

  (* find the id for (repository_id * package_name) or create it. *)
  let get_or_add_package_id r_id name = 
    try package_id_of_name (r_id, name)
    with Not_found -> let id = Incrementing_id.next package_current_id in add_package r_id name id; id in


    (* resources *)
  let resource_names          : Resource_name_set.t ref               = ref Resource_name_set.empty in (* all resource names *)
  let resource_ids            : Resource_id_set.t ref                 = ref Resource_id_set.empty in   (* all resource ids *)
  let resource_name_to_id_map : (resource_id Resource_name_map.t) ref = ref Resource_name_map.empty in (* mapping name -> id *)
  let resource_id_to_name_map : (resource_name Resource_id_map.t) ref = ref Resource_id_map.empty in   (* mapping id -> name *)

  let resource_id_of_name (name : resource_name) : resource_id   = Resource_name_map.find name !resource_name_to_id_map in
  let resource_name_of_id (id   : resource_id)   : resource_name = Resource_id_map.find   id   !resource_id_to_name_map in

  (* create all the structure to store the relation name <-> id. *)
  let add_resource name id = 
    resource_names          := Resource_name_set.add name !resource_names;
    resource_ids            := Resource_id_set.add id !resource_ids;
    resource_id_to_name_map := Resource_id_map.add id name !resource_id_to_name_map;
    resource_name_to_id_map := Resource_name_map.add name id !resource_name_to_id_map in

  (* for unique identifier creation *)
  let resource_current_id = Incrementing_id.create () in 

  (* get the id for the name, or create the data for it *)
  let get_or_add_resource_id name =
    try resource_id_of_name name 
    with Not_found -> let id = Incrementing_id.next resource_current_id in add_resource name id; id in


    (* locations *)
  let location_names          : Location_name_set.t ref               = ref Location_name_set.empty in  (* all location names *)
  let location_ids            : Location_id_set.t ref                 = ref Location_id_set.empty in    (* all location ids *)
  let location_name_to_id_map : (location_id Location_name_map.t) ref = ref Location_name_map.empty in  (* mapping name -> is *)
  let location_id_to_name_map : (location_name Location_id_map.t) ref = ref Location_id_map.empty in    (* mapping id -> name *)
  
  let location_id_of_name (name : location_name) : location_id   = Location_name_map.find name !location_name_to_id_map in
  let location_name_of_id (id   : location_id)   : location_name = Location_id_map.find   id   !location_id_to_name_map in

  (* add all the structure to store the mapping id <-> l#name. *)
  let add_location name id = 
    location_names    := Location_name_set.add name    (!location_names);
    location_ids      := Location_id_set.add   id      (!location_ids);
    location_name_to_id_map   := Location_name_map.add name id (!location_name_to_id_map);
    location_id_to_name_map := Location_id_map.add   id name (!location_id_to_name_map) in
  
  (* for unique identifier creation *)
  let location_current_id = Incrementing_id.create () in
  
  (* get the id for the name, or create the data for it *)
  let get_or_add_location_id name = 
    try location_id_of_name name
    with Not_found -> let id = Incrementing_id.next location_current_id in add_location name id; id in


    (* components *)
  let component_names          : Component_name_set.t ref = ref Component_name_set.empty in                 (* all component names *) 
  let component_ids            : Component_id_set.t ref = ref Component_id_set.empty in                     (* all component ids *)
  let component_name_to_id_map : (component_id Component_name_map.t) ref = ref Component_name_map.empty in  (* mapping name -> id *)
  let component_id_to_name_map : (component_name Component_id_map.t) ref = ref Component_id_map.empty in    (* mapping id -> name *)
  
  let component_id_of_name (name : component_name) : component_id   = Component_name_map.find name !component_name_to_id_map in
  let component_name_of_id (id   : component_id)   : component_name = Component_id_map.find   id   !component_id_to_name_map in

  (* add all the structure to store the mapping  id <-> c#name. *)
  let add_component name id =  
    component_names          := Component_name_set.add name    (!component_names);
    component_ids            := Component_id_set.add   id      (!component_ids);
    component_name_to_id_map := Component_name_map.add name id (!component_name_to_id_map);
    component_id_to_name_map := Component_id_map.add   id name (!component_id_to_name_map) in
  
  (* for unique identifier creation *)
  let component_current_id = Incrementing_id.create () in
  
  (* get the id for the name, or create the data for it *)
  let get_or_add_component_id name =
    try component_id_of_name name
    with Not_found -> let id = Incrementing_id.next component_current_id in add_component name id; id in
  


  (* do it! *)

  (* component types *)
  let add_component_type ct =
    let open Json_t in
    get_or_add_component_type_id (convert_component_type_name ct.component_type_name);
    List.map get_or_add_port_id     (List.map convert_port_name     (List.map fst ct.component_type_provide ));
    List.map get_or_add_port_id     (List.map convert_port_name     (List.map fst ct.component_type_require ));
    List.map get_or_add_port_id     (List.map convert_port_name                   ct.component_type_conflict);
    List.map get_or_add_resource_id (List.map convert_resource_name (List.map fst ct.component_type_consume )) in

  (* packages *)
  let add_package r_id k =
    let open Json_t in
    get_or_add_package_id r_id (convert_package_name "" k.package_name);
    List.map (get_or_add_package_id r_id) (List.map (convert_package_name "") (List.flatten k.package_depend  ));
    List.map (get_or_add_package_id r_id) (List.map (convert_package_name "")               k.package_conflict);
    List.map get_or_add_resource_id       (List.map convert_resource_name (List.map fst k.package_consume )) in

  (* repositories *)
  let add_repository r =
    let open Json_t in
    let r_id = get_or_add_repository_id (convert_repository_name r.repository_name) in
    List.map (add_package r_id) r.repository_packages in

  (* universe *)
  let add_universe u =
    let open Json_t in
    List.map add_component_type u.universe_component_types;
    List.map add_repository     u.universe_repositories in

  (* component *)
  let add_component c =
    let open Json_t in
    get_or_add_component_id (convert_component_name c.component_name);
    get_or_add_component_type_id (convert_component_type_name c.component_type); (* Hmm... What with deprecated component types? *)
    get_or_add_location_id (convert_location_name c.component_location) in (* This is useless, as if the location does not exist, the initial lonfig is not valid. *)

  (* binding *) (* Is this useful at all? *)
  let add_binding b =
    let open Json_t in
    get_or_add_port_id (convert_port_name b.binding_port);
    List.map get_or_add_component_id (List.map convert_component_name [b.binding_requirer; b.binding_provider]) in

  (* location *)
  let add_location l =
    let open Json_t in
    get_or_add_location_id (convert_location_name l.location_name);
    List.map get_or_add_resource_id (List.map convert_resource_name (List.map fst l.location_provide_resources));
    let r_id = get_or_add_repository_id (convert_repository_name l.location_repository) in (* What if the repository does not exist in the universe? *)
    List.map (get_or_add_package_id r_id) (List.map (convert_package_name "") l.location_packages_installed) in

  (* configuration *)
  let add_configuration c =
    let open Json_t in
    List.map add_location  c.configuration_locations;
    List.map add_component c.configuration_components;
    List.map add_binding   c.configuration_bindings in

  object
    (* component_type *)
    method component_type_ids        = !component_type_ids
    method component_type_names      = !component_type_names
    method component_type_name_of_id = component_type_name_of_id
    method component_type_id_of_name = component_type_id_of_name
    (* port *)
    method port_ids                  = !port_ids
    method port_names                = !port_names
    method port_name_of_id           = port_name_of_id
    method port_id_of_name           = port_id_of_name
    (* repository *)
    method repository_ids            = !repository_ids
    method repository_names          = !repository_names
    method repository_name_of_id     = repository_name_of_id
    method repository_id_of_name     = repository_id_of_name
    (* package *)
    method package_ids               = !package_ids
    method package_names             = !package_names
    method package_name_of_id        = package_name_of_id
    method package_id_of_name        = package_id_of_name
    (* resource *)
    method resource_ids              = !resource_ids
    method resource_names            = !resource_names
    method resource_name_of_id       = resource_name_of_id
    method resource_id_of_name       = resource_id_of_name
    (* location *)
    method location_ids              = !location_ids
    method location_names            = !location_names
    method location_name_of_id       = location_name_of_id
    method location_id_of_name       = location_id_of_name
    (* component *)
    method component_ids             = !component_ids
    method component_names           = !component_names
    method component_name_of_id      = component_name_of_id
    method component_id_of_name      = component_id_of_name
  end
  




(*/*************************************************\*)
(*| 1. Resource Names and Ids Extractions           |*)
(*\*************************************************/*)

(* empty ... *)


(*/*************************************************\*)
(*| 2. Universe Conversion                          |*)
(*\*************************************************/*)


(* function for port manipulation *)
(*  - check if a provide does really provide a port *)
let port_is_provide_strict prov = match prov with | Finite_provide i -> i > 0 | Infinite_provide -> true
(*  - computes the set of component types (id) that requires the port in parameter *)
let requirers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
  (fun id t -> if Port_id_set.mem port_id t#require_domain then (t#require port_id) > 0 else false) component_types)
(*  - computes the set of component types (id) that provides the port in parameter *)
let providers component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
  (fun id t -> if Port_id_set.mem port_id t#provide_domain then port_is_provide_strict (t#provide port_id) else false) component_types)
(*  - computes the set of component types (id) that are in conflict with the port in parameter *)
let conflicters component_types port_id = Component_type_id_map_extract_key.set_of_keys (Component_type_id_map.filter
  (fun id t -> Port_id_set.mem port_id (t#conflict)) component_types)



(*/*********************************)
(* class for loading a universe from a Json_j structure. Contains all conversion function concerning universes *)
class convert_universe get_resource_id get_resource_name external_repositories u = 
  (* 1. Data Storage *)

    (* ports *)
  let port_names    : Port_name_set.t ref = ref Port_name_set.empty in             (* all port names *)
  let port_ids      : Port_id_set.t ref   = ref Port_id_set.empty in               (* all port ids *)
  let port_get_id   : (port_id Port_name_map.t) ref = ref Port_name_map.empty in   (* mapping port name -> port id *)
  let port_get_name : (port_name Port_id_map.t) ref = ref Port_id_map.empty in     (* mapping port id -> port name *)

  let add_port name id = (* function that adds the relation name <-> id in the maps. Never used directly (see [find_port]) *)
    port_names    := Port_name_set.add name (!port_names); 
    port_ids      := Port_id_set.add id (!port_ids);
    port_get_id   := Port_name_map.add name id (!port_get_id);
    port_get_name := Port_id_map.add id name (!port_get_name) in

  let port_current_id = Incrementing_id.create () in  (* for unique identifier creation *)
  let find_port name = try Port_name_map.find name !port_get_id with (* look for the port in the maps, or create it. *)
    | Not_found -> let id = Incrementing_id.next port_current_id in add_port name id; id in

    (* component types *)
  let component_types         : Component_type_set.t ref = ref Component_type_set.empty in                                (* all component types *)
  let component_type_names    : Component_type_name_set.t ref = ref Component_type_name_set.empty in                      (* all component type names *)
  let component_type_ids      : Component_type_id_set.t ref = ref Component_type_id_set.empty in                          (* all component type ids *)

  let component_type_get      : (component_type Component_type_id_map.t) ref = ref Component_type_id_map.empty in         (* mapping component type id -> component type *)
  let component_type_get_id   : (component_type_id Component_type_name_map.t) ref = ref Component_type_name_map.empty in  (* mapping component type name -> component type id *)
  let component_type_get_name : (component_type_name Component_type_id_map.t) ref = ref Component_type_id_map.empty in    (* mapping component type id -> component type name *)

  let add_component_type t id =      (* function that adds the relation t <-> id <-> t#name in the maps. Never used directly (see [new_component_type]) *)
    component_types         := Component_type_set.add t (!component_types);
    component_type_names    := Component_type_name_set.add t#name (!component_type_names);
    component_type_ids      := Component_type_id_set.add id (!component_type_ids);
    component_type_get      := Component_type_id_map.add id t (!component_type_get);
    component_type_get_id   := Component_type_name_map.add t#name id (!component_type_get_id);
    component_type_get_name := Component_type_id_map.add id t#name (!component_type_get_name) in

  let component_type_current_id = Incrementing_id.create () in  (* for unique identifier creation *)
  let new_component_type t =                (* create all the data structure to store the component type in parameter *)
    let id = Incrementing_id.next component_type_current_id in add_component_type t id in
  let find_component_type_id name = try Component_type_name_map.find name !component_type_get_id with (* annex function, for the implementation relation *)
    | Not_found ->  (* erroneous case, but well, for sake of consistency, we pospone error message in the check phase *)
      let id = Incrementing_id.next component_type_current_id in (* create a shallow structure for the component type (but without an actual component type...) *)
        component_type_names    := Component_type_name_set.add name (!component_type_names);
        component_type_ids      := Component_type_id_set.add id (!component_type_ids);
        component_type_get_id   := Component_type_name_map.add name id (!component_type_get_id);
        component_type_get_name := Component_type_id_map.add id name (!component_type_get_name); 
        id in

    (* package *)
  let packages          : Package_set.t ref      = ref Package_set.empty in                                               (* all packages *)
  let package_names     : Package_name_set.t ref = ref Package_name_set.empty in                                          (* all package names *)
  let package_ids       : Package_id_set.t ref   = ref Package_id_set.empty in                                            (* all package ids *)

  let package_get       : (package Package_id_map.t) ref                    = ref Package_id_map.empty in                 (* mapping package id -> package *)
  let package_get_id    : (package_id Repository_id_package_name_map.t) ref = ref Repository_id_package_name_map.empty in (* mapping (repository id * package name) -> package id *)
  let package_get_name  : (package_name Package_id_map.t) ref               = ref Package_id_map.empty in                 (* mapping package id -> package name *)
  let repos_of_packages : (repository_id Package_id_map.t) ref              = ref Package_id_map.empty in                 (* mapping package id -> repository id *)
  
  let local_packages    : Package_set.t ref      = ref Package_set.empty in      (* packages local to a repository *)
  let local_package_ids : Package_id_set.t ref   = ref Package_id_set.empty in   (* ids of the packages local to a repository *)

  let add_package r name id = (* function that adds the relation (r * name) <-> id in the mapping. Never used directly (see [find_package]) *)
    repos_of_packages := Package_id_map.add id r !repos_of_packages;
    package_names     := Package_name_set.add name (!package_names);
    package_ids       := Package_id_set.add id (!package_ids);                             (* note that because of the dependencies and conflict between packages *)
    local_package_ids := Package_id_set.add id (!package_ids);                             (* we might encounter a package name before the package definition     *)
    package_get_id    := Repository_id_package_name_map.add (r,name) id (!package_get_id); (* hence the distinction between the shallow structure creation        *)
    package_get_name  := Package_id_map.add id name (!package_get_name) in                 (* [find_package] and the full one [new_package]                       *)

  let package_current_id = Incrementing_id.create () in (* for unique identifier creation *)
  let find_package r name = try Repository_id_package_name_map.find (r,name) !package_get_id with (* find the id for (r*name) or create a shallow structure for it *)
    | Not_found -> let id = Incrementing_id.next package_current_id in add_package r name id; id in
  let new_package r k = let id = find_package r k#name in (* create a full structure for the package *)
    packages       := Package_set.add k (!packages);
    local_packages := Package_set.add k (!packages);
    package_get    := Package_id_map.add id k (!package_get); (id,k) in

    (* repositories *)
  let repositories        : Repository_set.t ref      = ref Repository_set.empty in                       (* all repositories *)
  let repository_names    : Repository_name_set.t ref = ref Repository_name_set.empty in                  (* all repository names *)
  let repository_ids      : Repository_id_set.t ref   = ref Repository_id_set.empty in                    (* all repository ids *)

  let repository_get      : (repository Repository_id_map.t) ref      = ref Repository_id_map.empty in    (* mapping repository id -> repository *)
  let repository_get_id   : (repository_id Repository_name_map.t) ref = ref Repository_name_map.empty in  (* mapping repository name -> repository id *)
  let repository_get_name : (repository_name Repository_id_map.t) ref = ref Repository_id_map.empty in    (* mapping repository id -> repository name *)

  let add_repository r id = (* function that adds the relation r <-> id <-> r#name in the maps. Never used directly (see [new_repository]) *)
    repositories        := Repository_set.add r (!repositories);
    repository_names    := Repository_name_set.add r#name (!repository_names);
    repository_ids      := Repository_id_set.add id (!repository_ids);
    repository_get      := Repository_id_map.add id r (!repository_get);
    repository_get_id   := Repository_name_map.add r#name id (!repository_get_id);
    repository_get_name := Repository_id_map.add id r#name (!repository_get_name) in

  let repository_current_id = Incrementing_id.create () in (* for unique identifier creation *)
  let get_current_repository_id () = Incrementing_id.current repository_current_id in
  let new_repository r = (* create a full structure for the repository *)
    let id = Incrementing_id.next repository_current_id in add_repository r id in
  let find_repository r = try Repository_name_map.find r !repository_get_id with (* annex function, for the implementation relation *)
    | Not_found -> Zephyrus_log.log_missing_data "repository" r "repository declaration" in (* TODO: should I create a shallow structure also here? *)

  (* 2. Conversion *)
    
    (* component types *)
  let convert_component_type t =
    let port_local_provide : Port_id_set.t ref   = ref Port_id_set.empty in          (* all port ids provided by the current component *)
    let port_local_require : Port_id_set.t ref   = ref Port_id_set.empty in          (* all port ids required by the current component *)
    let add_port_provided id = port_local_provide := Port_id_set.add id (!port_local_provide) in (* function used to define the domain of the provide of a component type *)
    let add_port_required id = port_local_require := Port_id_set.add id (!port_local_require) in (* function used to define the domain of the require of a component type *)

    let implem_provide  = Port_id_map.map_of_list (* create the mapping for provide, filling as a side effect the port structures and [port_local_provide] *)
      (fun (p,n) -> (let id = find_port (convert_port_name p) in add_port_provided id; id, convert_provide_arity n)) t.Json_t.component_type_provide in
    let implem_require  = Port_id_map.map_of_list (* create the mapping for require, filling as a side effect the port structures and [port_local_require] *)
      (fun (p,n) -> (let id = find_port (convert_port_name p) in add_port_required id; id, convert_require_arity n)) t.Json_t.component_type_require in
    let implem_conflict = Port_id_set.set_of_list (fun p -> find_port (convert_port_name p)) t.Json_t.component_type_conflict in (* create the mapping for require, etc *)
    let implem_consume = Resource_id_map.map_of_list (* create the mapping for resource consumption *)
      (fun (r,n) -> (get_resource_id (convert_resource_name r), convert_resource_consume_arity n)) t.Json_t.component_type_consume in
    let port_local_provide = !port_local_provide in (* local definition to fix a glitch of how object's methods are implemented *)
    let port_local_require = !port_local_require in (* idem *)
    new_component_type (object(self)
      method name           = convert_component_type_name t.Json_t.component_type_name
      method provide      p = try Port_id_map.find p implem_provide with
        | Not_found -> let port_desc = "(" ^ (String_of.port_id p) ^ "," ^ (try Port_id_map.find p (!port_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "port" port_desc ("provides of the component type \"" ^ (self#name) ^ "\"")
      method provide_domain = port_local_provide
      method require      p = try Port_id_map.find p implem_require with
        | Not_found -> let port_desc = "(" ^ (String_of.port_id p) ^ "," ^ (try Port_id_map.find p (!port_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "port" port_desc ("requires of the component type \"" ^ (self#name) ^ "\"")
      method require_domain = port_local_require
      method conflict       = implem_conflict
      method consume      r = try Resource_id_map.find r implem_consume with Not_found -> 0
    end) in
    
    (* packages *)
  let convert_package r r_name k =
    let implem_depend = Package_id_set_set.set_of_list (* create the dependency sets, creating as a side effect shallow structure for non-yet-seen packages *)
      (fun s -> Package_id_set.set_of_list (fun n -> find_package r (convert_package_name r_name n)) s) k.Json_t.package_depend in
    let implem_conflict = Package_id_set.set_of_list (fun n -> find_package r (convert_package_name r_name n)) k.Json_t.package_conflict in (* similar *)
    let implem_consume = Resource_id_map.map_of_list (* create the mapping for resource consumption *)
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
    let local_package_ids = !local_package_ids in let local_packages = !local_packages in (* local definitions to fix a glitch of how object's methods are implemented *)
    new_repository (object(self) 
      method name          = implem_name
      method get_package k = try Package_id_map.find k implem_packages with
        | Not_found -> let package_desc = "(" ^ (string_of_int k) ^ "," ^ (try Package_id_map.find k (!package_get_name) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "package" package_desc ("package of the repository \"" ^ (self#name) ^ "\"")
      method packages = local_packages
      method package_ids = local_package_ids
    end) in

    (* universe *)
    let _ = List.iter convert_repository u.Json_t.universe_repositories        (* fill the repository and package tables 1/2 *) in   
    let _ = List.iter convert_repository external_repositories                 (* fill the repository and package tables 2/2 *) in   
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

  val mutable implem_ur = Port_id_map.empty; (* set of component type requiring. Not directly computed, filled when requested *)
  val mutable implem_up = Port_id_map.empty; (* set of component type providing. Not directly computed, filled when requested *)
  val mutable implem_uc = Port_id_map.empty; (* set of component type conflicting. Not directly computed, filled when requested *)

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
           | Not_found -> let tmp = (providers implem_get_component_type p) in implem_up <- Port_id_map.add p tmp implem_up; tmp)
  method uc p = (try Port_id_map.find p implem_uc with
           | Not_found -> let tmp = (conflicters implem_get_component_type p) in implem_uc <- Port_id_map.add p tmp implem_uc; tmp)

  (* methods for naming *)
  method get_port_id           n = try Port_name_map.find n implem_get_port_id with Not_found -> deprecated_package_id (* to deal with initial configurations *)
  method get_component_type_id n = try Component_type_name_map.find n implem_get_component_type_id with Not_found -> deprecated_component_type_id (* to deal with initial configurations *)
  method get_repository_id     n = Repository_name_map.find n implem_get_repository_id
  method get_package_id      r n = Repository_id_package_name_map.find (r,n) implem_get_package_id

  method get_port_name           id = Port_id_map.find id implem_get_port_name
  method get_component_type_name id = if id = deprecated_component_type_id then "!!deprecated_component!!" else Component_type_id_map.find id implem_get_component_type_name
  method get_repository_name     id = Repository_id_map.find id implem_get_repository_name
  method get_package_name        id = if id = deprecated_package_id then "!!deprecated_package!!" else Package_id_map.find id implem_get_package_name
end

(* Possible inconsistencies not detected during generation:
 - package presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
 - component type presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
*)


(*/*************************************************\*)
(*| 3. Configuration Conversion                     |*)
(*\*************************************************/*)

(* module for the [get_local_component] function that, given a location and component type, returns the set of components of the right location and type *)
module Location_component_type = struct
  type t = Data_model.location_id * Data_model.component_type_id
  let compare (l,t) (l',t') = let tmp = Data_model.Location_id.compare l l' in
    if tmp = 0 then Data_model.Component_type_id.compare t t' else tmp
end module Location_component_type_map = Data_common.Map.Make(Location_component_type)

(* module for the [get_local_package] function that tells if a package is installed on a location *)
module Location_package = struct
  type t = Data_model.location_id * Data_model.package_id
  let compare (l,t) (l',t') = let tmp = Data_model.Location_id.compare l l' in
    if tmp = 0 then Data_model.Package_id.compare t t' else tmp
end module Location_package_map = Data_common.Map.Make(Location_package)

(* functions that compute parts of the two functions just mentioned *)
let get_local_component l t map = Component_id_map_extract_key.set_of_keys (Component_id_map.filter (fun _ c -> (c#location = l) & (c#typ = t)) map)
let get_local_package l k map = try let l' = Location_id_map.find l map in Package_id_set.mem k l'#packages_installed with Not_found -> false



(*/*********************************)
(* class for loading a configuration from a Json_j structure. Contains all conversion function concerning configurations *)
class convert_configuration get_resource_id get_port_id get_component_type_id get_package_id get_repository_id c =
  (* 1. Data storage *)
    (* locations *)
  let locations         : Location_set.t ref = ref Location_set.empty in                          (* all locations *)
  let location_names    : Location_name_set.t ref = ref Location_name_set.empty in                (* all location names *)
  let location_ids      : Location_id_set.t ref = ref Location_id_set.empty in                    (* all location ids *)
  let location_get      : (location Location_id_map.t) ref = ref Location_id_map.empty in         (* mapping id -> location *)
  let location_get_id   : (location_id Location_name_map.t) ref = ref Location_name_map.empty in  (* mapping name -> is *)
  let location_get_name : (location_name Location_id_map.t) ref = ref Location_id_map.empty in    (* mapping id -> name *)
  let add_location l id = (* add all the structure to store the mapping l <-> id <-> l#name. Never used directly (see [new_location]) *)
    locations         := Location_set.add l (!locations);
    location_names    := Location_name_set.add l#name (!location_names);
    location_ids      := Location_id_set.add id (!location_ids);
    location_get      := Location_id_map.add id l (!location_get);
    location_get_id   := Location_name_map.add l#name id (!location_get_id);
    location_get_name := Location_id_map.add id l#name (!location_get_name) in
  let location_current_id = Incrementing_id.create () in (* for unique identifier creation *)
  let new_location l =  (* create all the structure to store the new location l *)
    let id = Incrementing_id.next location_current_id in add_location l id in
  let find_location l = try Location_name_map.find l (!location_get_id) with (* annex function, used for components *)
    | Not_found -> Zephyrus_log.log_missing_data "location" l "component" in
    (* components *)
  let components         : Component_set.t ref = ref Component_set.empty in                             (* all components *)
  let component_names    : Component_name_set.t ref = ref Component_name_set.empty in                   (* all component names *) 
  let component_ids      : Component_id_set.t ref = ref Component_id_set.empty in                       (* all component ids *)
  let component_get      : (component Component_id_map.t) ref = ref Component_id_map.empty in           (* mapping id -> component *)
  let component_get_id   : (component_id Component_name_map.t) ref = ref Component_name_map.empty in    (* mapping name -> id *)
  let component_get_name : (component_name Component_id_map.t) ref = ref Component_id_map.empty in      (* mapping id -> name *)
  let add_component c id =  (* add all the structure to store the mapping c <-> id <-> c#name. Never used directly (see [new_component]) *)
    components         := Component_set.add c (!components);
    component_names    := Component_name_set.add c#name (!component_names);
    component_ids      := Component_id_set.add id (!component_ids);
    component_get      := Component_id_map.add id c (!component_get);
    component_get_id   := Component_name_map.add c#name id (!component_get_id);
    component_get_name := Component_id_map.add id c#name (!component_get_name) in
  let component_current_id = Incrementing_id.create () in
  let new_component c =  (* create all the structure to store the new component c *)
    let id = Incrementing_id.next component_current_id in add_component c id in
  let find_component c = try Component_name_map.find c (!component_get_id) with (* annex function, used for the bindings *)
    | Not_found -> Zephyrus_log.log_missing_data "component" c "binding" in

  (* 2. Conversion *)
    (* location *)
  let convert_location l =
    let implem_name = convert_location_name l.Json_t.location_name in
    let r_name = convert_repository_name l.Json_t.location_repository in let implem_repository = get_repository_id r_name in
    let implem_package = Package_id_set.set_of_list (fun k -> get_package_id implem_repository (convert_package_name r_name k)) l.Json_t.location_packages_installed in
    let implem_resource = Resource_id_map.map_of_list (* create the mapping for resource provide *)
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
  val mutable implem_get_local_component = Location_component_type_map.empty; (* computed incrementally *)
  val mutable implem_get_local_package = Location_package_map.empty;          (* computed incrementally *)
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

(* this translation is a one to one mapping. Nothing much to say *)

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

(* TODO: move the difficulties of dealing with Settings elsewhere? *)

(* functions to load the Json encoding of each structure *)
(*   - universe *)
let load_universe () = if Settings.get_bool_basic Settings.data_generation_universe then Input_helper.parse_json Json_j.read_universe Settings.input_file_universe else None
(*   - external repositories *)
let load_repositories () = if Settings.get_bool_basic Settings.data_generation_repositories then List.fold_left
  (fun res (n,f) -> let r = Input_helper.parse_json Json_j.read_packages (ref (Some (f))) in match r with None -> res
      | Some(ks) -> { Json_j.repository_name = n; Json_j.repository_packages = ks }::res) [] !Settings.input_file_repositories else []
(*   - initial configuration *)
let load_initial_configuration () = if Settings.get_bool_basic Settings.data_generation_initial_configuration then
  Input_helper.parse_json Json_j.read_configuration Settings.input_file_initial_configuration else None
(*   - specification *)
let load_specification () = if Settings.get_bool_basic Settings.data_generation_specification then
  Input_helper.parse_standard Specification_parser.main Specification_lexer.token Settings.input_file_specification else None
(*   - optimization function (from Settings) *)
let load_optimization_function () = if Settings.get_bool_basic Settings.data_generation_optimization_function then
  (match !Settings.input_optimization_function with None -> None | Some(o) -> Some(convert_optimization_function o)) else None

(* full function that, as a side effect of constructing the structures, fill in the informations about the resources *)
let load_model () =
  (* 1. data structure to constuct the set of resource names *)
  let resource_names    : Resource_name_set.t ref = ref Resource_name_set.empty in               (* all resource names *)
  let resource_ids      : Resource_id_set.t ref = ref Resource_id_set.empty in                   (* all resource ids *)
  let resource_get_name : (resource_name Resource_id_map.t) ref = ref Resource_id_map.empty in   (* mapping id -> name *)
  let resource_get_id   : (resource_id Resource_name_map.t) ref = ref Resource_name_map.empty in (* mapping name -> id *)
  let add_resource name id = (* create all the structure to store the relation name <-> id, never used directly (see [get_resource_id]) *)
    resource_names    := Resource_name_set.add name !resource_names;
    resource_ids      := Resource_id_set.add id !resource_ids;
    resource_get_name := Resource_id_map.add id name !resource_get_name;
    resource_get_id   := Resource_name_map.add name id !resource_get_id in
  let resource_current_id = ref 0 in (* for unique identifier creation *)
  let get_resource_id name = try Resource_name_map.find name !resource_get_id with (* get the id for the name, or create the data for it *)
    | Not_found -> let id = !resource_current_id in resource_current_id := id + 1; add_resource name id; id in
  let get_resource_name id = try Resource_id_map.find id !resource_get_name with (* get the name from an id (if that fails, you're using a non declared id...) *)
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


