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

(* for unique identifier creation *)
module type Incrementing =
sig
  type t
  type id
  val create  : unit -> t
  val current : t -> id
  val next    : t -> id
end

module Incrementing_integer : Incrementing with type id = int =
struct
  type t = int ref
  type id = int
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


(*/*************************************************\*)
(*| 1. Resource Names and Ids Extractions           |*)
(*\*************************************************/*)

(*
  class type name_id_mapping = object
    method ids        : Component_type_id_set.t
    method names      : Component_type_name_set.t
    method name_of_id : component_type_id   -> component_type_name
    method id_of_name : component_type_name -> component_type_id
  end
*)


module Name_id_mapping =
  functor (Incrementing_id : Incrementing) ->
  functor (Id_set   : Set.S with type elt = Incrementing_id.id) ->
  functor (Name_set : Set.S) ->
  functor (Id_map   : Map.S with type key = Id_set.elt) ->
  functor (Name_map : Map.S with type key = Name_set.elt) ->
  struct

    type id   = Id_set.elt
    type name = Name_set.elt

    class type mapping_iface = object
      method ids           : Id_set.t
      method names         : Name_set.t
      method name_of_id    : id   -> name (* May throw Not_found exception. *)
      method id_of_name    : name -> id   (* May throw Not_found exception. *)
      method get_or_add_id : name -> id
    end

    class mapping : mapping_iface = 

      (* Data structures. *)
      let names          : Name_set.t ref      = ref Name_set.empty in  (* All names *)
      let ids            : Id_set.t ref        = ref Id_set.empty   in  (* All ids *)
      let name_to_id_map : (id Name_map.t) ref = ref Name_map.empty in  (* Mapping name -> id *)
      let id_to_name_map : (name Id_map.t) ref = ref Id_map.empty   in  (* Mapping id -> name *)
    
      (* Mapping functions. *)
      let id_of_name (name : name) : id   = Name_map.find name !name_to_id_map in
      let name_of_id (id   : id)   : name = Id_map  .find id   !id_to_name_map in
          
      (* Function that adds new name and id to appropriate sets 
         and adds the relation id <-> name in the maps. *)
      let add name id =
        names          := Name_set.add name    (!names);
        ids            := Id_set  .add id      (!ids);
        name_to_id_map := Name_map.add name id (!name_to_id_map);
        id_to_name_map := Id_map  .add id name (!id_to_name_map) in

      (* For unique identifier creation. *)
      let current_id = Incrementing_id.create () in

      (* Look for the name in the maps, if it does not exist create it and give it a fresh id. *)
      let get_or_add_id name = 
        try id_of_name name
        with Not_found -> let id = Incrementing_id.next current_id in add name id; id in

      object
        method ids           = !ids
        method names         = !names
        method id_of_name    = id_of_name
        method name_of_id    = name_of_id
        method get_or_add_id = get_or_add_id
      end

      class type closed_mapping_iface = object
        method ids           : Id_set.t
        method names         : Name_set.t
        method name_of_id    : id   -> name
        method id_of_name    : name -> id
      end

      class closed_mapping (mapping : mapping) (mapping_name : string) (string_of_id : id -> string) (string_of_name : name -> string) : closed_mapping_iface = object
        method ids             = mapping#ids
        method names           = mapping#names
        method name_of_id id   = try mapping#name_of_id id   with Not_found -> failwith (Printf.sprintf "%s#name_of_id %s" mapping_name (string_of_id id))
        method id_of_name name = try mapping#id_of_name name with Not_found -> failwith (Printf.sprintf "%s#id_of_name %s" mapping_name (string_of_name name))
      end
      
  end

module Component_type_name_mapping = Name_id_mapping(Incrementing_id)(Component_type_id_set)(Component_type_name_set       )(Component_type_id_map)(Component_type_name_map       )
module Port_name_mapping           = Name_id_mapping(Incrementing_id)(Port_id_set          )(Port_name_set                 )(Port_id_map          )(Port_name_map                 )
module Repository_name_mapping     = Name_id_mapping(Incrementing_id)(Repository_id_set    )(Repository_name_set           )(Repository_id_map    )(Repository_name_map           )
module Package_name_mapping        = Name_id_mapping(Incrementing_id)(Package_id_set       )(Repository_id_package_name_set)(Package_id_map       )(Repository_id_package_name_map)
module Resource_name_mapping       = Name_id_mapping(Incrementing_id)(Resource_id_set      )(Resource_name_set             )(Resource_id_map      )(Resource_name_map             )
module Location_name_mapping       = Name_id_mapping(Incrementing_id)(Location_id_set      )(Location_name_set             )(Location_id_map      )(Location_name_map             )
module Component_name_mapping      = Name_id_mapping(Incrementing_id)(Component_id_set     )(Component_name_set            )(Component_id_map     )(Component_name_map            )

(*
module Component_type_name_id_mapping = Name_id_mapping(Component_type_id_set)(Component_type_name_set)
*)

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
  method package_names             : Repository_id_package_name_set.t
  method package_name_of_id        : package_id -> Repository_id_package_name.t
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

  method to_string : string
end

class name_id_mappings_of_json universe additional_repositories initial_configuration specification : name_id_mappings = 

    (* component types *)
  let component_type_name_mapping = new Component_type_name_mapping.mapping in
  let get_or_add_component_type_id = component_type_name_mapping#get_or_add_id in
  
    (* ports *)
  let port_name_mapping = new Port_name_mapping.mapping in
  let get_or_add_port_id = port_name_mapping#get_or_add_id in
  
    (* repositories *)
  let repository_name_mapping = new Repository_name_mapping.mapping in
  let get_or_add_repository_id = repository_name_mapping#get_or_add_id in

    (* packages *)
  let package_name_mapping = new Package_name_mapping.mapping in
  let get_or_add_package_id = package_name_mapping#get_or_add_id in

    (* resources *)
  let resource_name_mapping = new Resource_name_mapping.mapping in
  let get_or_add_resource_id = resource_name_mapping#get_or_add_id in

    (* locations *)
  let location_name_mapping = new Location_name_mapping.mapping in
  let get_or_add_location_id = location_name_mapping#get_or_add_id in

    (* components *)
  let component_name_mapping = new Component_name_mapping.mapping in
  let get_or_add_component_id = component_name_mapping#get_or_add_id in

  (*  *)

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
    get_or_add_package_id (r_id, (convert_package_name "" k.package_name));
    List.map (fun x -> get_or_add_package_id (r_id, x)) (List.map (convert_package_name "") (List.flatten k.package_depend  ));
    List.map (fun x -> get_or_add_package_id (r_id, x)) (List.map (convert_package_name "")               k.package_conflict);
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
    List.map (fun x -> get_or_add_package_id (r_id, x)) (List.map (convert_package_name "") l.location_packages_installed) in

  (* configuration *)
  let add_configuration c =
    let open Json_t in
    List.map add_location  c.configuration_locations;
    List.map add_component c.configuration_components;
    List.map add_binding   c.configuration_bindings in

  (* do it! *)
  let _ = 
    begin
      match universe with
      | None   -> ()
      | Some u -> let _ = add_universe u in ()
    end;
    List.map add_repository additional_repositories;
    begin
      match initial_configuration with
      | None   -> ()
      | Some c -> let _ = add_configuration c in ()
    end;
    begin
      match specification with
      | None   -> ()
      | Some s -> ()
    end
  in

  object (self)
    (* component_type *)
    method component_type_ids        = component_type_name_mapping#ids
    method component_type_names      = component_type_name_mapping#names
    method component_type_name_of_id = component_type_name_mapping#name_of_id
    method component_type_id_of_name = component_type_name_mapping#id_of_name
    (* port *)
    method port_ids                  = port_name_mapping#ids
    method port_names                = port_name_mapping#names
    method port_name_of_id           = port_name_mapping#name_of_id
    method port_id_of_name           = port_name_mapping#id_of_name
    (* repository *)
    method repository_ids            = repository_name_mapping#ids
    method repository_names          = repository_name_mapping#names
    method repository_name_of_id     = repository_name_mapping#name_of_id
    method repository_id_of_name     = repository_name_mapping#id_of_name
    (* package *)
    method package_ids               = package_name_mapping#ids
    method package_names             = package_name_mapping#names
    method package_name_of_id        = package_name_mapping#name_of_id
    method package_id_of_name        = package_name_mapping#id_of_name
    (* resource *)
    method resource_ids              = resource_name_mapping#ids
    method resource_names            = resource_name_mapping#names
    method resource_name_of_id       = resource_name_mapping#name_of_id
    method resource_id_of_name       = resource_name_mapping#id_of_name
    (* location *)
    method location_ids              = location_name_mapping#ids
    method location_names            = location_name_mapping#names
    method location_name_of_id       = location_name_mapping#name_of_id
    method location_id_of_name       = location_name_mapping#id_of_name
    (* component *)
    method component_ids             = component_name_mapping#ids
    method component_names           = component_name_mapping#names
    method component_name_of_id      = component_name_mapping#name_of_id
    method component_id_of_name      = component_name_mapping#id_of_name

    method to_string =
      let module Component_type_id_map_extract_key   = Component_type_id_map  .Set_of_keys(Component_type_id_set)   in
      let module Component_type_name_map_extract_key = Component_type_name_map.Set_of_keys(Component_type_name_set) in
      let module PC = Data_common.Set.Convert(Repository_id_package_name_set)(Package_name_set) in

      String.concat "\n" [
        "component_types";
        String_of.component_type_id_set self#component_type_ids;
        String_of.component_type_name_set self#component_type_names;

        "ports";
        String_of.port_id_set self#port_ids;
        String_of.port_name_set self#port_names;

        "repositories";
        String_of.repository_id_set self#repository_ids;
        String_of.repository_name_set self#repository_names;

        "packages";
        String_of.package_id_set   self#package_ids;
        String_of.package_name_set (PC.convert snd self#package_names);

        "resources";
        String_of.resource_id_set self#resource_ids;
        String_of.resource_name_set self#resource_names;
      ]
  end

class name_id_mappings_of_json_failsafe (naming : name_id_mappings) : name_id_mappings = 
  object (self)
    (* component_type *)
    method component_type_ids        = naming#component_type_ids
    method component_type_names      = naming#component_type_names
    method component_type_name_of_id id   = try naming#component_type_name_of_id id   with Not_found -> failwith (Printf.sprintf "component_type_name_of_id %d" id)
    method component_type_id_of_name name = try naming#component_type_id_of_name name with Not_found -> failwith (Printf.sprintf "component_type_id_of_name %s" name)
    (* port *)
    method port_ids                  = naming#port_ids
    method port_names                = naming#port_names
    method port_name_of_id           id   = try naming#port_name_of_id id   with Not_found -> failwith (Printf.sprintf "port_name_of_id %d" id)
    method port_id_of_name           name = try naming#port_id_of_name name with Not_found -> failwith (Printf.sprintf "port_id_of_name %s" name)
    (* repository *)
    method repository_ids            = naming#repository_ids
    method repository_names          = naming#repository_names
    method repository_name_of_id     id   = try naming#repository_name_of_id id   with Not_found -> failwith (Printf.sprintf "repository_name_of_id %d" id)
    method repository_id_of_name     name = try naming#repository_id_of_name name with Not_found -> failwith (Printf.sprintf "repository_id_of_name %s" name)
    (* package *)
    method package_ids               = naming#package_ids
    method package_names             = naming#package_names
    method package_name_of_id        id   = try naming#package_name_of_id id   with Not_found -> failwith (Printf.sprintf "package_name_of_id %d" id)
    method package_id_of_name        name = try naming#package_id_of_name name with Not_found -> failwith (Printf.sprintf "package_id_of_name (%d, %s)" (fst name) (snd name))
    (* resource *)
    method resource_ids              = naming#resource_ids
    method resource_names            = naming#resource_names
    method resource_name_of_id       id   = try naming#resource_name_of_id id   with Not_found -> failwith (Printf.sprintf "resource_name_of_id %d" id)
    method resource_id_of_name       name = try naming#resource_id_of_name name with Not_found -> failwith (Printf.sprintf "resource_id_of_name %s" name)
    (* location *)
    method location_ids              = naming#location_ids
    method location_names            = naming#location_names
    method location_name_of_id       id   = try naming#location_name_of_id id   with Not_found -> failwith (Printf.sprintf "location_name_of_id %d" id)
    method location_id_of_name       name = try naming#location_id_of_name name with Not_found -> failwith (Printf.sprintf "location_id_of_name %s" name)
    (* component *)
    method component_ids             = naming#component_ids
    method component_names           = naming#component_names
    method component_name_of_id      id   = try naming#component_name_of_id id   with Not_found -> failwith (Printf.sprintf "component_name_of_id %d" id)
    method component_id_of_name      name = try naming#component_id_of_name name with Not_found -> failwith (Printf.sprintf "component_id_of_name %s" name)

    method to_string = naming#to_string
  end


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
class convert_universe (naming : name_id_mappings) external_repositories u = 
  
  (* 1. Data Storage *)

    (* ports *)
  let find_port name = naming#port_id_of_name in


    (* component types *)
  let component_types         : Component_type_set.t ref = ref Component_type_set.empty in                                (* all component types *)
  let component_type_get      : (component_type Component_type_id_map.t) ref = ref Component_type_id_map.empty in         (* mapping component type id -> component type *)
  
  let new_component_type t =                (* create all the data structure to store the component type in parameter *)
    let id = naming#component_type_id_of_name t#name in 
    component_types         := Component_type_set.add t (!component_types);
    component_type_get      := Component_type_id_map.add id t (!component_type_get) in

  let find_component_type_id = naming#component_type_id_of_name in


    (* package *)
  let packages          : Package_set.t ref                    = ref Package_set.empty in                    (* all packages *)
  let package_get       : (package Package_id_map.t) ref       = ref Package_id_map.empty in                 (* mapping package id -> package *)
  let repos_of_packages : (repository_id Package_id_map.t) ref = ref Package_id_map.empty in                 (* mapping package id -> repository id *)
    
  let find_package r name = naming#package_id_of_name (r, name) in

  let new_package r k = 
    let id = find_package r k#name in (* create a full structure for the package *)
    packages          := Package_set.add k (!packages);
    repos_of_packages := Package_id_map.add id r !repos_of_packages;
    package_get       := Package_id_map.add id k (!package_get); 
    (id, k) in (* <- this pair looks strange: LOOK INTO THIS *)


    (* repositories *)
  let repositories        : Repository_set.t ref      = ref Repository_set.empty in                       (* all repositories *)
  let repository_get      : (repository Repository_id_map.t) ref      = ref Repository_id_map.empty in    (* mapping repository id -> repository *)

  let add_repository r id = (* function that adds the relation r <-> id <-> r#name in the maps. Never used directly (see [new_repository]) *)
    repositories        := Repository_set.add r (!repositories);
    repository_get      := Repository_id_map.add id r (!repository_get) in

  let repository_current_id = ref 0 in (* for unique identifier creation *)
  let get_current_repository_id () = !repository_current_id in
  
  (* create a full structure for the repository *)
  let new_repository r =
    let id = naming#repository_id_of_name r#name in repository_current_id := id; add_repository r id in

  (* annex function, for the implementation relation *)
  let find_repository r = 
    try naming#repository_id_of_name r 
    with Not_found -> Zephyrus_log.log_missing_data "repository" r "repository declaration" in (* TODO: should I create a shallow structure also here? *)


  (* 2. Conversion *)
    
    (* component types *)
  let convert_component_type t =
    
    let name = convert_component_type_name t.Json_t.component_type_name in

    (* create the mapping for provide *)
    let implem_provide : provide_arity Port_id_map.t = 
      Port_id_map.map_of_list (fun (name, arity) -> 
        let id = naming#port_id_of_name (convert_port_name name) in 
        (id, convert_provide_arity arity)
      ) t.Json_t.component_type_provide in

    (* create the mapping for require *)
    let implem_require : require_arity Port_id_map.t = 
      Port_id_map.map_of_list (fun (name, arity) -> 
        let id = naming#port_id_of_name (convert_port_name name) in 
        (id, convert_require_arity arity)
      ) t.Json_t.component_type_require in

    (* create the mapping for conflict *)
    let implem_conflict : Port_id_set.t = 
      Port_id_set.set_of_list (fun name -> 
        naming#port_id_of_name (convert_port_name name)
      ) t.Json_t.component_type_conflict in 

    (* create the mapping for resource consumption *)
    let implem_consume : resource_consume_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (name, arity) -> 
        (naming#resource_id_of_name (convert_resource_name name), convert_resource_consume_arity arity)
      ) t.Json_t.component_type_consume in

    let port_local_provide : Port_id_set.t = Port_id_map_extract_key.set_of_keys implem_provide in (* local definition to fix a glitch of how object's methods are implemented *)
    let port_local_require : Port_id_set.t = Port_id_map_extract_key.set_of_keys implem_require in (* idem *)

    new_component_type (object(self)
      method name           = name
      method provide      p = try Port_id_map.find p implem_provide with
        | Not_found -> let port_desc = "(" ^ (String_of.port_id p) ^ "," ^ (try naming#port_name_of_id p with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "port" port_desc ("provides of the component type \"" ^ (self#name) ^ "\"")
      method provide_domain = port_local_provide
      method require      p = try Port_id_map.find p implem_require with
        | Not_found -> let port_desc = "(" ^ (String_of.port_id p) ^ "," ^ (try naming#port_name_of_id p with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "port" port_desc ("requires of the component type \"" ^ (self#name) ^ "\"")
      method require_domain = port_local_require
      method conflict       = implem_conflict
      method consume      r = try Resource_id_map.find r implem_consume with Not_found -> 0
    end) in
    
    (* packages *)
  let convert_package r_id r_name k =

    let name = convert_package_name r_name k.Json_t.package_name in

    (* create the dependency sets, creating as a side effect shallow structure for non-yet-seen packages *)
    let depend : Package_id_set_set.t = 
      Package_id_set_set.set_of_list (fun s -> 
        Package_id_set.set_of_list (fun n -> 
          naming#package_id_of_name (r_id, convert_package_name r_name n)) s
      ) k.Json_t.package_depend in
    
    (* similar *)
    let conflict : Package_id_set.t = 
      Package_id_set.set_of_list (fun n -> 
        naming#package_id_of_name (r_id, (convert_package_name r_name n))
      ) k.Json_t.package_conflict in
    
    (* create the mapping for resource consumption *)
    let consume : resource_consume_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (r_id,n) -> 
        (naming#resource_id_of_name (convert_resource_name r_id), convert_resource_consume_arity n)
      ) k.Json_t.package_consume in

    new_package r_id (object
      method name      = name
      method depend    = depend
      method conflict  = conflict
      method consume r = try Resource_id_map.find r_id consume with Not_found -> 0
    end) in

    (* repositories *)
  let convert_repository r = 

    let name = convert_repository_name r.Json_t.repository_name in

    let id = naming#repository_id_of_name name in
    
    let packages : package Package_id_map.t = 
      Package_id_map.map_of_list (fun k -> 
        convert_package id name k
      ) r.Json_t.repository_packages in
    
    let module Package_id_map_extract_value = Package_id_map.Set_of_values(Package_set) in

    let local_package_ids = Package_id_map_extract_key.set_of_keys     packages in 
    let local_packages    = Package_id_map_extract_value.set_of_values packages in
    
    new_repository (object(self) 
      method name          = name
      method get_package k = 
        try Package_id_map.find k packages 
        with Not_found -> 
          let package_desc = "(" ^ (string_of_int k) ^ "," ^ (try snd (naming#package_name_of_id k) with Not_found -> "") ^ ")" in
          Zephyrus_log.log_missing_data "package" package_desc ("package of the repository \"" ^ (self#name) ^ "\"")
      method packages    = local_packages
      method package_ids = local_package_ids
    end) in

    (* universe *)
    let _ = List.iter convert_repository     u.Json_t.universe_repositories    (* fill the repository and package tables 1/2 *) in   
    let _ = List.iter convert_repository     external_repositories             (* fill the repository and package tables 2/2 *) in   
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

  val implem_get_port_id             = naming#port_id_of_name
  val implem_get_component_type_id   = naming#component_type_id_of_name
  val implem_get_repository_id       = naming#repository_id_of_name
  val implem_get_package_id          = naming#package_id_of_name

  val implem_get_port_name           = naming#port_name_of_id
  val implem_get_component_type_name = naming#component_type_name_of_id
  val implem_get_repository_name     = naming#repository_name_of_id
  val implem_get_package_name        = naming#package_name_of_id

  (* methods *)
  method get_component_type id = Component_type_id_map.find id implem_get_component_type
  method get_implementation id = try Component_type_id_map.find id implem_get_implementation with | Not_found -> Package_id_set.empty
  method get_repository     id = Repository_id_map.find id implem_get_repository
  method get_package        id = Package_id_map.find id implem_get_package

  method repository_of_package id = Package_id_map.find id !repos_of_packages

  method get_component_types = !component_types
  method get_repositories    = !repositories
  method get_packages        = !packages

  method get_port_ids           = naming#port_ids
  method get_component_type_ids = naming#component_type_ids
  method get_repository_ids     = naming#repository_ids
  method get_package_ids        = naming#package_ids

  method get_port_names           = naming#port_names
  method get_component_type_names = naming#component_type_names
  method get_repository_names     = naming#repository_names
  method get_package_names        = let module M = Data_common.Set.Convert(Repository_id_package_name_set)(Package_name_set) in
                                    M.convert snd naming#package_names

  (* methods coming from the paper. Usually, aliases for well-named functions *)
  method u_dt = self#get_component_type_ids
  method u_dp = self#get_port_ids
  method u_dr = self#get_repository_ids
  method u_dk = self#get_package_ids
    
  method u_i  = self#get_implementation
  method u_w  = self#get_package

  method ur p = (try Port_id_map.find p implem_ur with
           | Not_found -> let tmp = (requirers implem_get_component_type p) in implem_ur <- Port_id_map.add p tmp implem_ur; tmp)
  method up p = (try Port_id_map.find p implem_up with
           | Not_found -> let tmp = (providers implem_get_component_type p) in implem_up <- Port_id_map.add p tmp implem_up; tmp)
  method uc p = (try Port_id_map.find p implem_uc with
           | Not_found -> let tmp = (conflicters implem_get_component_type p) in implem_uc <- Port_id_map.add p tmp implem_uc; tmp)

  (* methods for naming *)
  method get_port_id           n = try implem_get_port_id n           with Not_found -> deprecated_package_id (* to deal with initial configurations *)
  method get_component_type_id n = try implem_get_component_type_id n with Not_found -> deprecated_component_type_id (* to deal with initial configurations *)
  method get_repository_id     n = implem_get_repository_id n
  method get_package_id      r n = implem_get_package_id (r,n)

  method get_port_name           id = implem_get_port_name id
  method get_component_type_name id = if id = deprecated_component_type_id then "!!deprecated_component!!" else implem_get_component_type_name id
  method get_repository_name     id = implem_get_repository_name id
  method get_package_name        id = if id = deprecated_package_id then "!!deprecated_package!!" else snd (implem_get_package_name id)
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
class convert_configuration (naming : name_id_mappings) c =

  (* 1. Data storage *)

    (* locations *)
  let locations    : Location_set.t ref = ref Location_set.empty in                   (* all locations *)
  let location_get : (location Location_id_map.t) ref = ref Location_id_map.empty in  (* mapping id -> location *)
  
  (* create all the structure to store the new location l *)
  let new_location (l : location) =  
    let id = naming#location_id_of_name l#name in 
    locations     := Location_set.add l (!locations);
    location_get  := Location_id_map.add id l (!location_get) in
  
  let find_location l = 
    try naming#location_id_of_name l with (* annex function, used for components *)
    | Not_found -> Zephyrus_log.log_missing_data "location" l "component" in
  
    (* components *)
  let components    : Component_set.t ref = ref Component_set.empty in                    (* all components *)
  let component_get : (component Component_id_map.t) ref = ref Component_id_map.empty in  (* mapping id -> component *)
  
  (* create all the structure to store the new component c *)
  let new_component c =
    let id = naming#component_id_of_name c#name in
    components    := Component_set.add c (!components);
    component_get := Component_id_map.add id c (!component_get) in

  (* annex function, used for the bindings *)
  let find_component c = 
    try naming#component_id_of_name c
    with Not_found -> Zephyrus_log.log_missing_data "component" c "binding" in

  (* 2. Conversion *)
    (* location *)
  let convert_location l =
    let name = convert_location_name l.Json_t.location_name in
    let r_name = convert_repository_name l.Json_t.location_repository in 
    let repository = naming#repository_id_of_name r_name in
    let packages_installed = Package_id_set.set_of_list (fun k -> naming#package_id_of_name (repository, (convert_package_name r_name k))) l.Json_t.location_packages_installed in
    let resources = Resource_id_map.map_of_list (* create the mapping for resource provide *)
      (fun (r,n) -> (naming#resource_id_of_name (convert_resource_name r), convert_resource_provide_arity n)) l.Json_t.location_provide_resources in

    new_location (object
      method name                = name
      method repository          = repository
      method packages_installed  = packages_installed
      method provide_resources r = try Resource_id_map.find r resources with Not_found -> 0 
    end) in

    (* components *)
  let convert_component c =
    let name = convert_component_name c.Json_t.component_name in
    let typ = naming#component_type_id_of_name (convert_component_type_name c.Json_t.component_type) in
    let location = find_location (convert_location_name c.Json_t.component_location) in

    new_component (object
      method name     = name
      method typ      = typ
      method location = location 
    end) in
    
    (* bindings *)
  let convert_binding b =
    let port     = naming#port_id_of_name (convert_port_name b.Json_t.binding_port) in
    let requirer = find_component (convert_component_name b.Json_t.binding_requirer) in
    let provider = find_component (convert_component_name b.Json_t.binding_provider) in 

    (object
      method port     = port
      method requirer = requirer
      method provider = provider 
    end) in

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

  method get_location_ids  = naming#location_ids
  method get_component_ids = naming#component_ids

  method get_location_names  = naming#location_names
  method get_component_names = naming#component_names

  method c_l = self#get_location_ids
  method c_c = self#get_component_ids
  method c_type c = (self#get_component c)#typ

  method get_local_component l t = (try Location_component_type_map.find (l,t) implem_get_local_component with
         | Not_found -> let tmp = get_local_component l t (!component_get) in
           implem_get_local_component <- Location_component_type_map.add (l,t) tmp implem_get_local_component; tmp)
  method get_local_package l k = (try Location_package_map.find (l,k) implem_get_local_package with
         | Not_found -> let tmp = get_local_package l k (!location_get) in
           implem_get_local_package <- Location_package_map.add (l,k) tmp implem_get_local_package; tmp);

  method get_location_id  l = naming#location_id_of_name  l
  method get_component_id c = naming#component_id_of_name c

  method get_location_name  l = naming#location_name_of_id   l
  method get_component_name c =  naming#component_name_of_id c
end


(*****************************************************)
(** 4. Specification conversion                      *)
(*****************************************************)

(* this translation is a one to one mapping. Nothing much to say *)

let convert_spec_variable_name = fun x -> x
let convert_spec_const         = fun x -> x
let convert_spec_op o          = match o with
 | `Lt -> Lt | `LEq -> LEq | `Eq -> Eq | `GEq -> GEq | `Gt -> Gt | `NEq -> NEq

let convert_specification (naming : name_id_mappings) s =

  let convert_spec_local_element el = match el with
    | `SpecLocalElementPackage(r,k)       -> let r_name = convert_repository_name r in let repo = naming#repository_id_of_name r_name in
                                               Spec_local_element_package(naming#package_id_of_name (repo, (convert_package_name r_name k)))
    | `SpecLocalElementComponentType(t) -> Spec_local_element_component_type(naming#component_type_id_of_name (convert_component_type_name t))
    | `SpecLocalElementPort(p)       -> Spec_local_element_port(naming#port_id_of_name (convert_port_name p)) in
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

  let convert_spec_repository_constraint x = naming#repository_id_of_name (convert_repository_name x) in
  let convert_spec_resource_constraint (r, o, c) = (naming#resource_id_of_name (convert_resource_name r), convert_spec_op o, convert_spec_const c) in

  let convert_spec_element el = match el with
    | `SpecElementPackage(r,k)       -> let r_name = convert_repository_name r in let repo = naming#repository_id_of_name r_name in
                                               Spec_element_package(naming#package_id_of_name (repo, (convert_package_name r_name k)))
    | `SpecElementComponentType(t)       -> Spec_element_component_type(naming#component_type_id_of_name (convert_component_type_name t))
    | `SpecElementPort(p)                -> Spec_element_port(naming#port_id_of_name (convert_port_name p))
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
  (* Load stuff. *)
  let universe              = load_universe () in
  let repositories          = load_repositories () in
  let initial_configuration = load_initial_configuration () in
  let specification         = load_specification () in

  (* Prepare naming *)
  let fragile_naming = new name_id_mappings_of_json universe repositories initial_configuration specification in
  let naming = new name_id_mappings_of_json_failsafe fragile_naming in
  Printf.printf "\nNaming:\n%s\n" naming#to_string;

  (* 2. generate the universe (if required) *)
  let universe = 
    match universe with
    | None -> None
    | Some(u) -> Some(new convert_universe naming repositories u) in
  (* 3. generate the initial configuration (if required) *)
  let initial_configuration = 
    match initial_configuration with
    | None -> None
    | Some(c) -> Some(new convert_configuration naming c) in
  (* 4. generate the specification (if required) *)
  let specification = 
    match specification with
    | None -> None
    | Some(s) -> Some(convert_specification naming s) in
  (* 5. generation of the resource set *)
  let resources = object
    method resource_names = naming#resource_names
    method resource_ids   = naming#resource_ids
    method get_name    id = try naming#resource_name_of_id id   with Not_found -> Zephyrus_log.log_missing_data "resource id"   (String_of.resource_id id)     "universe"
    method get_id    name = try naming#resource_id_of_name name with Not_found -> Zephyrus_log.log_missing_data "resource name" (String_of.resource_name name) "universe"
  end in
  (* 6. generation of the optimization function (if required) *)
  let optimization_function = load_optimization_function () in
  (* 7. set all data in data_state *)
  Data_state.universe_full := universe;
  Data_state.initial_configuration_full := initial_configuration;
  Data_state.specification_full := specification;
  Data_state.resources_full := Some(resources);
  Data_state.optimization_function := optimization_function


