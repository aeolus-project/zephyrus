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
open Data_model_catalog

(* 0. Name Conversion *)

let convert_resource_name          x = x
let convert_resource_consume_arity x = x
let convert_resource_provide_arity x = x
let convert_port_name              x = x
let convert_component_type_name    x = x
let convert_provide_arity          x = match x with Abstract_io.InfiniteProvide -> Infinite_provide | Abstract_io.FiniteProvide(i) -> Finite_provide(i)
let convert_require_arity          x = x
let convert_package_name         r x = if Settings.find Settings.append_repository_to_package_name then r ^ x else x
let convert_repository_name        x = x
let convert_location_name          x = x
let convert_location_cost          x = x
let convert_component_name         x = x


(*/*************************************************\*)
(*| 1. Resource Names and Ids Extractions           |*)
(*\*************************************************/*)

(* A meta-catalog created from the Abstract_io model: universe, repositories and initial configuration. *)
let model_catalog_of_abstract_io 
  (universe                : Abstract_io.universe      option)
  (additional_repositories : Abstract_io.repository    list)
  (initial_configuration   : Abstract_io.configuration option)
  : model_catalog = 

  (* Mappings *)
  let component_type = new Component_type_catalog.catalog in (* component types *)
  let port           = new Port_catalog          .catalog in (* ports *)
  let repository     = new Repository_catalog    .catalog in (* repositories *)
  let package        = new Package_catalog       .catalog in (* packages *)
  let resource       = new Resource_catalog      .catalog in (* resources *)
  let location       = new Location_catalog      .catalog in (* locations *)
  let component      = new Component_catalog     .catalog in (* components *)


  (* 1. Universe *)

  (* component_types *)
  let add_component_type ct = component_type#add (convert_component_type_name ct.Abstract_io.component_type_name) in
  
  (* ports *)
  let add_component_type_ports ct =
    List.iter port#add (List.map convert_port_name (List.map fst ct.Abstract_io.component_type_provide )); (* add provide  ports *)
    List.iter port#add (List.map convert_port_name (List.map fst ct.Abstract_io.component_type_require )); (* add require  ports *)
    List.iter port#add (List.map convert_port_name               ct.Abstract_io.component_type_conflict)   (* add conflict ports *)
  in

  (* repositories *)
  let add_repository r = repository#add (convert_repository_name r.Abstract_io.repository_name) in
  let add_package r_id r_name k = package#add (r_id, (convert_package_name r_name k.Abstract_io.package_name)) in
  
  (* packages *)
  let add_repository_packages r =
    let add_package =
      let r_name = convert_repository_name r.Abstract_io.repository_name in (* repository name *)
      let r_id   = try repository#id_of_name r_name                    (* repository id   *)
                   with Not_found -> failwith "Loading packages from a repository before loading the repository first!" in
      add_package r_id r_name in
    List.iter add_package r.Abstract_io.repository_packages (* add available packages *)
  in

  (* resources *)
  let add_component_type_resources ct = List.iter resource#add (List.map convert_resource_name (List.map fst ct.Abstract_io.component_type_consume)) (* add consumed resources *) in
  let add_package_resources        k  = List.iter resource#add (List.map convert_resource_name (List.map fst k.Abstract_io.package_consume))         (* add consumed resources *) in
  
  (* Add all the universe data and all the additional repositories data. *)
  begin
    match universe with
    | None   -> ()
    | Some u -> 
        List.iter add_component_type           u.Abstract_io.universe_component_types; (* add component types *)
        List.iter add_component_type_ports     u.Abstract_io.universe_component_types; (* add ports from component types *)
        List.iter add_repository               u.Abstract_io.universe_repositories;    (* add repositories *)
        List.iter add_repository_packages      u.Abstract_io.universe_repositories;    (* add packages from repositories *)
        List.iter add_component_type_resources u.Abstract_io.universe_component_types; (* add resources from component types *)
        List.iter add_package_resources        (List.flatten (List.map (fun r -> r.Abstract_io.repository_packages) u.Abstract_io.universe_repositories)) (* add resources from packages*)
  end;
  List.iter add_repository          additional_repositories; (* add additional repositories *)
  List.iter add_repository_packages additional_repositories; (* add packages from additional repositories *)


  (* 2. Configuration *)

  (* location *)
  let add_location l = location#add (convert_location_name l.Abstract_io.location_name) in

  (* component *)
  let add_component c = component#add (convert_component_name c.Abstract_io.component_name)  in

  (* resources *)
  let add_location_resources l = List.iter resource#add (List.map convert_resource_name (List.map fst l.Abstract_io.location_provide_resources)) in

  (* deprecated component types *)
  let add_component_deprecated_component_type c =
    try let _ = component_type#id_of_name (convert_component_type_name c.Abstract_io.component_type) in ()
    (* If there is an exception it means that this component type does not exist in the universe - it is deprecated. *)
    with Not_found -> component_type#set_id_of_name (convert_component_type_name c.Abstract_io.component_type) (Fresh_id.special Data_common.Deprecated) in

  (* Add all the initial configuration data. *)
  begin
    match initial_configuration with
    | None   -> ()
    | Some c ->
        List.iter add_location           c.Abstract_io.configuration_locations;  (* add locations *)
        List.iter add_component          c.Abstract_io.configuration_components; (* add components *)
        List.iter add_location_resources c.Abstract_io.configuration_locations;  (* add resources from locations *)
        List.iter add_component_deprecated_component_type c.Abstract_io.configuration_components (* add deprecated component types *)
  end;

  (* The meta-catalog object: *)
  new model_catalog 
    ~component_type_catalog: component_type
    ~port_catalog:           port
    ~repository_catalog:     repository
    ~package_catalog:        package
    ~resource_catalog:       resource
    ~location_catalog:       location
    ~component_catalog:      component


(* This is a workaround necessary to keep the function polymorph. Honestly I don't know why it works... *)
type ('id, 'name) make_not_found_functions = string -> ('id -> string) -> ('name -> string) -> (('id -> 'name) * ('name -> 'id))
type make_not_found_functions_workaround = { workaround : 'id 'name. ('id, 'name) make_not_found_functions; }

let make_not_found_functions : ('id, 'name) make_not_found_functions = 
  fun catalog_name string_of_id string_of_name ->
    let id_not_found   (id   : 'id)   : 'name = failwith (Printf.sprintf "Load_model loaded catalog error: %s#name_of_id %s" catalog_name (string_of_id   id))
    and name_not_found (name : 'name) : 'id   = failwith (Printf.sprintf "Load_model loaded catalog error: %s#id_of_name %s" catalog_name (string_of_name name))
  in (id_not_found, name_not_found)


let model_catalog_of_abstract_io_with_exceptions (naming : closed_model_catalog) (make_not_found_functions : make_not_found_functions_workaround) : closed_model_catalog = 
  new closed_model_catalog
    ~component_type_catalog: (new Component_type_catalog .closed_catalog_with_exceptions naming#component_type (make_not_found_functions.workaround "component_type" String_of.component_type_id String_of.component_type_name))
    ~port_catalog:           (new Port_catalog           .closed_catalog_with_exceptions naming#port           (make_not_found_functions.workaround "port"           String_of.port_id           String_of.port_name))
    ~repository_catalog:     (new Repository_catalog     .closed_catalog_with_exceptions naming#repository     (make_not_found_functions.workaround "repository"     String_of.repository_id     String_of.repository_name))
    ~package_catalog:        (new Package_catalog        .closed_catalog_with_exceptions naming#package        (make_not_found_functions.workaround "package"        String_of.package_id        (fun (r_id, k) -> String_of.package_name k)))
    ~resource_catalog:       (new Resource_catalog       .closed_catalog_with_exceptions naming#resource       (make_not_found_functions.workaround "resource"       String_of.resource_id       String_of.resource_name))
    ~location_catalog:       (new Location_catalog       .closed_catalog_with_exceptions naming#location       (make_not_found_functions.workaround "location"       String_of.location_id       String_of.location_name))
    ~component_catalog:      (new Component_catalog      .closed_catalog_with_exceptions naming#component      (make_not_found_functions.workaround "component"      String_of.component_id      String_of.component_name))


(*/*************************************************\*)
(*| 2. Universe Conversion                          |*)
(*\*************************************************/*)

let convert_universe (catalog : #closed_model_catalog) external_repositories u : universe = 

  (* 1. Data Storage *)

  let component_types : Component_type_obj_catalog .obj_catalog_iface = new Component_type_obj_catalog .obj_catalog in
  let packages        : Package_obj_catalog        .obj_catalog_iface = new Package_obj_catalog        .obj_catalog in
  let repositories    : Repository_obj_catalog     .obj_catalog_iface = new Repository_obj_catalog     .obj_catalog in


  (* 2. Conversion *)
    
  (* component types *)
  let convert_component_type t : unit =

    (* name *)  
    let name = convert_component_type_name t.Abstract_io.component_type_name in

    (* id *)
    let id = catalog#component_type#id_of_name name in

    (* create the mapping for provide *)
    let provide : provide_arity Port_id_map.t = 
      Port_id_map.map_of_list (fun (name, arity) -> 
        let id = catalog#port#id_of_name (convert_port_name name) in 
        (id, convert_provide_arity arity)
      ) t.Abstract_io.component_type_provide in

    (* create the mapping for require *)
    let require : require_arity Port_id_map.t = 
      Port_id_map.map_of_list (fun (name, arity) -> 
        let id = catalog#port#id_of_name (convert_port_name name) in 
        (id, convert_require_arity arity)
      ) t.Abstract_io.component_type_require in

    (* create the set for conflict *)
    let conflict : Port_id_set.t = 
      Port_id_set.set_of_list (fun name -> 
        catalog#port#id_of_name (convert_port_name name)
      ) t.Abstract_io.component_type_conflict in 

    (* create the mapping for resource consumption *)
    let consume : resource_consume_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (name, arity) -> 
        (catalog#resource#id_of_name (convert_resource_name name), convert_resource_consume_arity arity)
      ) t.Abstract_io.component_type_consume in

    (* create the component type object *)
    let new_component_type = new component_type ~provide ~require ~conflict ~consume () in

    (* store the component type *)
    component_types#add_id_obj_pair id new_component_type
  in
    

  (* packages *)
  let convert_package r_id r_name k : package_id =

    (* name *)
    let name = convert_package_name r_name k.Abstract_io.package_name in

    (* id *)
    let id = catalog#package#id_of_name (r_id, name) in

    (* create the dependency sets *)
    let depend : Package_id_set_set.t = 
      Package_id_set_set.set_of_list (fun s -> 
        Package_id_set.set_of_list (fun n -> 
          catalog#package#id_of_name (r_id, convert_package_name r_name n)) s
      ) k.Abstract_io.package_depend in
    
    (* similar *)
    let conflict : Package_id_set.t = 
      Package_id_set.set_of_list (fun n -> 
        catalog#package#id_of_name (r_id, (convert_package_name r_name n))
      ) k.Abstract_io.package_conflict in
    
    (* create the mapping for resource consumption *)
    let consume : resource_consume_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (r_id,n) -> 
        (catalog#resource#id_of_name (convert_resource_name r_id), convert_resource_consume_arity n)
      ) k.Abstract_io.package_consume in

    (* create the package object *)
    let new_package = new package ~depend ~conflict ~consume () in

    (* store the package *)
    packages#add_id_obj_pair id new_package;

    (* return the package id *)
    id
  in


  (* repositories *)
  let convert_repository r : unit = 

    (* name *)
    let name = convert_repository_name r.Abstract_io.repository_name in

    (* id *)
    let id = catalog#repository#id_of_name name in

    (* packages *)
    let packages : Package_id_set.t = 
      Package_id_set.set_of_list (fun k -> 
        convert_package id name k (* this returns the package id, thus the set is built *)
      ) r.Abstract_io.repository_packages in

    (* create the repository object *)
    let new_repository = new repository
      ~packages: packages
      ()
    in

    (* store the repository *)
    repositories#add_id_obj_pair id new_repository
  in

  (* universe *)
  List.iter convert_repository     u.Abstract_io.universe_repositories;    (* include the repository and package data coming from the universe *) 
  List.iter convert_repository     external_repositories;                  (* include the repository and package data coming from the external repositories *) 
  List.iter convert_component_type u.Abstract_io.universe_component_types; (* include the component type data *)
  
  (* may add erroneous packages and component types *)
  let implementation = 
    Component_type_id_map.map_of_list (fun (t, ks) -> 
      (catalog#component_type#id_of_name (convert_component_type_name t), 
       Package_id_set.set_of_list (fun (r, k) -> 
         let r_name = convert_repository_name r in 
         catalog#package#id_of_name (catalog#repository#id_of_name r_name, convert_package_name r_name k)
       ) ks)
    ) u.Abstract_io.universe_implementation in
  
  new universe 
    ~ports:           catalog#port#ids
    ~packages:        packages#id_to_obj_map
    ~resources:       catalog#resource#ids
    ~component_types: component_types#id_to_obj_map
    ~implementation:  implementation
    ~repositories:    repositories#id_to_obj_map
    ()

(* Possible inconsistencies not detected during generation:
 - package presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
 - component type presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
*)


(*/*************************************************\*)
(*| 3. Configuration Conversion                     |*)
(*\*************************************************/*)

let convert_configuration (catalog : closed_model_catalog) c : configuration =

  (* 1. Data storage *)

  let locations  : Location_obj_catalog  .obj_catalog_iface = new Location_obj_catalog  .obj_catalog in
  let components : Component_obj_catalog .obj_catalog_iface = new Component_obj_catalog .obj_catalog in
  

  (* 2. Conversion *)
  
  (* location *)
  let convert_location l : unit =

    (* name *)
    let name = convert_location_name l.Abstract_io.location_name in

    (* id *)
    let id = catalog#location#id_of_name name in

    (* repository *)
    let r_name = convert_repository_name l.Abstract_io.location_repository in 
    let repository = catalog#repository#id_of_name r_name in

    (* packages installed *)
    let packages_installed : Package_id_set.t = 
      Package_id_set.set_of_list (fun k -> 
        catalog#package#id_of_name (repository, (convert_package_name r_name k))
      ) l.Abstract_io.location_packages_installed in

    (* provide_resources *)
    let provide_resources : resource_provide_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (r, n) -> 
        (catalog#resource#id_of_name (convert_resource_name r), convert_resource_provide_arity n)
      ) l.Abstract_io.location_provide_resources in (* needs to be completed with non-mentioned resources *)

    (* cost *)
    let cost = convert_location_cost l.Abstract_io.location_cost in

    (* create the location object *)    
    let new_location = new location ~repository ~packages_installed ~provide_resources ~cost () in

    (* store the location *)
    locations#add_id_obj_pair id new_location
  in

  (* components *)
  let convert_component c : unit =

    (* name *)
    let name = convert_component_name c.Abstract_io.component_name in

    (* id *)
    let id = catalog#component#id_of_name name in

    (* type *)
    let typ = catalog#component_type#id_of_name (convert_component_type_name c.Abstract_io.component_type) in

    (* location *)
    let location = catalog#location#id_of_name (convert_location_name c.Abstract_io.component_location) in

    (* create the component object *)
    let new_component = new component ~typ ~location () in
    
    (* store the component *)
    components#add_id_obj_pair id new_component
  in
    
  (* bindings *)
  let convert_binding b =
    let port     = catalog#port#id_of_name      (convert_port_name      b.Abstract_io.binding_port) in
    let requirer = catalog#component#id_of_name (convert_component_name b.Abstract_io.binding_requirer) in
    let provider = catalog#component#id_of_name (convert_component_name b.Abstract_io.binding_provider) in 

    new binding ~port ~requirer ~provider in

  (* configuration *)
  List.iter convert_location  c.Abstract_io.configuration_locations;
  List.iter convert_component c.Abstract_io.configuration_components;
  let bindings = Binding_set.set_of_list (convert_binding) c.Abstract_io.configuration_bindings in

  new configuration
    ~locations:  locations#id_to_obj_map
    ~components: components#id_to_obj_map
    ~bindings:   bindings
    ()

let empty_configuration = new configuration ()


(*****************************************************)
(** 4. Specification conversion                      *)
(*****************************************************)

(* This translation is a one to one mapping. *)

let convert_spec_variable_name = fun x -> x
let convert_spec_const         = fun x -> x

let convert_spec_op = function
| Abstract_io.Lt  -> Lt
| Abstract_io.LEq -> LEq
| Abstract_io.Eq  -> Eq
| Abstract_io.GEq -> GEq
| Abstract_io.Gt  -> Gt
| Abstract_io.NEq -> NEq

let convert_specification (catalog : closed_model_catalog) =

  let convert_spec_local_element el = match el with
  | Abstract_io.SpecLocalElementPackage       (r, k) -> let r_name = convert_repository_name r in 
                                                        let repo = catalog#repository#id_of_name r_name in
                                                        Spec_local_element_package        (catalog#package#id_of_name        (repo, (convert_package_name r_name k)))
  | Abstract_io.SpecLocalElementComponentType (t)    -> Spec_local_element_component_type (catalog#component_type#id_of_name (convert_component_type_name t))
  | Abstract_io.SpecLocalElementPort          (p)    -> Spec_local_element_port           (catalog#port#id_of_name           (convert_port_name           p)) in

  let rec convert_spec_local_expr e = match e with
  | Abstract_io.SpecLocalExprVar   (v)        -> Spec_local_expr_var   (convert_spec_variable_name v)
  | Abstract_io.SpecLocalExprConst (c)        -> Spec_local_expr_const (convert_spec_const c)
  | Abstract_io.SpecLocalExprArity (el)       -> Spec_local_expr_arity (convert_spec_local_element el)
  | Abstract_io.SpecLocalExprAdd   (ex1, ex2) -> Spec_local_expr_add   (convert_spec_local_expr ex1, convert_spec_local_expr ex2)
  | Abstract_io.SpecLocalExprSub   (ex1, ex2) -> Spec_local_expr_sub   (convert_spec_local_expr ex1, convert_spec_local_expr ex2)
  | Abstract_io.SpecLocalExprMul   (n, ex)    -> Spec_local_expr_mul   (convert_spec_const n, convert_spec_local_expr ex) in

  let rec convert_local_specification sl = match sl with 
  | Abstract_io.SpecLocalTrue               -> Spec_local_true
  | Abstract_io.SpecLocalOp   (ex1, o, ex2) -> Spec_local_op   (convert_spec_local_expr ex1, convert_spec_op o, convert_spec_local_expr ex2)
  | Abstract_io.SpecLocalAnd  (sl1, sl2)    -> Spec_local_and  (convert_local_specification sl1, convert_local_specification sl2)
  | Abstract_io.SpecLocalOr   (sl1, sl2)    -> Spec_local_or   (convert_local_specification sl1, convert_local_specification sl2)
  | Abstract_io.SpecLocalImpl (sl1, sl2)    -> Spec_local_impl (convert_local_specification sl1, convert_local_specification sl2)
  | Abstract_io.SpecLocalNot  (sl')         -> Spec_local_not  (convert_local_specification sl') in

  let convert_spec_repository_constraint x = catalog#repository#id_of_name (convert_repository_name x) in
  let convert_spec_resource_constraint (r, o, c) = (catalog#resource#id_of_name (convert_resource_name r), convert_spec_op o, convert_spec_const c) in

  let convert_spec_element el = match el with
  | Abstract_io.SpecElementPackage       (r, k)       -> let r_name = convert_repository_name r in
                                                         let repo = catalog#repository#id_of_name r_name in
                                                         Spec_element_package        (catalog#package#id_of_name (repo, (convert_package_name r_name k)))
  | Abstract_io.SpecElementComponentType (t)          -> Spec_element_component_type (catalog#component_type#id_of_name (convert_component_type_name t))
  | Abstract_io.SpecElementPort          (p)          -> Spec_element_port           (catalog#port#id_of_name (convert_port_name p))
  | Abstract_io.SpecElementLocalisation  (phi, r, sl) -> Spec_element_location(
                                                           List.map convert_spec_resource_constraint   phi,
                                                           List.map convert_spec_repository_constraint r,
                                                           convert_local_specification sl) in

  let rec convert_spec_expr e = match e with
  | Abstract_io.SpecExprVar   (v)        -> Spec_expr_var   (convert_spec_variable_name v)
  | Abstract_io.SpecExprConst (c)        -> Spec_expr_const (convert_spec_const c)
  | Abstract_io.SpecExprArity (el)       -> Spec_expr_arity (convert_spec_element el)
  | Abstract_io.SpecExprAdd   (ex1, ex2) -> Spec_expr_add   (convert_spec_expr ex1, convert_spec_expr ex2)
  | Abstract_io.SpecExprSub   (ex1, ex2) -> Spec_expr_sub   (convert_spec_expr ex1, convert_spec_expr ex2)
  | Abstract_io.SpecExprMul   (n, ex)    -> Spec_expr_mul   (convert_spec_const n,  convert_spec_expr ex) in

  let rec convert_specification s = match s with 
  | Abstract_io.SpecTrue               -> Spec_true
  | Abstract_io.SpecOp   (ex1, o, ex2) -> Spec_op   (convert_spec_expr ex1, convert_spec_op o, convert_spec_expr ex2)
  | Abstract_io.SpecAnd  (s1, s2)      -> Spec_and  (convert_specification s1, convert_specification s2)
  | Abstract_io.SpecOr   (s1, s2)      -> Spec_or   (convert_specification s1, convert_specification s2)
  | Abstract_io.SpecImpl (s1, s2)      -> Spec_impl (convert_specification s1, convert_specification s2)
  | Abstract_io.SpecNot  (s')          -> Spec_not  (convert_specification s') in 

  convert_specification


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

let load_versioned_object file = Input_helper.parse_json Json_versions_j.read_versioned_object file

let load_version file : int = 
  let versioned_object = load_versioned_object file in
  match versioned_object with
  | Some versioned_object' -> versioned_object'.Json_versions_t.version
  | None -> 0

let load_basic_universe file =
  let syntax_version = load_version file in
  Zephyrus_log.log_execution (Printf.sprintf "Universe syntax version = %d%!\n" syntax_version);
  match syntax_version with
  | 0 -> (match Input_helper.parse_json Json_v0_j.read_universe file with
          | None   -> None
          | Some u -> Some (Json_v0.To_abstract_io.universe u))
  | 1 -> (match Input_helper.parse_json Json_v1_j.read_universe file with
          | None   -> None
          | Some u -> Some (Json_v1.To_abstract_io.universe u))
  | _ -> failwith "Unsupported universe syntax version!"
  

let load_basic_repositories l = 
  List.fold_left (fun res (n, f) -> 
    let r = Input_helper.parse_json Json_v0_j.read_packages f in 
    match r with 
    | None    -> res
    | Some ks -> { 
        Abstract_io.repository_name     = n;
        Abstract_io.repository_packages = List.map Json_v0.To_abstract_io.package ks
      }::res
  ) [] l

let load_basic_configuration file = 
  let syntax_version = load_version file in
  Zephyrus_log.log_execution (Printf.sprintf "Configuration syntax version = %d%!\n" syntax_version);
    match syntax_version with
  | 0 -> (match Input_helper.parse_json Json_v0_j.read_configuration file with
          | None   -> None
          | Some c -> Some (Json_v0.To_abstract_io.configuration c))
  | 1 -> (match Input_helper.parse_json Json_v1_j.read_configuration file with
          | None   -> None
          | Some c -> Some (Json_v1.To_abstract_io.configuration c))
  | _ -> failwith "Unsupported configuration syntax version!"

  

let load_basic_specification file = 
  Input_helper.parse_standard Specification_parser.main Specification_lexer.token file

let load_catalog u rs c = 
  let model_catalog = model_catalog_of_abstract_io u rs c in 
  let closed_model_catalog = close_model_catalog model_catalog in 
  model_catalog_of_abstract_io_with_exceptions closed_model_catalog {workaround = make_not_found_functions;}

let load_universe              = convert_universe 
let load_configuration         = convert_configuration
let load_specification         = convert_specification
let load_optimization_function = convert_optimization_function


let model_of_file_options file_u file_rs file_c file_s optim =

  let u   : Abstract_io.universe             option = match file_u  with None -> None | Some(file_u')  -> load_basic_universe      file_u' in
  let rs  : Abstract_io.repository           list   = match file_rs with None -> []   | Some(file_rs') -> load_basic_repositories  file_rs' in
  let c   : Abstract_io.configuration        option = match file_c  with None -> None | Some(file_c')  -> load_basic_configuration file_c' in
  let s   : Abstract_io.specification        option = match file_s  with None -> None | Some(file_s')  -> load_basic_specification file_s' in
  let opt : Data_model.optimization_function option = match optim   with None -> None | Some(optim')   -> Some(load_optimization_function optim') in

  let catalog = load_catalog u rs c in
  let u : Data_model.universe      option = match u with None -> None | Some(u') -> Some(load_universe      catalog rs u') in
  let c : Data_model.configuration option = match c with None -> None | Some(c') -> Some(load_configuration catalog    c') in
  let s : Data_model.specification option = match s with None -> None | Some(s') -> Some(load_specification catalog    s') in

  (catalog, u, c, s, opt)

let model_of_settings () = 
  model_of_file_options
    (Settings.get_input_file_universe              ())
    (Settings.get_input_file_repositories          ())
    (Settings.get_input_file_initial_configuration ())
    (Settings.get_input_file_specification         ())
    (Settings.get_input_optimization_function      ())

let set_initial_model_of_settings () = 
  let (catalog, universe, initial_configuration, specification, optimization_function) = model_of_settings () in
  Data_state.catalog_full               := Some catalog;
  Data_state.universe_full              := universe;
  Data_state.initial_configuration_full := initial_configuration;
  Data_state.specification_full         := specification;
  Data_state.optimization_function      := optimization_function

let set_initial_model_of_benchmark (benchmark : Benchmarks.benchmark) =
  (* Written using the well known programming paradigm invented by Mr. Copy and Dr. Paste. *)
  let u   : Abstract_io.universe             = benchmark#universe in
  let rs  : Abstract_io.repository list      = [] in
  let c   : Abstract_io.configuration        = benchmark#initial_configuration in
  let s   : Abstract_io.specification        = benchmark#specification in
  let opt : Data_model.optimization_function = benchmark#optimisation_function in

  let catalog = load_catalog (Some u) rs (Some c) in
  let u : Data_model.universe      = load_universe      catalog rs u in
  let c : Data_model.configuration = load_configuration catalog c in
  let s : Data_model.specification = load_specification catalog s in

  Data_state.universe_full              := Some(u);
  Data_state.initial_configuration_full := Some(c);
  Data_state.specification_full         := Some(s);
  Data_state.optimization_function      := Some(opt)
