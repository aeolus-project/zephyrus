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
let convert_provide_arity          x = match x with | `InfiniteProvide -> Infinite_provide | `FiniteProvide(i) -> Finite_provide(i)
let convert_require_arity          x = x
let convert_package_name         r x = if Settings.find Settings.append_repository_to_package_name then r ^ x else x
let convert_repository_name        x = x
let convert_location_name          x = x
let convert_location_cost          x = x
let convert_component_name         x = x


(*/*************************************************\*)
(*| 1. Resource Names and Ids Extractions           |*)
(*\*************************************************/*)

(* A meta-catalog created from the Json_t model: universe, repositories, initial configuration and specification. *)
let model_catalog_of_json_t (universe : Json_t.universe option) (additional_repositories : Json_t.repository list) (initial_configuration : Json_t.configuration option) (specification : Json_t.specification option) : model_catalog = 

  (* Mappings *)
  let component_type = new Component_type_catalog.catalog in (* component types *)
  let port           = new Port_catalog          .catalog in (* ports *)
  let repository     = new Repository_catalog    .catalog in (* repositories *)
  let package        = new Package_catalog       .catalog in (* packages *)
  let resource       = new Resource_catalog      .catalog in (* resources *)
  let location       = new Location_catalog      .catalog in (* locations *)
  let component      = new Component_catalog     .catalog in (* components *)


  (* Functions for adding stuff *)

  (* 1. Universe *)

  (* component types *)
  let add_component_type ct =
    let open Json_t in
    component_type#add (convert_component_type_name ct.component_type_name);                          (* name *)
    List.iter port#add (List.map convert_port_name     (List.map fst ct.component_type_provide ));    (* provide: add port *)
    List.iter port#add (List.map convert_port_name     (List.map fst ct.component_type_require ));    (* require: add port *)
    List.iter port#add (List.map convert_port_name                   ct.component_type_conflict) ;    (* conflict: add port *)
    List.iter resource#add (List.map convert_resource_name (List.map fst ct.component_type_consume )) (* consume: add resources *)
  in

  (* packages *)
  let add_package r_id r_name k =
    let open Json_t in
    package#add (r_id, (convert_package_name "" k.package_name));                                                          (* name *)
    List.iter (fun x -> package#add (r_id, x)) (List.map (convert_package_name r_name) (List.flatten k.package_depend  )); (* depend: add packages *)
    List.iter (fun x -> package#add (r_id, x)) (List.map (convert_package_name r_name)               k.package_conflict);  (* conflict: add packages *)
    List.iter resource#add       (List.map convert_resource_name (List.map fst k.package_consume ))                        (* consume: add resources *)
  in

  (* repositories *)
  let add_repository r =
    let open Json_t in
    let r_name = convert_repository_name r.repository_name in (* name *)
    let r_id   = repository#get_else_add r_name in              (* id *)
    List.iter (add_package r_id r_name) r.repository_packages (* packages: add packages *)
  in

  (* universe *)
  let add_universe u =
    let open Json_t in
    List.iter add_component_type u.universe_component_types; (* component_types *)
                                                             (* TODO: implementation *)
    List.iter add_repository     u.universe_repositories     (* repositories *)
  in

  (* Add all the universe data and all the additional repositories data. *)
  let _ = 
    begin
      match universe with
      | None   -> ()
      | Some u -> add_universe u
    end;
    List.iter add_repository additional_repositories
  in


  (* 2. Configuration *)

  (* component *)
  let add_component c =
    let open Json_t in
    component#add      (convert_component_name      c.component_name);    (* name *)
    (* If the component's type is already in the catalog - do nothing. *)
    try let _ = component_type#id_of_name (convert_component_type_name c.component_type) in ()
    (* Otherwise it means that this component type does not exist in the universe - it is deprecated. *)
    with Not_found -> component_type#set_id_of_name (convert_component_type_name c.component_type) (Fresh_id.special Data_common.Deprecated)  (* type *)
    (* location#add       (convert_location_name       c.component_location) (* location *) *) (* TODO: This is useless, as if the location does not exist, the initial config is not valid. *)
  in

  (*
    (* binding *) (* TODO: Is this useful at all? *)
    let add_binding b =
      let open Json_t in
      port#add (convert_port_name b.binding_port);                                                       (* port_name *)
      List.iter component#add (List.map convert_component_name [b.binding_requirer; b.binding_provider]) (* requirer, provider*)
    in
  *)

  (* location *)
  let add_location l =
    let open Json_t in
    location#add (convert_location_name l.location_name);                                                         (* name *)
    List.iter resource#add (List.map convert_resource_name (List.map fst l.location_provide_resources));          (* provide_resources *)
    let r_id = repository#get_else_add (convert_repository_name l.location_repository) in                           (* repository id *) (* TODO: What if the repository does not exist in the universe? *)
    List.iter (fun x -> package#add (r_id, x)) (List.map (convert_package_name "") l.location_packages_installed) (* packages *)
  in

  (* configuration *)
  let add_configuration c =
    let open Json_t in
    List.iter add_location  c.configuration_locations;     (* locations *)
    List.iter add_component c.configuration_components     (* components *)
    (* List.iter add_binding   c.configuration_bindings *) (* bindings *)
  in

  (* Add all the initial configuration data. *)
  let _ = 
    begin
      match initial_configuration with
      | None   -> ()
      | Some c -> 
          (* Prepare the one way mapping between component_type id -1 and name "Deprecated" *) (* TODO: This is probably useless? *)
          (* component_type#set_name_of_id (Fresh_id.special Data_common.Deprecated) "Deprecated"; *)
          add_configuration c
    end
  in

  (* TODO: Add all the specification data. *)
  let _ =
    begin
      match specification with
      | None   -> ()
      | Some s -> ()
    end
  in

  (* The meta-catalog object. *)
  new model_catalog 
    ~component_type_catalog: component_type
    ~port_catalog:           port
    ~repository_catalog:     repository
    ~package_catalog:        package
    ~resource_catalog:       resource
    ~location_catalog:       location
    ~component_catalog:      component

let model_catalog_of_json_t_with_exceptions (naming : model_catalog) : closed_model_catalog = 
  new closed_model_catalog
    ~component_type_catalog: (new Component_type_catalog .closed_catalog_with_exceptions naming#component_type "component_type" String_of.component_type_id String_of.component_type_name)
    ~port_catalog:           (new Port_catalog           .closed_catalog_with_exceptions naming#port           "port"           String_of.port_id           String_of.port_name)
    ~repository_catalog:     (new Repository_catalog     .closed_catalog_with_exceptions naming#repository     "repository"     String_of.repository_id     String_of.repository_name)
    ~package_catalog:        (new Package_catalog        .closed_catalog_with_exceptions naming#package        "package"        String_of.package_id        (fun (r_id, k) -> String_of.package_name k))
    ~resource_catalog:       (new Resource_catalog       .closed_catalog_with_exceptions naming#resource       "resource"       String_of.resource_id       String_of.resource_name)
    ~location_catalog:       (new Location_catalog       .closed_catalog_with_exceptions naming#location       "location"       String_of.location_id       String_of.location_name)
    ~component_catalog:      (new Component_catalog      .closed_catalog_with_exceptions naming#component      "component"      String_of.component_id      String_of.component_name)


(*/*************************************************\*)
(*| 2. Universe Conversion                          |*)
(*\*************************************************/*)


(*/*********************************)
(* class for loading a universe from a Json_j structure. Contains all conversion function concerning universes *)
let convert_universe (catalog : closed_model_catalog) external_repositories u : universe = 
  
  (* 1. Data Storage *)

  (* component types *)
  let component_type           : Component_type_obj_catalog.obj_catalog_iface = new Component_type_obj_catalog.obj_catalog in
  
  (* store the component type in parameter *)
  let new_component_type id t =
    component_type#add_id_obj_pair id t in


  (* packages *)
  let package                   : Package_obj_catalog.obj_catalog_iface = new Package_obj_catalog.obj_catalog in
  let package_id_to_repo_id_map : (repository_id Package_id_map.t) ref  = ref Package_id_map.empty in (* mapping package id -> repository id *)

  (* store the package *)
  let new_package r id k =
    package#add_id_obj_pair id k;
    package_id_to_repo_id_map := Package_id_map.add id r !package_id_to_repo_id_map;
    (id, k) in (* <- this pair looks strange: LOOK INTO THIS *)


  (* repositories *)
  let repository : Repository_obj_catalog.obj_catalog_iface = new Repository_obj_catalog.obj_catalog in
    
  (* store the repository *)
  let new_repository id r =
    repository#add_id_obj_pair id r in

  (* annex function, for the implementation relation *)
  let find_repository r = 
    try catalog#repository#id_of_name r 
    with Not_found -> Zephyrus_log.log_missing_data "repository" r "repository declaration" in


  (* 2. Conversion *)
    
  (* component types *)
  let convert_component_type t =

    (* name *)  
    let name = convert_component_type_name t.Json_t.component_type_name in

    (* id *)
    let id = catalog#component_type#id_of_name name in

    (* create the mapping for provide *)
    let provide : provide_arity Port_id_map.t = 
      Port_id_map.map_of_list (fun (name, arity) -> 
        let id = catalog#port#id_of_name (convert_port_name name) in 
        (id, convert_provide_arity arity)
      ) t.Json_t.component_type_provide in

    (* create the mapping for require *)
    let require : require_arity Port_id_map.t = 
      Port_id_map.map_of_list (fun (name, arity) -> 
        let id = catalog#port#id_of_name (convert_port_name name) in 
        (id, convert_require_arity arity)
      ) t.Json_t.component_type_require in

    (* create the set for conflict *)
    let conflict : Port_id_set.t = 
      Port_id_set.set_of_list (fun name -> 
        catalog#port#id_of_name (convert_port_name name)
      ) t.Json_t.component_type_conflict in 

    (* create the mapping for resource consumption *)
    let consume : resource_consume_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (name, arity) -> 
        (catalog#resource#id_of_name (convert_resource_name name), convert_resource_consume_arity arity)
      ) t.Json_t.component_type_consume in

    new_component_type id (new component_type
      ~id:       id 
      ~provide:  provide 
      ~require:  require 
      ~conflict: conflict 
      ~consume:  consume)
    in
    

  (* packages *)
  let convert_package r_id r_name k =

    (* name *)
    let name = convert_package_name r_name k.Json_t.package_name in

    (* id *)
    let id = catalog#package#id_of_name (r_id, name) in

    (* create the dependency sets *)
    let depend : Package_id_set_set.t = 
      Package_id_set_set.set_of_list (fun s -> 
        Package_id_set.set_of_list (fun n -> 
          catalog#package#id_of_name (r_id, convert_package_name r_name n)) s
      ) k.Json_t.package_depend in
    
    (* similar *)
    let conflict : Package_id_set.t = 
      Package_id_set.set_of_list (fun n -> 
        catalog#package#id_of_name (r_id, (convert_package_name r_name n))
      ) k.Json_t.package_conflict in
    
    (* create the mapping for resource consumption *)
    let consume : resource_consume_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (r_id,n) -> 
        (catalog#resource#id_of_name (convert_resource_name r_id), convert_resource_consume_arity n)
      ) k.Json_t.package_consume in

    new_package r_id id (new package
      ~id:       id
      ~depend:   depend
      ~conflict: conflict
      ~consume:  consume) 
    in


  (* repositories *)
  let convert_repository r = 

    (* name *)
    let name = convert_repository_name r.Json_t.repository_name in

    (* id *)
    let id = catalog#repository#id_of_name name in

    (* packages *)
    let packages : package Package_id_map.t = 
      Package_id_map.map_of_list (fun k -> 
        convert_package id name k
      ) r.Json_t.repository_packages in
    
    new_repository id (new repository
      ~id:       id
      ~packages: packages) in

  (* universe *)
  List.iter convert_repository     u.Json_t.universe_repositories;    (* fill the repository and package tables 1/2 *) 
  List.iter convert_repository     external_repositories;             (* fill the repository and package tables 2/2 *) 
  List.iter convert_component_type u.Json_t.universe_component_types; (* fill the component type table *)
  
  let implementation = Component_type_id_map.map_of_list                     (* may add erroneous packages and component types in the table *)
    (fun (t,ks) -> (catalog#component_type#id_of_name (convert_component_type_name t), Package_id_set.set_of_list
      (fun (r,k) -> let r_name = convert_repository_name r in catalog#package#id_of_name ((find_repository r_name), (convert_package_name r_name k))) ks)) u.Json_t.universe_implementation in
  
  new universe 
    ~ports:           catalog#port#ids
    ~packages:        package#id_to_obj_map
    ~resources:       catalog#resource#ids
    ~component_types: component_type#id_to_obj_map
    ~implementation:  implementation
    ~repositories:    repository#id_to_obj_map

(* Possible inconsistencies not detected during generation:
 - package presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
 - component type presented in a dependency, but not declared. Can be detected by an id in [get_package_ids] without an entry in [get_package]
*)


(*/*************************************************\*)
(*| 3. Configuration Conversion                     |*)
(*\*************************************************/*)

(*/*********************************)
(* class for loading a configuration from a Json_j structure. Contains all conversion function concerning configurations *)
let convert_configuration (catalog : closed_model_catalog) c : configuration =

  (* 1. Data storage *)

  (* locations *)
  let location : Location_obj_catalog.obj_catalog_iface = new Location_obj_catalog.obj_catalog in
  
  (* create all the structure to store the new location l *)
  let new_location id (l : location) =
    location#add_id_obj_pair id l in

  (* annex function, used for components *)
  let find_location l = 
    try catalog#location#id_of_name l
    with Not_found -> Zephyrus_log.log_missing_data "location" l "component" in
  
  (* components *)
  let component : Component_obj_catalog.obj_catalog_iface = new Component_obj_catalog.obj_catalog in
  
  (* create all the structure to store the new component c *)
  let new_component id c =
    component#add_id_obj_pair id c in

  (* annex function, used for the bindings *)
  let find_component c = 
    try catalog#component#id_of_name c
    with Not_found -> Zephyrus_log.log_missing_data "component" c "binding" in

  (* 2. Conversion *)
  
  (* location *)
  let convert_location l =

    (* name *)
    let name = convert_location_name l.Json_t.location_name in

    (* id *)
    let id = catalog#location#id_of_name name in

    (* repository *)
    let r_name = convert_repository_name l.Json_t.location_repository in 
    let repository = catalog#repository#id_of_name r_name in

    (* packages installed *)
    let packages_installed : Package_id_set.t = 
      Package_id_set.set_of_list (fun k -> 
        catalog#package#id_of_name (repository, (convert_package_name r_name k))
      ) l.Json_t.location_packages_installed in

    (* resources *)
    let resources : resource_provide_arity Resource_id_map.t = 
      Resource_id_map.map_of_list (fun (r, n) -> 
        (catalog#resource#id_of_name (convert_resource_name r), convert_resource_provide_arity n)
      ) l.Json_t.location_provide_resources in (* needs to be completed with non-mentioned resources *)

    (* cost *)
    let cost = convert_location_cost l.Json_t.location_cost in

    new_location id (new location
      ~id:                 id
      ~repository:         repository
      ~packages_installed: packages_installed
      ~provide_resources:  resources
      ~cost:               cost)
    in

  (* components *)
  let convert_component c =

    (* name *)
    let name = convert_component_name c.Json_t.component_name in

    (* id *)
    let id = catalog#component#id_of_name name in

    (* type *)
    let typ = catalog#component_type#id_of_name (convert_component_type_name c.Json_t.component_type) in

    (* location *)
    let location = find_location (convert_location_name c.Json_t.component_location) in

    new_component id (new component
      ~id:       id
      ~typ:      typ
      ~location: location) 
    in
    
    (* bindings *)
  let convert_binding b =
    let port     = catalog#port#id_of_name (convert_port_name b.Json_t.binding_port) in
    let requirer = find_component (convert_component_name b.Json_t.binding_requirer) in
    let provider = find_component (convert_component_name b.Json_t.binding_provider) in 

    new binding
      ~port:     port
      ~requirer: requirer
      ~provider: provider 
    in

  (* configuration *)
  List.iter convert_location c.Json_t.configuration_locations;
  List.iter convert_component c.Json_t.configuration_components;
  let bindings = Binding_set.set_of_list (convert_binding) c.Json_t.configuration_bindings in

  new configuration
    ~locations:  location#id_to_obj_map
    ~components: component#id_to_obj_map
    ~bindings:   bindings
    ()

let empty_configuration = new configuration ()

(*****************************************************)
(** 4. Specification conversion                      *)
(*****************************************************)

(* this translation is a one to one mapping. Nothing much to say *)

let convert_spec_variable_name = fun x -> x
let convert_spec_const         = fun x -> x
let convert_spec_op o          = match o with
 | `Lt -> Lt | `LEq -> LEq | `Eq -> Eq | `GEq -> GEq | `Gt -> Gt | `NEq -> NEq

let convert_specification (catalog : closed_model_catalog) s =

  let convert_spec_local_element el = match el with
    | `SpecLocalElementPackage(r,k)       -> let r_name = convert_repository_name r in let repo = catalog#repository#id_of_name r_name in
                                               Spec_local_element_package(catalog#package#id_of_name (repo, (convert_package_name r_name k)))
    | `SpecLocalElementComponentType(t)   -> Spec_local_element_component_type(catalog#component_type#id_of_name (convert_component_type_name t))
    | `SpecLocalElementPort(p)            -> Spec_local_element_port(catalog#port#id_of_name (convert_port_name p)) in
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
    | `SpecLocalNot(sl')        -> Spec_local_not(convert_local_specification sl') in

  let convert_spec_repository_constraint x = catalog#repository#id_of_name (convert_repository_name x) in
  let convert_spec_resource_constraint (r, o, c) = (catalog#resource#id_of_name (convert_resource_name r), convert_spec_op o, convert_spec_const c) in

  let convert_spec_element el = match el with
    | `SpecElementPackage(r,k)           -> let r_name = convert_repository_name r in let repo = catalog#repository#id_of_name r_name in
                                            Spec_element_package(catalog#package#id_of_name (repo, (convert_package_name r_name k)))
    | `SpecElementComponentType(t)       -> Spec_element_component_type(catalog#component_type#id_of_name (convert_component_type_name t))
    | `SpecElementPort(p)                -> Spec_element_port(catalog#port#id_of_name (convert_port_name p))
    | `SpecElementLocalisation(phi,r,sl) -> Spec_element_location(
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

let load_basic_universe file      = Input_helper.parse_json Json_j.read_universe file
let load_basic_repositories l     = List.fold_left (fun res (n,f) -> let r = Input_helper.parse_json Json_j.read_packages f in match r with None -> res
      | Some(ks) -> { Json_j.repository_name = n; Json_j.repository_packages = ks }::res) [] l
let load_basic_configuration file = Input_helper.parse_json Json_j.read_configuration file
let load_basic_specification file = Input_helper.parse_standard Specification_parser.main Specification_lexer.token file


let load_catalog u rs c s        = model_catalog_of_json_t_with_exceptions (model_catalog_of_json_t u rs c s)
let load_universe catalog rs u   = convert_universe catalog rs u
let load_configuration catalog c = convert_configuration catalog c
let load_specification           = convert_specification
let load_optimization_function   = convert_optimization_function


let model_of_file_options file_u file_repos file_conf file_spec optim =
  let u  = match file_u with None -> None | Some(file_u') -> load_basic_universe file_u' in
  let rs = match file_repos with None -> [] | Some(file_repos') -> load_basic_repositories file_repos' in
  let c  = match file_conf with None -> None | Some(file_conf') -> load_basic_configuration file_conf' in
  let s  = match file_spec with None -> None | Some(file_spec') -> load_basic_specification file_spec' in
  let f  = match optim with None -> None | Some(optim') -> Some(load_optimization_function optim') in
  let catalog = load_catalog u rs c s in
  let final_u = match u with None -> None | Some(u') -> Some(load_universe catalog rs u') in
  let final_c = match c with None -> None | Some(c') -> Some(load_configuration catalog c') in
  let final_s = match s with None -> None | Some(s') -> Some(load_specification catalog s') in
  (catalog, final_u, final_c, final_s, f)

let model_of_settings () = model_of_file_options
    (Settings.get_input_file_universe ()) (Settings.get_input_file_repositories ()) (Settings.get_input_file_initial_configuration ())
    (Settings.get_input_file_specification ()) (Settings.get_input_optimization_function ())

let set_initial_model_of_settings () = let (catalog, universe, initial_configuration, specification, f) = model_of_settings () in
  Data_state.catalog_full               := Some catalog;
  Data_state.universe_full              := universe;
  Data_state.initial_configuration_full := initial_configuration;
  Data_state.specification_full         := specification;
  Data_state.optimization_function      := f

let set_initial_model_of_benchmark (benchmark : Benchmarks.benchmark) =
  (* Written using the well known programming paradigm invented by Mr. Copy and Dr. Paste. *)
  let u  : Json_t.universe                  = benchmark#universe in
  let rs = [] in
  let c  : Json_t.configuration             = benchmark#initial_configuration in
  let s  : Json_t.specification             = benchmark#specification in
  let f  : Data_model.optimization_function = benchmark#optimisation_function in

  let catalog               = load_catalog (Some u) rs (Some c) (Some s) in
  let universe              = load_universe      catalog rs u in
  let initial_configuration = load_configuration catalog c in
  let specification         = load_specification catalog s in

  Data_state.universe_full              := Some(universe);
  Data_state.initial_configuration_full := Some(initial_configuration);
  Data_state.specification_full         := Some(specification);
  Data_state.optimization_function      := Some(f)
