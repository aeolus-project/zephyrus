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


(* 0. Conversion *)

let convert_component_type_name  = fun x -> x
let convert_port_name            = fun x -> x
let convert_component_name       = fun x -> x
let convert_package_name         = fun x -> x
let convert_repository_name      = fun x -> x
let convert_location_name        = fun x -> x
let convert_resource_name        = fun x -> x
let convert_provide_arity prov   = match prov with | `InfiniteProvide -> InfiniteProvide | `FiniteProvide(i) -> FiniteProvide(i)
let convert_require_arity        = fun x -> x
let convert_resource_consumption = fun x -> x
let convert_resource_provide     = fun x -> x



let convert_component_type t = {
  component_type_name     = convert_component_type_name t.Json_t.component_type_name;
  component_type_provide  = Port_name_map.map_of_list (fun (p,n) -> (convert_port_name p, convert_provide_arity n)) t.Json_t.component_type_provide;
  component_type_require  = Port_name_map.map_of_list (fun (p,n) -> (convert_port_name p, convert_require_arity n)) t.Json_t.component_type_require;
  component_type_conflict = Port_name_set.set_of_list (convert_port_name) t.Json_t.component_type_conflict;
  component_type_consume  = Resource_name_map.map_of_list (fun (r,n) -> (convert_resource_name r, convert_resource_consumption n)) t.Json_t.component_type_consume }
 
let convert_package k = {
  package_name     = convert_package_name k.Json_t.package_name;
  package_depend   = Package_name_set_set.set_of_list (Package_name_set.set_of_direct_list) k.Json_t.package_depend;
  package_conflict = Package_name_set.set_of_direct_list k.Json_t.package_conflict;
  package_consume  = Resource_name_map.map_of_list (fun (r,n) -> (convert_resource_name r, convert_resource_consumption n)) k.Json_t.package_consume }

let convert_repository r = {
  repository_name = convert_repository_name r.Json_t.repository_name;
  repository_packages = Package_name_map.map_of_list (fun k -> let k' = convert_package k in (k'.package_name, k')) r.Json_t.repository_packages } 

let convert_universe u = {
  universe_component_types = Component_type_name_map.map_of_list (fun t -> let t' = convert_component_type t in (t'.component_type_name, t')) u.Json_t.universe_component_types;
  universe_implementation  = Component_type_name_map.map_of_list (fun (t,ks) -> (convert_component_name t, Package_name_set.set_of_direct_list ks))  u.Json_t.universe_implementation;
  universe_repositories    = Repository_name_map.map_of_list (fun r -> let r' = convert_repository r in (r'.repository_name, r')) u.Json_t.universe_repositories }



let convert_location l = {
  location_name               = convert_location_name l.Json_t.location_name;
  location_repository         = convert_repository_name l.Json_t.location_repository;
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



