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

(** Conversion between the STATEFUL and STATELESS Zephyrus model. *)

open Data_common

(*
module Stateful_id = struct 
  type t = (Stateful_abstract_io.component_type_name * Stateful_abstract_io.state_name)
  let compare = compare
end

module Stateful_id_set = Set.Make(Stateful_id)
module Stateful_id_map = Map.Make(Stateful_id)

module Stateless_id = struct
  type t = Abstract_io.component_type_name
  let compare = compare
end

module Stateless_id_set = Set.Make(Stateless_id)
module Stateless_id_map = Map.Make(Stateless_id)

module Stateful_to_stateless_mapping = Mapping(Stateful_id_set)(Stateless_id_set)(Stateful_id_map)
module Stateless_to_stateful_mapping = Mapping(Stateless_id_set)(Stateful_id_set)(Stateless_id_map)

class conversion = object (self)

  val mutable stateful_to_stateless_mapping = new Stateful_to_stateless_mapping.mapping
  val mutable stateless_to_stateful_mapping = new Stateless_to_stateful_mapping.mapping

  method private convert_stateful_id_to_stateless_id (component_type_name, state_name) =
    Printf.sprintf "%s_%s" component_type_name state_name

  method get_stateless_of_stateful (stateful_id : Stateful_id.t) : Stateless_id.t =
    try
      stateful_to_stateless_mapping#find stateful_id
    with Not_found -> 
      let stateless_id = convert_stateful_id_to_stateless_id stateful_id in
      stateful_to_stateless_mapping#add stateful_id stateless_id;
      stateless_to_stateful_mapping#add stateless_id stateful_id;
      stateless_id

  method get_stateful_of_stateless (stateless_id : Stateless_id.t) : Stateful_id.t =
    try
      stateless_to_stateful_mapping#find stateless_id
    with Not_found ->
      failwith (Printf.sprintf "There is no stateful component type and state with match the stateless component name %s" stateless_id)

end

class converter = object (self)

  val conversion = new conversion

  method private component_type_name_stateless_of_stateful component_type_name state_name =
    conversion#get_stateful_of_stateless (component_type_name, state_name)
    
  method private component_type_name_and_state_stateful_of_stateless component_type_name =
    conversion#get_stateless_of_stateful component_type_name

*)

let component_type_name_stateless_of_stateful component_type_name state_name =
  Printf.sprintf "%s_%s" component_type_name state_name

let component_type_name_and_state_stateful_of_stateless component_type_name =
  failwith "TODO!"


module To_stateless = struct

  module I = Stateful_abstract_io
  module O = Abstract_io

  let port_name           port_name'           = port_name'
  let resource_name       resource_name'       = resource_name'
  let component_type_name component_type_name' = component_type_name'
  let state_name          state_name'          = state_name'
  let package_name        package_name'        = package_name'
  let repository_name     repository_name'     = repository_name'
  let location_name       location_name'       = location_name'
  let component_name      component_name'      = component_name'
  
  let stateless_component_type_name component_type_name' state_name' =
    component_type_name_stateless_of_stateful (component_type_name component_type_name') (state_name state_name')

  let provide_arity provide_arity' =
  	match provide_arity' with
    | I.FiniteProvide i -> O.FiniteProvide i
    | I.InfiniteProvide -> O.InfiniteProvide

  let require_arity          require_arity'          = require_arity'
  let resource_consumption   resource_consumption'   = resource_consumption'
  let resource_provide_arity resource_provide_arity' = resource_provide_arity'

  let single_provide (port_name',     provide_arity'       ) = (port_name     port_name',     provide_arity        provide_arity'       )
  let single_require (port_name',     require_arity'       ) = (port_name     port_name',     require_arity        require_arity'       )
  let single_consume (resource_name', resource_consumption') = (resource_name resource_name', resource_consumption resource_consumption')

  let component_type component_type' : O.component_type list = 
    let state state' = {
      O.component_type_name     = stateless_component_type_name component_type'.I.component_type_name state'.I.state_name;
      O.component_type_provide  = List.map single_provide state'.I.state_provide;
      O.component_type_require  = List.map single_require state'.I.state_require;
      O.component_type_conflict = List.map port_name      state'.I.state_conflict;
      O.component_type_consume  = List.map single_consume component_type'.I.component_type_consume;
    } in
    List.map state component_type'.I.component_type_states

  let single_depend package_name_list = List.map package_name package_name_list

  let package package' = {
    O.package_name     = package_name            package'.I.package_name;
    O.package_depend   = List.map single_depend  package'.I.package_depend;
    O.package_conflict = List.map package_name   package'.I.package_conflict;
    O.package_consume  = List.map single_consume package'.I.package_consume;
  }

  let repository repository' = {
    O.repository_name     = repository_name  repository'.I.repository_name;
    O.repository_packages = List.map package repository'.I.repository_packages;
  }

  let implementation_package (repository_name', package_name') = 
    (repository_name repository_name', package_name package_name')

  let single_implementation (component_type_name', implementation_packages_list) = 
    (component_type_name component_type_name', List.map implementation_package implementation_packages_list)

  let universe universe' = {
    O.universe_component_types = List.flatten (List.map component_type        universe'.I.universe_component_types);
    O.universe_implementation  =               List.map single_implementation universe'.I.universe_implementation;
    O.universe_repositories    =               List.map repository            universe'.I.universe_repositories;
  }

  let location_cost location_cost' = location_cost'

  let single_provide_resources (resource_name', resource_provide_arity') = (resource_name resource_name', resource_provide_arity resource_provide_arity')

  let location location' = {
    O.location_name               = location_name                     location'.I.location_name;
    O.location_provide_resources  = List.map single_provide_resources location'.I.location_provide_resources;
    O.location_repository         = repository_name                   location'.I.location_repository;
    O.location_packages_installed = List.map package_name             location'.I.location_packages_installed;
    O.location_cost               = location_cost                     location'.I.location_cost;
  }

  let component component' = {
    O.component_name     = component_name                component'.I.component_name;
    O.component_type     = stateless_component_type_name component'.I.component_type component'.I.component_state;
    O.component_location = location_name                 component'.I.component_location;
  }

  let binding binding' = {
    O.binding_port     = port_name      binding'.I.binding_port;
    O.binding_requirer = component_name binding'.I.binding_requirer;
    O.binding_provider = component_name binding'.I.binding_provider;
  }

  let configuration configuration' = {
    O.configuration_locations  = List.map location  configuration'.I.configuration_locations;
    O.configuration_components = List.map component configuration'.I.configuration_components;
    O.configuration_bindings   = List.map binding   configuration'.I.configuration_bindings;
  }

  let spec_variable_name spec_variable_name' = spec_variable_name'

  let spec_const spec_const' = spec_const'

  let spec_local_element = function
    | I.SpecLocalElementPackage       (repository_name', package_name')   -> O.SpecLocalElementPackage       (repository_name repository_name', package_name package_name')  
    | I.SpecLocalElementComponentType (component_type_name', state_name') -> O.SpecLocalElementComponentType (stateless_component_type_name component_type_name' state_name')
    | I.SpecLocalElementPort          (port_name')                        -> O.SpecLocalElementPort          (port_name port_name')

  let rec spec_local_expr = function
    | I.SpecLocalExprVar   (spec_variable_name')                  -> O.SpecLocalExprVar   (spec_variable_name spec_variable_name')
    | I.SpecLocalExprConst (spec_const')                          -> O.SpecLocalExprConst (spec_const spec_const')
    | I.SpecLocalExprArity (spec_local_element')                  -> O.SpecLocalExprArity (spec_local_element spec_local_element')
    | I.SpecLocalExprAdd   (spec_local_expr_1, spec_local_expr_2) -> O.SpecLocalExprAdd   (spec_local_expr spec_local_expr_1, spec_local_expr spec_local_expr_2)
    | I.SpecLocalExprSub   (spec_local_expr_1, spec_local_expr_2) -> O.SpecLocalExprSub   (spec_local_expr spec_local_expr_1, spec_local_expr spec_local_expr_2)
    | I.SpecLocalExprMul   (spec_const', spec_local_expr')        -> O.SpecLocalExprMul   (spec_const spec_const', spec_local_expr spec_local_expr')

  let spec_op = function
    | I.Lt  -> O.Lt
    | I.LEq -> O.LEq
    | I.Eq  -> O.Eq
    | I.GEq -> O.GEq
    | I.Gt  -> O.Gt
    | I.NEq -> O.NEq

  let rec local_specification = function
    | I.SpecLocalTrue                                                  -> O.SpecLocalTrue
    | I.SpecLocalOp   (spec_local_expr_1, spec_op', spec_local_expr_2) -> O.SpecLocalOp   (spec_local_expr spec_local_expr_1, spec_op spec_op', spec_local_expr spec_local_expr_2)
    | I.SpecLocalAnd  (local_specification_1, local_specification_2)   -> O.SpecLocalAnd  (local_specification local_specification_1, local_specification local_specification_2)
    | I.SpecLocalOr   (local_specification_1, local_specification_2)   -> O.SpecLocalOr   (local_specification local_specification_1, local_specification local_specification_2)
    | I.SpecLocalImpl (local_specification_1, local_specification_2)   -> O.SpecLocalImpl (local_specification local_specification_1, local_specification local_specification_2)
    | I.SpecLocalNot  (local_specification')                           -> O.SpecLocalNot  (local_specification local_specification')

  let spec_repository_constraint = repository_name

  let spec_repository_constraints = (List.map spec_repository_constraint)

  let spec_resource_constraint (resource_name', spec_op', spec_const') = 
    (resource_name resource_name', spec_op spec_op', spec_const spec_const')

  let spec_resource_constraints = List.map spec_resource_constraint

  let spec_element = function
    | I.SpecElementPackage       (repository_name', package_name')   -> O.SpecElementPackage       (repository_name repository_name', package_name package_name')  
    | I.SpecElementComponentType (component_type_name', state_name') -> O.SpecElementComponentType (stateless_component_type_name component_type_name' state_name')
    | I.SpecElementPort          (port_name')                        -> O.SpecElementPort          (port_name port_name')
    | I.SpecElementLocalisation  (spec_resource_constraints', spec_repository_constraints', local_specification') ->
        O.SpecElementLocalisation (spec_resource_constraints spec_resource_constraints', spec_repository_constraints spec_repository_constraints', local_specification local_specification')

  let rec spec_expr = function
    | I.SpecExprVar   (spec_variable_name')      -> O.SpecExprVar   (spec_variable_name spec_variable_name')
    | I.SpecExprConst (spec_const')              -> O.SpecExprConst (spec_const spec_const')
    | I.SpecExprArity (spec_element')            -> O.SpecExprArity (spec_element spec_element')
    | I.SpecExprAdd   (spec_expr_1, spec_expr_2) -> O.SpecExprAdd   (spec_expr spec_expr_1, spec_expr spec_expr_2)
    | I.SpecExprSub   (spec_expr_1, spec_expr_2) -> O.SpecExprSub   (spec_expr spec_expr_1, spec_expr spec_expr_2)
    | I.SpecExprMul   (spec_const', spec_expr')  -> O.SpecExprMul   (spec_const spec_const', spec_expr spec_expr')

  let spec_location_names = List.map location_name

  let rec specification = function
    | I.SpecTrue                                                    -> O.SpecTrue
    | I.SpecOp   (spec_expr_1, spec_op', spec_expr_2)               -> O.SpecOp         (spec_expr spec_expr_1, spec_op spec_op', spec_expr spec_expr_2)
    | I.SpecAnd  (specification_1, specification_2)                 -> O.SpecAnd        (specification specification_1, specification specification_2)
    | I.SpecOr   (specification_1, specification_2)                 -> O.SpecOr         (specification specification_1, specification specification_2)
    | I.SpecImpl (specification_1, specification_2)                 -> O.SpecImpl       (specification specification_1, specification specification_2)
    | I.SpecNot  (specification')                                   -> O.SpecNot        (specification specification')
    | I.SpecEverywhere (                      local_specification') -> O.SpecEverywhere (                                          local_specification local_specification')
    | I.SpecAt         (spec_location_names', local_specification') -> O.SpecAt         (spec_location_names spec_location_names', local_specification local_specification')

  let optimization_function = function
    | I.Optimization_function_simple       -> O.Optimization_function_simple
    | I.Optimization_function_compact      -> O.Optimization_function_compact
    | I.Optimization_function_conservative -> O.Optimization_function_conservative
    | I.Optimization_function_spread       -> O.Optimization_function_spread
    | I.Optimization_function_none         -> O.Optimization_function_none


  let initial_model initial_model' = {
    O.universe              = Option.map universe              initial_model'.I.universe;
    O.initial_configuration = Option.map configuration         initial_model'.I.initial_configuration;
    O.specification         = Option.map specification         initial_model'.I.specification;
    O.optimization_function = Option.map optimization_function initial_model'.I.optimization_function;
  }

end

(*
module To_stateful = struct

  module I = Abstract_io
  module O = Stateful_abstract_io

  let port_name           port_name'           = port_name'
  let resource_name       resource_name'       = resource_name'
  let component_type_name component_type_name' = component_type_name'
  let state_name          state_name'          = state_name'
  let package_name        package_name'        = package_name'
  let repository_name     repository_name'     = repository_name'
  let location_name       location_name'       = location_name'
  let component_name      component_name'      = component_name'
  
  let stateful_component_type_name_and_state component_type_name' state_name' =
    component_type_name_and_state_stateful_of_stateless (component_type_name component_type_name')

  let location_cost location_cost' = location_cost'

  let single_provide_resources (resource_name', resource_provide_arity') = (resource_name resource_name', resource_provide_arity resource_provide_arity')

  let location location' = {
    O.location_name               = location_name                     location'.I.location_name;
    O.location_provide_resources  = List.map single_provide_resources location'.I.location_provide_resources;
    O.location_repository         = repository_name                   location'.I.location_repository;
    O.location_packages_installed = List.map package_name             location'.I.location_packages_installed;
    O.location_cost               = location_cost                     location'.I.location_cost;
  }

  let component component' = {
    O.component_name     = component_name                component'.I.component_name;
    O.component_type     = stateless_component_type_name component'.I.component_type component'.I.component_state;
    O.component_location = location_name                 component'.I.component_location;
  }

  let binding binding' = {
    O.binding_port     = port_name      binding'.I.binding_port;
    O.binding_requirer = component_name binding'.I.binding_requirer;
    O.binding_provider = component_name binding'.I.binding_provider;
  }

  let configuration configuration' = {
    O.configuration_locations  = List.map location  configuration'.I.configuration_locations;
    O.configuration_components = List.map component configuration'.I.configuration_components;
    O.configuration_bindings   = List.map binding   configuration'.I.configuration_bindings;
  }

end

end
*)