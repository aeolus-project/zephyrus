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

(** Static global variables stocking the Zephyrus model. *)

(** Note: Need significant refactoring. Storing these data in a bunch of global variables is horribly awful from the software engineering point of view. *)

(* Depends on
    - datatypes/Data_model (all universe, configuration, specification)
    - datatypes/Data_constraints (constraint and solution)
    - datatypes/Bin_packing (constraint) <- TODO
*)

open Data_model

(* 1. the different variant of inputs *)
let universe_full              : universe option ref = ref None
let initial_configuration_full : configuration option ref = ref None
let specification_full         : specification option ref = ref None
let optimization_function      : optimization_function option ref = ref None
let catalog_full               : Data_model_catalog.closed_model_catalog option ref = ref None

let universe_trim_component    : universe option ref = ref None
let universe_trim_package      : universe option ref = ref None
let universe_trim_package_full : universe option ref = ref None

let component_type_name_set_full : Component_type_name_set.t option ref = ref None
let component_type_set_full : Component_type_set.t option ref = ref None
let port_name_set_full : Component_type_name_set.t option ref = ref None

(*
let universe_flat              : universe_flat option ref = ref None
let universe_flat_trim         : universe_flat option ref = ref None
let universe_flat_graph        : universe_flat option ref = ref None
let universe_flat_constraint   : universe_flat option ref = ref None
let universe_flat_solution     : universe_flat option ref = ref None
*)



let specification_local        : specification option ref = ref None
let specification_global       : specification option ref = ref None
let specification_constraint   : specification option ref = ref None
let specification_solution     : specification option ref = ref None

let specification_is_fw        : bool option ref = ref None



(* 2. constraints *)

open Data_constraint (* warning, type name clash with optimization_function coming from Data_model *)

let constraint_universe_component_type_require        : (konstraint list) ref = ref []
let constraint_universe_component_type_provide        : (konstraint list) ref = ref []
let constraint_universe_component_type_conflict       : (konstraint list) ref = ref []
let constraint_universe_component_type_implementation : (konstraint list) ref = ref []
let constraint_universe_binding_unicity               : (konstraint list) ref = ref []
let constraint_universe_location_component_type       : (konstraint list) ref = ref []
let constraint_universe_location_package              : (konstraint list) ref = ref []
let constraint_universe_location_port                 : (konstraint list) ref = ref []
let constraint_universe_definition_port               : (konstraint list) ref = ref []
let constraint_universe_repository_unicity            : (konstraint list) ref = ref []
let constraint_universe_repository_package            : (konstraint list) ref = ref []
let constraint_universe_package_dependency            : (konstraint list) ref = ref []
let constraint_universe_package_conflict              : (konstraint list) ref = ref []
let constraint_universe_resource_consumption          : (konstraint list) ref = ref []
let constraint_universe_deprecated_element            : (konstraint list) ref = ref []
let constraint_universe_used_locations                : (konstraint list) ref = ref []
let constraint_universe_incompatibilities             : (konstraint list) ref = ref []


let constraint_specification_full : konstraint option ref = ref None
let constraint_configuration_full : (konstraint list) ref = ref []

let constraint_optimization_function : optimization_function option ref = ref None

let get_constraint_optimization_function () = match !constraint_optimization_function with None -> Data_constraint.Multi_objective.Satisfy | Some(f) -> f

let get_constraint_flat_universe () = [
    ("  Bindings require:  " , (Data_constraint.conj(!constraint_universe_component_type_require)));
    ("  Bindings provide:  " , (Data_constraint.conj(!constraint_universe_component_type_provide)));
    ("  Bindings conflict: " , (Data_constraint.conj(!constraint_universe_component_type_conflict)));
    ("  Bindings unicity:  " , (Data_constraint.conj(!constraint_universe_binding_unicity))) ]

let get_constraint_universe () = (get_constraint_flat_universe ()) @ [
    ("  Component implementation by packages: " , (Data_constraint.conj(!constraint_universe_component_type_implementation)));
    ("  Global component type t arity = sum of local component type t arities: " , (Data_constraint.conj(!constraint_universe_location_component_type)));
    ("  Global package k arity = sum of local package k arities: " , (Data_constraint.conj(!constraint_universe_location_package)));
    ("  Global port p arity = sum of local port p arities: " , (Data_constraint.conj(!constraint_universe_location_port)));
    ("  How much port p arity is provided in total on each location: " , (Data_constraint.conj(!constraint_universe_definition_port)));
    ("  Repository unicity (exactly one repository on each location): " , (Data_constraint.conj(!constraint_universe_repository_unicity)));
    ("  Repository packages: " , (Data_constraint.conj(!constraint_universe_repository_package)));
    ("  Package dependencies: " , (Data_constraint.conj(!constraint_universe_package_dependency)));
    ("  Package conflicts:  " , (Data_constraint.conj(!constraint_universe_package_conflict)));
    ("  Resources: " , (Data_constraint.conj(!constraint_universe_resource_consumption)));
    ("  Remove deprecated components: " , (Data_constraint.conj(!constraint_universe_deprecated_element)));
    ("  Number of used locations: " , (Data_constraint.conj(!constraint_universe_used_locations)));
    ("  Incompatibilities between components: " , (Data_constraint.conj(!constraint_universe_incompatibilities))) ]

let get_constraint_specification () = [
    ("  specification constraint" , match !constraint_specification_full with None -> Data_constraint.true_konstraint | Some(k) -> k) ]
let get_constraint_configuration () = [
    ("  configuration " , ((Data_constraint.conj(!constraint_configuration_full)))) ]

let get_constraint_full () = (get_constraint_universe ()) @ (get_constraint_specification ()) @ (get_constraint_configuration ())

let constraint_variable_bounds       : variable_bounds option ref = ref None



(*******************************************)
(** 6. Very Simple Bounds Definition       *)
(*******************************************)

let basic_bounds_function v = (** this function gives the basic bounds of every variable: [min = 0] and [max = \infty] except for packages and repositories *)
   match v with
  | Simple_variable           _ -> Bound.big
  | Global_variable           _ -> Bound.big
  | Local_variable       (_, e) -> (match e with | Package(_) -> Bound.small | _ -> Bound.big)
  | Binding_variable          _ -> Bound.big
  | Local_repository_variable _ -> Bound.small
  | Local_resource_variable   _ -> Bound.big
  | Location_used_variable    _ -> Bound.small

let get_variable_bounds () = match !constraint_variable_bounds with None -> basic_bounds_function | Some(b) -> b


(* let constraint_bin_packing           : konstraintbin_packing option ref = ref None *)
(*

(* 3. solutions *)

let solution : solution option ref = ref None


(* 4. generated configurations *)

let configuration_no_bindings : configuration option ref = ref None
let configuration_full        : configuration option ref = ref None
*)

 


