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

(* Depends on
    - datatypes/Data_model (all universe, configuration, specification)
    - datatypes/Data_constraints (constraint and solution)
    - datatypes/Bin_packing (constraint) <- TODO
*)

open Data_model

(*
open Constraints
open Bin_packing
*)

(* 1. the different variant of inputs *)
let resources_full             : resources option ref = ref None
let universe_full              : universe option ref = ref None
let initial_configuration_full : configuration option ref = ref None
let specification_full         : specification option ref = ref None
let optimization_function      : optimization_function option ref = ref None


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

let constraint_specification_full : konstraint option ref = ref None
let constraint_configuration_full : (konstraint list) ref = ref []

let constraint_optimization_function : optimization_function option ref = ref None
let constraint_variable_bounds       : variable_bounds option ref = ref None

(* let constraint_bin_packing           : konstraintbin_packing option ref = ref None *)
(*

(* 3. solutions *)

let solution : solution option ref = ref None


(* 4. generated configurations *)

let configuration_no_bindings : configuration option ref = ref None
let configuration_full        : configuration option ref = ref None
*)

 


