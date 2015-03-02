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

open Data_common
open Data_model

(** Constraints *)

open Data_constraint (* warning, type name clash with optimization_function coming from Data_model *)

type structured_constraints = (string * konstraint list) list

let variables_of_structured_constraints structured_constraints =
  List.fold_left (fun acc (_, ks) -> 
    List.fold_left (fun acc k -> 
      Variable_set.union (variables_of_konstraint k) acc
    ) acc ks
  ) Variable_set.empty structured_constraints

type constraint_universe = {
  constraint_universe_component_type_require        : konstraint list;
  constraint_universe_component_type_provide        : konstraint list;
  constraint_universe_component_type_conflict       : konstraint list;
  constraint_universe_binding_unicity               : konstraint list;
  constraint_universe_location_component_type       : konstraint list;
  constraint_universe_location_package              : konstraint list;
  constraint_universe_location_port                 : konstraint list;
  constraint_universe_definition_port               : konstraint list;
  constraint_universe_component_type_implementation : konstraint list;
  constraint_universe_repository_unicity            : konstraint list;
  constraint_universe_repository_package            : konstraint list;
  constraint_universe_package_dependency            : konstraint list;
  constraint_universe_package_conflict              : konstraint list;
  constraint_universe_resource_consumption          : konstraint list;
  constraint_universe_deprecated_element            : konstraint list;
  constraint_universe_used_locations                : konstraint list;
  constraint_universe_incompatibilities             : konstraint list;
}

let empty_constraint_universe = {
  constraint_universe_component_type_require        = [];
  constraint_universe_component_type_provide        = [];
  constraint_universe_component_type_conflict       = [];
  constraint_universe_binding_unicity               = [];
  constraint_universe_location_component_type       = [];
  constraint_universe_location_package              = [];
  constraint_universe_location_port                 = [];
  constraint_universe_definition_port               = [];
  constraint_universe_component_type_implementation = [];
  constraint_universe_repository_unicity            = [];
  constraint_universe_repository_package            = [];
  constraint_universe_package_dependency            = [];
  constraint_universe_package_conflict              = [];
  constraint_universe_resource_consumption          = [];
  constraint_universe_deprecated_element            = [];
  constraint_universe_used_locations                = [];
  constraint_universe_incompatibilities             = [];
}

let get_constraint_optimization_function (constraint_optimization_function : optimization_function option) : optimization_function = 
  match constraint_optimization_function with 
  | Some optimization_function  -> optimization_function
  | None                        -> Data_constraint.Multi_objective.Satisfy

let structured_constraints_of_flat_universe constraint_universe : structured_constraints = [
  ( "  Bindings require:  " , constraint_universe.constraint_universe_component_type_require  );
  ( "  Bindings provide:  " , constraint_universe.constraint_universe_component_type_provide  );
  ( "  Bindings conflict: " , constraint_universe.constraint_universe_component_type_conflict );
  ( "  Bindings unicity:  " , constraint_universe.constraint_universe_binding_unicity         )]

let structured_constraints_of_constraint_universe constraint_universe : structured_constraints = 
  (structured_constraints_of_flat_universe constraint_universe) @ [
  ( "  Global component type t arity = sum of local component type t arities: " , constraint_universe.constraint_universe_location_component_type       );
  ( "  Global package k arity = sum of local package k arities: "               , constraint_universe.constraint_universe_location_package              );
  ( "  Global port p arity = sum of local port p arities: "                     , constraint_universe.constraint_universe_location_port                 );
  ( "  How much port p arity is provided in total on each location: "           , constraint_universe.constraint_universe_definition_port               );
  ( "  Component implementation by packages: "                                  , constraint_universe.constraint_universe_component_type_implementation );
  ( "  Repository unicity (exactly one repository on each location): "          , constraint_universe.constraint_universe_repository_unicity            );
  ( "  Repository packages: "                                                   , constraint_universe.constraint_universe_repository_package            );
  ( "  Package dependencies: "                                                  , constraint_universe.constraint_universe_package_dependency            );
  ( "  Package conflicts:  "                                                    , constraint_universe.constraint_universe_package_conflict              );
  ( "  Resources: "                                                             , constraint_universe.constraint_universe_resource_consumption          );
  ( "  Remove deprecated components: "                                          , constraint_universe.constraint_universe_deprecated_element            );
  ( "  Number of used locations: "                                              , constraint_universe.constraint_universe_used_locations                );
  ( "  Incompatibilities between components: "                                  , constraint_universe.constraint_universe_incompatibilities             )]

let structured_constraints_of_constraint_specification (constraint_specification : konstraint option) : structured_constraints =
  [("  specification constraint" , List.singleton_of_option constraint_specification)]

let structured_constraints_of_constraint_configuration (constraint_configuration : konstraint list) : structured_constraints =
  [("  configuration " , constraint_configuration)]

let get_constraint_full constraint_universe constraint_specification constraint_configuration : structured_constraints = 
  (structured_constraints_of_constraint_universe       constraint_universe     ) @ 
  (structured_constraints_of_constraint_specification  constraint_specification) @ 
  (structured_constraints_of_constraint_configuration  constraint_configuration)



(** Bounds *)

type constraint_variable_bounds = variable_bounds option

(** this function gives the basic bounds of every variable: [min = 0] and [max = \infty] except for packages and repositories *)
let basic_bounds_function : variable_bounds = 
  function
  | Simple_variable           _ -> Bound.big
  | Global_variable           _ -> Bound.big
  | Local_variable       (_, e) -> (match e with | Package(_) -> Bound.small | _ -> Bound.big)
  | Binding_variable          _ -> Bound.big
  | Local_repository_variable _ -> Bound.small
  | Local_resource_variable   _ -> Bound.big
  | Location_used_variable    _ -> Bound.small

let get_variable_bounds (constraint_variable_bounds : constraint_variable_bounds) : variable_bounds = 
  match constraint_variable_bounds with 
  | Some bounds_function -> bounds_function
  | None                 -> basic_bounds_function


