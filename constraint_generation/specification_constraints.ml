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

open Helpers

open Aeolus_types_t
open Typing_context
open Variables
open Generic_constraints

let rec translate_spec_variable_name spec_variable_name =
  (SpecificationVariable spec_variable_name)

and translate_spec_const (spec_const : spec_const) : expr = 
  ( int2expr spec_const )

and translate_specification (location_names : location_name list) (specification : specification) : cstr =
  match specification with
  | `SpecTrue -> 
      truecstr

  | `SpecOp (l_spec_expr, spec_op, r_spec_expr) ->
      ( (translate_spec_op spec_op) (translate_spec_expr location_names l_spec_expr) (translate_spec_expr location_names r_spec_expr) )

  | `SpecAnd (l_specification, r_specification) ->
      ( (translate_specification location_names l_specification) &&~~ (translate_specification location_names r_specification) )

  | `SpecOr (l_specification, r_specification) ->
      ( (translate_specification location_names l_specification) ||~~ (translate_specification location_names r_specification) )

  | `SpecImpl (l_specification, r_specification) ->
      ( (translate_specification location_names l_specification) =>~~ (translate_specification location_names r_specification) )

  | `SpecNot (specification) ->
      ( not (translate_specification location_names specification) )

and translate_spec_expr (location_names : location_name list) (spec_expr : spec_expr) : expr =
  match spec_expr with
  | `SpecExprVar spec_variable_name ->
      ( var2expr ( translate_spec_variable_name spec_variable_name ) )

  | `SpecExprConst spec_const ->
      ( translate_spec_const spec_const )

  | `SpecExprArity spec_element ->
      ( translate_spec_element location_names spec_element )

  | `SpecExprAdd (l_spec_expr, r_spec_expr) ->
      ( (translate_spec_expr location_names l_spec_expr) +~ (translate_spec_expr location_names r_spec_expr) )

  | `SpecExprSub (l_spec_expr, r_spec_expr) ->
      ( (translate_spec_expr location_names l_spec_expr) -~ (translate_spec_expr location_names r_spec_expr) )

  | `SpecExprMul (spec_const, spec_expr) ->
      ( (translate_spec_const spec_const) *~ (translate_spec_expr location_names spec_expr) )

and translate_spec_element  (location_names : location_name list) (spec_element : spec_element) : expr =
  match spec_element with
  | `SpecElementPackage (package_name) ->
      ( var2expr ((GlobalElementVariable (Package package_name))) )

  | `SpecElementComponentType (component_type_name) ->
      ( var2expr ((GlobalElementVariable (ComponentType component_type_name))) )

  | `SpecElementPort (port_name) ->
      ( var2expr ((GlobalElementVariable (Port port_name))) )

  | `SpecElementLocalisation (spec_resource_constraints, spec_repository_constraints, local_specification) ->

      let exprs_to_sum = 
        List.map ( fun location_name ->
            
            let spec_resource_constraints_cstr =
              (translate_spec_resource_constraints location_name spec_resource_constraints)

            and spec_repository_constraints_cstr =
              (translate_spec_repository_constraints location_name spec_repository_constraints)


            and local_specification_cstr =
              (translate_local_specification location_name local_specification)

            in
            (reify (spec_resource_constraints_cstr &&~~ spec_repository_constraints_cstr &&~~ local_specification_cstr))

        ) location_names

      in
      (sum exprs_to_sum)


and translate_local_specification (location_name : location_name) (local_specification : local_specification) : cstr =
  match local_specification with
  | `SpecLocalTrue -> 
      truecstr

  | `SpecLocalOp (l_spec_local_expr, spec_op, r_spec_local_expr) ->
      ( (translate_spec_op spec_op) (translate_spec_local_expr location_name l_spec_local_expr) (translate_spec_local_expr location_name r_spec_local_expr) )

  | `SpecLocalAnd (l_local_specification, r_local_specification) ->
      ( (translate_local_specification location_name l_local_specification) &&~~ (translate_local_specification location_name r_local_specification) )

  | `SpecLocalOr (l_local_specification, r_local_specification) ->
      ( (translate_local_specification location_name l_local_specification) ||~~ (translate_local_specification location_name r_local_specification) )

  | `SpecLocalImpl (l_local_specification, r_local_specification) ->
      ( (translate_local_specification location_name l_local_specification) =>~~ (translate_local_specification location_name r_local_specification) )

  | `SpecLocalNot (local_specification) ->
      ( not (translate_local_specification location_name local_specification) )

and translate_spec_local_expr (location_name : location_name) (spec_local_expr : spec_local_expr) : expr =
  match spec_local_expr with
  | `SpecLocalExprVar spec_variable_name ->
      ( var2expr ( translate_spec_variable_name spec_variable_name ) )

  | `SpecLocalExprConst spec_const ->
      ( translate_spec_const spec_const )

  | `SpecLocalExprArity spec_local_element ->
      ( translate_spec_local_element location_name spec_local_element )

  | `SpecLocalExprAdd (l_spec_local_expr, r_spec_local_expr) ->
      ( (translate_spec_local_expr location_name l_spec_local_expr) +~ (translate_spec_local_expr location_name r_spec_local_expr) )

  | `SpecLocalExprSub (l_spec_local_expr, r_spec_local_expr) ->
      ( (translate_spec_local_expr location_name l_spec_local_expr) -~ (translate_spec_local_expr location_name r_spec_local_expr) )

  | `SpecLocalExprMul (spec_const, spec_local_expr) ->
      ( (translate_spec_const spec_const) *~ (translate_spec_local_expr location_name spec_local_expr) )


and translate_spec_local_element (location_name : location_name) (spec_local_element : spec_local_element) : expr =
  match spec_local_element with
  | `SpecLocalElementPackage (package_name) ->
      ( var2expr ((LocalElementVariable (location_name, (Package package_name)))) )

  | `SpecLocalElementComponentType (component_type_name) ->
      ( var2expr ((LocalElementVariable (location_name, (ComponentType component_type_name)))) )

  | `SpecLocalElementPort (port_name) ->
      ( var2expr ((LocalElementVariable (location_name, (Port port_name)))) )

and translate_spec_resource_constraints (location_name : location_name) (spec_resource_constraints : spec_resource_constraints) : cstr =
  List.fold_left (fun a spec_resource_constraint -> 
    (a &&~~ (translate_spec_resource_constraint location_name spec_resource_constraint)) 
  ) truecstr spec_resource_constraints

and translate_spec_resource_constraint (location_name : location_name) : (spec_resource_constraint -> cstr) =
  fun (resource_name, spec_op, spec_const) ->
    let op_function       = ( translate_spec_op spec_op )
    and resource_var_expr = ( var2expr ((LocalResourceVariable (location_name, resource_name))) )
    and spec_const_expr   = ( translate_spec_const spec_const )
    in
    ( op_function resource_var_expr spec_const_expr )

and translate_spec_repository_constraints (location_name : location_name) (spec_repository_constraints : spec_repository_constraints) : cstr =

  match spec_repository_constraints with
  
  (* The repository constraint is "_", which means that any repository should be accepted. *)
  | [] -> truecstr

  (* There is one or more repositories specified, exactly one of them must be present. *)
  | repository_names ->
  
    (* The left side expression: *)
    let exprs_to_sum = 
      List.map ( fun repository_name ->

        let local_repository_var = 
          (LocalRepositoryVariable (location_name, repository_name))
        in

        (* Part of the sum: R(location_name, repository_name) *)
        (var2expr local_repository_var)
          
      ) repository_names
    in
    let sum_of_local_repository_vars = (sum exprs_to_sum)

    (* The right side expression is a constant equal 1. *)

    in
    (* Constraint: *)
    ( sum_of_local_repository_vars =~ (int2expr 1) )

and translate_spec_op (spec_op : spec_op) : (expr -> expr -> cstr) =
  match spec_op with
  | `Lt  -> (  <~ )
  | `LEq -> ( <=~ )
  | `Eq  -> (  =~ )
  | `GEq -> ( >=~ )
  | `Gt  -> (  >~ )
  | `NEq -> ( <>~ )

let create_specification_constraints initial_configuration specification : cstr list =
  let location_names = get_location_names initial_configuration
  in
  [translate_specification location_names specification]





(* Extract all variable keys that appear in the specification. *)
(* TODO: Should it be in this module? *)

let rec extract_spec_variable_name spec_variable_name = [ SpecificationVariable spec_variable_name ]

and extract_spec_const (spec_const : spec_const) : variable list = []

and extract_specification (location_names : location_name list) (specification : specification) : variable list =
  match specification with
  | `SpecTrue -> []

  | `SpecOp (l_spec_expr, spec_op, r_spec_expr) ->
      ( (extract_spec_expr location_names l_spec_expr) @ (extract_spec_expr location_names r_spec_expr) )

  | `SpecAnd (l_specification, r_specification) ->
      ( (extract_specification location_names l_specification) @ (extract_specification location_names r_specification) )

  | `SpecOr (l_specification, r_specification) ->
      ( (extract_specification location_names l_specification) @ (extract_specification location_names r_specification) )

  | `SpecImpl (l_specification, r_specification) ->
      ( (extract_specification location_names l_specification) @ (extract_specification location_names r_specification) )

  | `SpecNot (specification) ->
      (extract_specification location_names specification)

and extract_spec_expr (location_names : location_name list) (spec_expr : spec_expr) : variable list =
  match spec_expr with
  | `SpecExprVar spec_variable_name ->
      ( extract_spec_variable_name spec_variable_name )

  | `SpecExprConst spec_const ->
      ( extract_spec_const spec_const )

  | `SpecExprArity spec_element ->
      ( extract_spec_element location_names spec_element )

  | `SpecExprAdd (l_spec_expr, r_spec_expr) ->
      ( (extract_spec_expr location_names l_spec_expr) @ (extract_spec_expr location_names r_spec_expr) )

  | `SpecExprSub (l_spec_expr, r_spec_expr) ->
      ( (extract_spec_expr location_names l_spec_expr) @ (extract_spec_expr location_names r_spec_expr) )

  | `SpecExprMul (spec_const, spec_expr) ->
      ( (extract_spec_const spec_const) @ (extract_spec_expr location_names spec_expr) )

and extract_spec_element  (location_names : location_name list) (spec_element : spec_element) : variable list =
  match spec_element with
  | `SpecElementPackage (package_name) ->
      [ GlobalElementVariable (Package package_name) ]

  | `SpecElementComponentType (component_type_name) ->
      [ GlobalElementVariable (ComponentType component_type_name) ]

  | `SpecElementPort (port_name) ->
      [ GlobalElementVariable (Port port_name) ]

  | `SpecElementLocalisation (spec_resource_constraints, spec_repository_constraints, local_specification) ->

      let variables_lists = 
        List.map ( fun location_name ->
            
            let spec_resource_constraints_cstr =
              (extract_spec_resource_constraints location_name spec_resource_constraints)

            and spec_repository_constraints_cstr =
              (extract_spec_repository_constraints location_name spec_repository_constraints)


            and local_specification_cstr =
              (extract_local_specification location_name local_specification)

            in
            (spec_resource_constraints_cstr @ spec_repository_constraints_cstr @ local_specification_cstr)

        ) location_names

      in
      (List.flatten variables_lists)


and extract_local_specification (location_name : location_name) (local_specification : local_specification) : variable list =
  match local_specification with
  | `SpecLocalTrue -> []

  | `SpecLocalOp (l_spec_local_expr, spec_op, r_spec_local_expr) ->
      ( (extract_spec_local_expr location_name l_spec_local_expr) @ (extract_spec_local_expr location_name r_spec_local_expr) )

  | `SpecLocalAnd (l_local_specification, r_local_specification) ->
      ( (extract_local_specification location_name l_local_specification) @ (extract_local_specification location_name r_local_specification) )

  | `SpecLocalOr (l_local_specification, r_local_specification) ->
      ( (extract_local_specification location_name l_local_specification) @ (extract_local_specification location_name r_local_specification) )

  | `SpecLocalImpl (l_local_specification, r_local_specification) ->
      ( (extract_local_specification location_name l_local_specification) @ (extract_local_specification location_name r_local_specification) )

  | `SpecLocalNot (local_specification) ->
      (extract_local_specification location_name local_specification)

and extract_spec_local_expr (location_name : location_name) (spec_local_expr : spec_local_expr) : variable list =
  match spec_local_expr with
  | `SpecLocalExprVar spec_variable_name ->
      ( extract_spec_variable_name spec_variable_name )

  | `SpecLocalExprConst spec_const ->
      ( extract_spec_const spec_const )

  | `SpecLocalExprArity spec_local_element ->
      ( extract_spec_local_element location_name spec_local_element )

  | `SpecLocalExprAdd (l_spec_local_expr, r_spec_local_expr) ->
      ( (extract_spec_local_expr location_name l_spec_local_expr) @ (extract_spec_local_expr location_name r_spec_local_expr) )

  | `SpecLocalExprSub (l_spec_local_expr, r_spec_local_expr) ->
      ( (extract_spec_local_expr location_name l_spec_local_expr) @ (extract_spec_local_expr location_name r_spec_local_expr) )

  | `SpecLocalExprMul (spec_const, spec_local_expr) ->
      ( (extract_spec_const spec_const) @ (extract_spec_local_expr location_name spec_local_expr) )


and extract_spec_local_element (location_name : location_name) (spec_local_element : spec_local_element) : variable list =
  match spec_local_element with
  | `SpecLocalElementPackage (package_name) ->
      [ GlobalElementVariable (Package package_name) ]

  | `SpecLocalElementComponentType (component_type_name) ->
      [ GlobalElementVariable (ComponentType component_type_name) ]

  | `SpecLocalElementPort (port_name) ->
      [ GlobalElementVariable (Port port_name) ]

and extract_spec_resource_constraints (location_name : location_name) (spec_resource_constraints : spec_resource_constraints) : variable list =
  List.fold_left (fun a spec_resource_constraint -> 
    (a @ (extract_spec_resource_constraint location_name spec_resource_constraint)) 
  ) [] spec_resource_constraints

and extract_spec_resource_constraint (location_name : location_name) : (spec_resource_constraint -> variable list) =
  fun (resource_name, spec_op, spec_const) ->
    let resource_var_expr = [ LocalResourceVariable (location_name, resource_name) ]
    and spec_const_expr   = ( extract_spec_const spec_const )
    in
    ( resource_var_expr @ spec_const_expr )

and extract_spec_repository_constraints (location_name : location_name) (spec_repository_constraints : spec_repository_constraints) : variable list =
  let repository_names = spec_repository_constraints
  in
  List.map ( fun repository_name ->
    LocalRepositoryVariable (location_name, repository_name)
  ) repository_names



let extract_variables_from_specification initial_configuration specification =
  let location_names = get_location_names initial_configuration
  in
  extract_specification location_names specification
