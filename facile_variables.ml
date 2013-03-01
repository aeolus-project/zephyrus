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


open Aeolus_types_t

open Facile
open Easy

open Helpers
open Typing_context

open Variable_keys
open Variables
open Solution

(* Types *)

type facile_variable = Facile.Var.Fd.t

type facile_variables = (variable_key * facile_variable) list

(* Accessing *)

let get_facile_variable facile_variables variable_key =
  try
    List.assoc variable_key facile_variables
  with
  | _ -> 
    failwith (Printf.sprintf 
      "the requested %s %s does not exist!"
      (descr_of_variable_key  variable_key)
      (string_of_variable_key variable_key) )


let get_facile_variables facile_variables =
  List.map (fun (key, variable) -> variable) facile_variables


let filter_variables_by_key (predicate : variable_key -> bool) (facile_variables : facile_variables) =
  BatList.filter_map ( fun (key, variable) ->
    if predicate key
    then Some (variable)
    else None
  ) facile_variables

let get_global_element_variables   = filter_variables_by_key pred_global_element_variable
let get_local_element_variables    = filter_variables_by_key pred_local_element_variable
let get_binding_variables          = filter_variables_by_key pred_binding_variable
let get_local_repository_variables = filter_variables_by_key pred_local_repository_variable
let get_local_resource_variables   = filter_variables_by_key pred_local_resource_variable
let get_specification_variables    = filter_variables_by_key pred_specification_variable



(* Creating *)

(* Our variables' domain contains in theory all natural numbers. 
 * In practice we restrain it for some obvious reasons (infinite calculation is
 * difficult to perform in finite time) and some less obvious reasons (to avoid
 * encountering integer overflow problem when computing a sum of the upper
 * bounds of our variables). *)

let variable_max = 100000

let create_new_facile_variable var_kind var_name =
  (* Prepare the domain : *)
  let var_domain = 
    match var_kind with
    | NaturalVariable ->
        (* The natural numbers domain. *)
        Domain.interval 0 variable_max

    | BooleanVariable ->
        (* The boolean domain. *)
        Domain.boolean
  in
  (* Create the variable. *)
  (Facile.Var.Fd.create ~name:var_name var_domain)

let create_new_variable (kind : variable_kind) (key : variable_key) =
  let var_name = string_of_variable_key key in
  let new_var  = create_new_facile_variable kind var_name in
  (key, new_var)

let create_facile_variables universe configuration specification = 
  let variable_keys = get_variable_keys universe configuration specification
  in
  List.map (fun variable_key ->
    let variable_kind = variable_kind_of_variable_key variable_key
    in
    create_new_variable variable_kind variable_key
  ) variable_keys




(* Printing *)

let string_of_facile_variable =
  Helpers.string_of_printing_function (Facile.Var.Fd.fprint)

let string_of_variable_assoc_list key_variable_assoc_list string_of_key =
  let strings = 
    List.map (fun ( key, variable ) ->
  
      Printf.sprintf 
        "%s = %s" 
        (string_of_key key)
        (string_of_facile_variable variable)
  
    ) key_variable_assoc_list
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)

let string_of_facile_variables facile_variables =
  string_of_variable_assoc_list facile_variables string_of_variable_key

(* Extracting the solution from variables *)

let solution_of_facile_variables (facile_variables : facile_variables) = 
  let solution_of_key_variable_assoc_list key_variable_assoc_list =
    List.map (fun (key, variable) ->
      (key, Facile.Var.Fd.elt_value variable)
    ) key_variable_assoc_list
  in
  solution_of_key_variable_assoc_list facile_variables