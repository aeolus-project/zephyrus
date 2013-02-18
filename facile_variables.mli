
open Aeolus_types_t

open Facile
open Easy

open Variable_keys
open Solution


(* Types *)

type facile_variable = Facile.Var.Fd.t

type facile_variables = (variable_key * facile_variable) list


(* Accessing *)

val get_facile_variable : facile_variables -> variable_key -> facile_variable


val get_facile_variables : facile_variables -> facile_variable list

val get_global_element_variables   : facile_variables -> facile_variable list
val get_local_element_variables    : facile_variables -> facile_variable list
val get_binding_variables          : facile_variables -> facile_variable list
val get_local_repository_variables : facile_variables -> facile_variable list
val get_local_resource_variables   : facile_variables -> facile_variable list
val get_specification_variables    : facile_variables -> facile_variable list


(* Creating *)

val create_facile_variables : universe -> configuration -> specification -> facile_variables


(* Printing *)

val string_of_facile_variable : facile_variable -> string

val string_of_facile_variables : facile_variables -> string


(* Extracting the solution from variables *)

val solution_of_facile_variables : facile_variables -> solution