
open Resource_types_t

open Facile
open Easy

open Typing_context

open Variable_keys

type variable = Facile.Var.Fd.t

(** Helper string_of function for FaCiLe variables. *)
val string_of_variable : variable -> string


type typing_context_variables

(** Get the variable corresponding to a given type and key. *)
val get_variable : typing_context_variables -> variable_key -> variable

(** Get a list of all variables corresponding to domain elements. *)
val domain_variables   : typing_context_variables -> variable list

(** Get a list of all variables corresponding to port names. *)
val port_variables     : typing_context_variables -> variable list

(** Get a list of all variables corresponding to bindings. *)
val binding_variables  : typing_context_variables -> variable list

(** Create a complete set of variables (for all elements in the domain and for all
    port names) for a given typing context. *)
val create_typing_context_variables : Typing_context.t -> typing_context_variables


val string_of_domain_variables    : typing_context_variables -> string
val string_of_port_variables      : typing_context_variables -> string
val string_of_binding_variables   : typing_context_variables -> string
val string_of_max_rest_variables  : typing_context_variables -> string


(* Solution *)
type solution = {
  solution_domain_elements : (domain_variable_key  * int) list;
  solution_bindings        : (binding_variable_key * int) list;
}

val get_solution       : typing_context_variables -> solution
val string_of_solution : solution -> string