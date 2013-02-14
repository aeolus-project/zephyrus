
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

val create_specification_constraints : configuration -> specification -> cstr list

val extract_variable_keys_from_specification : configuration -> specification -> variable_key list