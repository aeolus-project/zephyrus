
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

val create_local_resource_constraints : bare_architecture -> universe -> cstr list

val create_resource_constraints : bare_architecture -> universe -> cstr list