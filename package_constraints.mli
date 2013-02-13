
open Aeolus_types_j
open Typing_context
open Variable_keys
open Generic_constraints

val create_package_implementation_constraints : bare_architecture -> universe -> cstr list
val create_package_dependency_constraints     : bare_architecture -> universe -> cstr list
val create_package_conflict_constraints       : bare_architecture -> universe -> cstr list

val create_package_constraints : bare_architecture -> universe -> cstr list